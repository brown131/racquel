#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; main - Main module for the project
;;;;
;;;; Copyright (c) Scott Brown 2013

(require db json "keywords.rkt" "metadata.rkt" "mixin.rkt" "schema.rkt" "util.rkt"
         (for-syntax racket/syntax syntax/parse "stxclass.rkt"))
 
(provide data-class data-class* data-class? data-class-info data-object-state 
         gen-data-class make-data-object select-data-object select-data-objects save-data-object 
         insert-data-object update-data-object delete-data-object 
         get-column set-column! get-join json-data-class-mixin xml-data-class-mixin 
         default-table-name-normalizer default-column-name-normalizer default-join-name-normalizer
         set-odbc-dbsystem-type! (all-from-out "keywords.rkt"))

;;;; DATA CLASS DEFINITION
   

;;; Return the state of a data object.
(define (data-object-state obj)
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (get-field data-object-state obj))
       
;;; Primary key fields
(define (primary-key-fields cls)
  (let ([pkey (get-class-metadata primary-key cls)]) (if (list? pkey) (sort pkey string<? #:key symbol->string) (list pkey))))
   
;;; Set auto-increment id.
(define (set-autoincrement-id! con obj)
  (let* ([cls (object-class obj)]
         [auto-key (get-class-metadata autoincrement-key cls)])
    (when auto-key
      (dynamic-set-field! (get-class-metadata primary-key cls) obj
                          (query-value con (sql-autoincrement (dbsystem-type con) auto-key))))))

;;; Get the primary key from an object.
(define (get-primary-key obj)
 (map (lambda (f) (dynamic-get-field f obj)) (primary-key-fields (object-class obj))))

;;; Auto-increment key fields
(define (autoincrement-key-fields cls)
  (if (get-class-metadata autoincrement-key cls) (primary-key-fields cls) null))
    
;;; Columns without the autoincrement key
(define (savable-fields con cls)
  (sort (foldl (lambda (f l) 
           (if (member f (autoincrement-key-fields cls)) l (cons f l))) null (get-column-ids cls)) string<? #:key symbol->string))

;;; SQL where-clause for a key.
(define (key-where-clause-sql con cls key)
  (string-append " where " (sql-placeholder (string-join (map (lambda (f) (string-append (get-column-name f cls) "=?")) 
                                                              (if (list? key) key (list key))) " and ") (dbsystem-type con))))

;;; Insert SQL.
(define (insert-sql con cls)
  (let ([col-nms (map (lambda (f) (get-column-name f cls)) (savable-fields con cls))])
    (string-append "insert into " (get-class-metadata table-name cls)
                   " (" (string-join col-nms ", ") ")"
                   " values (" (sql-placeholder (string-join (make-list (length col-nms) "?") ", ") (dbsystem-type con)) ")")))

;;; Update SQL.
(define (update-sql con cls)
  (let ([key (primary-key-fields cls)]
        [values (sort (foldr (lambda (f l) (cons (string-append (get-column-name f cls) "=?") l)) 
                       null (savable-fields con cls)) string<?)])
    (sql-placeholder (string-append "update " (get-class-metadata table-name cls)
                                    " set " (string-join values ", ")
                                    " where " (string-join (map (lambda (f) (string-append (get-column-name f cls) "=?")) 
                                                                (if (list? key) key (list key))) " and ")) (dbsystem-type con))))

;;; Delete SQL.
(define (delete-sql con cls)
  (string-append "delete from " (get-class-metadata table-name cls)
                 (key-where-clause-sql con cls (primary-key-fields cls))))

;;; Define a data class.
(define-syntax (data-class stx)
  (syntax-parse stx 
    [(_ base-cls:id elem:data-class-element ...)
     (with-syntax ([cls-id (generate-temporary #'class-id-)])
       #'(let* ([m (new data-class-metadata%)])
           (unless (hash-has-key? *data-class-metadata* 'cls-id)
             elem.meta-expr ...
             (set-field! columns m (sort (append elem.col-defs ...) string<? #:key (lambda (k) (symbol->string (first k)))))
             (set-field! joins m (append elem.jn-defs ...))
             (hash-set! *data-class-metadata* 'cls-id m))
           (define-member-name cls-id (get-field class-id-key m))
           (define-member-name data-object-state (get-field state-key m))
           (class* base-cls (data-class<%>) 
             elem.cls-expr ...
             (field [cls-id #f]
                    [data-object-state 'new])
             (inspect #f)
             (define/public (set-data-join! con jn-fld jn-cls)
               (let* ([rows (append elem.jn-rows ...)])
                 (map (lambda (r) (let ([obj (new jn-cls)])
                                    (map (lambda (f v) (dynamic-set-field! f obj v)) 
                                         (get-column-ids jn-cls) (vector->list r))
                                    (define-member-name data-object-state (get-class-metadata state-key jn-cls))
                                    (set-field! data-object-state obj 'loaded)
                                    obj)) rows)))
             (define/private (base-data-class cls)
               (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info cls)])
                 (if (data-class? cls) (if sup-cls (base-data-class sup-cls) cls) cls)))
             (unless (get-field class (hash-ref *data-class-metadata* 'cls-id))
               (set-field! class (hash-ref *data-class-metadata* 'cls-id) (base-data-class this%))))))]))

;;; Define a data class with interfaces.
(define-syntax (data-class* stx)
  (syntax-parse stx 
    [(_ base-cls:id (i-face:id ...) elem:data-class-element ...) 
     (with-syntax ([cls-id (generate-temporary #'class-id-)])
       #'(let* ([m (new data-class-metadata%)])
           (unless (hash-has-key? *data-class-metadata* 'cls-id)
             elem.meta-expr ...
             (set-field! columns m (sort (append elem.col-defs ...) string<? #:key (lambda (k) (symbol->string (first k)))))
             (set-field! joins m (append elem.jn-defs ...))
             (hash-set! *data-class-metadata* 'cls-id m))
           (define-member-name cls-id (get-field class-id-key m))
           (define-member-name data-object-state (get-field state-key m))
           (class* base-cls (data-class<%> i-face ...) 
             elem.cls-expr ...
             (field [cls-id #f]
                    [data-object-state 'new])
             (inspect #f)
             (define/public (set-data-join! con jn-fld jn-cls)
               (let* ([rows (append elem.jn-rows ...)])
                 (map (lambda (r) (let ([obj (new jn-cls)])
                                    (map (lambda (f v) (dynamic-set-field! f obj v)) 
                                         (get-column-ids jn-cls) (vector->list r))
                                    (define-member-name data-object-state (get-class-metadata state-key jn-cls))
                                    (set-field! data-object-state obj 'loaded)
                                    obj)) rows)))
             (define/private (base-data-class cls)
               (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info cls)])
                 (if (data-class? cls) (if sup-cls (base-data-class sup-cls) cls) cls)))            
             (unless (get-field class (hash-ref *data-class-metadata* 'cls-id))
               (set-field! class (hash-ref *data-class-metadata* 'cls-id) (base-data-class this%))))))]))

;;; Get a data column.
(define-syntax-rule (get-column col obj) (get-field col obj))

;;; Set a data column.
(define-syntax-rule (set-column! col obj val) (set-field! col obj val))

;;; Get joined data objects. This will select the objects on first use.
(define-syntax (get-join stx)
  (syntax-case stx ()
    ([_ jn-fld obj con] 
     #'(begin
         (when (eq? (get-field jn-fld obj) #f)
           (let* ([jn-def (get-join-definition jn-fld (object-class obj))]     
                  [jn-cls-expr (join-definition-class jn-def)]
                  [jn-cls (cond [(class? jn-cls-expr) jn-cls-expr] 
                                [(symbol? jn-cls-expr) (get-class jn-cls-expr)])])
             (set-field! jn-fld obj (send obj set-data-join! con 'jn-fld jn-cls))
             (when (and (equal? (join-definition-cardinality jn-def) 'one-to-one) (> (length (get-field jn-fld obj)) 0))
               (set-field! jn-fld obj (first (get-field jn-fld obj))))))
         (get-field jn-fld obj)))))

;;; DATA CLASS GENERATION


;;; Get columns from the schema.
(define (get-schema-columns schema col-nm-norm)
  (let ([col-nms (foldl (lambda (f l) (if (member (schema-column f) l) l (cons (schema-column f) l))) null schema)])
    (sort (map (lambda (f) (list (string->symbol (col-nm-norm f)) #f f)) col-nms) 
          string<? #:key (lambda (k) (symbol->string (first k))))))

;;; RQL where-clause syntax for a key.
(define (key-where-clause-rql jn-cls jn-columns tbl-nm-norm col-nm-norm)
  (let ([jns (map (lambda (j) #`(= ('#,(string->symbol (tbl-nm-norm jn-cls)) #,(string->symbol (col-nm-norm j))) ?)) jn-columns)])
    (if (eq? (length jn-columns) 1) #`(where #,@jns) #`(where (and #,@jns)))))

;;; Get the cardinality of a join based on schema metadata.
(define (join-cardinality con schema-nm dbsys-type jn-tbl-nm jn-key) 
  (let* ([jn-schema (load-schema con schema-nm jn-tbl-nm #:db-system-type dbsys-type)]
         [row (findf (lambda (r) (equal? (schema-column r) (if (list? jn-key) (first jn-key) jn-key))) jn-schema)])
    (unless row (error 'join-cardinality "row not found for join table: ~a key: ~a" jn-tbl-nm jn-key))
    (if (equal? (schema-constraint-type row) "P") #''one-to-one #''one-to-many)))
 
;;; Join information from the schema.
(define (get-join-schema schema)
  (foldl (lambda (r l) 
           (if (and (not (sql-null? (schema-join-table r))) 
                    (or (equal? (schema-constraint-type r) "F") (equal? (schema-constraint-type r) "R")))
               (let ([jn-def (findf (lambda (m) (string=? (schema-constraint r) (first m))) l)]) 
                 (if jn-def (cons (append (take jn-def 4) (list (append (last jn-def) (list (schema-join-column r)))))
                                  (remove (first jn-def) l (lambda (a b) (string=? a (first b)))))
                     (cons (list (schema-constraint r)
                                 (schema-join-table r)
                                 (schema-join-column r) 
                                 (schema-column r) 
                                 (list (schema-join-column r))) l))) l)) null schema))

;;; Find primary key fields in a table schema.
(define (find-primary-key-fields schema col-nm-norm)
  (let ([pkey (map (lambda (v) (string->symbol (col-nm-norm (schema-column v))))
                   (filter (lambda (f) (if (string? (schema-constraint-type f)) (string=? (schema-constraint-type f) "P") #f))
                           schema))])
    (if (eq? (length pkey) 1) (first pkey) pkey)))
    
;;; Get the autoincrement key from the table schema.
(define (get-autoincrement-key schema dbsys-type) (let ([row (findf (lambda (f) (cond [(eq? dbsys-type 'postgresql) (string? (schema-autoincrement f))]
                                                                                      [(eq? dbsys-type 'oracle) (string? (schema-autoincrement f))]
                                                                                      [else (eq? (schema-autoincrement f) 1)])) schema)])
                                                    (if row (if (string? (schema-autoincrement row)) (schema-autoincrement row) #t) #f)))

;;; Get joins from the schema.
(define (get-schema-joins con schema-nm schema dbsys-type tbl-nm-norm join-nm-norm col-nm-norm)
  (map (lambda (j) (list (string->symbol (join-nm-norm (second j) (eval-syntax (join-cardinality con schema-nm dbsys-type (second j) (third j))))) 
                         #`'#,(string->symbol (tbl-nm-norm (second j)))
                         #'#:cardinality (join-cardinality con schema-nm dbsys-type (second j) (third j)) 
                         (key-where-clause-rql (second j) (last j) tbl-nm-norm col-nm-norm)
                         (string->symbol (col-nm-norm (fourth j))))) (get-join-schema schema)))

;;; Default name normalizer. Replaces underscores and mixed case with hyphens. Returns all lower case.
(define mixed-case-norm-regexp (regexp "([a-z])([A-Z])"))
(define (default-column-name-normalizer s) 
  (string-downcase (string-replace (regexp-replace* mixed-case-norm-regexp s "\\1-\\2")  "_" "-")))
(define (default-table-name-normalizer s) 
  (string-append (default-column-name-normalizer s) "%"))
(define (default-join-name-normalizer s (c 'one-to-many)) 
  (string-append (default-column-name-normalizer s) 
                 (if (eq? c 'one-to-many) (if (string=? (substring s (- (string-length s) 1) (string-length s)) "s") "es" "s") "")))

;;; Generate a class using database schema information.
(define (gen-data-class con tbl-nm 
                        #:db-system-type (dbsys-type (dbsystem-type con))
                        #:generate-joins? (gen-joins? #t)
                        #:generate-reverse-joins? (gen-rev-joins? #f)
                        #:schema-name (schema-nm #f)
                        #:inherits (base-cls 'object%)
                        #:table-name-normalizer (tbl-nm-norm (lambda (n) (default-table-name-normalizer n))) 
                        #:column-name-normalizer (col-nm-norm (lambda (n) (default-column-name-normalizer n))) 
                        #:join-name-normalizer (join-nm-norm (lambda (n (c 'one-to-many)) (default-join-name-normalizer n c))) 
                        #:table-name-externalizer (tbl-nm-extern (lambda (n) (begin n)))
                        #:print? (prnt? #f)
                        . rest) 
  (let* ([schema (load-schema con schema-nm tbl-nm #:reverse-join? gen-rev-joins? #:db-system-type dbsys-type)]
         [cls-nm (string->symbol (tbl-nm-norm tbl-nm))]
         [pkey (find-primary-key-fields schema col-nm-norm)]
         [jns (if (or gen-joins? gen-rev-joins?)
                  (get-schema-joins con schema-nm schema dbsys-type tbl-nm-norm join-nm-norm col-nm-norm) null)]
         [auto-key (get-autoincrement-key schema dbsys-type)]
         [stx #`(let ([#,cls-nm
                       (data-class #,base-cls
                                   (table-name #,tbl-nm #,(tbl-nm-extern tbl-nm))
                                   #,(append '(column) (get-schema-columns schema col-nm-norm))
                                   #,(if auto-key (list 'primary-key pkey '#:autoincrement auto-key) (list 'primary-key pkey))
                                   #,(if (and gen-joins? (list? jns) (> (length jns) 0)) (append '(join) jns) '(begin #f))
                                   (super-new)
                                   #,@rest
                                   )])
                  #,cls-nm)])
    (if prnt? (syntax->datum stx) (eval-syntax stx racquel-namespace))))


;;;; PERSISTENCE


;;; Define a global table holding data objects.
(define *data-objects* (make-multi-hash #:weak? #t))

;;; Set the data in a data object.
(define (set-data-object! obj row)
  (map (lambda (f v) (dynamic-set-field! f obj v)) 
       (get-column-ids (object-class obj)) (vector->list row))
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (set-field! data-object-state obj 'loaded))

;;; Create a primary key from a database row.
(define (create-primary-key cls row)
  (let* ([pkey-flds (primary-key-fields cls)]
         [pkey (foldl (lambda (r c l) (if (memf (lambda (k) (equal? k (get-column-id c cls))) pkey-flds) (cons r l) l)) 
                     null (vector->list row) (sort (get-column-names cls) string<?))])
    (when (eq? (length pkey) 1) (set! pkey (first pkey)))
    pkey))

;;; Create a data object from a database row.
(define (create-data-object con cls row #:primary-key (primary-key #f))
  (let ([pkey (if primary-key primary-key (create-primary-key cls row))])
    (if (multi-hash-has-key? *data-objects* con cls pkey) (multi-hash-ref *data-objects* con cls pkey)
        (let* ([obj (new cls)])
          (set-data-object! obj row)
          (multi-hash-set! *data-objects* obj con cls pkey)
          obj))))

;;; Select a data object from the database.
(define-syntax (select-data-object stx)
  (syntax-parse stx
    #:literals (join where)
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) join-expr:join-expr where-expr:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let ([sql (select-sql con cls (string-append join-expr.expr ... where-expr.expr ...))])
           (if prnt? sql (create-data-object con cls (query-row con sql rest ...)))))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) where-expr:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let ([sql (select-sql con cls (string-append where-expr.expr ...))])
           (if prnt? sql (create-data-object con cls (query-row con sql rest ...)))))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) where-expr:expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let ([sql (select-sql con cls where-expr)])
           (if prnt? sql (create-data-object con cls (query-row con sql rest ...)))))]))

;;; Select data objects from the database.
(define-syntax (select-data-objects stx)
  (syntax-parse stx
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)))
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let ([sql (select-sql con cls "")])
           (if prnt? sql (map (lambda (r) (create-data-object con cls r)) (query-rows con sql)))))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) join-expr:join-expr where-expr:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let ([sql (select-sql con cls (string-append join-expr.expr ... where-expr.expr ...))])
           (if prnt? sql (map (lambda (r) (create-data-object con cls r)) (query-rows con sql rest ...)))))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) where-expr:where-expr)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let ([sql (select-sql con cls (string-append where-expr.expr ...))])
           (if prnt? sql (map (lambda (r) (create-data-object con cls r)) (query-rows con sql)))))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) where-expr:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let ([sql (select-sql con cls (string-append where-expr.expr ...))])
           (if prnt? sql (map (lambda (r) (create-data-object con cls r)) (query-rows con sql rest ...)))))]
    ))

;;; Load a data object from the database by primary key.
(define-syntax-rule (make-data-object con cls pkey) 
  (create-data-object con cls 
    (query-row con (select-sql con cls (key-where-clause-sql con cls (primary-key-fields cls))) pkey) #:primary-key pkey))

;;; Save a data object.
(define-syntax-rule (save-data-object con obj) 
  (if (eq? (data-object-state obj) 'new) (insert-data-object con obj) 
      (update-data-object con obj)))

;;; Insert a data object
(define-syntax-rule (insert-data-object con obj) 
  (let* ([cls (object-class obj)]
         [sql (insert-sql con cls)]
         [flds (map (lambda (f) (dynamic-get-field f obj)) (savable-fields con cls))])
    (apply query-exec con sql flds)
    (set-autoincrement-id! con obj)
    (define-member-name data-object-state (get-class-metadata state-key cls))
    (set-field! data-object-state obj 'saved)
    (multi-hash-set! *data-objects* obj con cls (get-primary-key obj))))

;;; Update a data object.
(define-syntax-rule (update-data-object con obj) 
  (let* ([cls (object-class obj)]
         [sql (update-sql con cls)]
         [flds (map (lambda (f) (dynamic-get-field f obj)) (savable-fields con cls))]
         [pkey (map (lambda (f) (dynamic-get-field f obj)) (primary-key-fields cls))])
    (apply query-exec con sql (append flds pkey))
    (define-member-name data-object-state (get-class-metadata state-key cls))
    (set-field! data-object-state obj 'saved)))

;;; Delete a data object.
(define-syntax-rule (delete-data-object con obj) 
  (let ([sql (delete-sql con (object-class obj))]
        [pkey (map (lambda (f) (dynamic-get-field f obj)) 
                   (primary-key-fields (object-class obj)))])
    (apply query-exec con sql pkey)
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'deleted)))

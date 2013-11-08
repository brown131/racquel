#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; main - Main module for the project
;;;;
;;;; Copyright (c) Scott Brown 2013

(require db json "keywords.rkt" "metadata.rkt" "mixin.rkt" "schema.rkt" "util.rkt"
         (for-syntax syntax/parse "stxclass.rkt"))
 
(provide data-class data-class* data-class? data-class-info data-object-state gen-data-class 
         make-data-object select-data-object select-data-objects save-data-object 
         insert-data-object update-data-object delete-data-object 
         get-join get-column set-column! json-data-class-mixin xml-data-class-mixin
         (all-from-out "keywords.rkt"))


;;;; DATA CLASS DEFINITION
   
;;; Return the state of a data object.
(define (data-object-state obj)
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (get-field data-object-state obj))
     
;;; Set autoincrement id.
(define (set-autoincrement-id! con obj)
  (when (get-class-metadata autoincrement-key (object-class obj))
    (dynamic-set-field! (get-class-metadata autoincrement-key (object-class obj)) obj
                        (query-value con (sql-autoincrement (dbsystem-type con))))))

;;; Primary key fields
(define (primary-key-fields cls)
  (let ([pkey (get-class-metadata primary-key cls)]) (if (list? pkey) (sort pkey string<? #:key symbol->string) (list pkey))))

;;; Autoincrement key fields
(define (autoincrement-key-fields cls)
  (let ([pkey (get-class-metadata autoincrement-key cls)]) (if (list? pkey) (sort pkey string<? #:key symbol->string) (list pkey))))
    
;;; Columns without the autoincrement key
(define (savable-fields con cls)
  (sort (foldl (lambda (f l) 
           (if (member f (autoincrement-key-fields cls)) l (cons f l))) null (get-column-ids cls)) string<? #:key symbol->string))

;;; SQL where-clause for a key.
(define (key-where-clause con cls key)
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
                 (key-where-clause con cls (primary-key-fields cls))))

;;; Define a data class.
(define-syntax (data-class stx)
  (syntax-parse stx 
    [(_ base-cls:id elem:data-class-element ...) 
     #'(let* ([m (new data-class-metadata%)])
         (define-member-name data-object-state (get-field state-key m))
         (class* base-cls (data-class<%>) elem.expr ...
           (field [data-object-state 'new])
           (unless (hash-has-key? *data-class-metadata* this%)
             (set-field! columns m (sort (append elem.col-defs ...) string<? #:key (lambda (k) (symbol->string (first k)))))
             (set-field! joins m (append elem.jn-defs ...))
             (hash-set! *data-class-metadata* this% m))
           (define/public (set-data-join! con jn-fld jn-cls)
             (let* ([col-nms (sort (get-column-names jn-cls) string<?)]                          
                    [rows (append elem.jn-rows ...)]
                    [objs (make-list (length rows) (new jn-cls))])
               (map (lambda (o r) (map (lambda (f v) (dynamic-set-field! f o v)) 
                                       (get-column-ids (object-class o)) (vector->list r))
                      (define-member-name data-object-state (get-class-metadata state-key (object-class o)))
                      (set-field! data-object-state o 'loaded)) objs rows)
               objs))
           this))]))

;;; Define a data class with interfaces.
(define-syntax (data-class* stx)
  (syntax-parse stx 
    [(_ base-cls:id (i-face:id ...) elem:data-class-element ...) 
     #'(let* ([m (new data-class-metadata%)])
         (define-member-name data-object-state (get-field state-key m))
         (class* base-cls (data-class<%> i-face ...) elem.expr ...
           (field [data-object-state 'new])
           (unless (hash-has-key? *data-class-metadata* this%)
             (set-field! columns m (append elem.col-defs ...))
             (set-field! joins m (append elem.jn-defs ...))
             (hash-set! *data-class-metadata* this% m))
           (define/public (set-data-join! con jn-fld jn-cls)
             (let* ([col-nms (sort (get-column-names jn-cls) string<?)]                          
                    [rows (append elem.jn-rows ...)]
                    [objs (make-list (length rows) (new jn-cls))])
               (map (lambda (o r) (map (lambda (f v) (dynamic-set-field! f o v)) 
                                       (get-column-ids (object-class o)) (vector->list r))
                      (define-member-name data-object-state (get-class-metadata state-key (object-class o)))
                      (set-field! data-object-state o 'loaded)) objs rows)
              rows)); objs))
           this))]))

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
                  [jn-cls-expr (second jn-def)]
                  [jn-cls (cond [(class? jn-cls-expr) jn-cls-expr]
                                [(symbol? jn-cls-expr) (get-class jn-cls-expr)])])             
             (set-field! jn-fld obj (send obj set-data-join! con 'jn-fld jn-cls))
             (when (and (equal? (third jn-def) 'one-to-one) (> (length (get-field jn-fld obj)) 0))
               (set-field! jn-fld obj (first (get-field jn-fld obj))))))
         (get-field jn-fld obj)))))



;;; DATA CLASS GENERATION


;;; Define namespace anchor.
(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

;;; Get columns from the schema.
(define (get-schema-columns schema col-nm-norm)
  (let ([col-nms (foldl (lambda (f l) (if (member (vector-ref f 0) l) l (cons (vector-ref f 0) l))) null schema)])
    (sort (map (lambda (f) (list (string->symbol (col-nm-norm f)) #f f)) col-nms) 
          string<? #:key (lambda (k) (symbol->string (first k))))))

;;; Get joins from the schema.
;;; TODO: NEEDS TO SUPPORT MULTI-PART KEYS
(define (get-schema-joins con schema-nm schema dbsys-type join-nm-norm col-nm-norm gen-joins? gen-rev-joins?)
  (let ([jn-cardinality (lambda (jn-tbl-nm jn-key) 
                          (let* ([jn-schema (load-schema con schema-nm jn-tbl-nm #:reverse-join? gen-rev-joins? #:db-system-type dbsys-type)]
                                 [row (findf (lambda (r) (equal? (vector-ref r 0) jn-key)) jn-schema)])
                                 (if (equal? (vector-ref row 1) "P") #''one-to-one #''one-to-many)))])
    (if (or gen-joins? gen-rev-joins?) 
        (foldl (lambda (r l) (let ([jn-cls (get-table-class (vector-ref r 4))])
                               (if (or (equal? (vector-ref r 1) "F") (equal? (vector-ref r 1) "R"))
                                   (cons (list (string->symbol (join-nm-norm (vector-ref r 4))) 
                                               #`'#,(get-class-name jn-cls)
                                               #'#:cardinality (jn-cardinality (vector-ref r 4) (vector-ref r 5))
                                               #`(where (= (#,(get-class-name jn-cls) #,(string->symbol (vector-ref r 5))) ?)) 
                                               (string->symbol (col-nm-norm (vector-ref r 0))))
                                         l) l)))
               null schema) null)))

;;; Find primary key fields in a table schema.
(define (find-primary-key-fields schema)
  (let ([pkey (map (lambda (v) (string->symbol (vector-ref v 0)) )
                   (filter (lambda (f) (if (string? (vector-ref f 1)) (string=? (vector-ref f 1) "P") #f))
                           schema))])
    (if (eq? (length pkey) 1) (first pkey) pkey)))
    
;;; Find autoincrement key in the table schema.
(define (has-autoincrement-key? schema) (vector? (findf (lambda (f) (eq? (vector-ref f 3) 1)) schema)))
  
;;; Default name normalizer. Replaces underscores and mixed case with hyphens. Returns all lower case.
(define mixed-case-norm-regexp (regexp "([a-z])([A-Z])"))
(define (default-name-normalizer s) 
  (string-downcase (string-replace (regexp-replace* mixed-case-norm-regexp s "\\1-\\2")  "_" "-")))

;;; Generate a class using database schema information.
(define (gen-data-class con tbl-nm 
                        #:db-system-type (dbsys-type (dbsystem-type con))
                        #:generate-joins? (gen-joins? #t)
                        #:generate-reverse-joins? (gen-rev-joins? #f)
                        #:schema-name (schema-nm #f)
                        #:inherits (base-cls 'object%)
                        #:table-name-normalizer (tbl-nm-norm (lambda (n) (string-append (default-name-normalizer n) "%"))) 
                        #:column-name-normalizer (col-nm-norm (lambda (n) (default-name-normalizer n))) 
                        #:join-name-normalizer (join-nm-norm (lambda (n) (default-name-normalizer n))) 
                        #:table-name-externalizer (tbl-nm-extern (lambda (n) (begin n)))
                        #:print? (prnt? #f)
                        . rest) 
  (let* ([schema (load-schema con schema-nm tbl-nm #:reverse-join? gen-rev-joins? #:db-system-type dbsys-type)]
         [cls-nm (string->symbol (tbl-nm-norm tbl-nm))]
         [pkey (find-primary-key-fields schema)]
         [jns (get-schema-joins con schema-nm schema dbsys-type join-nm-norm col-nm-norm gen-joins? gen-rev-joins?)]
         [stx #`(let ([#,cls-nm
                       (data-class #,base-cls
                                   (table-name #,tbl-nm #,(tbl-nm-extern tbl-nm))
                                   #,(append '(column) (get-schema-columns schema col-nm-norm))
                                   #,(if (has-autoincrement-key? schema) (list 'primary-key pkey '#:autoincrement #t)
                                         (list 'primary-key pkey))
                                   #,(if (and gen-joins? (list? jns) (> (length jns) 0)) (append '(join) jns) '(begin #f))
                                   (super-new)
                                   (inspect #f)
                                   #,@rest
                                   )])
                  #,cls-nm)])
    (if prnt? (syntax->datum stx) (eval-syntax stx ns))))


;;;; PERSISTENCE


;;; Set the data in a data object.
(define (set-data-object! obj row)
  (map (lambda (f v) (dynamic-set-field! f obj v)) 
       (get-column-ids (object-class obj)) (vector->list row))
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (set-field! data-object-state obj 'loaded))

;;; Select a data object from the database.
(define-syntax (select-data-object stx)
  (syntax-parse stx
    #:literals (join where)
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) join-expr:join-expr where-expr:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let* ([obj (new cls)]
                [sql (select-sql con cls (string-append join-expr.expr ... where-expr.expr ...))])
           (unless prnt? (set-data-object! obj (query-row con rest ...)))
           (if prnt? sql obj)))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) where-expr:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let* ([obj (new cls)]
                [sql (select-sql con cls (string-append where-expr.expr ...))])
           (unless prnt? (set-data-object! obj (query-row con sql rest ...)))
           (if prnt? sql obj)))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) where-expr:expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let* ([obj (new cls)]
                [sql (select-sql con cls where-expr)])
           (unless prnt? (set-data-object! obj (query-row con sql rest ...)))
           (if prnt? sql obj)))]))

;;; Select data objects from the database.
(define-syntax (select-data-objects stx)
  (syntax-parse stx
    [(_ con:id cls:id where-expr:where-expr rest:expr ...)
     #'(let* ([rows (query-rows con (select-sql con cls where-expr.expr ...) rest ...)]
              [objs (make-list (length rows) (new cls))])
         (map (lambda (o r) (set-data-object! o r)) objs rows)
         objs)]
    [(_ con:id cls:id where-expr:expr rest:expr ...)
     #'(let* ([rows (query-rows con (select-sql con cls where-expr) rest ...)]
              [objs (make-list (length rows) (new cls))])
         (map (lambda (o r) (set-data-object! o r)) objs rows)
         objs)]))

;;; Load a data object from the database by primary key.
(define-syntax-rule (make-data-object con cls pkey)
  (let* ([obj (new cls)])
    (set-data-object! obj (query-row con (select-sql con cls (key-where-clause con cls (primary-key-fields cls))) pkey))
    obj
    ))

;;; Save a data object.
(define-syntax-rule (save-data-object con obj) 
  (if (eq? (data-object-state obj) 'new) (insert-data-object con obj) 
      (update-data-object con obj)))

;;; Insert a data object
(define-syntax-rule (insert-data-object con obj) 
  (let ([sql (insert-sql con (object-class obj))]
        [flds (map (lambda (f) (dynamic-get-field f obj)) 
                   (savable-fields con (object-class obj)))])
    (apply query-exec con sql flds)
    (set-autoincrement-id! con obj)
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'saved)
    ))

;;; Update a data object.
(define-syntax-rule (update-data-object con obj) 
  (let ([sql (update-sql con (object-class obj))]
        [flds (map (lambda (f) (dynamic-get-field f obj)) 
                   (savable-fields con (object-class obj)))]
        [pkey (map (lambda (f) (dynamic-get-field f obj)) 
                   (primary-key-fields (object-class obj)))])
    (apply query-exec con sql (append flds pkey))
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'saved)))

;;; Delete a data object.
(define-syntax-rule (delete-data-object con obj) 
  (let ([sql (delete-sql con (object-class obj))]
        [pkey (map (lambda (f) (dynamic-get-field f obj)) 
                   (primary-key-fields (object-class obj)))])
    (apply query-exec con sql pkey)
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'deleted)))

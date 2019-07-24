#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; metadata - Data class metadata 

;;; Define namespace anchor.
(define-namespace-anchor racquel-namespace-anchor)
(define racquel-namespace (namespace-anchor->namespace racquel-namespace-anchor))

(require "schema.rkt")

(provide (all-defined-out))

;;; Define a global hash table holding data class metadata.
(define *data-class-metadata* (make-hash))

;;; Define data class metadata class.
(define data-class-metadata% 
  (class object% 
    (field [class #f]
           [class-id-key (generate-member-key)]
           [state-key (generate-member-key)]
           [table-name #f] 
           [columns null] 
           [joins null]
           [primary-key #f] 
           [autoincrement-key #f]
           [external-name #f])
    (super-new)
    (inspect #f)))

;;; Get metadata for a class.
(define (get-class-metadata-object cls)
  (if (class? cls)
      (let ([md (findf (λ (v) (equal? (get-field class v) cls)) 
                       (hash-values *data-class-metadata*))])
        (if md md
          (let ([md-pair (findf (λ (p) 
                                  (if (get-field class (cdr p)) #f                            
                                      (eval-syntax #`(with-handlers ([exn:fail? (λ (e) #f)])
                                                       (define-member-name 
                                                         #,(car p) (get-field class-id-key #,(cdr p)))
                                                       (class-field-accessor #,cls #,(car p))) 
                                                   racquel-namespace))) 
                                (hash->list *data-class-metadata*))])
            (if md-pair (cdr md-pair) #f))))
      (raise-argument-error 'get-class-metadata-object "argument ~a is not a class" cls)))

;;; Set metadata for a class.
(define (set-class-metadata-object! cls)
  (if (class? cls)
      (let ([md-pair (findf (λ (p)
                              (if (get-field class (cdr p)) #f                            
                                  (eval-syntax #`(with-handlers ([exn:fail? (λ (e) #f)])
                                                   (define-member-name 
                                                     #,(car p) (get-field class-id-key #,(cdr p)))
                                                   (class-field-accessor #,cls #,(car p))) 
                                               racquel-namespace))) 
                            (hash->list *data-class-metadata*))])
        (when md-pair (set-field! class (cdr md-pair) cls)))
      (raise-argument-error 'set-class-metadata-object! "argument ~a is not a class" cls)))

;;; Find a class metadata by class name.
(define (find-class-name-metadata cls-name)
  (findf (λ (v) (let ([cls (get-field class v)])
                       (if cls (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) 
                                             (class-info cls)])
                                 (equal? cls-nm (if (string? cls-name) 
                                                    (string->symbol cls-name) cls-name))) #f))) 
         (hash-values *data-class-metadata*)))

;;; Find a class metadata by external name.
(define (find-external-name-metadata ext-name)
  (findf (λ (v) (equal? (if (string? ext-name) ext-name (symbol->string ext-name))
                        (get-field external-name v))) (hash-values *data-class-metadata*))) 

;;; Get a class from the metadata by name or symbol.
(define (get-class cls-name)
  (if (class? cls-name) cls-name (get-field class (find-class-name-metadata cls-name))))

;;; Get a class from the metadata by name or symbol.
(define (get-class-name cls)
    (if (class? cls) (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) 
                                   (class-info cls)]) cls-nm)
      (raise-argument-error 'get-class-name "argument ~a is not a class" cls)))

;;; Get a data class metadata field.
(define-syntax-rule (get-class-metadata fld cls)
  (if (class? cls) (get-field fld (get-class-metadata-object cls)) 
      (error 'get-class-metadata "argument ~a is not a class. field: ~a" cls 'fld)))

;;; Set a data class metadata field.
(define (set-class-metadata! id cls val)
  (set-field! id (get-class-metadata-object cls) val))

;;; Dynamically get a data class metadata field.
(define (dynamic-get-class-metadata id cls)
  (dynamic-get-field id (get-class-metadata-object cls)))

;;; Dynamically set a data class metadata field.
(define (dynamic-set-class-metadata! id cls val)
  (dynamic-set-field! id (get-class-metadata-object cls) val))

;;; Return info about a data class.
(define (data-class-info cls)
  (if (class? cls) (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) 
                                 (class-info data-class-metadata%)])
                     (apply values (map (λ (f) (dynamic-get-class-metadata f cls)) fld-nms)))
      (raise-argument-error 'data-class-info "argument ~a is not a class" cls)))

;;; Get a list of column ids for a class.
(define (get-column-ids cls)
  (if (class? cls) (map first (get-class-metadata columns cls))
      (raise-argument-error 'get-column-ids "argument ~a is not a class" cls)))

;;; Get a list of column names for a class.
(define (get-column-names cls)
  (if (class? cls) (map second (get-class-metadata columns cls))
      (raise-argument-error 'get-column-names "argument ~a is not a class" cls)))
    
;;; Get the column name for a column field in a class.
(define (get-column-name f cls)
  (if (class? cls) (let ([col-def (findf (λ (c) (equal? f (first c))) 
                                         (get-class-metadata columns cls))])
                     (if col-def (second col-def) 
                         (error (format "column name for id ~a class ~a not found" f cls))))
      (raise-argument-error 'get-column-name "argument ~a is not a class" cls)))

;;; Get a column name from the context list.
(define (get-column-name-from-context col-fld ctxt dbsys-type)
    (let ([col-nm (for*/first ([cls-nm ctxt]
                               [col-def (get-class-metadata columns (get-class cls-nm))]
                               #:when (equal? col-fld (first col-def)))
                    (second col-def))])
      (if col-nm (sql-escape col-nm dbsys-type) (format "~a" col-fld))))

;;; Get the column id for a column name in a class.
(define (get-column-id col-nm cls)
    (if (class? cls) (let ([col-def (findf (λ (c) (equal? col-nm (second c))) 
                                           (get-class-metadata columns cls))])
                       (if col-def (first col-def) 
                           (error (format "column id for name ~a class ~a not found" col-nm cls))))
      (raise-argument-error 'get-column-id "argument ~a is not a class" cls)))

;;; Join definition accessors.
(define (join-definition-name jn-def) (first jn-def))
(define (join-definition-class jn-def) (second jn-def))
(define (join-definition-cardinality jn-def) (third jn-def))
(define (join-definition-where-clause jn-def) (fourth jn-def))

;;; Get a join definition.
(define-syntax-rule (get-join-definition jn-fld cls)
  (if (class? cls) (findf (λ (f) (eq? 'jn-fld (first f))) (get-class-metadata joins cls))
      (raise-argument-error 'get-join-definition "argument ~a is not a class" cls)))


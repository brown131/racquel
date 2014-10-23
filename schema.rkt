#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; schema - Set database-specific SQL for various platforms.
;;;;
;;;; Copyright (c) Scott Brown 2013
;;;;
;;;; This file is part of Racquel
;;;;
;;;; Racquel is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require db)

(provide (all-defined-out))

;;; Set placeholders in a SQL string by database system type.
(define (sql-placeholder sql dbsys-type (i 1)) 
  (if (eq? dbsys-type 'postgresql) 
      (let ([new-sql (string-replace sql "?" (~a "$" i) #:all? #f)]) 
        (if (equal? sql new-sql) sql (sql-placeholder new-sql dbsys-type (+ i 1))))
      sql))

;;; Set auto-increment value retrieval SQL string by database system type.
(define (sql-autoincrement dbsys-type (seq #f)) 
    (cond [(eq? dbsys-type 'mysql) "select last_insert_id()"]
          [(eq? dbsys-type 'postgresql) (string-append "select currval('" seq "')")]
          [(eq? dbsys-type 'sqlite3) "select last_insert_rowid()"]
          [(eq? dbsys-type 'sqlserver) "select @@identity"]
          [(eq? dbsys-type 'oracle) (string-append "select " seq ".currval from dual")]
          [(eq? dbsys-type 'db2) "select identity_val_local() as lastid from sysibm.sysdummy1"]
          [else (error 'sql-autoincrement "auto-increment not defined for database system ~a" 
                       dbsys-type)]))

;;; Schema accessors.
(define (schema-autoincrement row) (vector-ref row 3))
(define (schema-column row) (vector-ref row 0))
(define (schema-constraint row) (vector-ref row 6))
(define (schema-constraint-type row) (vector-ref row 1))
(define (schema-join-column row) (vector-ref row 5))
(define (schema-join-table row) (vector-ref row 4))
(define (schema-ordinal-position row) (vector-ref row 2))

;;; Load MySQL schema.
(define (load-mysql-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, 
   substring(cons.constraint_type, 1, 1) as constraint_type, fkey.ordinal_position, 
   case when cols.extra='auto_increment' then 1 end, fkey.referenced_table_name, 
   fkey.referenced_column_name, cons.constraint_name
from information_schema.columns as cols
left join information_schema.key_column_usage as fkey
   on fkey.column_name=cols.column_name
   and fkey.table_name=cols.table_name
   and fkey.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
   on cons.constraint_name=fkey.constraint_name
   and cons.constraint_schema=fkey.constraint_schema
   and cons.table_name=fkey.table_name
   and cons.table_schema=fkey.table_schema
where cols.table_name='" tbl-nm "'")])
    (when schema-nm 
      (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.referenced_column_name, 'F', fkey.ordinal_position, 
   case when cols.extra='auto_increment' then 1 end, cols.table_name, cols.column_name, 
   cons.constraint_name
from information_schema.columns as cols
left join information_schema.key_column_usage as fkey
   on fkey.column_name=cols.column_name
   and fkey.table_name=cols.table_name
   and fkey.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
   on cons.constraint_name=fkey.constraint_name
   and cons.constraint_schema=fkey.constraint_schema
   and cons.table_name=fkey.table_name
   and cons.table_schema=fkey.table_schema
where fkey.referenced_table_name='" tbl-nm "'"))
             (when schema-nm (set! schema-sql 
                                   (string-append schema-sql " and fkey.referenced_table_schema='" 
                                                  schema-nm "'")))))
    (set! schema-sql (string-append schema-sql 
                                    " order by constraint_name, ordinal_position, column_name"))
    (query-rows con schema-sql)))

;;; Load PostgreSQL schema.
(define (load-postgresql-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name as sortname, 
  substring(cons.constraint_type, 1, 1) as constraint_type, keycols.ordinal_position, 
  cols.column_default, fkey.table_name, fkey.column_name, cons.constraint_name
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name=cols.column_name
  and keycols.table_name=cols.table_name
  and keycols.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
  on cons.constraint_name=keycols.constraint_name
  and cons.constraint_schema=cons.constraint_schema
left join information_schema.referential_constraints as refs
  on  refs.constraint_schema = cons.constraint_schema
  and refs.constraint_name = cons.constraint_name
left join information_schema.key_column_usage as fkey
  on fkey.constraint_schema = refs.unique_constraint_schema
  and fkey.constraint_name = refs.unique_constraint_name
where (cons.constraint_type is null or cons.constraint_type <> 'UNIQUE') 
  and cols.table_name='" tbl-nm "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql 
                                                    " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.column_name, 'F', fkey.ordinal_position, 
  cols.column_default, cols.table_name, cols.column_name, refs.constraint_name 
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name=cols.column_name
  and keycols.table_name=cols.table_name
  and keycols.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
  on cons.constraint_name=keycols.constraint_name
  and cons.constraint_schema=cons.constraint_schema
left join information_schema.referential_constraints as refs
  on  refs.constraint_schema = cons.constraint_schema
  and refs.constraint_name = cons.constraint_name
left join information_schema.key_column_usage as fkey
  on fkey.constraint_schema = refs.unique_constraint_schema
  and fkey.constraint_name = refs.unique_constraint_name
where fkey.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql 
                                                      " and fkey.table_schema='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by 7 nulls first, 3, 1"))
    (let ([rows (query-rows con schema-sql)])
      (when (eq? (length rows) 0) 
        (error 'load-postgres-schema "No schema found for table ~a owner ~a\n~a" 
               tbl-nm schema-nm schema-sql))    
             (map (lambda (r) 
                    (for/vector ([i (in-range 0 (vector-length r))])
                      (let ([val (vector-ref r i)])
                        (if (and (eq? i 3) (string? val)) 
                            (let ([match (regexp-match #px"(?i:nextval)\\(\\s*'(\\w+)'" val)])
                              (if match (second match) sql-null)) val)))) rows))
      ))

;;; Load SQLite3 schema.
(define (load-sqlite3-schema con schema-nm tbl-nm rev-jn?)
  (let ([tbl-pragma (query-rows con (string-append "pragma table_info('" tbl-nm "')"))]
        [fk-pragma 
         (with-handlers ([exn:fail? (lambda (e) null)])
           (query-rows con (string-append "pragma foreign_key_list('" tbl-nm "')")))]
        [rev-jn-schema 
         (if rev-jn? 
             (let ([tbls (query-rows con "select * from sqlite_master where type='table'")])
               (foldl (lambda (r l) 
                        (if (equal? (vector-ref r 2) tbl-nm) l
                            (let* ([rev-tbl-pragma (with-handlers ([exn:fail? (lambda (e) null)])
                                                     (query-rows con 
                                                                 (string-append 
                                                                  "pragma foreign_key_list('" 
                                                                  (vector-ref r 2) "');")))]
                                   [rfkp (findf (lambda (f) (equal? (vector-ref f 2) tbl-nm)) 
                                                rev-tbl-pragma)])
                              (if rfkp (let ([row (make-vector 7)])
                                         (vector-set! row 0 (vector-ref rfkp 4))
                                         (vector-set! row 1 "F")
                                         (vector-set! row 2 1)
                                         (vector-set! row 3 sql-null)
                                         (vector-set! row 4 (vector-ref r 2))
                                         (vector-set! row 5 (vector-ref rfkp 3))
                                         (vector-set! row 6 (string-append tbl-nm "_" 
                                                                           (vector-ref rfkp 4) 
                                                                           "_fkey"))
                                         (cons row l)) l)))) null tbls)) null)])
    (append (map (lambda (tblp) 
                   (let ([row (make-vector 7)]
                         [fkp (findf (lambda (f) (equal? (vector-ref tblp 1) (vector-ref f 3))) 
                                     fk-pragma)])
                     (vector-set! row 0 (vector-ref tblp 1))
                     (vector-set! row 1 (if (eq? (vector-ref tblp 5) 1) "P" (if fkp "F" sql-null)))
                     (vector-set! row 2 (vector-ref tblp 0))
                     (vector-set! row 3 (query-value con 
                                                     (string-append 
                                                      "select exists(select * from sqlite_sequence 
where name='" tbl-nm "')")))
                     (vector-set! row 4 (if fkp (vector-ref fkp 2) sql-null))
                     (vector-set! row 5 (if fkp (vector-ref fkp 4) sql-null))
                     (vector-set! row 6 (if fkp (string-append tbl-nm "_" (vector-ref fkp 3) 
                                                               "_fkey") sql-null))
                     row)) tbl-pragma) rev-jn-schema)))

;;; Load SQL Server schema.
(define (load-sqlserver-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, substring(cons.constraint_type,1,1), 
  keycols.ordinal_position, 
  case when columnproperty(object_id(cols.table_name), cols.column_name, 'isidentity')=1 then 1 end,
  fkey.table_name, fkey.column_name, cons.constraint_name
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name=cols.column_name
  and keycols.table_name=cols.table_name
  and keycols.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
  on cons.constraint_name=keycols.constraint_name
  and cons.constraint_schema=cons.constraint_schema
left join information_schema.referential_constraints as refs
  on  refs.constraint_schema = cons.constraint_schema
  and refs.constraint_name = cons.constraint_name
left join information_schema.key_column_usage as fkey
  on fkey.constraint_schema = refs.unique_constraint_schema
  and fkey.constraint_name = refs.unique_constraint_name
where cols.table_name='" tbl-nm "'")])
    (when schema-nm (set! schema-sql 
                          (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.column_name, 'F', fkey.ordinal_position, 
   case when columnproperty(object_id(cols.table_name), cols.column_name, 'isidentity')=1 then 1 end, 
   cols.table_name, cols.column_name, cons.constraint_name
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name=cols.column_name
  and keycols.table_name=cols.table_name
  and keycols.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
  on cons.constraint_name=keycols.constraint_name
  and cons.constraint_schema=cons.constraint_schema
left join information_schema.referential_constraints as refs
  on  refs.constraint_schema = cons.constraint_schema
  and refs.constraint_name = cons.constraint_name
left join information_schema.key_column_usage as fkey
  on fkey.constraint_schema = refs.unique_constraint_schema
  and fkey.constraint_name = refs.unique_constraint_name
where fkey.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql 
                            (string-append schema-sql " and fkey.table_schema='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql 
                                    " order by constraint_name, ordinal_position, cols.column_name"))
    (query-rows con schema-sql)))

;;; Load Oracle schema
(define (load-oracle-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, cons.constraint_type,
   cc.position, null, rcons.table_name, rcc.column_name, cons.constraint_name
from all_tab_cols cols
left outer join all_cons_columns cc
   on cols.owner=cc.owner
   and cols.table_name=cc.table_name
   and cols.column_name=cc.column_name
left outer join all_constraints cons
   on cc.constraint_name=cons.constraint_name
   and cols.owner=cons.owner
   and cons.constraint_type in ('P','R')
left outer join all_constraints rcons
   on cons.r_constraint_name=rcons.constraint_name
   and cons.r_owner=rcons.owner 
left outer join all_cons_columns rcc
   on rcons.constraint_name=rcc.constraint_name
   and rcons.owner=rcc.owner
   and rcons.table_name=rcc.table_name
where cols.table_name='" (string-upcase tbl-nm) "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.owner='" 
                                                    (string-upcase schema-nm) "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select rcc.column_name, 'F', cc.position, 
   null, cols.table_name, cols.column_name, cons.constraint_name
from all_tab_cols cols
join all_cons_columns cc
   on cols.owner=cc.owner
   and cols.table_name=cc.table_name
   and cols.column_name=cc.column_name
left outer join all_constraints cons
   on cc.constraint_name=cons.constraint_name
   and cols.owner=cons.owner
   and cons.constraint_type in ('P','R')
left outer join all_constraints rcons
   on cons.r_constraint_name=rcons.constraint_name
   and cons.r_owner=rcons.owner 
left outer join all_cons_columns rcc
   on rcons.constraint_name=rcc.constraint_name
   and rcons.owner=rcc.owner
   and rcons.table_name=rcc.table_name
where rcons.table_name='" (string-upcase tbl-nm) "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and rcons.owner='" 
                                                      (string-upcase schema-nm) "'"))))
    (set! schema-sql (string-append schema-sql " order by 7, 3, 1"))
    (let ([rows (query-rows con schema-sql)])
      (when (eq? (length rows) 0) 
        (error 'load-oracle-schema  "No schema found for table ~a owner ~a\n~a" tbl-nm schema-nm 
               schema-sql))      
      (foldl (lambda (r l) 
               (if (findf (lambda (rl) (and (equal? (vector-ref rl 0) (vector-ref r 0)) 
                                            (equal? (vector-ref r 1) sql-null))) l) l (cons r l))) 
             null 
             (map (lambda (r) 
                    (for/vector ([i (in-range 0 (vector-length r))])
                       (let ([val (vector-ref r i)])
                         (if (and (member i '(0 4 5 6)) (string? val)) (string-downcase val)
                             (if (and (eq? i 3) (equal? (vector-ref r 1) "P")) 
                                 (let ([trg-sql (string-append "select trigger_body from all_triggers 
where table_name='" (string-upcase tbl-nm) "'
and triggering_event='INSERT'
and status='ENABLED'")])
                                   (when schema-nm (set! schema-sql 
                                                         (string-append trg-sql " and owner='" 
                                                                        (string-upcase schema-nm) 
                                                                        "'")))                      
                                   (let* ([trg-body (query-rows con trg-sql)]
                                          [match (if (eq? (length trg-body) 1) 
                                                     (regexp-match 
                                                      #px"([a-zA-Z0-9_$#]+)\\.(?i:nextval)"
                                                      (vector-ref (first trg-body) 0)) #f)])
                                     (if match (second match) sql-null))) val))))) rows))
    )))

;;; Load DB/2 schema.
(define (load-db2-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.colname, cons.type, fkey.colseq,
  case when cols.identity = 'Y' then 1 end, 
  case when cons.type = 'F' then refs.reftabname end,
  case when cons.type = 'F' then rcols.colname end,
  fkey.constname
from syscat.columns cols
left outer join syscat.keycoluse fkey
  on cols.tabname=fkey.tabname
  and cols.tabschema=fkey.tabschema 
  and cols.colname=fkey.colname
left outer join syscat.tabconst cons
  on fkey.constname=cons.constname
  and fkey.tabname=cons.tabname
  and fkey.tabschema=cons.tabschema 
left outer join syscat.references refs
  on cols.tabname=refs.tabname
  and cols.tabschema=refs.tabschema
left outer join syscat.keycoluse rcols
  on rcols.constname=refs.refkeyname
  and rcols.tabname=refs.reftabname
  and rcols.tabschema=refs.tabschema 
where cols.tabname='" (string-upcase tbl-nm) "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.tabschema='"
                                                    (string-upcase schema-nm) "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select rcols.colname, 'F', rcols.colseq,
  case when cols.identity = 'Y' then 1 end, 
  cols.tabname,
  cols.colname,
  cons.constname
from syscat.columns cols
join syscat.keycoluse fkey
  on cols.tabname=fkey.tabname
  and cols.tabschema=fkey.tabschema 
  and cols.colname=fkey.colname
join syscat.tabconst cons
  on fkey.constname=cons.constname
  and fkey.tabname=cons.tabname
  and fkey.tabschema=cons.tabschema 
join syscat.references refs
  on cols.tabname=refs.tabname
  and cols.tabschema=refs.tabschema
  and cons.constname=refs.constname
join syscat.keycoluse rcols
  on rcols.constname=refs.refkeyname
  and rcols.tabname=refs.reftabname
  and rcols.tabschema=refs.tabschema 
where rcols.tabname='" (string-upcase tbl-nm) "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and rcols.tabschema='" 
                                                      (string-upcase schema-nm) "'"))))
    (set! schema-sql (string-append schema-sql " order by 7, 3, 1"))
    (let ([rows (query-rows con schema-sql)])
      (when (eq? (length rows) 0) 
        (error 'load-db2-schema  "No schema found for table ~a owner ~a\n~a" tbl-nm schema-nm 
               schema-sql))      
             (map (lambda (r) 
                    (for/vector ([i (in-range 0 (vector-length r))])
                      (let ([val (vector-ref r i)])
                        (if (and (member i '(0 4 5 6)) (string? val)) (string-downcase val) val)))) 
                  rows))
    ));)

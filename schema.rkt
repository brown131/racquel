#lang racket
;;;;
;;;; schema - Set database-specific SQL for various platforms.
;;;;
;;;; Copyright (c) Scott Brown 2013

(require db)

(provide (all-defined-out))

;;; Set placeholders in a SQL string by database system type.
(define (sql-placeholder sql dbsys-type (i 1)) 
  (if (eq? dbsys-type 'postgresql) 
      (let ([new-sql (string-replace sql "?" (~a "$" i) #:all? #f)]) 
        (if (equal? sql new-sql) sql (sql-placeholder new-sql dbsys-type (+ i 1))))
      sql))

;;; Set autoincrement value retrieval SQL string by database system type.
(define (sql-autoincrement dbsys-type) 
    (cond [(eq? dbsys-type 'mysql) "select last_insert_id()"]
          [(eq? dbsys-type 'postgresql) "select currval('auto_id_seq')"]
          [(eq? dbsys-type 'sqlserver) "select @@identity"]
          [else (error "autocrement not defined for this database")]))

;;; Load MySQL schema.
(define (load-mysql-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, substring(cons.constraint_type, 1, 1) as constraint_type, fkey.ordinal_position, 
   case when cols.extra='auto_increment' then 1 end, fkey.referenced_table_name, fkey.referenced_column_name, cons.constraint_name
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
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.referenced_column_name, 'R', fkey.ordinal_position, 
   case when cols.extra='auto_increment' then 1 end, cols.table_name, cols.column_name, cons.constraint_name
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
             (when schema-nm (set! schema-sql (string-append schema-sql " and fkey.referenced_table_schema='" schema-nm "'")))))
    (set! schema-sql (string-append schema-sql " order by constraint_name, ordinal_position, column_name"))
    (query-rows con schema-sql)))

;;; Load Oracle schema
(define (load-oracle-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, cons.constraint_type
   cc.position, null, rcons.table_name, rcc.column_name, cons.constraint_name
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
where cols.table_name='" tbl-nm "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.owner='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select rcc.column_name, 'R', cc.position, 
   case when cols.extra='auto_increment' then 1 end, cols.table_name, cols.column_name, cons.constraint_name
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
where rcons.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and rcols.owner='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by constraint_name, ordinal_position, column_name"))
    (query-rows con schema-sql)))

;;; Load PostgreSQL schema.
(define (load-postgresql-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, substring(cons.constraint_type, 1, 1) as constraint_type, keycols.ordinal_position, 
  case when substring(cols.column_default, 1, 7) = 'nextval' then 1 end, fkey.table_name, fkey.column_name, cons.constraint_name
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
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.column_name, 'R', fkey.ordinal_position, 
   case when substring(cols.column_default from 1 for 6) = 'nextval' then 1 end, cols.table_name, cols.column_name, cons.constraint_name
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
where fkey.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and fkey.table_schema='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by constraint_name, ordinal_position, cols.column_name"))
    (query-rows con schema-sql)))

;;; Load SQLite3 schema.
(define (load-sqlite3-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "pragma table_info(" tbl-nm ");")])
    (query-rows con schema-sql)))

;;; Load SQL Server schema.
(define (load-sqlserver-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, cons.constraint_type, keycols.ordinal_position, 
  case when columnproperty(object_id(table_name), column_nam, 'isidentity')=1 then 1 end,
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
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.column_name, 'R', fkey.ordinal_position, 
   case when columnproperty(object_id(table_name), column_nam, 'isidentity')=1 then 1 end, 
   cols.table_name, cols.column_name, cons.constraint_name
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
where fkey.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and fkey.table_schema='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by constraint_name, ordinal_position, column_name"))
    (query-rows con schema-sql)))

;;; Load default schema.
(define (load-default-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name as col_name, cons.constraint_type, keycols.ordinal_position, null,
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
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.column_name, 'R', fkey.ordinal_position, null, 
   cols.table_name, cols.column_name, cons.constraint_name
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
where fkey.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and fkey.table_schema='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by constraint_name, ordinal_position, col_name"))
    (query-rows con schema-sql)))

/* Racquel - An ORM for Racket

db2 - Test schema for DB/2
*/

/* DDL */

create table "simple" (
  "id" int not null,
  "name" varchar(30) default null,
  "description" varchar(500) default null,
  "x" decimal(10,2) not null,
  constraint pk_simple primary key ("id")
);

create table "auto" (
  "id" int not null generated always as identity (start with 1 increment by 1),
  "name" varchar(30) default null,
  "description" varchar(500) default null,
  constraint pk_auto primary key ("id")
);

create table "multipartkey" (
  "simple_id" int not null,
  "auto_id" int not null,
  "name" varchar(30) default null,
  "description" varchar(500) default null,
  constraint pk_multipartkey primary key ("simple_id", "auto_id"),
  constraint multipartKey_simple_id_fkey foreign key ("simple_id") references "simple" ("id"),
  constraint multipartkey_auto_id_fkey foreign key ("auto_id") references "auto" ("id")
);

create table "person" (
  "id" int not null,
  "first_name" varchar(30) default null,
  "last_name" varchar(30) default null,
  "age" int default null,
  constraint pk_person primary key ("id")
);

create table "address" (
  "id" int not null generated always as identity (start with 1 increment by 1),
  "person_id" int not null,
  "line" varchar(30) default null,
  "city" varchar(30) default null,
  "state" varchar(30) default null,
  "zip_code" varchar(30) default null,
  constraint pk_address primary key ("id"),
  constraint address_person_id_fkey foreign key ("person_id") references "person" ("id")
);
 
/* DML */

insert into "simple" ("id", "name", "description", "x") 
values (1, 'join test', 'join test', 2.1);

insert into "simple" ("id", "name", "description", "x")
values (2, 'join test 2', 'join test 2', 3);

insert into "auto" ("name", "description") 
values ('join test', 'join test');

insert into "multipartkey" ("simple_id", "auto_id", "name", "description")
values (1, 1, 'multi', 'multi part key');

insert into "person" ("id", "first_name", "last_name", "age") 
values (1, 'john', 'smith', 25);

insert into "address" ("person_id", "line", "city", "state", "zip_code") 
values (1, '123 Main Street', 'Chicago', 'IL', '60606');

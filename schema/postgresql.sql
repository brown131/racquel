/* Test schema for PostgreSQL */

/* DDL */

create table simple (
  id int not null,
  name varchar(30) default null,
  description varchar(500) default null,
  x real not null,
  primary key (id)
);

create sequence auto_id_seq;

create table auto (
  id int not null default nextval('auto_id_seq'),
  name varchar(30) default null,
  description varchar(500) default null,
  primary key (id)
);

create table multipartKey (
  simple_id int not null references simple (id),
  auto_id int not null references auto (id),
  primary key (simple_id, auto_id),
  unique (simple_id)
);

create table person (
  id int not null,
  first_name varchar(30) default null,
  last_name varchar(30) default null,
  age int default null,
  primary key (id)
);
  
create sequence address_id_seq;

create table address (
  id int not null default nextval('address_id_seq'),
  person_id int not null references person (id),
  line varchar(30) default null,
  city varchar(30) default null,
  state varchar(30) default null,
  zip_code varchar(30) default null,
  primary key (id),
  unique (person_id)
);
  
/* DML */

insert into simple (id, name, description, x)
values (1, 'join test', 'join test', 2.1);

insert into auto (name, description)
values ('join test', 'join test');

insert into multipartkey (simple_id, auto_id)
values (1, 1);

insert into person (id, first_name, last_name, age)
values (1, 'john', 'smith', 25);

insert into address (person_id, line, city, state, zip_code)
values (1, '123 Main Street', 'Chicago', 'IL', '60606');


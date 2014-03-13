/* Racquel - An ORM for Racket

db2 - Test schema for PostgreSQL

Copyright (c) Scott Brown 2013

This file is part of Racquel

Racquel is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/* DDL */

create user test with password 'test';

create table simple (
  id int not null,
  name varchar(30) default null,
  description varchar(500) default null,
  x decimal(10,2) not null,
  primary key (id)
);
grant all privileges on table simple to test;

create sequence auto_id_seq;
create table auto (
  id int not null default nextval('auto_id_seq'),
  name varchar(30) default null,
  description varchar(500) default null,
  primary key (id)
);
grant all privileges on table address to test;
grant all privileges on sequence auto_id_seq to test;

create table multipartkey (
  simple_id int not null references simple (id),
  auto_id int not null references auto (id),
  primary key (simple_id, auto_id),
  unique (simple_id)
);
grant all privileges on table multipartkey to test;

create table person (
  id int not null,
  first_name varchar(30) default null,
  last_name varchar(30) default null,
  age int default null,
  primary key (id)
);
grant all privileges on table person to test;

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
grant all privileges on table person to test;
grant all privileges on sequence address_id_seq to test;

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


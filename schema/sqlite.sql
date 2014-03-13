/* Racquel - An ORM for Racket

db2 - Test schema for SQLite

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

create table simple (
  id integer primary key,
  name varchar(30) default null,
  description varchar(500) default null,
  x decimal(10,2) not null
);

create table auto (
  id integer primary key autoincrement,
  name varchar(30) default null,
  description varchar(500) default null
);

create table if not exists multipartkey
  simple_id integer not null,
  auto_id integer not null,
  primary key (simple_id, auto_id),
  constraint multipartkey_simple_id_fkey foreign key (simple_id) references simple (id),
  constraint multipartkey_auo_id_fkey foreign key (auto_id) references auto (id)
);

create table if not exists person (
  id integer primary key,
  first_name varchar(30) default null,
  last_name varchar(30) default null,
  age int default null
);
  
create table if not exists address (
  id integer primary key autoincrement,
  person_id int not null,
  line varchar(30) default null,
  city varchar(30) default null,
  state varchar(30) default null,
  zip_code varchar(30) default null,
  constraint address_person_id_fkey foreign key (person_id) references person (id)
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


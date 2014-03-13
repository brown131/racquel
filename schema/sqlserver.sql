/* Racquel - An ORM for Racket

db2 - Test schema for SQL Server

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
  id int primary key not null,
  [name] varchar(30) default null,
  description varchar(500) default null,
  x int not null
)

create table [auto] (
  id int identity primary key not null,
  [name] varchar(30) default null,
  description varchar(500) default null
);

create table multipartkey (
  simple_id int not null,
  auto_id int not null,
  primary key (simple_id, auto_id),
  constraint multipartKey_simple_id_fkey foreign key (simple_id) references simple (id),
  constraint multipartkey_auto_id_fkey foreign key (auto_id) references auto (id)
);

create table person (
  id int primary key not null,
  first_name varchar(30) default null,
  last_name varchar(30) default null,
  age int default null
);
  
create table address (
  id int identity primary key not null,
  person_id int not null,
  line varchar(30) default null,
  city varchar(30) default null,
  state varchar(30) default null,
  zip_code varchar(30) default null,
  constraint address_person_id_fkey foreign key (person_id) references person (id)
);
  
/* DML */

insert simple (id, [name], description, x)
values (1, 'join test', 'join test', 2.1);

insert [auto] ([name], description)
values ('join test', 'join test');

insert multipartkey (simple_id, auto_id)
values (1, 1);

insert person (id, first_name, last_name, age)
values (1, 'john', 'smith', 25);

insert address (person_id, line, city, state, zip_code)
values (1, '123 Main Street', 'Chicago', 'IL', '60606');


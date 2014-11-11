/* Racquel - An ORM for Racket

db2 - Test schema for MySQL

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
  id int(11) not null,
  `name` varchar(30) default null,
  description varchar(500) default null,
  x decimal(10,2) not null,
  primary key (id)
) engine=InnoDB auto_increment=1 default charset=latin1;

create table auto (
  id int(11) not null auto_increment,
  `name` varchar(30) default null,
  description varchar(500) default null,
  primary key (id)
) engine=InnoDB auto_increment=1 default charset=latin1;

create table multipartkey (
  simple_id int(11) not null,
  auto_id int(11) not null,
  `name` varchar(30) default null,
  description varchar(500) default null,
  primary key (simple_id, auto_id),
  key multipartkey_simple_id_key (simple_id),
  constraint multipartKey_simple_id_fkey foreign key (simple_id) references simple (id),
  constraint multipartkey_auto_id_fkey foreign key (auto_id) references auto (id)
) engine=InnoDB default charset=latin1;

create table person (
  id int(11) not null,
  first_name varchar(30) default null,
  last_name varchar(30) default null,
  age int(11) default null,
  primary key (id)
) engine=InnoDB default charset=latin1;
  
create table address (
  id int(11) not null auto_increment,
  person_id int(11) not null,
  line varchar(30) default null,
  city varchar(30) default null,
  state varchar(30) default null,
  zip_code varchar(30) default null,
  primary key (id),
  key address_person_id_key (person_id),
  constraint address_person_id_fkey foreign key (person_id) references person (id)
) engine=InnoDB default charset=latin1;
  
/* DML */

insert simple (id, `name`, description, x)
values (1, 'join test', 'join test', 2.1);

insert simple (id, `name`, description, x)
values (2, 'join test 2', 'join test 2', 3);

insert auto (`name`, description)
values ('join test', 'join test');

insert multipartkey (simple_id, auto_id, `name`, description)
values (1, 1, 'multi', 'multi part key');

insert person (id, first_name, last_name, age)
values (1, 'john', 'smith', 25);

insert address (person_id, line, city, state, zip_code)
values (1, '123 Main Street', 'Chicago', 'IL', '60606');

/* Test schema for MySQL */

/* DDL */

create table simple (
  id int(11) not null,
  `name` varchar(30) default null,
  description varchar(500) default null,
  x real not null,
  primary key (id)
) engine=InnoDB auto_increment=1 default charset=latin1;

create table auto (
  id int(11) not null auto_increment,
  `name` varchar(30) default null,
  description varchar(500) default null,
  primary key (id)
) engine=InnoDB auto_increment=1 default charset=latin1;

create table if not exists multipartKey (
  simple_id int(11) not null,
  auto_id int(11) not null,
  primary key (simple_id, auto_id),
  key k_simple_id_multipartKey (simple_id),
  constraint fk_simple_id_multipartKey foreign key (simple_id) references simple (id),
  constraint fk_auto_id_multipartKey foreign key (auto_id) references auto (id)
) engine=InnoDB default charset=latin1;

create table if not exists `person` (
  `id` int(11) not null,
  `first_name` varchar(30) default null,
  `last_name` varchar(30) default null,
  `age` int(11) default null,
  primary key (id)
) engine=InnoDB default charset=latin1;
  
create table if not exists `address` (
  `id` int(11) not null auto_increment,
  `person_id` int(11) not null,
  `line` varchar(30) default null,
  `city` varchar(30) default nullL,
  `state` varchar(30) default null,
  `zip_code` varchar(30) default null,
  primary key (id)
  key `fk_person_id_address` (`person_id`),
  constraint `fk_person_id_address` foreign key (`person_id`) references `person` (`id`)
) engine=InnoDB default charset=latin1;
  
/* DML */

insert simple (id, `name`, description, x)
values (1, 'join test', 'join test', 2.1)

insert auto (`name`, description)
values ('join test', 'join test')

insert multipartkey (simple_id, auto_id)
values (1, 1)

insert person (id, `first_name`, last_name, age)
values (1, 'john', 'smith', 25)

insert address (person_id, line, city, state, zip_code)
values (1, '123 Main Street', 'Chicago', 'IL', '60606')

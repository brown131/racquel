/* Test schema for MySQL */

create table simple (
  id int(11) not null,
  `name` varchar(30) default null,
  description varchar(500) default null,
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

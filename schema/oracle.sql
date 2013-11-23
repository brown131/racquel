/* Test schema for Oracle */

/* Create Database */
create user test
identified by test;
default tablespace users
temporary tablespace temp
quota 10M on users;

grant connect to test;
grant resource to test;
grant create session to test;
grant create table to test;
grant create view to test;

connect test/test

/* DDL */

create table simple (
	id number(*,0) not null enable, 
	name varchar2(30) default null, 
	description varchar2(500) default null, 
	x number(10,2) not null enable, 
	primary key (id) enable
);
grant all privileges on table simple to test;

create sequence auto_id_seq;
create table auto (
	id number(*,0) not null enable, 
	name varchar2(30) default null, 
	description varchar2(500) default null, 
	primary key (id) enable
);
grant all privileges on table auto to test;
create or replace trigger auto_id_trg 
before insert on auto 
for each row
begin
  select auto_id_seq.nextval into :new.id from dual;
end;
/
alter trigger auto_id_trg enable;

create table multipartkey (
	simple_id number(*,0) not null enable, 
	auto_id number(*,0) not null enable, 
	primary key (simple_id, auto_id) enable, 
	unique (simple_id) enable
);
alter table multipartkey add foreign key (simple_id) references simple (id) enable;
alter table multipartkey add foreign key (auto_id) references auto (id) enable;
grant all privileges on table multipartkey to test;

create table person (
	id number(*,0) not null enable, 
	first_name varchar2(30) default null, 
	last_name varchar2(30) default null, 
	age number(*,0) default null, 
	primary key (id) enable
);
grant all privileges on table person to test;

create sequence address_id_seq;
create table address (
	id number not null enable, 
	person_id number not null enable, 
	line varchar2(30), 
	city varchar2(30), 
	state varchar2(30), 
	zip_code varchar2(30), 
	constraint address_pk primary key (id) enable
);
alter table address add constraint address_person_id_fkey foreign key (person_id)
	  references  person (id) on delete cascade enable;
grant all privileges on table address to test;
create or replace trigger address_id_trg
before insert on address              
for each row  
begin   
  if :new.id is null then 
    select address_id_seq.nextval into :new.id from dual; 
  end if; 
end; 
/
alter trigger address_id_trg enable;

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


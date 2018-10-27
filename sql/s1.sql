BEGIN TRANSACTION;

insert into persons (id, name) values (1, 'n1');
insert into persons (id, name) values (2, 'n2');
insert into persons (id, name) values (3, 'n3');

COMMIT TRANSACTION;

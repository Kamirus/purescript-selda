BEGIN TRANSACTION;

insert into people (id, name) values (1, 'nejm1');
insert into people (id, name) values (2, 'nejm2');
insert into people (id, name) values (3, 'nejm3');

COMMIT TRANSACTION;

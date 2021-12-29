create database if not exists lb_kiirotori character set utf8mb4 COLLATE utf8mb4_general_ci;
use lb_kiirotori;

create table if not exists authorized (
    id varchar(33) not null comment "user id (U[0-9a-f]{32}), group id (C[0-9a-f]{32}), room id (R[0-9a-f]{32})",
    type tinyint unsigned not null comment "id type: user (0), group (1) or room (2)",
    created_at datetime not null comment "authorized datetime",
    group_name varchar(80) comment "group name", -- 4 bytes * 20 characters
    authed_user_name varchar(80) comment "name that performed the authentication", -- 4 bytes * 20 characters
    primary key (id, type)
) engine=InnoDB default charset=utf8mb4;


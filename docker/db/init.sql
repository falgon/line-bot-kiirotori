create databse if not exists line_bot_kiirotori character set utf8mb4 COLLATE utf8mb4_general_ci;
use line_bot_kiirotori;

create table if not exists authorized_ids (
    authorized_id text not null comment "id",
    authorized_id_type tinyint unsigned not null comment "id type: userId (0), groupId (1) or roomId (2)",
    authorized_datetime datetime not null comment "authorized datetime",
    user_name varchar(80) comment "username that performed the authentication", -- 4 bytes * 20 characters
    group_name varchar(80) comment "group name", -- 4 bytes * 20 characters
    primary key (authorized_id, authorized_id_type)
) engine=InnoDB default charset=utf8mb4;


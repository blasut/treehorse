CREATE TABLE fields (
    id          serial PRIMARY KEY,
    type        varchar(40) NOT NULL,
    name        varchar(50) NOT NULL,
    values      varchar(200),
    timestamp timestamp default current_timestamp
);

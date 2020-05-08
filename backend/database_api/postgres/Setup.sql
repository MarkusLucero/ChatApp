Create TABLE users(
user_id BIGSERIAL NOT NULL PRIMARY KEY,
username VARCHAR(60) NOT NULL UNIQUE,
password VARCHAR(200) NOT NULL,
timestamp TIMESTAMP NOT NULL);

CREATE TABLE groups(
group_id BIGSERIAL NOT NULL PRIMARY KEY,
groupname VARCHAR(60) NOT NULL);

CREATE TABLE group_users(
group_id BIGINT NOT NULL,
user_id BIGINT NOT NULL,
username VARCHAR(60) NOT NULL,
CONSTRAINT group_users_fk1 FOREIGN KEY (group_id) REFERENCES groups(group_id),
CONSTRAINT group_users_fk2 FOREIGN KEY (user_id) REFERENCES users(user_id));

CREATE TABLE messages(
message_id BIGSERIAL NOT NULL PRIMARY KEY,
username VARCHAR(60) NOT NULL,
groupname VARCHAR(60),
message TEXT,
timestamp TIMESTAMP NOT NULL,
status SMALLINT,
user_id BIGINT,
group_id BIGINT,
CONSTRAINT message_fk1 FOREIGN KEY (user_id) REFERENCES users(user_id),
CONSTRAINT message_fk2 FOREIGN KEY (group_id) REFERENCES groups(group_id));

CREATE TABLE friendlist(
user_id BIGINT,
friend_id BIGINT,
username VARCHAR(60) NOT NULL,
friendname VARCHAR(60) NOT NULL,
status SMALLINT,
CONSTRAINT friendlist_fk1 FOREIGN KEY (user_id) REFERENCES users(user_id),
CONSTRAINT friendlist_fk2 FOREIGN KEY (friend_id) REFERENCES users(user_id));

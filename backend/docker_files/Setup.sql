DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

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

CREATE TABLE servers(
server_id BIGSERIAL PRIMARY KEY,
servername VARCHAR(120),
threadlist BIGINT,
timestamp TIMESTAMP
/*CONSTRAINT servers_fk1 FOREIGN KEY (threadlist_id) REFERENCES threadlist(thread_id)*/
);

CREATE TABLE thread(
thread_id BIGSERIAL PRIMARY KEY,
server_id BIGINT NOT NULL,
user_id BIGINT NOT NULL,
username VARCHAR(60) NOT NULL,
root_header TEXT,
root_text TEXT,
timestamp TIMESTAMP,
commentlist_id BIGINT,
CONSTRAINT thread_fk1 FOREIGN KEY (server_id) REFERENCES servers(server_id),
CONSTRAINT thread_fk2 FOREIGN KEY (user_id) REFERENCES users(user_id)
/*CONSTRAINT thread_fk3 FOREIGN KEY (commentlist_id) REFERENCES commentlist(commentlist_id)*/
);

CREATE TABLE commentlist(
commentlist_id BIGSERIAL PRIMARY KEY,
user_id BIGINT NOT NULL,
thread_id BIGINT NOT NULL,
username VARCHAR(60) NOT NULL,
index BIGINT,
reply_index BIGINT,
text TEXT,
timestamp TIMESTAMP,
rating INTEGER,
CONSTRAINT commentlist_fk1 FOREIGN KEY (user_id) REFERENCES users(user_id),
CONSTRAINT commentlist_fk2 FOREIGN KEY (thread_id) REFERENCES thread(thread_id));

CREATE TABLE threadlist(
server_id BIGSERIAL NOT NULL,
thread_id BIGSERIAL NOT NULL,
CONSTRAINT threadlist_fk1 FOREIGN KEY (thread_id) REFERENCES thread(thread_id),
CONSTRAINT threadlist_fk2 FOREIGN KEY (server_id) REFERENCES servers(server_id));

INSERT INTO servers (server_id, servername) VALUES (0, 'test server');

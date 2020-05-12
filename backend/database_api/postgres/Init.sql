CREATE USER adrenaline WITH encrypted password '1234';
CREATE DATABASE adrenaline_db;
GRANT ALL PRIVILEGES ON DATABASE adrenaline_db TO adrenaline;
ALTER USER adrenaline WITH SUPERUSER;

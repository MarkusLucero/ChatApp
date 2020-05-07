**STEG 1:**
Hitta din _odbcinst.ini_ fil. Min ligger i _/usr/local/etc/odbcinst.ini_ men ni kan ha den någon annan stans.
TIPS:
```
whereis odbcinst.ini
```
**STEG 2:**  
Lägg in följande text i odbcinst.ini:

```
[PostgreSQL Adrenaline]  
Description=PostgreSQL ODBC driver (Adrenaline version)  
Driver=/usr/lib/x86_64-linux-gnu/odbc/psqlodbca.so  
Setup=libodbcpsqlS.so  
Debug=0  
CommLog=1  
UsageCount=  
```
OBS!! DRIVER= _din sökväg till filen psqlodbca.so_ (det finns en möjlighet att din driver fil heter något annat)

**STEG 3:**

Hitta din _odbc.ini_ fil. Min ligger i _/usr/local/etc/odbc.ini_ men ni kan ha den någon annan stans.

TIPS:
```
whereis odbc.ini
```

**STEG 4:**

Lägg in följande text i odbc.ini:
```
[PostgreSQL test]
Description=PostgreSQL  
Driver=PostgreSQL Adrenaline  
Trace=No  
TraceFile=/tmp/psqlodbc.log  
Database=adrenaline_db  
Servername=localhost  
UserName=adrenaline  
Password=1234  
Port=5432  
ReadOnly=Yes  
RowVersioning=No  
ShowSystemTables=No  
ShowOidColumn=No  
FakeOidIndex=No  
ConnSettings=  
```  
**STEG 5:**

Logga in i postgres som root user.

Linux:
```
sudo su postgres
```
MAC: ????

**STEG 6:**

kör följande kommando när du är inloggad i postgres:  
```
<psql -f Init.sql
```
OBS!!! Du måste ha med sökvägen till Setup.sql filen. tex _/home/skooben/adrenaline/backend/database_api/postgres/Init.sql_

**STEG 7:**  

kör följande kommando när du är inloggad i postgres:  

```
psql -d adrenaline_db -a -f Setup.sql
```
OBS!!! Du måste ha med sökvägen till Setup.sql filen. tex _/home/skooben/adrenaline/backend/database_api/postgres/Setup.sql_

**STEG 8:** 

Kör kommandot 
```
make test
```
i database_api folder och be till gudarna att allt fungerar.

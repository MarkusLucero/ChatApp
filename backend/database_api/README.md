**STEG 1:**
Hitta din "odbcinst.ini" fil. Min ligger i "/usr/local/etc/odbcinst.ini" men ni kan ha den någon annan stans.
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
OBS!! DRIVER= "din sökväg till filen psqlodbca.so" (det finns en möjlighet att din driver fil heter något annat)

**STEG 3:**
Hitta din "odbc.ini" fil. Min ligger i "/usr/local/etc/odbc.ini" men ni kan ha den någon annan stans.

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
OBS!!! Du måste ha med sökvägen till Setup.sql filen. tex /home/skooben/adrenaline/backend/database_api/postgres/Init.sql

**STEG 7:**
kör följande kommando när du är inloggad i postgres:  

```
psql -d adrenaline_db -a -f Setup.sql
```
OBS!!! Du måste ha med sökvägen till Setup.sql filen. tex /home/skooben/adrenaline/backend/database_api/postgres/Setup.sql

**STEG 8:**
Kör kommandot 
```
make test
```
i database_api folder och be till gudarna att allt fungerar.

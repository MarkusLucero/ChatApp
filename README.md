# Chat up! (Adrenaline)

Projektarbete på kursen [Operativsystem och processorienterad programmering
(1DT096) våren 2020][homepage], [Uppsala universitet][uu].

[homepage]: https://www.it.uu.se/education/course/homepage/os/vt19/project/

[uu]: https://www.uu.se/

Chat up! är en applikation som drar inspiration från redan befintliga chat- och kommunikationsplattformar. Syftet med applikationen är att kombinera de mest användbara aspekterna från andra plattformar till en och samma plattform. Användare skall kunna skapa enkel- och gruppchatt kanaler tillsammans med andra användare och samtidigt kunna prenumerera/ansluta sig till olika servrar som tillhandahåller ett slags forumliknande vy där anslutna användare kan skapa trådar och interagera med varandra på det viset också.


## Backend
Instruktionerna är för linux.
### Dokumentation och tester
För att generera dokumentation går du in i katalogen för den komponent du vill generera dokumentation för (backend/database_api eller backend/web_api). Därefter kör du `make edoc`. Om du vill köra testerna gör du detsamma, men går in i den korrekta mappen och kör `make tests`.

### Om placeholder-namn i kommandorna
- `[USERNAME]` är ditt linux-användarnamn
- `[IMAGE_NAME]` kan vara vad som helst, bara samma används i alla kommandon
- `[CONTAINER_NAME]` kan vara vad som helst, bara samma används i alla kommandon

### Se till att
- docker är installerat
- docker servicen är startad
    - `sudo systemctl start docker` fungerar på de flesta linux distron
- du är i `docker` gruppen
    - `sudo gpasswd -a [USERNAME] docker` fungerar på de flesta linux distron
        - logga ut och in igen efter kommandot på raden ovan

### För att starta backend första gången
1. `docker build . -t [IMAGE_NAME]`
2. `docker run -p8080:8080 --name=[CONTAINER_NAME] -it [IMAGE_NAME]`
3. `docker exec -it [CONTAINER_NAME] start_app` (i annan terminal)

### För att backenden efter första gången
1. `docker start -i [CONTAINER_NAME]`
2. `docker exec -it [CONTAINER_NAME] start_app` (i annan terminal)

### För att ta bort containern
1. `docker rm [CONTAINER_NAME]`

## Om nuvarande `Dockerfile`
### Varför behöver vi köra `docker exec`?
p.g.a hur postgres Dockerfile, som används som bas för den i det här repot, är gjord.
För att den ska fungera korrekt måste `CMD` börja med `"postgres"`.

### Två saker i samma container
Just nu körs `database_api` och `web_api` i samma container.
Det skulle vara en bra ide att separera ut de i olika containrar senare.

## Frontend

Testats på Mozilla Firefox, Google Chrome och senaste varianten av Node. Vi antar att du har den senaste varianten av Node JS installerad för att följande kommanden skall garanterat funka.

Följande steg sker i /frontend mappen.
- Om det är första gången du startar upp servern, kör "npm install" för att automatiskt installera alla dependencies.
- För att sätta igång webb servern så kör du "npm start" 
- För att generera dokument kör du "npm run doc - du behöver själv öppna html filen som kommer ligga i /docs mappen.

## Katalogstruktur
<pre>
├── backend  
│   ├── database_api  
│   │   └── src  
│   ├── web_api  
│   │   ├── config  
│   |   └── src  
|   └── docker_files
|
└── frontend    
    ├── public  
    └── src  
        ├── actions  
        ├── assets  
        ├── components  
        ├── img  
        └── reducers  

</pre>

### Frontend
/public innehåller den enda html filen som behövs för att rendera alla komponenter.  

/actions och /reducers innehåller de actions och reducer-funktioner som krävs för tillståndshanteringen med Redux.   

/components innehåller alla React-komponenter som bygger upp vår applikation.   

/img innehåller alla svg filer och bilder som används i applikationen.

### Backend
/web_api innehåller allt som krävs för web_api:ns körning.

/web_api/config innehåller konfiguration för BEAM VM:en

/web_api/src innehåller all källkod för Web API:n

/docker_files innehåller skript och andra filer som krävs för docker-körningen av programmet

/database_api innehåller allt som krävs för att databasens körning

/database_api/src innehåller källkod för databas API:n

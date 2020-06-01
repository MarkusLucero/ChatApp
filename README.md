# Chat up! (Adrenaline)

Projektarbete på kursen [Operativsystem och processorienterad programmering
(1DT096) våren 2020][homepage], [Uppsala universitet][uu].

[homepage]: https://www.it.uu.se/education/course/homepage/os/vt19/project/

[uu]: https://www.uu.se/

Chat up! är en applikation som drar inspiration från redan befintliga chat- och kommunikationsplattformar. Syftet med applikationen är att kombinera de mest användbara aspekterna från andra plattformar till en och samma plattform. Användare skall kunna skapa enkel- och gruppchatt kanaler tillsammans med andra användare och samtidigt kunna prenumerera/ansluta sig till olika servrar som tillhandahåller ett slags forumliknande vy där anslutna användare kan skapa trådar och interagera med varandra på det viset också.

**TODO:** Lägg till kortfattade instruktioner som beskriver hur projektet byggs
(kompileras eller liknande), testas (automatiska tester) och startas. Lägg gärna
till lämpliga underrubriker.

## Frontend

Testats på Mozilla Firefox, Google Chrome och senaste varianten av Node. Vi antar att du har den senaste varianten av Node JS installerad för att följande kommanden skall garanterat funka.

Följande steg sker i /frontend mappen.
- Om det är första gången du startar upp servern, kör "npm install" för att automatiskt installera alla dependencies.
- För att sätta igång webb servern så kör du "npm start" 
- För att generera dokument kör du "npm run doc - du behöver själv öppna html filen som kommer ligga i /docs mappen.

## Katalogstruktur
<pre>
├── backend  
│   ├── database_api  
│   │   ├── backup   
│   │   ├── .erlang.mk  
│   │   └── src  
│   ├── web_api  
│   │   ├── config  
│   |   └── src  
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

#### Frontend
/public innehåller den enda html filen som behövs för att rendera alla komponenter.  
/actions och /reducers innehåller de actions och reducer-funktioner som krävs för tillståndshanteringen med Redux.   
/components innehåller alla React-komponenter som bygger upp vår applikation.   
/img innehåller alla svg filer och bilder som används i applikationen.

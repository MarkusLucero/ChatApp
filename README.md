# Chat up! (Adrenaline)

Projektarbete på kursen [Operativsystem och processorienterad programmering
(1DT096) våren 2020][homepage], [Uppsala universitet][uu].

[homepage]: https://www.it.uu.se/education/course/homepage/os/vt19/project/

[uu]: https://www.uu.se/

Chat up! är en applikation som drar inspiration från redan befintliga chat- och kommunikationsplattformar. Syftet med applikationen är att kombinera de mest användbara aspekterna från andra plattformar till en och samma plattform. Användare skall kunna skapa enkel- och gruppchatt kanaler tillsammans med andra användare och samtidigt kunna prenumerera/ansluta sig till olika servrar som tillhandahåller ett slags forumliknande vy där anslutna användare kan skapa trådar och interagera med varandra på det viset också.
## Kom igång

**INFO:** Detta avsnitt är till för de som i framtiden är intresserade av ert
projekt.

**TODO:** Lägg till kortfattade instruktioner som beskriver hur projektet byggs
(kompileras eller liknande), testas (automatiska tester) och startas. Lägg gärna
till lämpliga underrubriker.

## Frontend

Testats på Mozilla Firefox och Google Chrome.  

Följande steg sker i /frontend mappen.
- Om det är första gången du startar upp servern, kör "npm install" för att automatiskt installera alla dependencies.
- För att sätta igång webb servern så kör du "npm start" 
- För att generera dokument och öppna den i webläsaren kör du "npm run doc"
- För att köra enhetstester kör du "npm test" och följer kommandon som visas upp i terminalen.

## Backend

För att generera dokumentation i backenden går du in i den delens katalog (/database_api eller /web_api)
och kör make edoc. Därefter dyker det upp en katalog som kallas doc. I denna finns html-filer med dokumentation
som du kan öppna i din webbläsare.

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

## Färdigställ
- Allt eftersom projektet fortskrider kan ni lägga till fler rubriker i detta
  dokument med kompletterande information.

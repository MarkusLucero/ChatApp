# Instruktioner för docker
Instruktionerna är för linux.

## Om placeholder-namn i kommandorna
- `[USERNAME]` är ditt linux-användarnamn
- `[IMAGE_NAME]` kan vara vad som helst, bara samma används i alla kommandon
- `[CONTAINER_NAME]` kan vara vad som helst, bara samma används i alla kommandon

## Se till att
- docker är installerat
- docker servicen är startad
    - `sudo systemctl start docker` fungerar på de flesta linux distron
- du är i `docker` gruppen
    - `sudo gpasswd -a [USERNAME] docker` fungerar på de flesta linux distron
        - logga ut och in igen efter kommandot på raden ovan

## För att starta backend första gången
1. `docker build . -t [IMAGE_NAME]`
2. `docker run -p8080:8080 --name=[CONTAINER_NAME] -it [IMAGE_NAME]`
3. `docker exec -it [CONTAINER_NAME] start_app` (i annan terminal)

## För att backenden efter första gången
1. `docker start -i [CONTAINER_NAME]`
2. `docker exec -it [CONTAINER_NAME] start_app` (i annan terminal)

## För att ta bort containern
1. `docker rm [CONTAINER_NAME]`

# Om nuvarande `Dockerfile`
## Varför behöver vi köra `docker exec`?
p.g.a hur postgres Dockerfile, som används som bas för den i det här repot, är gjord.
För att den ska fungera korrekt måste `CMD` börja med `"postgres"`.

## Två saker i samma container
Just nu körs `database_api` och `web_api` i samma container.
Det skulle vara en bra ide att separera ut de i olika containrar senare.

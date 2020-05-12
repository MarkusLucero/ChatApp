# Instruktioner för docker

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

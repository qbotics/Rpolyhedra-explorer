# Docker 
In order to run in docker, the following has to be executed. 

```sh
docker build -t rpolyhedra-explorer-image . 
docker run -d --rm -p 3838:3838 rpolyhedra-explorer-image
```
# PRAIA

PRAIA is the acronym for Platform for Reduction of Astronomical Images Automatically.

According to the author:
"PRAIA performs high precision differential photometry and astrometry on digitized images (CCD frames, Schmidt plate surveys, etc). The package main characteristics are automation, accuracy and processing speed. Written in FORTRAN 77, it can run in scripts and interact with any visualization and analysis software. PRAIA is in cope with the ever growing amount of observational data available from private and public sources, including data mining and next generation fast telescope all sky surveys, like SDSS, Pan-STARRS and others. PRAIA was officially assigned as the astrometric supporting tool for participants in the GAIA-FUNSSO activities and will be freely available for the astronomical community."

src: https://www.researchgate.net/publication/258559018_PRAIA_-_Platform_for_Reduction_of_Astronomical_Images_Automatically

This repository contains the `Dockerfile` used to build a docker image for the PRAIA_astrometry application.

## How to use
Está aplicação foi desenvolvida para executar um Objeto por vez, com pelo menos uma imagem ou varias. 
executando em sequencia os seguintes programas
1. PRAIA Header extraction
2. PRAIA Astrometry (executa em paralelo com threads que podem ser limitadas atraves do argumento --max_workers)
3. PRAIA Targets
4. Plot Astrometry

No final da execução retorna um json com todos os resultados gerados e estatisticas da execução, incluindo mensagens de erro e tempo de execução para cada etapa.

para execução em paralelo, basta instanciar varias imagens cada uma com um volume /data contendo os inputs de um unico Objeto. 

### Inputs
 **TODO**  Detalhar cada input e seu formato.
- Lista das imagens (csv)
- BSP do Objeto
- Catalogo de Referência 


TODO criar exemplo executando com os inputs de exemplo Comando Simplificado.
```
docker run -it --rm --user $(id -u):$(id -g) -v $PWD/data/Eris:/data -v ${PWD}/data/ccd_images:/images praia_astrometry python /app/run.py Eris  --catalog gaia2
```


é Necessário montar os seguintes diretórios :

| Dentro do Container | Fora do Container                                              |
| ------------------- | -------------------------------------------------------------- |
| /data               | diretório com os arquivos de entrada e onde ficaram os outputs |
| /external_catalogs  | diretório onde tem os catalogos 2MASS, u4b, UCAC5, gaia1.      |
| /images             | diretório onde estão as imagens .fits                          |

OBS: Esta versão do PRAIA requer os catalogos em formato arquivo. 

Para executar no ambiente do LInea os paths são: 

| Dentro do Container | Fora do Container                         |
| ------------------- | ----------------------------------------- |
| /data               | pode ser qualquer diretório com os inputs |
| /external_catalogs  | /archive/tno/data/external_catalogs       |
| /images             | /archive/tno/ccd_images                   |






## For developers
### How to local build this image
```sh
 git clone https://github.com/linea-it/tno_astrometry.git
 cd tno_astrometry
 docker build -t praia_astrometry .
```
<!-- ### How to commit and push
Create a container before this.

 ```sh
 docker login
 docker commit <container_id> linea/praia:v20_09>     # e.g: v20_09
 docker tag <image_id> linea/praia:v20_09             # the id of the image created before
 docker push linea/praia:v20_09  
 ``` -->

### How to run PRAIA

Exemplo de comando de execução montando o diretorio de codigo:

Assuming you're connected to the LIneA's environment.

```
docker run -it --rm --user $(id -u):$(id -g) -v $PWD/:/app  -v /archive/des/tno/testing:/data -v /archive/des/tno/ccd_images:/images -v /archive/external_catalogs:/catalogs:ro praia_astrometry python /app/run.py Eris --path /proccess/4/objects/Eris  --catalog gaia2

```

Exemplo de execução com imagem build:
```
docker run -it --rm --user $(id -u):$(id -g) -v $PWD/data:/data -v /archive/tno/ccd_images:/images -v /archive/external_catalogs:/catalogs:ro linea/tno_astrometry:latest python /app/run.py Eris --path /proccess/4/objects/Eris  --catalog gaia2
```

Comando para apagar todos os resultados:
```
rm -f *.xy rm *.reg *.mes astrometry_reduction_* *.dat *.log *.txt astrometry_photometry_* *.fits *.cat *.json fort.* *.png
```


# PRAIA

PRAIA is the acronym for Platform for Reduction of Astronomical Images Automatically.

According to the author:
"PRAIA performs high precision differential photometry and astrometry on digitized images (CCD frames, Schmidt plate surveys, etc). The package main characteristics are automation, accuracy and processing speed. Written in FORTRAN 77, it can run in scripts and interact with any visualization and analysis software. PRAIA is in cope with the ever growing amount of observational data available from private and public sources, including data mining and next generation fast telescope all sky surveys, like SDSS, Pan-STARRS and others. PRAIA was officially assigned as the astrometric supporting tool for participants in the GAIA-FUNSSO activities and will be freely available for the astronomical community."

src: https://www.researchgate.net/publication/258559018_PRAIA_-_Platform_for_Reduction_of_Astronomical_Images_Automatically

This repository contains the `Dockerfile` used to build a docker image for the PRAIA_astrometry application.

### How to build the image

```sh
 git clone git@github.com:LIneA-Science/praia_astrometry.git
 cd praia_astrometry
 docker build -t praia .
```

### How to commit and push

Create a container before this.

 ```sh
 docker login
 docker commit <container_id> linea/praia:v20_09>     # e.g: v20_09
 docker tag <image_id> linea/praia:v20_09             # the id of the image created before
 docker push linea/praia:v20_09  
 ```

How to run PRAIA

 ```sh
 docker run -it --name teste -v $PWD:/data -v /archive:/archive linea/praia:v30_06
 docker exec teste sh -c '/app/PRAIA_astrometry_30_06 < /data/PRAIA_astrometry_30_06_10.dat'
 ```

OBS: Assuming you're connected to the LIneA's environment.



Exemplo de comando de execução montando o diretorio de codigo:

```
docker run -it --rm -v $PWD/data:/data -v /archive/tno/ccd_images:/images -v $PWD/:/app -v /archive/external_catalogs:/catalogs:ro linea/tno_astrometry:latest python /app/run.py Eris

```

Exemplo de execução com imagem montada:
```
docker run -it --rm -v $PWD/data:/data -v /archive/tno/ccd_images:/images -v /archive/external_catalogs:/catalogs:ro linea/tno_astrometry:latest python /app/run.py Eris
```


é Necessário montar os diretórios :
dentro do container | fora do container 
/data               | diretório onde vão ficar os arquivos de entrada e os resultados. 
/external_catalogs  | diretório onde tem os catalogos 2MASS, u4b, UCAC5, gaia. neste exemplo um mesmo diretório tem os 4 catalogos. 
/images             | diretório onde estão as imagens .fits


Comando para apagar todos os resultados:
```
rm -f *.xy rm *.reg *.mes astrometry_reduction_* *.dat *.log *.txt astrometry_photometry_* *.fits
```
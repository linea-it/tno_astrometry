FROM alpine
MAINTAINER Carlos Adean <carlosadean@linea.gov.br>

ENV APP_DIR=/app
ENV DATA_DIR=/data

RUN apk --no-cache --update-cache add gfortran
RUN mkdir $APP_DIR 
RUN mkdir $DATA_DIR 

WORKDIR $APP_DIR
COPY PRAIA_astrometry_20_09.f $APP_DIR

RUN gfortran PRAIA_astrometry_20_09.f -o PRAIA_astrometry_20_09

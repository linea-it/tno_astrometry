FROM alpine
MAINTAINER Carlos Adean <carlosadean@linea.gov.br>

ENV APP_DIR="/app"
ENV DATA_DIR="/data"
ENV PRAIA_VERSION="30_06"
ENV PATH="/app:${PATH}"

RUN apk --no-cache --update-cache add gfortran
RUN mkdir ${APP_DIR} 
RUN mkdir ${DATA_DIR}

WORKDIR ${APP_DIR}
COPY PRAIA_astrometry_${PRAIA_VERSION}.f ${APP_DIR}

RUN gfortran PRAIA_astrometry_${PRAIA_VERSION}.f -o PRAIA_astrometry_${PRAIA_VERSION}

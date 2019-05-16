FROM python:3.7
MAINTAINER Glauber Costa Vila Verde <galuber.vila.verde@gmail.com>

ENV APP_PATH=/app
ENV DATA_DIR=/data
ENV IMAGES_PATH=/images
ENV PRAIA_HEADER=PRAIA_header_extraction_30_08
ENV PRAIA_ASTROMETRY=PRAIA_astrometry_30_06
ENV PRAIA_TARGET=PRAIA_targets_search_20_03
ENV CDS2REF=cds2ref

# Informacoes para download do BSP Planetary
ENV BSP_PLANETARY_PATH=$APP_PATH/bsp_planetary
ENV BSP_PLANETARY=de435.bsp
ENV BSP_PLANETARY_URL=https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/${BSP_PLANETARY}

# Informacoes para download do Leap Seconds
ENV LEAP_SENCOND_PATH=$APP_PATH/leap_sencond
ENV LEAP_SENCOND=naif0012.tls
ENV LEAP_SENCOND_URL=https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/${LEAP_SENCOND}



RUN apt-get update && apt-get install -y  \
    gfortran \
    gzip \
    curl \
    && rm -rf /var/lib/apt/lists/*


RUN mkdir $APP_PATH \ 
    && mkdir $BSP_PLANETARY_PATH \
    && mkdir $LEAP_SENCOND_PATH

WORKDIR $APP_PATH

RUN pip install --upgrade pip && pip install \
    psycopg2-binary \
    SQLAlchemy \
    humanize \
    numpy \
    spiceypy

COPY src/ $APP_PATH/src


COPY run.py $APP_PATH
COPY praia_header.py $APP_PATH
COPY praia_astrometry.py $APP_PATH
COPY praia_target.py $APP_PATH

# Compile Praia Header Extraction
RUN gfortran src/${PRAIA_HEADER}.f -o /bin/${PRAIA_HEADER}

# Compile Praia Astrometry
RUN gfortran src/${PRAIA_ASTROMETRY}.f -o /bin/${PRAIA_ASTROMETRY}

# Compile Praia Target Search
RUN gfortran src/${PRAIA_TARGET}.f -o /bin/${PRAIA_TARGET}

# Compile CDS2REF 
RUN gfortran src/${CDS2REF}.f -o /bin/${CDS2REF}


# Download BSP PLANETARY
RUN curl ${BSP_PLANETARY_URL} --output ${BSP_PLANETARY_PATH}/${BSP_PLANETARY}

# Download Leap Second
RUN curl ${LEAP_SENCOND_URL} --output ${LEAP_SENCOND_PATH}/${LEAP_SENCOND}

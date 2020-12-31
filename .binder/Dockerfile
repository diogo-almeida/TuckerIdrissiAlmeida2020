FROM rocker/binder:3.6.3
LABEL maintainer='diogo-almeida'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/diogo-almeida/TuckerIdrissiAlmeida2020/raw/main/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2020-02-29/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0

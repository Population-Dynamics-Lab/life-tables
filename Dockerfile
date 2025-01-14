FROM rocker/binder:4.4.1

COPY --chown=${NB_USER} . ${HOME}

RUN if [ -f install.R ]; then R --quiet -f install.R; fi
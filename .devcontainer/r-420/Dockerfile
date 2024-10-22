FROM ghcr.io/r-hub/evercran/4.2.0

RUN apt-get -qq update && \
 apt-get install -y --no-install-recommends git

COPY DESCRIPTION .

RUN Rscript -e '                           \
  install.packages("remotes");             \
  remotes::install_deps(dependencies = c(  \
    "Depends",                             \
    "Imports",                             \
    "Config/needs/development"             \
  ))                                       \
'

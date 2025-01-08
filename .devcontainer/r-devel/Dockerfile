FROM docker.io/rhub/r-minimal:devel

RUN apk update && \
  apk add --no-cache git gcc g++ linux-headers musl-dev

COPY DESCRIPTION .

RUN Rscript -e '                           \
  install.packages("remotes");             \
  remotes::install_deps(dependencies = c(  \
    "Depends",                             \
    "Imports",                             \
    "Config/needs/development"             \
  ))                                       \
'

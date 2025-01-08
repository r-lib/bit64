FROM docker.io/rhub/r-minimal:devel

RUN apk update && \
  apk add --no-cache git gcc g++ linux-headers musl-dev

COPY DESCRIPTION .

RUN Rscript -e '                                \
  options(repos="https://cloud.r-project.org"); \
  install.packages("remotes");                  \
  remotes::install_deps(dependencies = c(       \
    "Depends",                                  \
    "Imports",                                  \
    "Config/needs/development"                  \
  ))                                            \
'

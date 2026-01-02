FROM docker.io/rhub/r-minimal:devel

RUN apk update && \
  apk add --no-cache git gcc g++ linux-headers musl-dev

COPY DESCRIPTION .

RUN Rscript -e '                                          \ 
read.dcf("DESCRIPTION", c("Imports", "Suggests")) |>      \
  tools:::.split_dependencies() |>                        \
  names() |>                                              \
  setdiff(tools:::.get_standard_package_names()$base) |>  \
  install.packages()                                      \
'

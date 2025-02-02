FROM ghcr.io/r-hub/evercran/3.4.0

RUN apt-get -qq update && \
 apt-get install -y --no-install-recommends git

COPY DESCRIPTION .

RUN Rscript -e '                                                     \
options(repos = "https://cloud.r-project.org");                      \
dcf = read.dcf("DESCRIPTION", c("Depends", "Imports", "Suggests"));  \
deps = names(tools:::.split_dependencies(dcf));                      \
default_pkgs = tools:::.get_standard_package_names()$base;           \
installable = setdiff(rownames(available.packages()), default_pkgs); \
message("All deps: ", toString(deps));                               \
deps = intersect(deps, installable);                                 \
message("Installing: ", toString(deps));                             \
install.packages(deps);                                              \
'

# Needed for testthat equivalency on pre-testthat R
WORKDIR /root
COPY .devcontainer/r-340/.Rprofile .

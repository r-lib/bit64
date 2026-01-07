# Find all tested revdeps as follows:
#
# install.packages("BiocManager")
#
# bioc_db = available.packages(repos = BiocManager::repositories())
# bioc_revdeps = union(
#   tools::dependsOnPkgs("bit64", installed=bioc_db),
#   tools::dependsOnPkgs("bit64", dependencies="all", recursive=FALSE, installed=bioc_db)
# )
#
# cran_db = available.packages()
# cran_revdeps = union(
#   tools::dependsOnPkgs("bit64", installed=cran_db),
#   tools::dependsOnPkgs("bit64", dependencies="all", recursive=FALSE, installed=cran_db)
# )
#
# bioc_only_revdeps = bioc_revdeps[grep("bioconductor", bioc_db[bioc_revdeps, "Repository"])]
# cran_only_revdeps = setdiff(cran_revdeps, bioc_revdeps)

# Also recommend adding -Wno-ignored-attributes to ~/.R/Makevars to reduce RcppEigen logs dump

# As of 2026-01-02, the following system requirements are needed, too:
apt-get update
apt install cmake
apt-get install -y --no-install-recommends \
  libxml2-dev libcurl4-openssl-dev libssl-dev libv8-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libcairo2-dev \
  libudunits2-dev libgdal-dev libabsl-dev \
  libmpfr-dev libglpk-dev libssh-dev libmagick++-dev default-jdk \
  libsasl2-dev tcl-dev tk-dev libgsl-dev

# Find all tested revdeps as follows:
# db = available.packages()
# all_revdeps = union(
#   tools::dependsOnPkgs("bit64", installed=db),
#   tools::dependsOnPkgs("bit64", dependencies="all", recursive=FALSE, installed=db)
# )

# Also recommend adding -Wno-ignored-attributes to ~/.R/Makevars to reduce RcppEigen logs dump

# As of 2026-01-02, the following system requirements are needed, too:
apt-get update
apt install cmake
apt-get install -y --no-install-recommends \
  libxml2-dev libcurl4-openssl-dev libssl-dev libv8-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libcairo2-dev \
  libudunits2-dev libgdal-dev libabsl-dev

# Find all tested revdeps as follows:
# db = available.packages()
# all_revdeps = union(
#   tools::dependsOnPkgs("bit64", installed=db),
#   tools::dependsOnPkgs("bit64", dependencies="all", recursive=FALSE, installed=db)
# )

# Also recommend adding -Wno-ignored-attributes to ~/.R/Makevars to reduce RcppEigen logs dump

# As of 2026-01-02, the following system requirements are needed, too:
apt-get install -y --no-install-recommends \
  libxml2-dev

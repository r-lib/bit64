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
# all_revdeps = union(bioc_revdeps, cran_revdeps)
# cran_only_revdeps = setdiff(all_revdeps, bioc_only_revdeps)
#
# uninstallable = c(
#   "rstanarm",
#   "rvinecopulib",
#   "prophet", # stan compilation blows out memory even under -O0
#   "iClick",  # requires DISPLAY for Tk during installation
#   "dowser",  # broken by Biostrings refactoring
#   NULL
# )
# uninstallable_blast_radius = c(uninstallable, tools::dependsOnPkgs(uninstallable, installed=cran_db))
# cran_revdeps = setdiff(cran_revdeps, uninstallable_blast_radius)

# Also recommend adding -Wno-ignored-attributes to ~/.R/Makevars to reduce RcppEigen logs dump

# As of 2026-01-02, the following system requirements are needed, too:
apt-get update
apt install cmake jags
apt-get install -y --no-install-recommends \
  libxml2-dev libcurl4-openssl-dev libssl-dev libv8-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libcairo2-dev \
  libudunits2-dev libgdal-dev libabsl-dev \
  libmpfr-dev libglpk-dev libssh-dev libmagick++-dev default-jdk \
  libsasl2-dev tcl-dev tk-dev libgsl-dev librsvg2-dev libprotobuf-dev \
  libsodium-dev libjq-dev librdf0-dev libgrpc++-dev protobuf-compiler-grpc \
  libgit2-dev libpoppler-cpp-dev

# not currently on CRAN:
# install.packages("remotes")
# remotes::install_github("cran/TFMPvalue")
#
# only installs with '-O0' in limited memory environment:
# install.packages("withr")
# withr::with_makevars(
#   new = list(CXXFLAGS = "-O0", CXX17FLAGS = "-O0"),
#   assignment = "+=", 
#   code = install.packages(c("mmrm", "trialr", "parTimeROC"))
# )

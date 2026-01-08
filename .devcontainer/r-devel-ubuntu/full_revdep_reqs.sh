# Find all tested revdeps as follows:
#
# install.packages("BiocManager")
#
# db = available.packages(repos = BiocManager::repositories()) # includes CRAN
# max_depth = 2L
# revdeps_by_depth = vector("list", max_depth + 1L)
# revdeps_by_depth[[1L]] = "bit64"
# for (ii in seq_len(max_depth)) {
#   revdeps_by_depth[[ii + 1L]] = tools::dependsOnPkgs(revdeps_by_depth[[ii]], recursive=FALSE, installed=db)
# }
# revdeps = Reduce(union, c(
#   revdeps_by_depth,
#   list(tools::dependsOnPkgs("bit64", dependencies="Suggests", recursive=FALSE, installed=db))
# ))
#
# uninstallable = c(
#   "glmmTMB",  # stan compilation blows out memory even under -O0
#   "rstanarm", # ditto
#   "rvinecopulib", # ditto
#   "prophet",  # ditto
#   "iClick",   # requires DISPLAY for Tk during installation
#   "gWidgets2tcltk", # ditto
#   "dowser",   # broken by Biostrings refactoring
#   "seqminer", # refuses to install for obscure reasons
#   NULL
# )
# uninstallable_blast_radius = c(uninstallable, tools::dependsOnPkgs(uninstallable, installed=db))
# revdeps = setdiff(revdeps, uninstallable_blast_radius)

# revdeps_df = data.frame(package = revdeps)
# revdeps_df$repo = db[revdeps, "Repository"]


# Also recommend adding -Wno-ignored-attributes to ~/.R/Makevars to reduce RcppEigen logs dump

# As of 2026-01-08, the following system requirements are needed, too:
apt-get update
apt install cmake jags
apt-get install -y --no-install-recommends \
  libxml2-dev libcurl4-openssl-dev libssl-dev libv8-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libcairo2-dev \
  libudunits2-dev libgdal-dev libabsl-dev \
  libmpfr-dev libglpk-dev libssh-dev libmagick++-dev default-jdk \
  libsasl2-dev tcl-dev tk-dev libgsl-dev librsvg2-dev libprotobuf-dev \
  libsodium-dev libjq-dev librdf0-dev libgrpc++-dev protobuf-compiler-grpc \
  libgit2-dev libpoppler-cpp-dev libavfilter-dev rustc cargo \
  coinor-symphony coinor-libcgl-dev coinor-libsymphony-dev

# not currently on CRAN:
# install.packages("remotes")
# remotes::install_github("cran/TFMPvalue")
#
# only install with '-O0' in limited memory environment:
# install.packages("withr")
# withr::with_makevars(
#   new = list(CXXFLAGS = "-O0", CXX17FLAGS = "-O0"),
#   assignment = "+=", 
#   code = install.packages(c("mmrm", "trialr", "parTimeROC"))
# )
#
# Use BiocManager to install CRAN packages so that Bioconductor requirements
#   are found in the process.
# BiocManager::install(
#  revdeps_df$package[grepl("cloud.r-project.org", revdeps_df$repo)]
# )

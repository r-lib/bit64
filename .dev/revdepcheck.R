library(withr)
library(xml2)

rev_dep_span = "https://cran.r-project.org/web/packages/bit64/index.html" |>
  read_html() |>
  xml_find_all("
    //h4[contains(text(), 'Reverse dependencies')]
      /following-sibling::table[1]
      /tr[not(td[contains(text(), 'enhances')])]
      /td
      /a
      /span
  ")

rev_df = data.frame(
  package = xml_text(rev_dep_span),
  source = xml_attr(rev_dep_span, "class")
)

message(sprintf(
  "Found %d reverse dependencies, %d of which are on CRAN",
  nrow(rev_df), sum(rev_df$source == "CRAN")
))

system("
  sudo apt update && sudo apt-get update && \
  sudo apt install libgsl-dev mpich libopenmpi-dev && \
  sudo apt-get install \
    libgrpc++-dev libprotobuf-dev protobuf-compiler-grpc pkg-config libssh-dev libarchive-dev
")
# install rust in the most insane way possible
system("curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")

if (!require("BiocManager")) { install.packages("BiocManager"); require("BiocManager") }

cran_reqs = rev_df$package[rev_df$source == "CRAN"]
bioc_reqs = rev_df$package[rev_df$source == "BioC"]

# First-level downstreams typically need their Suggests available to pass their own
#   tests (and even if they pass without Suggests, they might have a much richer suite
#   with their Suggests available), hence install their Suggests, however, _don't_ use
#   dependencies=TRUE since that installs Suggests-of-Suggests, which is much too wide
#   a net to case for the purposes here. We can use the default dependencies=NA on the
#   output of this function.
first_level_reqs = function(pkgs) {
  direct_full_req =
    available.packages()[pkgs, c("Depends", "Imports", "Suggests", "LinkingTo")]
  c(direct_full_req) |>
    na.omit() |>
    tools:::.split_dependencies() |>
    names() |>
    setdiff("R")
}

# This iteration is mainly to ensure second-order deps are also installed,
#   including Suggests to minimize spurious test failures even though it can
#   massively increase install time.
message("Installing all revdeps to latest CRAN version (indiscriminately)")
install.packages(first_level_reqs(cran_reqs))
# TODO: figure out how to do the same for BioConductor
install(bioc_reqs, dependencies=TRUE)

if (!all(rev_df$package %in% rownames(installed.packages())))
  stop("Some packages failed to install, necessitating some manual intervention...")

message("Installing all revdeps (again), this time with --install-tests")
install.packages(cran_reqs, INSTALL_opts = "--install-tests")
install(         bioc_reqs, INSTALL_opts = "--install-tests")

dir.create("revdep", showWarnings=FALSE)
setwd("revdep")
con = file("all_test_output.log", "a")
for (pkg in rev_df$package) {
  cat(pkg,"")
  with_tempfile("tmp", {
    system2("Rscript",
      c("-e", shQuote(sprintf("tools::testInstalledPackage('%s')", pkg))),
      stderr = tmp, stdout = tmp
    )
    writeLines(readLines(tmp), con)
  })
}
close(con)

failing_pkg = unique(sub("-.*", "", list.files(recursive=TRUE, pattern="\\.Rout\\.fail$")))

# examine failure logs manually...

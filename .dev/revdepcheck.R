library(BiocManager)

db = available.packages(repos = BiocManager::repositories())

rev_deps = unlist(tools::package_dependencies("bit64", reverse=TRUE, recursive=FALSE, db=db, which='all'), use.names=FALSE)

cat(sprintf(
  "Found %d reverse dependencies, %d of which are on CRAN\n",
  length(rev_deps), sum(!grepl("bioconductor", db[rev_deps, "Repository"]))
))

apt_packages = c(
  "cmake",
  NULL
)
# NB: libnode-dev / libv8-dev are recommended by V8 config,
#   which worked on Codespaces but caused major headaches on
#   my Linux Mint desktop; handle this for your platform. What
#   wound up working on Mint is Sys.setenv(DOWNLOAD_STATIC_LIBV8=1).
apt_get_packages = c(
  "libcurl4-openssl-dev",
  "libssl-dev",
  "libfontconfig1-dev",
  "libharfbuzz-dev",
  "libfribidi-dev",
  "libxml2-dev",
  "libnetcdf-dev",
  "libgrpc++-dev",
  "libprotobuf-dev",
  "protobuf-compiler-grpc",
  "pkg-config",
  "libgdal-dev",
  "libgeos-dev",
  "libproj-dev",
  "openjdk-21-jdk",
  "libmpfr-dev",
  "libgmp-dev",
  "libudunits2-dev",
  "libgsl-dev",
  "libfftw3-dev",
  "libmagick++-dev",
  "libuv1-dev",
  "libcairo2-dev",
  "rustc",
  "libssh-dev",
  "librsvg2-dev",
  "libjq-dev",
  NULL
)

cat(sprintf(
  "Installing %d system requirements...\n",
  length(apt_packages) + length(apt_get_packages)
))

sudo = Sys.which("sudo")
cmd_update = paste(sudo, c("apt modernize-sources", "apt update", "apt-get update"), collapse = " && ")
cmd_apt = paste(sudo, "apt install", paste(apt_packages, collapse = " "))
cmd_apt_get = paste(sudo, "apt-get install -y", paste(apt_get_packages, collapse = " "))

system(cmd_update)
system(cmd_apt)
system(cmd_apt_get)

cat("Installing downstreams with --install-tests\n")

makevars = "~/.R/Makevars"
if (
  !dir.exists(dirname(makevars))
  || !file.exists(makevars)
  || !any(grepl("ignored-attributes", readLines(makevars)))
) {
  dir.create(dirname(makevars), showWarnings=FALSE)
  # suppress extremely noisy compiler warnings
  cat(
    paste0("CXX", c("", 11, 14, 17), "FLAGS+=-Wno-ignored-attributes"),
    file=makevars, sep="\n", append=TRUE
  )
}
Sys.setenv(DOWNLOAD_STATIC_LIBV8=1)
system("R CMD javareconf")
install(rev_deps, INSTALL_opts="--install-tests", dependencies=TRUE)

if (length(failed_to_install <- setdiff(rev_deps, rownames(installed.packages()))))
  stop(sprintf(
    "%d packages failed to install, e.g. %s, necessitating some manual intervention, see `failed_to_install`...",
    length(failed_to_install), toString(head(failed_to_install))
  ))

if (basename(getwd()) != "bit64")
  stop("The proceeding assumes you're in the bit64 package directory.")

run_revdep_tests = function(pkgs) {
  log_file = 'all_test_output.log'
  file.create(log_file)
  log_file = normalizePath(log_file)
  for (pkg in pkgs) {
    cat(pkg, "")
    dir.create(pkg, showWarnings=FALSE)
    local({
      tmp <- tempfile()
      setwd(pkg)
      on.exit({unlink(tmp); setwd("..")})

      system2("Rscript",
        c("-e", shQuote(sprintf("tools::testInstalledPackage('%s')", pkg))),
        stderr = tmp, stdout = tmp
      )
      cat(readLines(tmp), sep='\n', file=log_file, append=TRUE)
    })
  }
  cat("\n")
}

## REVDEPS USING DEVEL VERSION
system("R CMD INSTALL .")

dir.create("revdep", showWarnings=FALSE)
setwd("revdep")

dir.create("devel", showWarnings=FALSE)
setwd("devel")
run_revdep_tests(rev_deps)

failing_pkgs = unique(sub("/.*", "", list.files(recursive=TRUE, pattern="\\.Rout\\.fail$")))
setwd("..")

## REVDEPS USING CRAN VERSION (for baseline among failing packages)

install('bit64', force=TRUE)

dir.create("cran", showWarnings=FALSE)
setwd("cran")
run_revdep_tests(failing_pkgs)

failing_on_cran = unique(sub("/.*", "", list.files(recursive=TRUE, pattern="\\.Rout\\.fail$")))

cat(sprintf(
  "The following packages fail on CRAN as well as with devel and are ignored:\n  %s\n",
  paste(failing_on_cran, collapse = " ")
))

cat(sprintf(
  "The following packages are broken by the devel version of bit64:\n  %s\n",
  paste(setdiff(failing_pkgs, failing_on_cran), collapse = " ")
))

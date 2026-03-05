library(BiocManager)

db = available.packages(repos = BiocManager::repositories())

rev_deps = unlist(tools::package_dependencies("bit64", reverse=TRUE, recursive=FALSE, db=db, which='all'), use.names=FALSE)

cat(sprintf(
  "Found %d reverse dependencies, %d of which are on CRAN\n",
  length(rev_deps), sum(grepl("cloud.r-project.org", db[rev_deps, "Repository"]))
))

apt_packages = c(
  "cmake",
  NULL
)
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
  "libv8-dev",
  "libfftw3-dev",
  "libmagick++-dev",
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

cat(sprintf("Installing downstreams with --install-tests\n"))

message("Installing all revdeps (again), this time with --install-tests")
install(rev_deps, INSTALL_opts="--install-tests", dependencies=TRUE)

if (!all(rev_deps %in% rownames(installed.packages())))
  stop("Some packages failed to install, necessitating some manual intervention...")

if (!basename(getwd()) == "bit64")
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

setdiff(failing_pkgs, failing_on_cran)


set.seed(1L)
bit64::factor(sample(letters[1:3], 10L, replace=TRUE), levels=letters[1:5])
set.seed(1L)
base::factor(sample(letters[1:3], 10L, replace=TRUE), levels=letters[1:5])

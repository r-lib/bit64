# /*
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
# */


# require(rhub)
# rhub_bit64_4.5.0 <- check_for_cran(
#   path = "../bit64_4.5.0.tar.gz"
# , email = "Jens.Oehlschlaegel@truecluster.com"
# , check_args = "--as-cran"
# , env_vars = c('_R_CHECK_FORCE_SUGGESTS_'= "false",'_R_CHECK_CRAN_INCOMING_USE_ASPELL_'= "true", '_R_CHECK_XREFS_MIND_SUSPECT_ANCHORS_'="true")
# , platforms = NULL
# , show_status = FALSE
# )

# > require(rhub)
# > rhub_setup()
# > rhub_doctor()
# > rhub_check(platforms = c("linux", "macos", "windows", "ubuntu-clang", "ubuntu-gcc12"))
# ✔ Found git repository at /home/jo/SIK/truecluster/bit.
# ✔ Found GitHub PAT.                                 
# ✔ Check started: linux, macos, windows, ubuntu-clang, ubuntu-gcc12 (aspherical-sphinx).
# See <https://github.com/truecluster/bit/actions> for live output!



.onLoad <- function(lib, pkg) {
  ##library.dynam("bit64", pkg, lib) use useDynLib(bit) in NAMESPACE instead
  ##packageStartupMessage("Loading package bit64 ", packageDescription("bit64", fields="Version"))
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("Attaching package bit64")
  packageStartupMessage("package:bit64 (c) 2011-2017 Jens Oehlschlaegel")
  packageStartupMessage("creators: integer64 runif64 seq :")
  packageStartupMessage("coercion: as.integer64 as.vector as.logical as.integer as.double as.character as.bitstring")
  packageStartupMessage("logical operator: ! & | xor != == < <= >= >")
  packageStartupMessage("arithmetic operator: + - * / %/% %% ^")
  packageStartupMessage("math: sign abs sqrt log log2 log10")
  packageStartupMessage("math: floor ceiling trunc round")
  packageStartupMessage("querying: is.integer64 is.vector [is.atomic} [length] format print str")
  packageStartupMessage("values: is.na is.nan is.finite is.infinite")
  packageStartupMessage("aggregation: any all min max range sum prod")
  packageStartupMessage("cumulation: diff cummin cummax cumsum cumprod")
  packageStartupMessage("access: length<- [ [<- [[ [[<-")
  packageStartupMessage("combine: c rep cbind rbind as.data.frame")
  packageStartupMessage("WARNING don't use as subscripts")
  packageStartupMessage("WARNING semantics differ from integer")
  packageStartupMessage("for more help type ?bit64")
}
.onUnload <- function(libpath){
   packageStartupMessage("Unloading package bit64")
   library.dynam.unload("bit64", libpath)
}

# /*
# S3 atomic 64bit integers for R
# (c) 2011-2024 Jens Oehlschägel
# (c) 2025 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# */

# nocov start
.onUnload <- function(libpath) {
  library.dynam.unload("bit64", libpath)
}
# nocov end

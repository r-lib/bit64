# /*
# S3 atomic 64bit integers for R
# (c) 2011-2024 Jens Oehlschägel
# (c) 2025 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# */

# backports
# R < 3.6.0
if (getRversion() < "3.6.0") {
  errorCondition = function(message, ..., class = NULL, call = NULL) {
    obj <- list(message = as.character(message), call = call, ...)
    class(obj) = c(class, "error", "condition")
    obj
  }

  warningCondition = function(message, ..., class = NULL, call = NULL) {
    obj <- list(message = as.character(message), call = call, ...)
    class(obj) = c(class, "warning", "condition")
    obj
  }
}

.onLoad = function(libname, pkgname) {
  # TODO(R >= 4.6.0): remove this.
  if (!utils::isS3method("print.bitstring")) {
    registerS3method("print", "bitstring", function(x, ...) {
      reset_class = minusclass(class(x), 'bitstring')
      attributes(x) = NULL
      oldClass(x) = reset_class
      NextMethod(x)
    })
  }
}

.onAttach = function(libname, pkgname) {
  packageStartupMessage( # in **bold**
    "\033[1mThe assignment of character values to integer64 vectors and ",
    "matrices with automatic coercion to integer64 will change to a more ",
    "R consistent behaviour of coercing to character in future versions of ",
    "bit64. If you wish you can update your code to the new behaviour by ",
    "setting the option 'bit64.promoteInteger64ToCharacter' to TRUE.\033[22m"
  )
}

# nocov start
.onUnload = function(libpath) {
  library.dynam.unload("bit64", libpath)
}
# nocov end

# The call stack is searched for a given sequence of function names. If the last element of function_names is found, 
# we try to match as many elements in function_names with the function names in the call stack. The sequence must be 
# adhered to. The complete call of the function name of the last match is returned. If no match exists, the top call
# is returned. It is also possible to change the function name of the matched return value by providing its new name
# with name_to_display.
# Examples: 
# * call stack: [A, B, C, D, E]; function_names = c("C", "D") returns C
# * call stack: [A, B, C, D, E]; function_names = c("E", "D") returns D
# * call stack: [A, B, C, D, E]; function_names = c("E", "X") returns A
choose_sys_call = function(function_names, name_to_display=NULL, callStack=NULL) {
  calls = if (is.null(callStack)) sys.calls() else callStack
  if (length(calls) == 1L || length(function_names) == 0L) return(calls[[1L]])
  # find last occurrence of last name in function_names
  function_names_rev = rev(as.character(function_names))
  for (sel in rev(seq_along(calls))) {
    el = calls[[sel]]
    if (!is.function(el[[1L]]) && rev(as.character(el[[1L]]))[1L] == function_names_rev[1L]) break
  }
  # now check further backwards to match as far as possible
  for (fn in function_names_rev[-1L]) {
    if (sel == 1L) break
    el = calls[[sel - 1L]]
    if (is.function(el[[1L]]) || rev(as.character(el[[1L]]))[[1L]] != fn) break
    sel = sel - 1L
  }
  ret = calls[[sel]]
  if (!is.null(name_to_display))
    ret[[1L]] = as.name(name_to_display)
  ret
}

withCallingHandlers_and_choose_call = function(expr, function_names, name_to_display=NULL, callStack=NULL) {
  wch = substitute(
    withCallingHandlers(expr, error=error, warning=warning),
    list(
      expr = sys.call()[[2L]],
      error = function(e) stop(errorCondition(e$message, call=choose_sys_call(function_names, name_to_display, callStack=callStack))),
      warning = function(w) {
        warning(warningCondition(w$message, call=choose_sys_call(function_names, name_to_display, callStack=callStack)))
        invokeRestart("muffleWarning")
      }
    )
  )
  eval(wch, envir=parent.frame())
}

# function to determine target class and sample value for union, intersect, setdiff, setequal, min, max, range, sum, prod, c, cbind and rbind functions
getClassesOfElements = function(x, recursive) {
  classes = vapply(x, function(el) if (inherits(el, c("list", "data.frame"))) "list" else class(el)[1L], character(1L))
  if (recursive) {
    union(classes[classes != "list"], unlist(lapply(x[classes == "list"], function(el) getClassesOfElements(el, recursive=TRUE))))
  } else {
    unique(classes)
  }
}

target_class = function(x, recursive=FALSE, POSIXltAsCharacter=FALSE) {

  classes = getClassesOfElements(x, recursive=isTRUE(recursive))
  
  if ("POSIXlt" %in% classes && isTRUE(POSIXltAsCharacter)) return("character")
  if ("complex" %in% classes) return("complex")
  if (any(c("character", "factor", "ordered") %in% classes)) {
    # TODO(#44): next Release: change default behavior; subsequent Release: change from message to warning; subsequent Release: change from warning to error; subsequent Release: remove option
    if (isTRUE(getOption("bit64.promoteInteger64ToCharacter", FALSE))) return("character")
  }
  "integer64"
}

# quote() requires >0 arguments and substitute(...) does not WAI, so mix substitute() and quote(...)
missing_or_dots = function(x) identical(x, substitute()) || identical(x, quote(...))

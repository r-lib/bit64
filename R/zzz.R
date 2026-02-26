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

generic_call_in_stack = function(generic_name) {
  calls = sys.calls()
  # we define `:.default` --> avoid infinite loop
  for (jj in base::`:`(length(calls), 1L)) {
    this_call = calls[[jj]][[1L]]
    if (identical(this_call, as.name(generic_name))) return(TRUE)
    if (is.call(this_call)) {
      # namespaced equivalent, e.g. for bit:: generics
      if (this_call[[1L]] != "::") next
      if (identical(this_call[[3L]], as.name(generic_name))) return(TRUE)
    } else if (is.function(this_call)) {
      # substitute() equivalent
      if (identical(this_call, get(generic_name))) return(TRUE)
    }
  }
  FALSE
}

# some, but not all, primitives are totally absent from sys.calls(). try
#   and separate these two classes of is.primitive functions
primitive_generic_appears_in_stack  = function(generic_name) {
  generic = get(generic_name)
  generic_nm = as.name(generic_name)
  if (!is.primitive(generic)) return(TRUE)

  fake_method = paste0(generic_name, ".xxx")
  eval(bquote({
    .(fake_method) = function(...) {
      sc = sys.calls()
      would_be_generic_call = sc[[length(sc) - 1L]]
      identical(would_be_generic_call[[1L]], .(generic_nm))
    }
    generic_args = args(generic)
    n_args = if (is.null(generic_args)) 1L else length(formals(generic_args))
    call_args = vector("list", n_args)
    d = 1L
    class(d) = "xxx"
    call_args[[1L]] = d
    do.call(.(generic_nm), call_args)
  }))
}

deprecate_exported_s3_methods = function(..., verbose=FALSE) {
  methods = list(...)
  method_names = vapply(substitute(list(...))[-1L], deparse, character(1L))
  # this happens to work here -- no affected classes use '.', so the class is the last part
  generic_method = vapply(
    strsplit(method_names, ".", fixed=TRUE),
    function(parts) c(paste(head(parts, -1L), collapse="."), tail(parts, 1L)),
    character(2L)
  )

  if (verbose) primitives = character()

  for (ii in seq_along(methods)) {
    method = methods[[ii]]
    method_name = method_names[ii]
    generic_name = generic_method[1L, ii]

    # call stack may/may not work correctly for primitives. optionally report what's skipped.
    if (!primitive_generic_appears_in_stack(generic_name)) {
      if (verbose) primitives = c(primitives, generic_name)
      next
    }

    # Prepend the warning check to the function body
    warning_expr = bquote(
      if (
        getOption("bit64.warn.exported.s3.method", TRUE) &&
          !generic_call_in_stack(.(generic_name))
      ) {
        warning(
          "Detected that '", .(method_name), "' was called directly. ",
          "Instead only call '", .(generic_name), "' and rely on S3 dispatch. ",
          "To suppress this warning, e.g. if this is a false positive, ",
          "use options(bit64.warn.exported.s3.method = FALSE). ",
          "In the next version, this symbol will stop being exported.",
          domain=NA
        )
      }
    )

    # in 'function(x) x', body() is a name --> subsetting breaks
    # if un-braced, as.list() will break up the first (and only) expression
    if (!is.name(body(method)) && identical(body(method)[[1L]], as.name("{"))) {
      exprs = as.list(body(method)[-1L])
    } else {
      exprs = body(method)
    }
    body(method) = as.call(c(as.name("{"), warning_expr, exprs))

    assign(method_name, method, envir = parent.frame())
  }

  if (verbose && length(primitives))
    cat(sprintf("Skipped these primitive functions: %s\n", toString(sort(unique(primitives)))))
  invisible()
}

# commented out: those primitives which don't appear in the call stack
deprecate_exported_s3_methods(
  `:.default`,
  `:.integer64`,
  `[.integer64`,
  `[[.integer64`,
  `[[<-.integer64`,
  `%in%.default`,
  `%in%.integer64`,
  `length<-.integer64`,
  all.equal.integer64,
  as.bitstring.integer64,
  as.integer64.bitstring,
  as.integer64.factor,
  as.integer64.integer64,
  as.integer64.NULL,
  as.list.integer64,
  as.logical.integer64,
  cbind.integer64,
  diff.integer64,
  duplicated.integer64,
  hashdup.cache_integer64,
  hashfin.cache_integer64,
  hashfun.integer64,
  hashmap.integer64,
  hashmaptab.integer64,
  hashmapuni.integer64,
  hashmapupo.integer64,
  hashpos.cache_integer64,
  hashrev.cache_integer64,
  hashrin.cache_integer64,
  hashtab.cache_integer64,
  hashuni.cache_integer64,
  hashupo.cache_integer64,
  is.double.default,
  is.double.integer64,
  is.finite.integer64,
  is.infinite.integer64,
  is.nan.integer64,
  is.sorted.integer64,
  is.vector.integer64,
  keypos.integer64,
  match.default,
  match.integer64,
  mean.integer64,
  median.integer64,
  mergeorder.integer64,
  mergesort.integer64,
  mergesortorder.integer64,
  na.count.integer64,
  nties.integer64,
  nunique.integer64,
  nvalid.integer64,
  order.default,
  order.integer64,
  orderdup.integer64,
  orderfin.integer64,
  orderkey.integer64,
  ordernut.integer64,
  orderpos.integer64,
  orderqtl.integer64,
  orderrnk.integer64,
  ordertab.integer64,
  ordertie.integer64,
  orderuni.integer64,
  orderupo.integer64,
  prank.integer64,
  print.bitstring,
  qtile.integer64,
  quantile.integer64,
  quickorder.integer64,
  quicksort.integer64,
  quicksortorder.integer64,
  radixorder.integer64,
  radixsort.integer64,
  radixsortorder.integer64,
  ramorder.integer64,
  ramsort.integer64,
  ramsortorder.integer64,
  rank.default,
  rbind.integer64,
  scale.integer64,
  shellorder.integer64,
  shellsort.integer64,
  shellsortorder.integer64,
  sort.integer64,
  sortfin.integer64,
  sortnut.integer64,
  sortorderdup.integer64,
  sortorderkey.integer64,
  sortorderpos.integer64,
  sortorderrnk.integer64,
  sortordertab.integer64,
  sortordertie.integer64,
  sortorderuni.integer64,
  sortorderupo.integer64,
  sortqtl.integer64,
  sorttab.integer64,
  sortuni.integer64,
  summary.integer64,
  tiepos.integer64,
  unipos.integer64
)
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
choose_sys_call = function(function_names, name_to_display=NULL) {
  calls = sys.calls()
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

withCallingHandlers_and_choose_call = function(expr, function_names, name_to_display=NULL) {
  wch = substitute(
    withCallingHandlers(expr, error=error, warning=warning),
    list(
      expr = sys.call()[[2L]],
      error = function(e) stop(errorCondition(e$message, call=choose_sys_call(function_names, name_to_display))),
      warning = function(w) {
        warning(warningCondition(w$message, call=choose_sys_call(function_names, name_to_display)))
        invokeRestart("muffleWarning")
      }
    )
  )
  eval(wch, envir=parent.frame())
}

# function to determine target class and sample value for union, intersect, setdiff, setequal, min, max, range, sum, prod, c, cbind and rbind functions
target_class_and_sample_value = function(x, recursive=FALSE, errorClasses="") {
  
  getClassesOfElements = function(x, recursive, errorClasses) {
    classes = vapply(x, function(el) if (class(el)[1L] == "list" || "data.frame" %in% class(el)) "list" else class(el)[1L], character(1L))
    if (recursive) {
      union(classes[classes != "list"], unlist(lapply(x[classes == "list"], function(el) getClassesOfElements(el, recursive=TRUE, errorClasses=errorClasses))))
    } else {
      unique(classes)
    }
  }
  classes = getClassesOfElements(x, recursive=isTRUE(recursive), errorClasses=errorClasses)
  if (length(sel <- intersect(errorClasses, classes)))
    stop(errorCondition(sprintf(gettext("invalid 'type' (%s) of argument", domain="R"), sel[1L]), call=sys.call(max(sys.nframe() - 1L, 1L))))
  
  if (any(c("character", "factor", "ordered") %in% classes)) {
    # TODO(#44): next Release: change default behavior; subsequent Release: change from message to warning; subsequent Release: change from warning to error; subsequent Release: remove option
    if (!isTRUE(getOption("bit64.promoteInteger64ToCharacter", FALSE))) {
      valueClass = "integer64"
    } else {
      valueClass = "character"
    }
  } else if ("complex" %in% classes) {
    valueClass = "complex"
  } else if (any(c("Date", "POSIXct", "POSIXlt", "difftime") %in% classes)) {
    valueClass = "integer64"
  } else {
    valueClass = "integer64"
  }
  valueClass
}

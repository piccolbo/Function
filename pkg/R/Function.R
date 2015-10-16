# function families
# create families of function around reusable element such as arguments

# duplicated from bettR to untangle dependency
# remove when available in other package
#return all args to a function, evaluated, as a list
all.args =
  function(fun, matched.call) {
    args = formals(fun)
    args = args[discard(names(args), ~.=="...")]
    actual.args = as.list(matched.call)[-1]
    nact = names(actual.args)
    args[nact] = NULL
    lapply(c(args, actual.args), eval, envir = parent.frame(2))}


# a default value for a mandatory argument
nodefault = mandatory = quote(expr = )

# reusable argument
A = Argument =
  function(
    name, #name of the argument
    priority, #priority when deciding order
    default = ~mandatory(name), #default value
    validate = function(x) TRUE, #validate argument
    process = identity,
    help = NULL) { #transform argument
    args = all.args(A,  match.call())
    stopifnot(identical(default, ~mandatory(name)) || validate(default))
    structure(
      args,
      class = "Argument")}

as.Argument = function(x, ...) UseMethod("as.Argument")

as.Argument.default =
  function(x, ...)
    do.call(Argument, as.list(x))

.onLoad =
  function(libname,  pkgename)
    assign("dots..", A(help = "Additional arguments", default = nodefault), envir = environment(A))

# reusable function defs
F = Function =
  function(
    ...,
    body = NULL,
    export = TRUE,
    help = NULL,
    tests = list(),
    precondition = function(...) TRUE,
    postcondition = function(x) TRUE,
    args = NULL) {
    fargs = list(...)
    names(fargs) = map(fargs, "name")
    if(is.null(body)){
      body = tail(fargs, 1)[[1]]
      fargs = fargs[-length(fargs)]}
    fargs = c(fargs, args)
    pre =
      function(){
        args = all.args(pre, match.call())
        arge =
          setNames(
            lapply(
              names(args),
              function(n) {
                if(n == "") n = "..."
                stopifnot(fargs[[n]]$validate(args[[n]]))
                fargs[[n]]$process(args[[n]])}),
            nm = names(args))
        stopifnot(do.call(precondition, args))
        args }
    core = function(){}
    body(core) = as.list(body)[[2]]
    retval = function() {
      retval = do.call(core, do.call(pre, all.args(retval, match.call())))
      stopifnot(postcondition(retval))
      retval}
    vals = map(fargs, "default")
    formals(pre) =
      formals(core) =
      formals(retval) =
        setNames(
          object = vals ,
          nm = map(fargs, "name"))
    if(is.null(help$args))
      help$args = paste("\n", names(fargs), "\n:   ", unlist(map(fargs, bettR::help)))
    if(is.null(help$usage))
      help$usage = paste(head(deparse(args(core)), -1), collapse = "\n")
    structure(
      retval,
      class = "Function",
      help = help,
      tests = tests)}

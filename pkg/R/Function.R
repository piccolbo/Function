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
    priority = 0, #priority when deciding order
    default = nodefault, #default value
    validate = function(x) TRUE, #validate argument
    process = identity,
    help = NULL) { #transform argument
    args = all.args(A,  match.call())
    stopifnot(identical(default, nodefault) || validate(default))
    structure(
      args,
      class = "Argument")}

is.Argument = function(x) "Argument" %in% class(x)

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
    export = FALSE,
    help = NULL,
    tests = list(),
    precondition = function(...) TRUE,
    postcondition = function(x) TRUE,
    args = NULL) {
    fargs = list(...)
    nargs = names(named_dots(...))
    if(is.null(body)){
      body = tail(fargs, 1)[[1]]
      fargs = head(fargs, -1)
      nargs = head(nargs, -1)}
    fargs = c(fargs, args)
    names(fargs) = c(nargs, names(args))
    names(fargs) = unlist(map_if(names(fargs), ~. == "dots..", ~{"..."}))
    preprocess =
      function(){
        args = all.args(preprocess, match.call())
        stopifnot(do.call(precondition, args))
        map2(
          args,
          map_if(names(args), ~.=="" ||!(. %in% names(fargs)), ~{"..."}),
          function(val, nm) {
            stopifnot(fargs[[nm]]$validate(val))
            fargs[[nm]]$process(val)})}
    body.fun = function(){}
    body(body.fun) = as.list(body)[[2]]
    environment(body.fun) = environment(body)
    retval = function() {
      retval = do.call(body.fun, do.call(preprocess, all.args(retval, match.call())))
      stopifnot(postcondition(retval))
      retval}
    vals = map(fargs, "default")
    formals(preprocess) =
      formals(body.fun) =
      formals(retval) =
      setNames(
        object = vals ,
        nm = names(fargs))
    if(is.null(help$args))
      help$arguments = map(fargs, "help")
    if(is.null(help$usage))
      help$usage = paste(head(deparse(args(body.fun)), -1), collapse = "\n")
    structure(
      retval,
      class = "Function",
      body = body,
      export = export,
      help = help,
      tests = tests)}

is.Function = function(x) "Function" %in% class(x)

as.Function = function(x) UseMethod("as.Function")
as.Function.function =
  function(x) {
    body = eval(call(name = "~", body(x)))
    environment(body) = environment(x)
    Function(args = map(formals(x), Argument), body)}

export =
  function(x) {
    pf = parent.frame()
    x.val = get(x, envir = pf)
    attr(x.val, "export")  = TRUE
    assign(x, x.val, pf)}

exported  =
  function(env)
    keep(as.list(as.environment(env)), ~{isTRUE(attributes(.)$export)})

load.exports =
  function(){
    pf = parent.frame()
    name = capture.output(print(pf))
    assign(
      ".onAttach",
      function(libname, pkgname) {
        attach(exported(pf), name = name,  pos = 3L)},
      envir = pf)
    assign(
      ".onDetach",
      function(libpath) {message("detaching ", name); detach(name)},
      envir = pf)}

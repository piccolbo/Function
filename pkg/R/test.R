test  = function(x, ...) UseMethod("test")

test.default =
  function(x, ...)
    map(attr(x, "tests", exact = TRUE), ~.())

test.list =
test.environment =
  function(x, filter = testable, ...)
    map(keep(as.list(x), filter), ~test(.))

testable =
  function(x)
    !is.null(attributes(x)$tests)

package.test =
  function(x, filter = testable, ...) {
    library(x, character.only = TRUE)
    env = as.environment(paste0("package:", x))
    test(env, filter = filter)}
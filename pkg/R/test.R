test  = function(x, ...) UseMethod("tests")

test.Function =
  function(x, ...)
    map(attr(x, "tests", exact = TRUE), ~.())


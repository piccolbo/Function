test  = function(x, ...) UseMethod("test")

test.Function =
  function(x, ...)
    map(attr(x, "tests", exact = TRUE), ~.())


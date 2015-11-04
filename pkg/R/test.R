test  = function(x, ...) UseMethod("test")

test.default =
  function(x, ...)
    map(attr(x, "tests", exact = TRUE), ~.())


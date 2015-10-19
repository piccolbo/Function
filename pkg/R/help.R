help = function(x, ...) UseMethod("help")

help.Function = function(x, ...)
  view.help(x)

help.Argument = function(x, ...) x$help

help.default =
  function(x, ...) {
    if(is.null(attr(x, "help"))){
      x = as.character(substitute(x))
      utils::help(x, ...)}
    else
      view.help(x)}

Help =
  function(
    title,
    description,
    value,
    usage = NULL,
    arguments = NULL,
    details = NULL,
    see.also = NULL,
    examples = NULL)
    structure(
      all.args(Help, match.call()),
      class = "Help")

title = function(x)  paste("#", x, "\n")
subtitle = function(x)  paste("##", x, "\n")
paragraph = function(x) paste(x, "\n")

section =
  function(x, heading, formatting = paragraph)
    if(!is.null(x)) paste0(subtitle(heading), formatting(x))

view.help =
  function(x) {
    h = attributes(x)$help
    view = getOption("viewer")
    markdown =
      paste0(
        title(h$title),
        subtitle("Description"),
        paragraph(h$description),
        subtitle("Usage"),
        paragraph(h$usage),
        section(
          h$arguments,
          "Arguments",
          function(x)
            paragraph(
              paste0("\n", names(x), "\n:   ", unlist(x), collapse = "\n"))),
        section(h$details,"Details"),
        section(h$see.also, "See Also"),
        section(h$examples, "Examples"))
    tmpin = tempfile(fileext = ".md")
    tmpout = tempfile(fileext = ".html")
    writeLines(text = markdown, con = file(tmpin))
    renderMarkdown(tmpin, tmpout)
    view(tmpout)}

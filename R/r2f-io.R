# Handlers for diagnostic output: cat, print.

r2f_handlers[["cat"]] <- function(args, scope, ...) {
  if (
    length(args) != 1L || !is_string(args[[1L]]) || any(nzchar(names(args)))
  ) {
    stop(
      "cat() only supports one unnamed string literal ending in a newline",
      call. = FALSE
    )
  }
  label <- args[[1L]]
  if (!endsWith(label, "\n")) {
    stop("cat() string must end with a newline", call. = FALSE)
  }
  # Encode bytes rather than embedding raw newlines, quotes, or backslashes
  # in Fortran source. labelpr's length is a byte count.
  bytes <- as.integer(charToRaw(enc2utf8(substring(
    label,
    1L,
    nchar(label) - 1L
  ))))
  # labelpr skips zero-length labels. A NUL byte prints an empty label while
  # retaining its terminating newline.
  if (!length(bytes)) {
    bytes <- 0L
  }
  literal <- paste0("achar(", bytes, ")", collapse = " // ")
  Fortran(glue('call labelpr({literal}, {length(bytes)})'))
}

r2f_handlers[["print"]] <- function(args, scope, ..., hoist = NULL) {
  if (length(args) != 1L || any(!names(args) %in% c("", "x"))) {
    stop(
      "print() only supports one logical, integer, or double value",
      call. = FALSE
    )
  }
  value <- r2f(args[[1L]], scope, ..., hoist = hoist)
  if (
    is.null(value@value) ||
      !value@value@mode %in% c("logical", "integer", "double")
  ) {
    stop(
      "print() only supports logical, integer, or double values",
      call. = FALSE
    )
  }
  # R's native diagnostic printers take integer storage for logical values.
  # Materialize expressions so side effects run once, before printing.
  if (value@value@mode == "logical") {
    value <- cast_to_mode(value, "integer", "print()")
  }
  if (passes_as_scalar(value@value) && is.null(value@value@name)) {
    # Length-one constructors may still be Fortran arrays. The printers'
    # scalar entry points require a scalar, regardless of expression syntax.
    value <- Fortran(glue("sum([{value}])"), Variable(value@value@mode))
  }
  value <- hoist_unless_name(value, hoist)
  printer <- if (value@value@mode == "integer") "intpr" else "dblepr"
  if (passes_as_scalar(value@value)) {
    Fortran(glue('call {printer}1("", 0, {value})'))
  } else {
    Fortran(glue('call {printer}("", 0, {value}, size({value}))'))
  }
}

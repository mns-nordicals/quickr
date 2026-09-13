# r2f-dimensions.R
# Handlers for dimension queries: length, nrow, ncol, dim

# --- Handlers ---

r2f_handlers[["length"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  if (x@value@has_dim && passes_as_scalar(x@value)) {
    return(Fortran("1_c_int", Variable("integer")))
  }
  Fortran(glue("size({x})"), Variable("integer"))
}

r2f_handlers[["nrow"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  if (x@value@has_dim && passes_as_scalar(x@value)) {
    return(Fortran("1_c_int", Variable("integer")))
  }
  Fortran(glue("size({x}, 1)"), Variable("integer"))
}

r2f_handlers[["ncol"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x}, 2)"), Variable("integer"))
}

r2f_handlers[["dim"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  if (x@value@has_dim && x@value@rank == 1L) {
    extent <- if (passes_as_scalar(x@value)) "1_c_int" else glue("size({x})")
    return(Fortran(extent, Variable("integer", list(1L))))
  }
  Fortran(glue("shape({x})"), Variable("integer", x@value@rank))
}

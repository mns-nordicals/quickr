# Take parsed R code (anything returnable by base::str2lang()) and returns
# a Fortran object, which is a string of Fortran code and some attributes
# describing the value.
lang2fortran <- r2f <- function(
  e,
  scope = NULL,
  ...,
  calls = character(),
  hoist = NULL
) {
  ## 'hoist()' is a function that individual handlers can call to pre-emit some
  ## Fortran code. E.g., to setup a temporary variable if the generated Fortran
  ## code doesn't neatly translate into a single expression.
  hoisted <- character()
  if (is.null(hoist)) {
    delayedAssign("hoist_connection", textConnection("hoisted", "w", TRUE))
    hoist <- function(...) {
      writeLines(as.character(unlist(c(character(), ...))), hoist_connection)
    }
    # if performance with textConnection() becomes an issue, maybe switch to an
    # anonymous file(), though, each hoisting context is typically shortlived and
    # usually 0 lines are hoisted per context, and if they are hoisted, a small number.
  }

  fortran <- switch(
    typeof(e),
    language = {
      # a call
      handler <- get_r2f_handler(callable <- e[[1L]])

      match.fun <- attr(handler, "match.fun", TRUE)
      if (is.null(match.fun)) {
        match.fun <- get0(callable, parent.env(globalenv()), mode = "function")
        # this is a best effort to, eg. resolve `seq.default` from `seq`.
        # This should likely be moved into attaching the `match.fun` attr
        # to handlers, for more involved resolution (e.g., with getS3Method())
        if ("UseMethod" %in% all.names(body(match.fun))) {
          match.fun <- get0(
            paste0(callable, ".default"),
            parent.env(globalenv()),
            mode = "function",
            ifnotfound = match.fun
          )
        }
      }
      if (typeof(match.fun) == "closure") {
        e <- match.call(match.fun, e)
      }

      if (isTRUE(getOption("quickr.r2f.debug"))) {
        try(handler(
          as.list(e)[-1L],
          scope,
          ...,
          calls = c(calls, as.character(callable)),
          hoist = hoist
        )) -> res
        if (inherits(res, "try-error")) {
          debugonce(handler)
          handler(
            as.list(e)[-1L],
            scope,
            ...,
            calls = c(calls, as.character(callable)),
            hoist = hoist
          )
        }

        res
      } else {
        handler(
          as.list(e)[-1L],
          scope,
          ...,
          calls = c(calls, as.character(callable)),
          hoist = hoist
        )
      }
    },

    integer = ,
    double = ,
    complex = ,
    logical = atomic2Fortran(e),

    symbol = {
      s <- as.character(e)
      # logicals that come in from R are passed as integer types,
      # so for all fortran ops we cast to logical with /=0
      if (
        !is.null(scope[[e]] -> val) &&
          val@mode == "logical" &&
          val@is_external
      ) {
        s <- paste0("(", s, "/=0)")
      }
      Fortran(s, value = scope[[e]])
    },

    ## handling 'object' and 'closure' here are both bad ideas,
    ## TODO: delete both
    # "object" = {
    #   if (inherits(e, Variable))
    #     e <- Fortran(character(), e)
    #   stopifnot(inherits(e, Fortran))
    #   e
    # },

    closure = {
      if (is.null(name <- attr(e, "name", TRUE))) {
        name <- if (is.symbol(name <- substitute(e))) {
          as.character(name)
        } else {
          "anonymous_function"
        }
      }

      stopifnot(is.null(scope))
      new_fortran_subroutine(name, e)
    },

    ## all the other typeof() possible values
    # "character",
    # "raw" ,
    # "list",
    # "NULL",
    # "function",
    # "special",
    # "builtin",
    # "environment",
    # "S4",
    # "pairlist",
    # "promise",
    # "char",
    # "...",
    # "any",
    # "expression",
    # "externalptr",
    # "bytecode",
    # "weakref"
    # default
    stop("Unsupported object type encountered: ", typeof(e))
  )

  if (length(hoisted)) {
    combined <- str_flatten_lines(c(hoisted, fortran))
    attributes(combined) <- attributes(fortran)
    fortran <- combined
  }

  attr(fortran, "r") <- e
  fortran
}


atomic2Fortran <- function(x) {
  stopifnot(is_scalar_atomic(x))
  s <- switch(
    typeof(x),
    double = ,
    integer = num2fortran(x),
    logical = if (x) ".true." else ".false.",
    complex = sprintf("(%s, %s)", num2fortran(Re(x)), num2fortran(Im(x)))
  )
  Fortran(s, Variable(typeof(x)))
}

num2fortran <- function(x) {
  stopifnot(typeof(x) %in% c("integer", "double"))
  digits <- 7L
  nsmall <- switch(typeof(x), integer = 0L, double = 1L)
  repeat {
    s <- format.default(x, digits = digits, nsmall = nsmall, scientific = 1L)
    if (x == eval(str2lang(s))) {
      # eval() needed for negative and complex numbers
      break
    }
    add(digits) <- 1L
    if (digits > 22L) {
      stop("number formatting error: ", x, " formatted as : ", s)
    }
  }
  paste0(s, switch(typeof(x), double = "_c_double", integer = "_c_int"))
}


r2f_handlers := new.env(parent = emptyenv())


get_r2f_handler <- function(name) {
  stopifnot("All functions called must be named as symbols" = is.symbol(name))
  get0(name, r2f_handlers) %||%
    stop("Unsupported function: ", name, call. = FALSE)
}

r2f_default_handler <- function(args, scope = NULL, ..., calls) {
  # stopifnot(is.call(e), is.symbol(e[[1L]]))

  x <- lapply(args, r2f, scope = scope, calls = calls, ...)
  s <- sprintf("%s(%s)", last(calls), str_flatten_commas(x[-1]))
  Fortran(s)
}

## ??? export as S7::convert() methods?
register_r2f_handler <- function(name, fun) {
  for (nm in name) {
    r2f_handlers[[nm]] <- fun
  }
  invisible(fun)
}

.r2f_handler_not_implemented_yet <- function(e, scope, ...) {
  stop(
    gettextf("'%s' is not implemented yet", as.character(e[[1L]])),
    call. = FALSE
  )
}

r2f_handlers[["declare"]] <- function(args, scope, ...) {
  for (a in args) {
    if (is_missing(a)) {
      next
    }
    if (is_type_call(a)) {
      var <- type_call_to_var(a)
      var@is_arg <- var@name %in% names(formals(scope@closure))
      scope[[var@name]] <- var
    } else if (is_call(a, quote(`{`))) {
      Recall(as.list(a)[-1], scope)
    }
  }

  Fortran("")
}


r2f_handlers[["Fortran"]] <- function(args, scope = NULL, ...) {
  if (!is_string(args[[1]])) {
    stop("Fortran() must be called with a string")
  }
  Fortran(args[[1]])
  # enable passing through literal fortran code
  # used like:
  #   Fortran("nearest(x, 1)", double(length(x)))
  #   Fortran("nearest(x, 1)", x)
  #   Fortran("x = nearest(x, 1)")
}

r2f_handlers[["("]] <- function(args, scope, ...) {
  r2f(args[[1L]], scope, ...)
}

r2f_handlers[["{"]] <- function(args, scope, ..., hoist = NULL) {
  # every top level R-expr / fortran statement gets its own hoist target.
  x <- lapply(args, r2f, scope, ...)
  code <- str_flatten_lines(x)

  # browser()
  value <- (if (length(args)) last(x)@value) %||% Variable()
  Fortran(code, value)
}


# ---- reduction intrinsics ----

create_mask_hoist <- function() {
  .hoisted_mask <- NULL

  try_set <- function(mask) {
    stopifnot(inherits(mask, Fortran), mask@value@mode == "logical")
    # each hoist can only accept one mask.
    if (is.null(.hoisted_mask)) {
      .hoisted_mask <<- mask
      return(TRUE)
    }
    # if the mask is identical, we accept it.
    if (identical(.hoisted_mask, mask)) {
      return(TRUE)
    }
    # can't hoist this mask.
    FALSE
  }

  get_hoisted <- function() .hoisted_mask

  environment()
}

register_r2f_handler(
  c("max", "min", "sum", "prod"),
  function(
    args,
    scope,
    ...
  ) {
    intrinsic <- switch(
      last(list(...)$calls),
      max = "maxval",
      min = "minval",
      sum = "sum",
      prod = "product"
    )

    reduce_arg <- function(arg) {
      mask_hoist <- create_mask_hoist()
      x <- r2f(arg, scope, ..., hoist_mask = mask_hoist$try_set)
      if (x@value@rank == 0) {
        return(x)
      }
      hoisted_mask <- mask_hoist$get_hoisted()
      s <- glue(
        if (is.null(hoisted_mask)) {
          "{intrinsic}({x})"
        } else {
          "{intrinsic}({x}, mask = {hoisted_mask})"
        }
      )
      Fortran(s, Variable(x@value@mode))
    }

    if (length(args) == 1) {
      reduce_arg(args[[1]])
    } else {
      args <- lapply(args, reduce_arg)
      mode <- reduce_promoted_mode(args)
      s <- switch(
        last(list(...)$calls),
        max = glue("max({str_flatten_commas(args)})"),
        min = glue("min({str_flatten_commas(args)})"),
        sum = glue("({str_flatten(args, ' + ')})"),
        prod = glue("({str_flatten(args, ' * ')})")
      )
      Fortran(s, Variable(mode))
    }
  }
)


r2f_handlers[["which.max"]] <- r2f_handlers[["which.min"]] <-
  function(args, scope = NULL, ...) {
    stopifnot(length(args) == 1)
    x <- r2f(args[[1L]], scope, ...)
    stopifnot(
      "Values passed to which.max()/which.min() must be 1d arrays" = x@value@rank ==
        1
    )
    valout <- Variable(mode = "integer") # integer scalar

    if (x@value@mode == "logical") {
      val <- switch(
        last(list(...)$calls),
        which.max = ".true.",
        which.min = ".false."
      )
      f <- glue("findloc({x}, {val}, 1)")
    } else {
      intrinsic <- switch(
        last(list(...)$calls),
        which.max = "maxloc",
        which.min = "minloc"
      )
      f <- glue("{intrinsic}({x}, 1)")
    }

    Fortran(f, valout)
  }


r2f_handlers[["["]] <- function(
  args,
  scope,
  ...,
  hoist_mask = function(mask) FALSE
) {
  # only a subset of R's x[...] features can be translated here. `...` can only be:
  # - a single logical mask, of the same rank as `x`. returns a rank 1 vector.
  # - a number of arguments matching the rank of `x`, with each being
  #   an integer of rank 0 or 1. In this case, a rank 1 logical becomes
  #   converted to an integer with

  var <- args[[1]]
  var <- r2f(var, scope, ...)

  idxs <- whole_doubles_to_ints(args[-1])
  idxs <- imap(idxs, function(idx, i) {
    if (is_missing(idx)) {
      Fortran(":", Variable("integer", var@value@dims[[i]]))
    } else {
      sub <- r2f(idx, scope, ...)
      if (sub@value@mode == "double") {
        # Fortran subscripts must be integers; coerce numeric expressions
        Fortran(
          glue("int({sub}, kind=c_ptrdiff_t)"),
          Variable("integer", sub@value@dims)
        )
      } else {
        sub
      }
    }
  })

  if (
    length(idxs) == 1 &&
      idxs[[1]]@value@mode == "logical" &&
      idxs[[1]]@value@rank == var@value@rank
  ) {
    mask <- idxs[[1]]
    if (hoist_mask(mask)) {
      return(var)
    }
    return(Fortran(
      glue("pack({var}, {mask})"),
      Variable(var@value@mode, dims = NA)
    ))
  }

  if (length(idxs) != var@value@rank) {
    stop(
      "number of args to x[...] must match the rank of x, received:",
      deparse1(as.call(c(quote(`[`, args))))
    )
  }

  drop <- args$drop %||% TRUE

  idxs <- lapply(idxs, function(subscript) {
    # if (!idx@value@rank %in% 0:1)
    #   stop("all args to x[...] must have rank 0 or 1",
    #        deparse1(as.call(c(quote(`[`,args )))))
    switch(
      paste0(subscript@value@mode, subscript@value@rank),
      logical0 = {
        Fortran(":", Variable("integer", NA))
      },
      logical1 = {
        # we convert to a temp integer vector, doing the equivalent of R's which()
        i <- scope@get_unique_var("integer")
        f <- glue("pack([({i}, {i}=1, size({subscript}))], {subscript})")
        return(Fortran(f, Variable("int", NA)))
      },
      integer0 = {
        if (drop) {
          subscript
        } else {
          Fortran(glue("{subscript}:{subscript}"), Variable("int", 1))
        }
      },
      integer1 = {
        subscript
      },
      # double0 = { },
      # double1 = { },
      stop(
        "all args to x[...] must be logical or integer of rank 0 or 1",
        deparse1(as.call(c(quote(`[`, args))))
      )
    )
  })

  dims <- drop_nulls(lapply(idxs, \(idx) idx@value@dims[[1]]))
  outval <- Variable(var@value@mode, dims)
  Fortran(glue("{var}({str_flatten_commas(idxs)})"), outval)
}


r2f_handlers[[":"]] <- function(args, scope, ...) {
  # depending on context, this translation can vary.

  # x[a:b]    becomes   x(a:b)
  # for(i in a:b){}  becomes  do i = a,b ...
  # c(a:b)  becomes  ({tmp}, {tmp}=a,b)
  args <- whole_doubles_to_ints(args)
  .[start, end] <- lapply(args, r2f, scope, ...)
  step <- glue("sign(1, {end}-{start})")
  val <- Variable("integer", NA)
  fr <- switch(
    list(...)$calls |> drop_last() |> last(),
    "[" = glue("{start}:{end}:{step}"),
    "for" = glue("{start}, {end}, {step}"),
    {
      i <- scope@get_unique_var("integer")
      glue("[ ({i}, {i} = {start}, {end}, {step}) ]")
    }
    # default
  )
  Fortran(fr, val)
}


r2f_handlers[["seq"]] <- function(args, scope, ...) {
  args <- whole_doubles_to_ints(args) # only casts if trunc(dbl) == dbl
  if (!is.null(args$length.out) || !is.null(args$along.with)) {
    stop("seq(length.out=, along.with=) not implemented yet")
  }

  .[from, to, by] <- lapply(args, r2f, scope, ...)[c("from", "to", "by")]
  by <- by %||% Fortran(glue("sign(1, {to}-{from})"), Variable("integer"))

  # Fortran only supports integer sequences in do and implicit do contexts.
  # to make a double sequence, needs to be in via an implied map() call, like
  # seq(1, 10, .1) ->    [(x * 0.1, x = 10, 50)]
  #
  # e.g., i <- scope@get_unique_var("integer")
  # glue("[({i} * by, {i} = int(from/by), int(to/by))]")
  if (
    from@value@mode != "integer" ||
      to@value@mode != "integer" ||
      by@value@mode != "integer"
  ) {
    stop("non-integer seq()'s not implemented yet.")
  }

  # depending on context, this translation can vary.
  #
  # x[a:b]    becomes   x(a:b)
  # for(i in a:b){}  becomes  do i = a,b ...
  # c(a:b)  becomes  ({tmp}, {tmp}=a,b)
  val <- Variable("integer", NA)
  fr <- switch(
    list(...)$calls |> drop_last() |> last(),
    "[" = glue("{from}:{to}:{by}"),
    "for" = glue("{from}, {to}, {by}"),
    {
      i <- scope@get_unique_var("integer")
      glue("[ ({i}, {i} = {from}, {to}, {by}) ]")
    }
    # default
  )
  Fortran(fr, val)
}


r2f_handlers[["ifelse"]] <- function(args, scope, ...) {
  .[mask, tsource, fsource] <- lapply(args, r2f, scope, ...)
  # (tsource, fsource, mask)
  mode <- tsource@value@mode
  dims <- conform(mask@value, tsource@value, fsource@value)@dims
  Fortran(glue("merge({tsource}, {fsource}, {mask})"), Variable(mode, dims))
}

# ---- pure elemental unary math intrinsics ----

## real and complex intrinsics
register_r2f_handler(
  c(
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sqrt",
    "exp",
    "log",
    "floor",
    "ceiling"
  ),
  function(args, scope, ...) {
    stopifnot(length(args) == 1L)
    arg <- r2f(args[[1]], scope, ...)
    intrinsic <- last(list(...)$calls)
    Fortran(
      glue("{intrinsic}({arg})"),
      Variable(mode = arg@value@mode, dims = arg@value@dims)
    )
  }
)

r2f_handlers[["log10"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  f <- if (arg@value@mode == "complex") {
    glue("(log({arg}) / log(10.0_c_double))")
  } else {
    glue("log10({arg})")
  }
  Fortran(
    f,
    Variable(mode = arg@value@mode, dims = arg@value@dims)
  )
}

## accepts real, integer, or complex
r2f_handlers[["abs"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- if (arg@value@mode == "complex") {
    Variable(mode = "double", dims = arg@value@dims)
  } else {
    Variable(mode = arg@value@mode, dims = arg@value@dims)
  }
  Fortran(glue("abs({arg})"), val)
}


# ---- complex elemental unary intrinsics ----

r2f_handlers[["Re"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "double", dims = arg@value@dims)
  Fortran(glue("real({arg})"), val)
}

r2f_handlers[["Im"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "double", dims = arg@value@dims)
  Fortran(glue("aimag({arg})"), val)
}

# Modulus (magnitude)
r2f_handlers[["Mod"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "double", dims = arg@value@dims)
  Fortran(glue("abs({arg})"), val)
}

# Argument (phase angle, radians)
r2f_handlers[["Arg"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "double", dims = arg@value@dims)
  Fortran(glue("atan2(aimag({arg}), real({arg}))"), val)
}

# conjg() returns a complex value; R uses Conj()
r2f_handlers[["Conj"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "complex", dims = arg@value@dims)
  Fortran(glue("conjg({arg})"), val)
}


# ---- elemental binary infix operators ----

maybe_cast_double <- function(x) {
  if (x@value@mode == "logical") {
    Fortran(
      glue("merge(1_c_double, 0_c_double, {x})"),
      Variable("double", x@value@dims)
    )
  } else if (x@value@mode == "integer") {
    Fortran(
      glue("real({x}, kind=c_double)"),
      Variable("double", x@value@dims)
    )
  } else {
    x
  }
}

r2f_handlers[["+"]] <- function(args, scope, ...) {
  # Support both binary and unary plus
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ...)
    Fortran(glue("(+{x})"), Variable(x@value@mode, x@value@dims))
  } else {
    .[left, right] <- lapply(args, r2f, scope, ...)
    Fortran(glue("({left} + {right})"), conform(left@value, right@value))
  }
}

r2f_handlers[["-"]] <- function(args, scope, ...) {
  # Support both binary and unary minus
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ...)
    Fortran(glue("(-{x})"), Variable(x@value@mode, x@value@dims))
  } else {
    .[left, right] <- lapply(args, r2f, scope, ...)
    Fortran(glue("({left} - {right})"), conform(left@value, right@value))
  }
}

r2f_handlers[["*"]] <- function(args, scope = NULL, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  Fortran(glue("({left} * {right})"), conform(left@value, right@value))
}

r2f_handlers[["/"]] <- function(args, scope = NULL, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  left <- maybe_cast_double(left)
  right <- maybe_cast_double(right)
  Fortran(glue("({left} / {right})"), conform(left@value, right@value))
}

r2f_handlers[["as.double"]] <- function(args, scope = NULL, ...) {
  stopifnot(length(args) == 1L)
  maybe_cast_double(r2f(args[[1]], scope, ...))
}

r2f_handlers[["^"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  Fortran(glue("({left} ** {right})"), conform(left@value, right@value))
}


# ---- matrix operations ----

# Helper: is the Fortran expression a simple named variable reference?
is_simple_var_ref <- function(x) {
  is.character(x) && grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)
}

# Ensure an expression is bound to a named variable for BLAS calls
ensure_array_var <- function(expr, scope, hoist) {
  if (is_simple_var_ref(as.character(expr))) {
    return(as.character(expr))
  }
  tmp <- scope@get_unique_var("double")
  tmp@dims <- attr(expr, "value", TRUE)@dims %||% expr@value@dims
  assign(tmp@name, tmp, scope)
  hoist(glue("{tmp} = {expr}"))
  as.character(tmp)
}

# t(x) -> transpose(x)
r2f_handlers[["t"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1]], scope, ...)
  if (x@value@rank != 2) {
    stop("t(x) supports rank-2 arrays only in v1")
  }
  dims <- rev(x@value@dims)
  Fortran(glue("transpose({x})"), Variable("double", dims))
}

# colSums/rowSums/colMeans/rowMeans for rank-2 arrays
r2f_handlers[["colSums"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  X <- r2f(args[[1]], scope, ...)
  if (X@value@rank != 2) stop("colSums(x) expects a rank-2 matrix")
  X <- maybe_cast_double(X)
  m <- X@value@dims[[1]]
  n <- X@value@dims[[2]]
  Fortran(glue("sum({X}, dim=1)"), Variable("double", list(n)))
}

r2f_handlers[["diag"]] <- function(args, scope, ..., hoist = NULL) {
  # Supports:
  #  - diag(X) where X is matrix: extract diagonal vector
  #  - diag(v) where v is vector: diagonal matrix with v on the diagonal
  #  - diag(k) where k is scalar: identity matrix of size k x k
  stopifnot(length(args) == 1L)
  A <- r2f(args[[1]], scope, ...)
  gv <- attr(scope, "get_unique_var", exact = TRUE)
  if (!is.function(gv)) {
    stop("internal error: invalid scope@get_unique_var")
  }

  # Matrix: extract diagonal vector
  if (A@value@rank == 2) {
    m <- A@value@dims[[1]]
    n <- A@value@dims[[2]]
    Ai <- ensure_array_var(A, scope, hoist)
    i <- gv("integer")
    fr <- glue("[({Ai}({i}, {i}), {i} = 1, min({m}, {n}))]")
    out_dims <- list(call("min", m, n))
    return(Fortran(fr, Variable(A@value@mode, out_dims)))
  }

  # Vector: build a diagonal matrix
  if (A@value@rank == 1 && !passes_as_scalar(A@value)) {
    m <- A@value@dims[[1]]
    V <- ensure_array_var(maybe_cast_double(A), scope, hoist)
    out <- gv("double")
    out@dims <- list(m, m)
    assign(out@name, out, scope)
    i <- gv("integer")
    hoist(glue("{out} = 0.0_c_double"))
    hoist(glue(
      "do {i} = 1, {m}\n  {out}({i}, {i}) = {V}({i})\nend do"
    ))
    return(Fortran(as.character(out), out))
  }

  # Scalar: identity matrix (also accept length-1 vectors)
  if (A@value@rank == 0 || passes_as_scalar(A@value)) {
    # Determine size expression from R-level arg for shape, and Fortran expr for loop bounds
    k_expr <- r2size(args[[1]], scope)
    K <- as.character(A)
    out <- gv("double")
    out@dims <- list(k_expr, k_expr)
    assign(out@name, out, scope)
    i <- gv("integer")
    hoist(glue("{out} = 0.0_c_double"))
    hoist(glue(
      "do {i} = 1, {K}\n  {out}({i}, {i}) = 1.0_c_double\nend do"
    ))
    return(Fortran(as.character(out), out))
  }

  stop("diag(x) expects a scalar, vector, or matrix")
}

r2f_handlers[["rowSums"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  X <- r2f(args[[1]], scope, ...)
  if (X@value@rank != 2) stop("rowSums(x) expects a rank-2 matrix")
  X <- maybe_cast_double(X)
  m <- X@value@dims[[1]]
  n <- X@value@dims[[2]]
  Fortran(glue("sum({X}, dim=2)"), Variable("double", list(m)))
}

r2f_handlers[["colMeans"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  X <- r2f(args[[1]], scope, ...)
  if (X@value@rank != 2) stop("colMeans(x) expects a rank-2 matrix")
  X <- maybe_cast_double(X)
  m <- X@value@dims[[1]]
  n <- X@value@dims[[2]]
  Fortran(glue("sum({X}, dim=1) / real({m}, kind=c_double)"), Variable("double", list(n)))
}

r2f_handlers[["rowMeans"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  X <- r2f(args[[1]], scope, ...)
  if (X@value@rank != 2) stop("rowMeans(x) expects a rank-2 matrix")
  X <- maybe_cast_double(X)
  m <- X@value@dims[[1]]
  n <- X@value@dims[[2]]
  Fortran(glue("sum({X}, dim=2) / real({n}, kind=c_double)"), Variable("double", list(m)))
}

# qr(A): thin QR via LAPACK dgeqrf; returns upper-triangular R (k x n), k=min(m,n)
r2f_handlers[["qr"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  A_sym <- args[[1]]
  stopifnot(is.symbol(A_sym))
  A_var <- get(A_sym, scope)
  if (A_var@rank != 2) {
    stop("qr(A) expects rank-2 matrix")
  }
  m <- A_var@dims[[1]]
  n <- A_var@dims[[2]]

  gv <- attr(scope, "get_unique_var", exact = TRUE)
  if (!is.function(gv)) {
    stop(paste0(
      "internal error: invalid scope@get_unique_var; type:", typeof(gv),
      " class:", paste(class(gv) %||% character(), collapse = ","),
      " attrs:", paste(names(attributes(scope)) %||% character(), collapse = ",")
    ))
  }

  F <- gv("double"); F@dims <- list(m, n); scope[[as.character(F)]] <- F
  tau <- gv("double"); tau@dims <- list(call("min", m, n)); scope[[as.character(tau)]] <- tau
  work <- gv("double"); work@dims <- list(n); scope[[as.character(work)]] <- work
  info <- gv("integer"); scope[[as.character(info)]] <- info
  iidx <- gv("integer"); jidx <- gv("integer")

  # Copy A, factorize in-place
  hoist(glue("{F} = {as.character(A_sym)}"))
  hoist(glue("call dgeqrf({m}, {n}, {F}, {m}, {tau}, {work}, {n}, {info})"))
  # Temporary error reporting
  hoist(glue(
    "if ({info} /= 0) then\n",
    "  if ({info} < 0) then\n",
    "    call labelpr('dgeqrf: illegal arg; INFO=', 28)\n",
    "  else\n",
    "    call labelpr('dgeqrf: failure; INFO=', 24)\n",
    "  end if\n",
    "  call intpr1('', 0, {info})\n",
    "end if"
  ))

  # Build R (k x n): upper-triangular from F, zeros below diag
  R <- gv("double"); R@dims <- list(call("min", m, n), n); scope[[as.character(R)]] <- R
  hoist(glue(
    "do {jidx} = 1, {n}\n",
    "  do {iidx} = 1, min({m}, {n})\n",
    "    if ({iidx} <= {jidx}) then\n",
    "      {R}({iidx}, {jidx}) = {F}({iidx}, {jidx})\n",
    "    else\n",
    "      {R}({iidx}, {jidx}) = 0.0_c_double\n",
    "    end if\n",
    "  end do\n",
    "end do"
  ))

  Fortran(as.character(R), R)
}

# %*% (matrix multiply)
r2f_handlers[["%*%"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 2L)
  .[left, right] <- lapply(args, r2f, scope, ...)
  left <- maybe_cast_double(left)
  right <- maybe_cast_double(right)

  lr <- left@value@rank
  rr <- right@value@rank

  # matrix %*% matrix -> dgemm
  if (lr == 2 && rr == 2) {
    m <- left@value@dims[[1]]
    k <- left@value@dims[[2]]
    k2 <- right@value@dims[[1]]
    n <- right@value@dims[[2]]

    # result dims: (m x n)
    out <- scope@get_unique_var("double")
    out@dims <- list(m, n)
    assign(out@name, out, scope)

    A <- ensure_array_var(left, scope, hoist)
    B <- ensure_array_var(right, scope, hoist)
    C <- as.character(out)

    lda <- m
    ldb <- k2
    ldc <- m
    hoist(glue(
      "call dgemm('N','N', {m}, {n}, {k}, 1.0_c_double, {A}, {lda}, {B}, {ldb}, 0.0_c_double, {C}, {ldc})"
    ))
    return(Fortran(C, out))
  }

  # matrix %*% vector -> dgemv('N')
  if (lr == 2 && rr == 1) {
    m <- left@value@dims[[1]]
    n <- left@value@dims[[2]]
    nx <- right@value@dims[[1]]
    out <- scope@get_unique_var("double")
    out@dims <- list(m)
    assign(out@name, out, scope)

    A <- ensure_array_var(left, scope, hoist)
    x <- ensure_array_var(right, scope, hoist)
    y <- as.character(out)
    lda <- m
    incx <- 1
    incy <- 1
    hoist(glue(
      "call dgemv('N', {m}, {n}, 1.0_c_double, {A}, {lda}, {x}, {incx}, 0.0_c_double, {y}, {incy})"
    ))
    return(Fortran(y, out))
  }

  # vector %*% matrix -> dgemv('T')
  if (lr == 1 && rr == 2) {
    m <- right@value@dims[[1]]
    n <- right@value@dims[[2]]
    mx <- left@value@dims[[1]]
    out <- scope@get_unique_var("double")
    out@dims <- list(n)
    assign(out@name, out, scope)

    A <- ensure_array_var(right, scope, hoist)
    x <- ensure_array_var(left, scope, hoist)
    y <- as.character(out)
    lda <- m
    incx <- 1
    incy <- 1
    hoist(glue(
      "call dgemv('T', {m}, {n}, 1.0_c_double, {A}, {lda}, {x}, {incx}, 0.0_c_double, {y}, {incy})"
    ))
    return(Fortran(y, out))
  }

  # vector %*% vector -> dot_product (scalar)
  if (lr == 1 && rr == 1) {
    x <- ensure_array_var(left, scope, hoist)
    y <- ensure_array_var(right, scope, hoist)
    tmp <- scope@get_unique_var("double")
    assign(tmp@name, tmp, scope)
    hoist(glue("{tmp} = dot_product({x}, {y})"))
    return(Fortran(as.character(tmp), tmp))
  }

  stop("%*% not implemented for provided operand ranks")
}

# crossprod(x, y) = t(x) %*% y
r2f_handlers[["crossprod"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) %in% 1:2)
  x <- r2f(args[[1]], scope, ...)
  x <- maybe_cast_double(x)
  if (length(args) == 1L) {
    # x' * x
    if (x@value@rank == 2) {
      m <- x@value@dims[[1]]
      k <- x@value@dims[[2]]
      out <- scope@get_unique_var("double")
      out@dims <- list(k, k)
      assign(out@name, out, scope)
      X <- ensure_array_var(x, scope, hoist)
      lda <- m
      ldc <- k
      hoist(glue(
        "call dgemm('T','N', {k}, {k}, {m}, 1.0_c_double, {X}, {lda}, {X}, {lda}, 0.0_c_double, {out}, {ldc})"
      ))
      return(Fortran(as.character(out), out))
    } else if (x@value@rank == 1) {
      X <- ensure_array_var(x, scope, hoist)
      tmp <- scope@get_unique_var("double")
      assign(tmp@name, tmp, scope)
      hoist(glue("{tmp} = dot_product({X}, {X})"))
      return(Fortran(as.character(tmp), tmp))
    }
    stop("crossprod(x) expects rank-1 or rank-2 argument")
  }

  y <- r2f(args[[2]], scope, ...)
  y <- maybe_cast_double(y)
  if (!(x@value@rank %in% 1:2 && y@value@rank %in% 1:2)) {
    stop("crossprod(x, y) expects rank-1 or rank-2 inputs")
  }
  # Handle vectors by treating them as (n x 1) or (n x 1)
  if (x@value@rank == 1 && y@value@rank == 1) {
    X <- ensure_array_var(x, scope, hoist)
    Y <- ensure_array_var(y, scope, hoist)
    tmp <- scope@get_unique_var("double")
    assign(tmp@name, tmp, scope)
    hoist(glue("{tmp} = dot_product({X}, {Y})"))
    return(Fortran(as.character(tmp), tmp))
  }

  # For matrix cases: t(x) %*% y -> dgemm('T','N')
  # Derive dims and leading dimensions
  if (x@value@rank == 1) {
    # treat x as (m x 1), so t(x) is (1 x m)
    m <- x@value@dims[[1]]
    n <- if (y@value@rank == 1) 1L else y@value@dims[[2]]
    k <- m
    out <- scope@get_unique_var("double")
    out@dims <- if (y@value@rank == 1) list(1L) else list(1L, n)
    assign(out@name, out, scope)
    X <- ensure_array_var(x, scope, hoist)
    Y <- ensure_array_var(y, scope, hoist)
    lda <- m
    ldb <- if (y@value@rank == 1) m else y@value@dims[[1]]
    ldc <- 1L
    hoist(glue(
      "call dgemm('T','N', 1, {n}, {m}, 1.0_c_double, {X}, {lda}, {Y}, {ldb}, 0.0_c_double, {out}, {ldc})"
    ))
    return(Fortran(as.character(out), out))
  }

  # x is matrix
  m <- x@value@dims[[1]]
  k <- x@value@dims[[2]]
  if (y@value@rank == 1) {
    # t(x) %*% y -> result (k)
    out <- scope@get_unique_var("double")
    out@dims <- list(k)
    assign(out@name, out, scope)
    X <- ensure_array_var(x, scope, hoist)
    yv <- ensure_array_var(y, scope, hoist)
    lda <- m
    incx <- 1
    incy <- 1
    # use gemv('T')
    hoist(glue(
      "call dgemv('T', {m}, {k}, 1.0_c_double, {X}, {lda}, {yv}, {incx}, 0.0_c_double, {out}, {incy})"
    ))
    return(Fortran(as.character(out), out))
  } else {
    # both matrices
    n <- y@value@dims[[2]]
    Y <- ensure_array_var(y, scope, hoist)
    X <- ensure_array_var(x, scope, hoist)
    out <- scope@get_unique_var("double")
    out@dims <- list(k, n)
    assign(out@name, out, scope)
    lda <- m
    ldb <- y@value@dims[[1]]
    ldc <- k
    hoist(glue(
      "call dgemm('T','N', {k}, {n}, {m}, 1.0_c_double, {X}, {lda}, {Y}, {ldb}, 0.0_c_double, {out}, {ldc})"
    ))
    return(Fortran(as.character(out), out))
  }
}

# tcrossprod(x, y) = x %*% t(y)
r2f_handlers[["tcrossprod"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) %in% 1:2)
  x <- r2f(args[[1]], scope, ...)
  x <- maybe_cast_double(x)
  if (length(args) == 1L) {
    # x %*% t(x)
    if (x@value@rank == 2) {
      m <- x@value@dims[[1]]
      k <- x@value@dims[[2]]
      out <- scope@get_unique_var("double")
      out@dims <- list(m, m)
      assign(out@name, out, scope)
      X <- ensure_array_var(x, scope, hoist)
      lda <- m
      ldc <- m
      hoist(glue(
        "call dgemm('N','T', {m}, {m}, {k}, 1.0_c_double, {X}, {lda}, {X}, {lda}, 0.0_c_double, {out}, {ldc})"
      ))
      return(Fortran(as.character(out), out))
    } else if (x@value@rank == 1) {
      # Vector outer product: (m) -> (m x m)
      m <- x@value@dims[[1]]
      out <- scope@get_unique_var("double")
      out@dims <- list(m, m)
      assign(out@name, out, scope)
      X <- ensure_array_var(x, scope, hoist)
      lda <- m
      ldb <- m
      ldc <- m
      hoist(glue(
        "call dgemm('N','T', {m}, {m}, 1, 1.0_c_double, {X}, {lda}, {X}, {ldb}, 0.0_c_double, {out}, {ldc})"
      ))
      return(Fortran(as.character(out), out))
    }
    stop("tcrossprod(x) expects rank-1 or rank-2 argument")
  }

  y <- r2f(args[[2]], scope, ...)
  y <- maybe_cast_double(y)
  if (!(x@value@rank %in% 1:2 && y@value@rank %in% 1:2)) {
    stop("tcrossprod(x, y) expects rank-1 or rank-2 inputs")
  }

  if (x@value@rank == 1 && y@value@rank == 1) {
    # Vector outer product: (m) %*% t(n) -> (m x n)
    m <- x@value@dims[[1]]
    n <- y@value@dims[[1]]
    out <- scope@get_unique_var("double")
    out@dims <- list(m, n)
    assign(out@name, out, scope)
    X <- ensure_array_var(x, scope, hoist)
    Y <- ensure_array_var(y, scope, hoist)
    lda <- m
    ldb <- n
    ldc <- m
    hoist(glue(
      "call dgemm('N','T', {m}, {n}, 1, 1.0_c_double, {X}, {lda}, {Y}, {ldb}, 0.0_c_double, {out}, {ldc})"
    ))
    return(Fortran(as.character(out), out))
  }

  if (x@value@rank == 1) {
    # (m) %*% t(B) where B is (n x m) -> result (n)
    n <- y@value@dims[[1]]
    m <- y@value@dims[[2]]
    out <- scope@get_unique_var("double")
    out@dims <- list(n)
    assign(out@name, out, scope)
    X <- ensure_array_var(x, scope, hoist)
    Y <- ensure_array_var(y, scope, hoist)
    lda <- m
    incx <- 1
    incy <- 1
    hoist(glue(
      "call dgemv('N', {n}, {m}, 1.0_c_double, {Y}, {n}, {X}, {incx}, 0.0_c_double, {out}, {incy})"
    ))
    return(Fortran(as.character(out), out))
  }

  m <- x@value@dims[[1]]
  k <- x@value@dims[[2]]
  if (y@value@rank == 1) {
    # A %*% t(y) -> gemv('N') with y vector treated as cols
    out <- scope@get_unique_var("double")
    out@dims <- list(m)
    assign(out@name, out, scope)
    X <- ensure_array_var(x, scope, hoist)
    yv <- ensure_array_var(y, scope, hoist)
    lda <- m
    incx <- 1
    incy <- 1
    hoist(glue(
      "call dgemv('N', {m}, {k}, 1.0_c_double, {X}, {lda}, {yv}, {incx}, 0.0_c_double, {out}, {incy})"
    ))
    return(Fortran(as.character(out), out))
  } else {
    n <- y@value@dims[[1]]
    X <- ensure_array_var(x, scope, hoist)
    Y <- ensure_array_var(y, scope, hoist)
    out <- scope@get_unique_var("double")
    out@dims <- list(m, n)
    assign(out@name, out, scope)
    lda <- m
    ldb <- n
    ldc <- m
    hoist(glue(
      "call dgemm('N','T', {m}, {n}, {k}, 1.0_c_double, {X}, {lda}, {Y}, {ldb}, 0.0_c_double, {out}, {ldc})"
    ))
    return(Fortran(as.character(out), out))
  }
}

# solve(A, b): linear solve via LAPACK dgesv (general dense)
r2f_handlers[["solve"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) %in% 1:2)
  A <- r2f(args[[1]], scope, ...)
  A <- maybe_cast_double(A)
  if (A@value@rank != 2) {
    stop("solve(A, b) expects A to be rank-2 square matrix")
  }
  n <- A@value@dims[[1]]
  if (!identical(A@value@dims[[1]], A@value@dims[[2]])) {
    stop("solve(A, b) expects a square matrix A")
  }

  if (length(args) == 1L) {
    stop("solve(A) (matrix inverse) not implemented yet")
  }

  B <- r2f(args[[2]], scope, ...)
  B <- maybe_cast_double(B)
  if (!(B@value@rank %in% 1:2)) {
    stop("solve(A, b) expects b to have rank 1 or 2")
  }

  nrhs <- if (B@value@rank == 1) 1L else B@value@dims[[2]]

  # Prepare outputs and temporaries
  X <- scope@get_unique_var("double")
  X@dims <- if (B@value@rank == 1) list(n) else list(n, nrhs)
  assign(X@name, X, scope)

  A_ <- scope@get_unique_var("double")
  A_@dims <- list(n, n)
  assign(A_@name, A_, scope)

  ipiv <- scope@get_unique_var("integer")
  ipiv@dims <- list(n)
  assign(ipiv@name, ipiv, scope)

  info <- scope@get_unique_var("integer")
  assign(info@name, info, scope)

  # Copies to avoid modifying inputs
  hoist(glue("{A_} = {A}"))
  hoist(glue("{X} = {B}"))

  lda <- n
  ldb <- n
  hoist(glue(
    "call dgesv({n}, {nrhs}, {A_}, {lda}, {ipiv}, {X}, {ldb}, {info})"
  ))
  # Temporary error reporting: print LAPACK info instead of throwing
  hoist(glue(
    "if ({info} /= 0) then\n" ,
    "  if ({info} < 0) then\n" ,
    "    call labelpr('dgesv: illegal arg; INFO=', 26)\n" ,
    "  else\n" ,
    "    call labelpr('dgesv: singular; INFO=', 24)\n" ,
    "  end if\n" ,
    "  call intpr1('', 0, {info})\n" ,
    "end if"
  ))

  # Do not propagate as R error yet; return X regardless
  Fortran(as.character(X), X)
}

# chol(A): Cholesky factorization via LAPACK dpotrf, returns upper-triangular U
r2f_handlers[["chol"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  A_sym <- args[[1]]
  stopifnot(is.symbol(A_sym))
  A_var <- get(A_sym, scope)
  if (A_var@rank != 2) {
    stop("chol(A) expects rank-2 matrix")
  }
  n <- A_var@dims[[1]]
  if (!identical(A_var@dims[[1]], A_var@dims[[2]])) {
    stop("chol(A) expects a square matrix")
  }

  gv <- attr(scope, "get_unique_var", exact = TRUE)
  if (!is.function(gv)) {
    stop(paste0(
      "internal error: invalid scope@get_unique_var; type:", typeof(gv),
      " class:", paste(class(gv) %||% character(), collapse = ","),
      " attrs:", paste(names(attributes(scope)) %||% character(), collapse = ",")
    ))
  }

  F <- gv("double")
  F@dims <- list(n, n)
  scope[[as.character(F)]] <- F
  info <- gv("integer")
  iidx <- gv("integer")
  jidx <- gv("integer")

  hoist(glue("{F} = {as.character(A_sym)}"))
  hoist(glue("call dpotrf('U', {n}, {F}, {n}, {info})"))
  # Temporary error reporting: print LAPACK info instead of throwing
  hoist(glue(
    "if ({info} /= 0) then\n" ,
    "  if ({info} < 0) then\n" ,
    "    call labelpr('dpotrf: illegal arg; INFO=', 28)\n" ,
    "  else\n" ,
    "    call labelpr('dpotrf: not PD; INFO=', 24)\n" ,
    "  end if\n" ,
    "  call intpr1('', 0, {info})\n" ,
    "end if"
  ))
  hoist(glue(
    "do {jidx} = 1, {n}\n  do {iidx} = {jidx}+1, {n}\n    {F}({iidx}, {jidx}) = 0.0_c_double\n  end do\nend do"
  ))
  Fortran(as.character(F), F)
}

r2f_handlers[["chol2inv"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  U_sym <- args[[1]]
  stopifnot(is.symbol(U_sym))
  U_var <- get(U_sym, scope)
  if (U_var@rank != 2) {
    stop("chol2inv(U) expects rank-2 matrix (Cholesky factor)")
  }
  n <- U_var@dims[[1]]
  if (!identical(U_var@dims[[1]], U_var@dims[[2]])) {
    stop("chol2inv(U) expects a square matrix")
  }

  gv <- attr(scope, "get_unique_var", exact = TRUE)
  if (!is.function(gv)) {
    stop("internal error: invalid scope@get_unique_var")
  }

  F <- gv("double"); F@dims <- list(n, n); scope[[as.character(F)]] <- F
  info <- gv("integer"); scope[[as.character(info)]] <- info
  iidx <- gv("integer"); jidx <- gv("integer")

  hoist(glue("{F} = {as.character(U_sym)}"))
  hoist(glue("call dpotri('U', {n}, {F}, {n}, {info})"))
  hoist(glue(
    "if ({info} /= 0) then\n",
    "  if ({info} < 0) then\n",
    "    call labelpr('dpotri: illegal arg; INFO=', 28)\n",
    "  else\n",
    "    call labelpr('dpotri: not PD; INFO=', 24)\n",
    "  end if\n",
    "  call intpr1('', 0, {info})\n",
    "end if"
  ))
  # Symmetrize: copy upper triangle to lower
  hoist(glue(
    "do {jidx} = 1, {n}\n  do {iidx} = {jidx}+1, {n}\n    {F}({iidx}, {jidx}) = {F}({jidx}, {iidx})\n  end do\nend do"
  ))

  Fortran(as.character(F), F)
}


# svd(A): singular values via LAPACK dgesvd (values-only)
r2f_handlers[["svd"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  A_sym <- args[[1]]
  stopifnot(is.symbol(A_sym))
  A_var <- get(A_sym, scope)
  if (A_var@rank != 2) {
    stop("svd(A) expects rank-2 matrix")
  }
  m <- A_var@dims[[1]]
  n <- A_var@dims[[2]]
  k <- call("min", m, n)

  gv <- attr(scope, "get_unique_var", exact = TRUE)
  if (!is.function(gv)) {
    stop("internal error: invalid scope@get_unique_var")
  }

  F <- gv("double"); F@dims <- list(m, n); scope[[as.character(F)]] <- F
  S <- gv("double"); S@dims <- list(k); scope[[as.character(S)]] <- S
  # Dummies for U and VT since JOBU=JOBVT='N'
  U <- gv("double"); U@dims <- list(1L, 1L); scope[[as.character(U)]] <- U
  VT <- gv("double"); VT@dims <- list(1L, 1L); scope[[as.character(VT)]] <- VT
  # Workspace: lwork >= max(3*min(m,n) + max(m,n), 5*min(m,n))
  lwork_expr <- call(
    "max",
    call("+", call("*", 3L, k), call("max", m, n)),
    call("*", 5L, k)
  )
  WORK <- gv("double"); WORK@dims <- list(lwork_expr); scope[[as.character(WORK)]] <- WORK
  info <- gv("integer"); scope[[as.character(info)]] <- info

  hoist(glue("{F} = {as.character(A_sym)}"))
  hoist(glue(
    "call dgesvd('N','N', {m}, {n}, {F}, {m}, {S}, {U}, 1, {VT}, 1, {WORK}, size({WORK}), {info})"
  ))
  hoist(glue(
    "if ({info} /= 0) then\n",
    "  if ({info} < 0) then\n",
    "    call labelpr('dgesvd: illegal arg; INFO=', 28)\n",
    "  else\n",
    "    call labelpr('dgesvd: failure; INFO=', 24)\n",
    "  end if\n",
    "  call intpr1('', 0, {info})\n",
    "end if"
  ))

  Fortran(as.character(S), S)
}

# eigen(A): eigenvalues (symmetric) via LAPACK dsyev (values-only)
r2f_handlers[["eigen"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  A_sym <- args[[1]]
  stopifnot(is.symbol(A_sym))
  A_var <- get(A_sym, scope)
  if (A_var@rank != 2) {
    stop("eigen(A) expects rank-2 matrix")
  }
  if (!identical(A_var@dims[[1]], A_var@dims[[2]])) {
    stop("eigen(A) expects a square matrix")
  }
  n <- A_var@dims[[1]]

  gv <- attr(scope, "get_unique_var", exact = TRUE)
  if (!is.function(gv)) {
    stop("internal error: invalid scope@get_unique_var")
  }

  F <- gv("double"); F@dims <- list(n, n); scope[[as.character(F)]] <- F
  W <- gv("double"); W@dims <- list(n); scope[[as.character(W)]] <- W
  # Workspace: lwork >= max(1, 3*n - 1)
  lwork_expr <- call("max", 1L, call("-", call("*", 3L, n), 1L))
  WORK <- gv("double"); WORK@dims <- list(lwork_expr); scope[[as.character(WORK)]] <- WORK
  info <- gv("integer"); scope[[as.character(info)]] <- info

  hoist(glue("{F} = {as.character(A_sym)}"))
  hoist(glue("call dsyev('N','U', {n}, {F}, {n}, {W}, {WORK}, size({WORK}), {info})"))
  hoist(glue(
    "if ({info} /= 0) then\n",
    "  if ({info} < 0) then\n",
    "    call labelpr('dsyev: illegal arg; INFO=', 28)\n",
    "  else\n",
    "    call labelpr('dsyev: failure; INFO=', 24)\n",
    "  end if\n",
    "  call intpr1('', 0, {info})\n",
    "end if"
  ))

  Fortran(as.character(W), W)
}


r2f_handlers[[">="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} >= {right})"), var)
}
r2f_handlers[[">"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} > {right})"), var)
}
r2f_handlers[["<"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} < {right})"), var)
}
r2f_handlers[["<="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} <= {right})"), var)
}
r2f_handlers[["=="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} == {right})"), var)
}
r2f_handlers[["!="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} /= {right})"), var)
}

# ---- unary logical not ----
r2f_handlers[["!"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ...)
  if (x@value@mode != "logical") {
    stop("'!' expects a logical value; numeric coercions not yet supported")
  }
  Fortran(glue("(.not. {x})"), Variable("logical", x@value@dims))
}


# ---- remainder (%%) and integer division (%/%) ----
#
# R semantics:
#   x %%  y  ==  r   where  r has the sign of y  (divisor)
#   x %/% y  ==  q   where  q = floor(x / y)
# and  x == r + y * q  (within rounding error)
#
# Fortran intrinsics:
#   - MODULO(a,p)   : remainder with sign(p)
#   - FLOOR(x)      : greatest integer ≤ x      (real)
#   - AINT(x)       : truncation toward 0       (real)

r2f_handlers[["%%"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  out_val <- conform(left@value, right@value)
  # MODULO gives result with sign(right) – matches R %% behaviour
  Fortran(glue("modulo({left}, {right})"), out_val)
}

r2f_handlers[["%/%"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  out_val <- conform(left@value, right@value)

  expr <- switch(
    out_val@mode,
    integer = glue("int(floor(real({left}) / real({right})))"),
    double = glue("floor({left} / {right})"),
    stop("%/% only implemented for numeric types")
  )

  Fortran(expr, out_val)
}


# TODO: the scalar || probably need some more type checking.
# TODO: gfortran supports implicit casting that of logical to integer when
# assigning a logical to a variable declared integer, converting `.true.` to `1`,
# but this is not a standard language feature, and Intel's `ifort` uses `-1` for `.true`.
# We should explicitly use
#   `merge(1_c_int, 0_c_int, <lgl>)` to cast logical to int.
register_r2f_handler(
  c("&", "&&", "|", "||"),
  function(args, scope, ...) {
    args <- lapply(args, r2f, scope, ...)
    args <- lapply(args, function(a) {
      if (a@value@mode != "logical") {
        stop("must be logical")
      }
      a
    })
    .[left, right] <- args

    operator <- switch(
      last(list(...)$calls),
      `&` = ,
      `&&` = ".and.",
      `|` = ,
      `||` = ".or."
    )

    s <- glue("{left} {operator} {right}")
    val <- conform(left@value, right@value)
    val@mode <- "logical"
    Fortran(s, val)
  }
)

# --- constructors  ----

r2f_handlers[["c"]] <- function(args, scope = NULL, ...) {
  ff <- lapply(args, r2f, scope, ...)
  s <- glue("[ {str_flatten_commas(ff)} ]")
  lens <- lapply(ff[order(map_int(ff, \(f) f@value@rank))], function(e) {
    rank <- e@value@rank
    if (rank == 0) {
      1L
    } else if (rank == 1) {
      e@value@dims[[1]]
    } else {
      stop("all args passed to c() must be scalars or 1-d arrays")
    }
  })
  mode <- reduce_promoted_mode(ff)
  len <- Reduce(
    \(l1, l2) {
      if (is_scalar_na(l1) || is_scalar_na(l2)) {
        NA
      } else if (is_wholenumber(l1) && is_wholenumber(l2)) {
        l1 + l2
      } else {
        call("+", l1, l2)
      }
    },
    lens
  )
  Fortran(s, Variable(mode, list(len)))
}


r2f_handlers[["cbind"]] <- function(e, scope) {
  .NotYetImplemented()
  ee <- lapply(e[-1], r2f, scope)
  ncols <- lapply(ee, function(f) {
    if (f@value@rank %in% c(0, 1)) {
      1
    } else if (f@value@rank == 2) {
      f@value@dims[[2]]
    }
  })
  ncols <- Reduce(\(a, b) call("+", a, b), ncols)
  ncols <- eval(ncols, scope@sizes)
}


r2f_handlers[["<-"]] <- function(args, scope, ...) {
  target <- args[[1]]
  if (is.call(target)) {
    # given a call like `foo(x) <- y`, dispatch to `foo<-`
    target_callable <- target[[1]]
    stopifnot(is.symbol(target_callable))
    name <- as.symbol(paste0(as.character(target_callable), "<-"))
    handler <- get_r2f_handler(name)
    return(handler(args, scope, ...)) # new hoist target
  }

  # It sure seems like it's be nice if the Fortran() constructor
  # took mode and dims as args directly,
  # without needing to go through Variable...
  stopifnot(is.symbol(target))
  name <- as.character(target)

  value <- args[[2]]
  value <- r2f(value, scope, ...)

  # immutable / copy-on-modify usage of Variable()
  if (is.null(var <- get0(name, scope))) {
    # The var does not exist -> this is a binding to a new symbol
    # Create a fresh Variable carrying only mode/dims and a new name.
    src <- value@value
    var <- Variable(mode = src@mode, dims = src@dims)
    var@name <- name
    # keep a reference to the R expression assigned, if available
    try({ var@r <- attr(value, "r", TRUE) }, silent = TRUE)
    scope[[name]] <- var
  } else {
    # The var already exists, this assignment is a modification / reassignment
    check_assignment_compatible(var, value@value)
    var@modified <- TRUE
    # could probably drop this @modified property, and instead track
    # if the var populated by declare is identical at the end (e.g., perhaps by
    # address, or by attaching a unique id to each var, or ???)
    assign(name, var, scope)
  }

  Fortran(glue("{name} = {value}"))
}


r2f_handlers[["[<-"]] <- function(args, scope = NULL, ...) {
  # TODO: handle logical subsetting here, which must become a where a construct like:
  #   x[lgl] <- val
  # becomes
  # where (lgl)
  #   x = val
  # end where
  # ! but if {va} references {x}, it will only see the subset x, not the full {x}
  # e.g.,
  # sum(x) is not the same as `where lgl \n sum(x) \n end where`
  # ditto for ifelse() ?
  # e <- as.list(e)

  stopifnot(is_call(target <- args[[1L]], "["))
  target <- r2f(target, scope)

  value <- r2f(args[[2L]], scope)
  Fortran(glue("{target} = {value}"))
}

reduce_promoted_mode <- function(...) {
  getmode <- function(d) {
    if (inherits(d, Fortran)) {
      d <- d@value
    }
    if (inherits(d, Variable)) {
      return(d@mode)
    }
    if (is.list(d) && length(d)) {
      lapply(d, getmode)
    }
  }
  modes <- unique(unlist(getmode(list(...))))

  if ("double" %in% modes) {
    "double"
  } else if ("integer" %in% modes) {
    "integer"
  } else if ("logical" %in% modes) {
    "logical"
  } else {
    NULL
  }
}


r2f_handlers[["="]] <- r2f_handlers[["<-"]]

r2f_handlers[["logical"]] <- function(args, scope, ...) {
  Fortran(".false.", Variable(mode = "logical", dims = r2dims(args, scope)))
}

r2f_handlers[["integer"]] <- function(args, scope, ...) {
  Fortran("0", Variable(mode = "integer", dims = r2dims(args, scope)))
}

r2f_handlers[["double"]] <- function(args, scope, ...) {
  Fortran("0", Variable(mode = "double", dims = r2dims(args, scope)))
}

r2f_handlers[["numeric"]] <- r2f_handlers[["double"]]

r2f_handlers[["runif"]] <- function(args, scope, ..., hoist = NULL) {
  attr(scope, "uses_rng") <- TRUE

  dims <- r2dims(args$n, scope)
  var <- Variable("double", dims)

  min <- args$min %||% 0
  max <- args$max %||% 1
  default_min <- identical(min, 0) || identical(min, 0L)
  default_max <- identical(max, 1) || identical(max, 1L)

  if (default_min && default_max) {
    get1rand <- "unif_rand()"
  } else if (default_min) {
    max <- r2f(max, scope, ..., hoist = hoist)
    get1rand <- glue("unif_rand() * {max}")
  } else {
    max <- r2f(max, scope, ..., hoist = hoist)
    min <- r2f(min, scope, ..., hoist = hoist)
    get1rand <- glue("({min} + (unif_rand() * ({max} - {min})))")
  }

  if (passes_as_scalar(var)) {
    fortran <- get1rand
  } else {
    tmp_i <- scope@get_unique_var("integer") ## would be better as uint64...
    fortran <- glue("[({get1rand}, {tmp_i}=1, {dims[[1L]]})]")
  }

  Fortran(fortran, var)
}


r2f_handlers[["character"]] <- r2f_handlers[["raw"]] <-
  .r2f_handler_not_implemented_yet


r2f_handlers[["matrix"]] <- function(args, scope = NULL, ...) {
  args$data %||% stop("matrix(data=) must be provided, cannot be NA")
  out <- r2f(args$data, scope, ...)
  out@value <- Variable(
    mode = out@value@mode,
    dims = r2dims(list(args$nrow, args$ncol), scope)
  )
  out

  # TODO: reshape() if !passes_as_scalar(out)
}


conform <- function(..., mode = NULL) {
  var <- NULL
  # technically, types are implicit promoted, but we'll let <- handle that.
  for (var in drop_nulls(list(...))) {
    if (passes_as_scalar(var)) {
      next
    } else {
      break
    }
  }
  if (is.null(var)) {
    NULL
  } else {
    Variable(mode %||% var@mode, var@dims)
  }
}


# ---- printers ----

r2f_handlers[["cat"]] <- function(args, scope, ...) {
  args <- lapply(args, r2f, scope, ...)
  # can do a lot more here still, just a POC for now
  stopifnot(length(args) == 1, typeof(args[[1]]) == "character")
  label <- args[[1]]
  if (!endsWith(label, "\n")) {
    stop("cat(<strings>) must end with '\n'")
  }
  label <- substring(label, 1, nchar(label) - 1)

  Fortran(glue('call labelpr("{label}", {nchar(label)})'))
}

r2f_handlers[["print"]] <- function(args, scope = NULL, ...) {
  # args <- lapply(as.list(e)[-1], r2f, scope)
  # args <- as.list(e)[-1]
  # can do alot more here still, just a POC for now
  stopifnot(length(args) == 1, typeof(args[[1]]) == "symbol")
  name <- args[[1]]
  var <- get(name, envir = scope)
  name <- as.character(name)
  if (var@mode == "logical") {
    name <- sprintf("(%s/=0)", name)
  }
  label <- ""
  # browser()
  if (passes_as_scalar(var)) {
    # } "scalar"
    # paste0(c(var@mode, scalar) collapse = "_"),
    printer <- switch(
      var@mode,
      logical = ,
      integer = "intpr1",
      double = "dblepr1",
      {
        print(var)
        stop("Unsupported type in print()")
      }
    )

    Fortran(glue('call {printer}("{label}", {nchar(label)}, {name})'))
  } else {
    printer <- switch(
      var@mode,
      logical = ,
      integer = "intpr",
      double = "dblepr",
      {
        print(var)
        stop("Unsupported type in print()")
      }
    )

    Fortran(glue(
      'call {printer}("{label}", {nchar(label)}, {name}, size({name}))'
    ))
  }
}

# r2f_handlers[["ifelse"]] <- function(e, scope) {
#   # TODO:
#   #   <- and [<- need to be aware of this construct for it to make sense.
#   .[test, yes, no] <- lapply(e[-1], r2f, scope)
#   Fortran(glue("where ({test}}
#                 {indent(yes)}
#                 elsewhere
#                 {indent({no})
#                 end where"))
# }

r2f_handlers[["length"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x})"), Variable("integer"))
}
r2f_handlers[["nrow"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x}, 1)"), Variable("integer"))
}
r2f_handlers[["ncol"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x}, 2)"), Variable("integer"))
}
r2f_handlers[["dim"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("shape({x})"), Variable("integer", x@value@rank))
}


# this is just `[` handler
r2f_slice <- function(args, scope, ...) {}


# ---- control flow ----

r2f_handlers[["if"]] <- function(args, scope, ..., hoist = NULL) {
  # cond uses the current hoist context.
  cond <- r2f(args[[1]], scope, ..., hoist = hoist)

  # true and false branchs gets their own hoist target.
  true <- r2f(args[[2]], scope, ..., hoist = NULL)

  if (length(args) == 2) {
    Fortran(glue(
      "
      if ({cond}) then
      {indent(true)}
      end if
      "
    ))
  } else {
    false <- r2f(args[[3]], scope, ..., hoist = NULL)
    Fortran(glue(
      "
      if ({cond}) then
      {indent(true)}
      else
      {indent(false)}
      end if
      "
    ))
  }
}


# TODO: return

# ---- repeat ----
r2f_handlers[["repeat"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  body <- r2f(args[[1]], scope, ...)
  Fortran(glue(
    "do
    {indent(body)}
    end do
    "
  ))
}

# ---- break ----
r2f_handlers[["break"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 0L)
  Fortran("exit")
}

# ---- break ----
r2f_handlers[["next"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 0L)
  Fortran("cycle")
}

# ---- while ----
r2f_handlers[["while"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 2L)
  cond <- r2f(args[[1]], scope, ...)
  body <- r2f(args[[2]], scope, ...) ## should we set a new hoist target here?
  Fortran(glue(
    "do while ({cond})
    {indent(body)}
    end do
    "
  ))
}

## ---- for ----
r2f_iterable <- function(e, scope, ...) {
  .NotYetImplemented()

  if (is.symbol(e)) {
    var <- get(e, scope)
    iterable <- r2f(...)
  }

  # list(var, iterable, body_prefix)
}


r2f_handlers[["for"]] <- function(args, scope, ...) {
  .[var, iterable, body] <- args
  stopifnot(is.symbol(var))
  var <- as.character(var)
  scope[[var]] <- Variable(mode = "integer", name = var)

  iterable <- r2f_iterable_handlers[[as.character(iterable[[1]])]](
    iterable,
    scope
  )
  body <- r2f(body, scope, ...)

  Fortran(glue(
    "do {var} = {iterable}
    {indent(body)}
    end do
    "
  ))
}

r2f_iterable_handlers := new.env()

r2f_iterable_handlers[["seq_len"]] <- function(e, scope, ...) {
  x <- as.list(e)[-1]
  if (length(x) != 1) {
    stop("too many args to seq_len()")
  }
  x <- x[[1]]
  start <- 1L
  end <- r2f(x)
  glue("{start}, {end}")
}

r2f_iterable_handlers[["seq"]] <- function(e, scope) {
  ee <- match.call(seq.default, e)
  ee <- whole_doubles_to_ints(ee)

  start <- r2f(ee$from, scope)
  end <- r2f(ee$to, scope)
  step <- if (is.null(ee$by)) {
    glue("sign(1, {end}-{start})")
  } else {
    r2f(ee$by, scope)
  }

  str_flatten_commas(
    start,
    end,
    step
  )
}

r2f_iterable_handlers[[":"]] <- function(e, scope) {
  ee <- whole_doubles_to_ints(e)
  .[start, end] <- as.list(ee)[-1] |> lapply(r2f, scope)

  glue("{start}, {end}, sign(1, {end}-{start})")
}


r2f_iterable_handlers[["seq_along"]] <- function(e, scope) {
  x <- as.list(e)[-1]
  if (length(x) != 1) {
    stop("too many args to seq_along()")
  }
  x <- x[[1]]
  start <- 1
  end <- sprintf("size(%s)", r2f(x, scope))
  glue("{start}, {end}")
}


# ---- helpers ----

check_call <- function(e, nargs) {
  if (length(e) != (nargs + 1L)) {
    stop("Too many args to: ", as.character(e[[1L]]))
  }
}

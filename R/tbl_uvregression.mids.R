#' Univariable regression model summary
#'
#' @description
#' This function estimates univariable regression models and returns them in
#' a publication-ready table.
#' It can create regression models holding
#' either a covariate or an outcome constant.
#'
#' @section `x` and `y` arguments:
#' For models holding outcome constant, the function takes as arguments a data frame,
#' the type of regression model, and the outcome variable `y=`. Each column in the
#' data frame is regressed on the specified outcome. The `tbl_uvregression()`
#' function arguments are similar to the [`gtsummary::tbl_regression()`] arguments. Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{tbl_uvregression vignette}
#' for detailed examples.
#'
#' You may alternatively hold a single covariate constant. For this, pass a data
#' frame, the type of regression model, and a single
#' covariate in the `x=` argument. Each column of the data frame will serve as
#' the outcome in a univariate regression model. Take care using the `x` argument
#' that each of the columns in the data frame are appropriate for the same type
#' of model, e.g. they are all continuous variables appropriate for [lm], or
#' dichotomous variables appropriate for logistic regression with [glm].
#'
#' @inheritSection gtsummary::tbl_regression Methods
#'
#' @param data (`data.frame`, `survey.design`, `mids`)\cr
#'   A data frame or a survey design object.
#' @param method (`string`/`function`)\cr
#'   Regression method or function, e.g. [lm], [glm], [survival::coxph], `survey::svyglm`, etc.
#'   Methods may be passed as functions (`method=lm`) or as strings (`method='lm'`).
#' @param y,x (`expression`, `string`)\cr
#'   Model outcome (e.g. `y=recurrence` or `y=Surv(time, recur)`) or
#'   covariate (e.g. `x=trt`.
#'   All other column specified in `include` will be regressed against the constant `y` or `x`.
#'   Specify one and only one of `y` or `x`.
#' @param formula (`string`)\cr
#'   String of the model formula.
#'   Uses [`glue::glue()`] syntax. Default is `"{y} ~ {x}"`, where `{y}`
#'   is the dependent variable, and `{x}` represents a single covariate. For a
#'   random intercept model, the formula may be `formula = "{y} ~ {x} + (1 | gear)"`.
#' @param method.args (named `list`)\cr
#'   Named list of arguments assed to `method`.
#' @param hide_n (scalar `logical`)\cr
#'   Hide N column. Default is `FALSE`
#' @inheritParams gtsummary::tbl_regression
#' @author Daniel D. Sjoberg
#'
#' @seealso See gtsummary::tbl_regression \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{vignette}  for detailed examples
#' @name tbl_uvregression
#'
#' @return A `tbl_uvregression` object
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"), reference_pkg = "gtsummary")
#' # Example 1 ----------------------------------
#' tbl_uvregression(
#'   trial,
#'   method = glm,
#'   y = response,
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE,
#'   include = c("age", "grade")
#' )
#'
#' # Example 2 ----------------------------------
#' # rounding pvalues to 2 decimal places
#' library(survival)
#'
#' tbl_uvregression(
#'   trial,
#'   method = coxph,
#'   y = Surv(ttdeath, death),
#'   exponentiate = TRUE,
#'   include = c("age", "grade", "response"),
#'   pvalue_fun = label_style_pvalue(digits = 2)
#' )
NULL

# method for mids objects
#' @export
#' @name tbl_uvregression
tbl_uvregression.mids <- function(data,
                                  y = NULL,
                                  x = NULL,
                                  method,
                                  method.args = list(),
                                  exponentiate = FALSE,
                                  label = NULL,
                                  include = everything(),
                                  tidy_fun = pool_and_tidy_mice,
                                  hide_n = FALSE,
                                  show_single_row = NULL,
                                  conf.level = 0.95,
                                  estimate_fun = ifelse(exponentiate, label_style_ratio(), label_style_sigfig()),
                                  pvalue_fun = label_style_pvalue(digits = 1),
                                  formula = "{y} ~ {x}",
                                  add_estimate_to_reference_rows = FALSE,
                                  conf.int = TRUE, ...) {
  set_cli_abort_call()
  y <- enquo(y)
  x <- enquo(x)
  method.args <- enquo(method.args)

  # all transformations done on original unimputed data..
  complete_data <- data$data

  # setting default values -----------------------------------------------------
  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_deprecated_theme_element("tbl_regression-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun", default = pvalue_fun)
  }
  pvalue_fun <- as_function(pvalue_fun, arg = "pvalue_fun")

  check_scalar_logical(exponentiate)
  if (missing(estimate_fun)) {
    estimate_fun <-
      get_theme_element("tbl_regression-arg:estimate_fun", default = estimate_fun)
  }
  estimate_fun <- as_function(estimate_fun, arg = "estimate_fun")

  if (missing(conf.int)) {
    conf.int <- get_theme_element("tbl_regression-arg:conf.int", default = conf.int)
  }
  if (missing(conf.level)) {
    conf.level <- get_theme_element("tbl_regression-arg:conf.level", default = conf.level)
  }
  if (missing(add_estimate_to_reference_rows)) {
    add_estimate_to_reference_rows <-
      get_theme_element("tbl_regression-arg:add_estimate_to_reference_rows",
                        default = add_estimate_to_reference_rows)
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(method)
  check_scalar_logical(hide_n)
  check_scalar_logical(add_estimate_to_reference_rows)
  check_scalar_logical(conf.int)
  check_scalar_range(conf.level, range = c(0, 1))
  check_uvregression_formula(formula)

  # check that only one of arguments x and y is specified
  if ((!is_quo_empty(x) && !is_quo_empty(y)) || (is_quo_empty(x) && is_quo_empty(y))) {
    cli::cli_abort(
      "Must specify one and only one of arguments {.arg x} and {.arg y}.",
      call = get_cli_abort_call()
    )
  }


  # process inputs -------------------------------------------------------------
  x <- .process_x_and_y_args_as_string(complete_data, x)
  y <- .process_x_and_y_args_as_string(complete_data, y)
  check_scalar(x, allow_empty = TRUE)
  check_scalar(y, allow_empty = TRUE)


  cards::process_selectors(
    as.data.frame(complete_data),
    include = {{ include }},
    show_single_row = {{ show_single_row }}
  )

  # styler: off
  # remove any variables specified in arguments `x`/`y` from include
  include <- include |>
    setdiff(tryCatch(stats::reformulate(c(x, y)) |> all.vars(), error = \(e) character()))
  # remove any variables not in include
  show_single_row <-
    if (is_empty(x)) intersect(show_single_row, include)
  else intersect(show_single_row, x)
  #styler: on

  cards::process_formula_selectors(
    as.data.frame(complete_data)[include],
    label = label
  )
  cards::check_list_elements(
    x = label,
    predicate = \(x) is_string(x),
    error_msg = "Each value passed in the {.arg label} argument must be a string of length {.val {1}}."
  )

  .check_haven_labelled(as.data.frame(complete_data)[include])

  # fill in labels
  label <-
    purrr::map(include, ~label[[.x]] %||% attr(as.data.frame(complete_data)[[.x]], 'label') %||% .x) |>
    set_names(include)

  # will return call, and all object passed to in table1 call
  # the object func_inputs is a list of every object passed to the function
  tbl_uvregression_inputs <- as.list(environment())

  # construct models -----------------------------------------------------------
  lst_models <-
    include |>
    # construct a formula for each model
    .construct_uvregression_formulas(formula = formula, x = x, y = y) |>
    # build models
    .construct_uvregression_models_mids(data = data, method = method, method.args = !!method.args)

  # summarize each regression model with `tbl_regression()` --------------------
  lst_tbls <-
    lst_models |>
    .construct_uvregression_tbls(
      label = label, exponentiate = exponentiate, tidy_fun = tidy_fun,
      show_single_row = show_single_row, conf.level = conf.level,
      estimate_fun = estimate_fun, pvalue_fun = pvalue_fun,
      add_estimate_to_reference_rows = add_estimate_to_reference_rows,
      conf.int = conf.int, x = x, ...
    )

  # if the outcome varied, then replace the variable names within tbls
  if (is_empty(y)) {
    lst_tbls <- lst_tbls |>
      purrr::imap(
        function(tbl, variable) {
          tbl$table_body$variable <- variable
          tbl$table_body$var_type <- NA_character_
          tbl
        }
      )
  }

  # stacking results to return -------------------------------------------------
  results <- gtsummary::tbl_stack(lst_tbls)
  class(results) <- c("tbl_uvregression", "gtsummary")

  # update column header if `x=` was used --------------------------------------
  if (!is_empty(x)) {
    results <- gtsummary::modify_table_styling(results, columns = "label", label = "**Outcome**")
  }

  # removing modify_stat_* columns ---------------------------------------------
  results$table_styling$header <-
    results$table_styling$header %>%
    dplyr::select(-starts_with("modify_stat_"))

  # adding column of N ---------------------------------------------------------
  # if (hide_n == FALSE) results <- add_n(results, location = "label") # styler: off

  # exporting results ----------------------------------------------------------
  results$inputs <- tbl_uvregression_inputs
  results$call_list <- list(tbl_uvregression = match.call())

  results
}

.construct_uvregression_models_mids <- function(formulas, data, method, method.args) {
  method.args <- enquo(method.args)
  imap(
    formulas,
    \(formula, variable) {
      # Use mice::with() to apply the formula and fit the model for each imputed dataset
      model_i <- mice_in_tbl_uvregression(data = data,
                                          method = method,
                                          formula = formula,
                                          method.args = !!method.args)
      model_i
    }
  )
}


mice_in_tbl_uvregression <- function(data,
                                     method,
                                     formula = NULL,
                                     method.args = NULL) {
  # set up the formula
  environment(formula) <- environment()
  formula_expr <- deparse(as.formula(formula))  # This converts the formula to text
  # use function from cardx to convert method args to list, which can then become text for glue function
  method.args <- .as_list_of_exprs({{ method.args }})

  # confirm method.args is a list, and then convert to text
  if (!is.null(method.args) && is.list(method.args)) {
    if (is.null(names(method.args))) {
      stop("method.args must be a named list, e.g., list(family = binomial(link = 'log'))")
    }

    # Extract the arguments from the list, ensuring they are named
    method_args_expr <- paste0(
      ", ",
      paste(
        sapply(seq_along(method.args), function(i) {
          arg_name <- names(method.args)[i]
          arg_value <- deparse(method.args[[i]])
          glue::glue("{arg_name} = {arg_value}")
        }),
        collapse = ", "
      )
    )
  } else {
    method_args_expr <- ""
  }

  # Construct the call expression
  fun_call <- parse(
    text = glue::glue(
      "with(data = {deparse(substitute(data))}, expr = {deparse(substitute(method))}(as.formula('{formula_expr}'){method_args_expr}))"
    )
  )
  # Evaluate the constructed call
  out <- eval(fun_call)
  out$call$expr[[2]] <- out$call$expr[[2]][[2]]
  out
  # Check if the result is a mira object
  if (inherits(out, "mira")) {
    return(out)
  } else {
    stop("The result is not a mira object!")
  }
}

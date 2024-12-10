# unexported helper functions from the gt summary package


# Constructor helpers
.construct_uvregression_tbls <- function(models, label, exponentiate, tidy_fun,
                                         show_single_row, conf.level,
                                         estimate_fun, pvalue_fun,
                                         add_estimate_to_reference_rows,
                                         conf.int, x, ...) {
  imap(
    models,
    function(model, variable) {
      tbl_i <-
        cards::eval_capture_conditions(
          tbl_regression(
            x = model,
            include = ifelse(is_empty(x), variable, x),
            label =
              # styler: off
              if (is_empty(x)) label[variable]
            else label[variable] |> set_names(x),
            # styler: on
            exponentiate = exponentiate,
            tidy_fun = tidy_fun,
            show_single_row =
              # styler: off
              if (is_empty(x)) intersect(variable, show_single_row)
            else intersect(x, show_single_row),
            # styler: on
            conf.level = conf.level,
            estimate_fun = estimate_fun,
            pvalue_fun = pvalue_fun,
            add_estimate_to_reference_rows = add_estimate_to_reference_rows,
            conf.int = conf.int,
            ...
          )
        )

      if (!is_empty(tbl_i[["error"]])) {
        cli::cli_abort(
          c("There was an {cli::col_red('error')} running {.fun tbl_regression} for variable {.val {variable}}. See message below.",
            "x" = "{tbl_i[['error']]}"),
          call = get_cli_abort_call()
        )
      }
      if (!is_empty(tbl_i[["warning"]])) {
        cli::cli_inform(
          c("There was a {cli::col_yellow('warning')} running {.fun tbl_regression} for variable {.val {variable}}. See message below.",
            "!" = "{tbl_i[['warning']]}")
        )
      }

      tbl_i[["result"]]
    }
  )

}

.construct_uvregression_formulas <- function(include, formula, x, y) {
  include |>
    # first, construct formula
    map(
      \(variable) {
        formula_i <-
          cards::eval_capture_conditions(
            glue_data(
              .x = list(y = ifelse(is_empty(y), cardx::bt(variable), y),
                        x = ifelse(is_empty(x), cardx::bt(variable), x)),
              formula
            ) |>
              stats::as.formula()
          )
        if (!is_empty(formula_i[["error"]])) {
          cli::cli_abort(
            c("There was an error constructing the formula for variable {.val {variable}}. See message below.",
              "x" = "{formula_i[['error']]}"),
            call = get_cli_abort_call()
          )
        }
        formula_i[["result"]]
      }
    ) |>
    set_names(include)
}

check_uvregression_formula <- function(formula) {
  # first formula must be a string
  check_string(formula)
  formula_split <- strsplit(formula, split = "~", fixed = TRUE)[[1]]
  if (length(formula_split) != 2L) {
    cli::cli_abort(
      "The {.arg formula} argument must be have structure of a standard formula, e.g. {.val {{y}} ~ {{x}}}.",
      call = get_cli_abort_call()
    )
  }

  # {y} must appear once in the string and on the LHS of the formula
  if (length(unlist(regmatches(formula, m = gregexpr("{y}", formula, fixed = TRUE)))) != 1L ||
      length(unlist(regmatches(formula_split[[1]], m = gregexpr("{y}", formula_split[[1]], fixed = TRUE)))) != 1L) {
    cli::cli_abort(
      c("Error in argument {.arg formula} structure.",
        i = "The substring {.val {{y}}} must appear once in the string and it must be on the LHS of the formula."),
      call = get_cli_abort_call()
    )
  }

  # {x} must appear once in the string and on the RHS of the formula
  if (length(unlist(regmatches(formula, m = gregexpr("{x}", formula, fixed = TRUE)))) != 1L ||
      length(unlist(regmatches(formula_split[[2]], m = gregexpr("{x}", formula_split[[2]], fixed = TRUE)))) != 1L) {
    cli::cli_abort(
      c("Error in argument {.arg formula} structure.",
        i = "The substring {.val {{x}}} must appear once in the string and it must be on the RHS of the formula."),
      call = get_cli_abort_call()
    )
  }

  invisible(formula)
}

.process_x_and_y_args_as_string <- function(data, x, arg_name = rlang::caller_arg(x)) {
  # if quo is empty, then return NULL
  if (is_quo_empty(x)) return(NULL) # styler: off

  # if a character was passed, return it as it
  if (tryCatch(is.character(eval_tidy(x)), error = \(e) FALSE)) return(eval_tidy(x)) # styler: off

  # try tidy evaluation, and if that doesn't work, then return string of input
  tryCatch(
    cards::cards_select(data = as.data.frame(data), expr = x) |> cardx::bt(),
    error = function(e) {
      tryCatch(
        # lastly, convert quosure to a string
        expr_deparse(quo_get_expr(x)),
        error = function(e) {
          cli::cli_abort(
            "There was a problem processing argument {.arg {arg_name}}.",
            call = get_cli_abort_call()
          )
        }
      )
    }
  )
}



#' Set Call Environment for [cli::cli_abort()]
#'
#' Set a call environment to be used as the `call` parameter in [cli::cli_abort()] for package checks. This function
#' is used to ensure that the correct user-facing function is reported for errors generated by internal checks that
#' use [cli::cli_abort()].
#'
#' @param env (`enviroment`)\cr
#'   call environment used as the `call` parameter in [cli::cli_abort()] for package checks
#'
#' @seealso [get_cli_abort_call()]
#'
#' @keywords internal
#' @noRd
set_cli_abort_call <- function(env = rlang::caller_env()) {
  if (getOption("cli_abort_call") |> is.null()) {
    options(cli_abort_call = env)
    set_call <- as.call(list(function() options(cli_abort_call = NULL)))
    do.call(on.exit, list(expr = set_call, add = TRUE, after = FALSE), envir = env)
  }
  invisible()
}

#' Get Call Environment for [cli::cli_abort()]
#'
#' @inheritParams set_cli_abort_call
#' @seealso [set_cli_abort_call()]
#'
#' @keywords internal
#' @noRd
get_cli_abort_call <- function() {
  getOption("cli_abort_call", default = parent.frame())
}

get_theme_element <- function(x, default = NULL, eval = TRUE, env = caller_env()) {
  # returning theme element
  # if eval is FALSE, then returning the un-evaluated theme element
  if (isFALSE(eval)) {
    return(env_gtsummary_theme[[x]] %||% default)
  }

  # the theme element is evaluated in the caller env so it may conditionally
  # set a default depending on other objects only known at the time it is called
  eval_tidy(env_gtsummary_theme[[x]], env = env) %||% default
}

get_deprecated_theme_element <- function(x, default = NULL, eval = TRUE, env = caller_env(), version = "2.0.0") {
  if (!is_empty(env_gtsummary_theme[[x]])) {
    lifecycle::deprecate_warn(
      when = version,
      what = I(glue("gtsummary theme element {shQuote(x)}"))
    )
  }

  get_theme_element(x = x, default = default, eval = eval, env = env)
}


is_quo_empty <- function(x) {
  tryCatch(is_empty(eval_tidy(x)), error = \(e) FALSE)
}

.check_haven_labelled <- function(data) {
  if (some(data, ~ inherits(., "haven_labelled"))) {
    # list of columns with haven_labelled
    haven_labelled_vars <-
      map_lgl(data, ~ inherits(.x, "haven_labelled")) %>%
      keep(identity) %>%
      names()

    cnvt_funs <-
      c("haven::as_factor", "labelled::to_factor", "labelled::unlabelled", "unclass")

    hyperlinks <- c(
      "https://haven.tidyverse.org/articles/semantics.html",
      "https://larmarange.github.io/labelled/articles/intro_labelled.html#unlabelled"
    )

    c(
      "!" = "Column(s) {.val {haven_labelled_vars}} are class {.val haven_labelled}.",
      "i" = "This is an intermediate datastructure not meant for analysis.",
      "i" = "Convert columns with {.fun {cnvt_funs}}. Failure to convert may have unintended consequences or result in error.",
      paste0("{.url ", hyperlinks, "}")
    ) |>
      cli::cli_inform()
  }

  invisible()
}

# new environment
env_gtsummary_theme <- rlang::new_environment()

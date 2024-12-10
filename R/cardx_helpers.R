# unexported helpter functions from the cardx package.
# https://github.com/insightsengineering/cardx

.as_list_of_exprs <- function(x, arg_name = "method.args") {
  x_enexpr <- enexpr(x)
  if (is_call_simple(x_enexpr)) {
    return(call_args(x_enexpr))
  }

  cli::cli_abort(
    c("There was an error processing the {.arg {argname}} argument.",
      i = "Expecting a simple call. See {.help rlang::is_call_simple} for details."
    ),
    call = get_cli_abort_call()
  )
}

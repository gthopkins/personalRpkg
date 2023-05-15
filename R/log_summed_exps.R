#' Grant's implementation of log of sum of exponentiated values
#'
#' @param x a vector of type numeric
#' @param base a positive real number with which we
#' exponentiate. The default is Euler's number, e.
#'
#' @return the log of sum of exponentiated values
#'
#' @examples
#' log_summed_exps(1:2000)
#' log_summed_exps(c(1, 394, 102342, 18))
#'
#' @export
log_summed_exps <- function(x, base = exp(1)) {
  x_max <- which.max(x)
  return(x_max + log(sum(base^c(x - x_max)), base))
}

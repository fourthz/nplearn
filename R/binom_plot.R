#' Binomial Probability Density Plot
#'
#' Plot a simple density plot to illustrate the binomial distribution for a
#' specified number of trials and a constant probability of success.
#'
#' @param n integer
#' @param p double
#'
#' @return plot
#' @importFrom graphics plot
#' @importFrom stats dbinom
#' @export
#'
#' @examples
#' binom_plot(8, 0.5)
binom_plot <- function(n, p) {
  x <- 0:n
  prob <- dbinom(x, n, p)
  plot(x, prob, type="h")
}

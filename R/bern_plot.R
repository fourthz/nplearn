#' Bernoulli Density Plot
#'
#' Plot a simple density plot to illustrate the Bernoulli distribution for a
#' specified probability of success of a Bernoulli event.
#'
#' @param p double
#'
#' @return plot
#' @importFrom Rlab dbern
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @export
#'
#' @examples
#' bern_plot(0.7)
bern_plot <- function(p) {
  x <- 0:1
  prob <- Rlab::dbern(x, p)
  plot(x, prob, type="h", xaxt = "n", xlim = c(-1, 2), ylim = c(0, 1))
  axis(1, at = c(0, 1))
}

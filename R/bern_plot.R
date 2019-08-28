#' Bernoulli Density Plot
#'
#' @param p double
#'
#' @return plot
#' @export
#'
#' @examples
bern_plot <- function(p) {
  x <- 0:1
  prob <- dbern(x, p)
  plot(x, prob, type="h", xaxt = "n", xlim = c(-1, 2), ylim = c(0, 1))
  axis(1, at = c(0, 1))
}

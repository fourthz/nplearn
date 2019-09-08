#' Power Plot
#'
#' Create a plot of power as a function of sample size for an upper-tail
#' binomial exact test using simple null and alternative hypotheses,
#' a maximum Type I error rate, and an optional power target.
#'
#' @param h_0 double
#' @param h_1 double
#' @param alpha double
#' @param line double
#'
#' @return plot
#' @export
#'
#' @examples
#' power_plot(h_0 = 0.5, h_1 = 0.6, alpha = .05, line = 0.80)
power_plot <- function(h_0, h_1, alpha = .05, line = 0) {
  x <- 10:200
  prob <- pbinom(qbinom(1-alpha, x, h_0), x, h_1, lower.tail = FALSE)
  plot(x,
       prob,
       type = "s",
       main = "Power as a Function of Sample Size",
       xlab = "Sample Size",
       ylab = "Power")

  if (line > 0 & line < 1) {
    abline(h = 0.80)
  }
}

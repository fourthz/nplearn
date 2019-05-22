binom_plot <- function(n, p) {
  x <- 0:n
  prob <- dbinom(x, n, p)
  plot(x, prob, type="h")
}

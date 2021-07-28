#' Product Binomial Distribution
#'
#' @param n1 integer
#' @param n2 integer
#' @param p1 double
#' @param p2 double
#'
#' @return data frame of probability distribution
#' @export
#'
#' @examples
#' prodbinom_dist(5, 3, .5, .4, two.sided = TRUE)
prodbinom_dist <- function(n1, n2, p1, p2, two.sided = TRUE) {

  pos1 <- rep(0:n1, each = n2+1)
  pos2 <- rep(0:n2, n1+1)

  delta <- (pos1/n1) - (pos2/n2)
  abs.delta <- abs(delta)

  prob1 <- dbinom(pos1, n1, p1)
  prob2 <- dbinom(pos2, n2, p2)

  prob <- prob1*prob2

  if (two.sided) {
    dist <- cbind(pos1, pos2, delta, abs.delta, prob)
    dist <- dist[order(dist[, 4]),]
    cumul.prob.up <- round(cumsum(dist[, 5]), 6)
    cumul.prob.down <- round(rev(cumsum(rev(dist[, 5]))), 6)
    prob <- round(dist[, 5], 6)
    dist <- cbind(dist[, 1:4], prob, cumul.prob.up, cumul.prob.down)
  } else {
    dist <- cbind(pos1, pos2, delta, prob)
    dist <- dist[order(dist[, 3]),]
    cumul.prob.up <- round(cumsum(dist[, 4]), 6)
    cumul.prob.down <- round(rev(cumsum(rev(dist[, 4]))), 6)
    prob <- round(dist[, 4], 6)
    dist <- cbind(dist[, 1:3], prob, cumul.prob.up, cumul.prob.down)
  }

  return(as.data.frame(dist))
}

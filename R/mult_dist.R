#' Multinomial Distribution
#'
#' @param x integer vector
#' @param pd numeric vector
#' @param chi2.sort boolean
#'
#' @return data frame of probability distribution
#' @export
#'
#' @examples
#' observed <- c(5, 3, 0, 0)
#' probabilities <- c(0.25, 0.25, 0.25, 0.25)
#' mult_dist(observed, probabilities)
mult_dist <- function(x, pd, chi2.sort = FALSE) {
  n = sum(x)
  k = length(x)

  my.fun.1 <- function(x){
    dmultinom(x, prob = pd)
  }

  my.fun.2 <- function(x){
    exp.x <- pd*n
    sum((x - exp.x)^2/exp.x)
  }

  values <- rep(0:n, k)
  dim(values) <- c(n+1, k)
  values <- as.data.frame(values)
  my.table <- as.matrix(expand.grid(values))
  my.table <- my.table[rowSums(my.table) == n, ]
  prob <- apply(my.table, 1, my.fun.1)

  if (chi2.sort) {
    chi.sq <- apply(my.table, 1, my.fun.2)
    my.table <- cbind(my.table, prob, chi.sq)
    my.table <- my.table[order(my.table[,ncol(my.table)], decreasing = TRUE),]
  } else {
    my.table <- cbind(my.table, prob)
  }

  return(as.data.frame(my.table))
}

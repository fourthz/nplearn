#' Randomization Test Distribution
#'
#' Display the distribution of the test statistic for a single-sample
#' randomization test.
#'
#' @param x double
#'
#' @return output
#' @importFrom gtools permutations
#' @export
#'
#' @examples
#' scores <- (5, 3, -7)
#' rand_dist(scores)
rand_dist <- function(x) {

  # Find all the possible sign permutations

  y = c(0,1)
  signs <- gtools::permutations(n = 2,
                                r = length(x),
                                v = y, repeats.allowed = TRUE)

  # Remove the sign of the values

  x_abs <- abs(x)

  # Create a matrix of all possible values for positive scores

  possibles <- sweep(signs, MARGIN = 2, x_abs, '*')

  # Obtain the sums of each for all possible values of T and sort

  T <- rowSums(possibles)
  T <- sort(T)

  # Calculate the probability for each pattern of signs

  prob <- 1:nrow(signs)
  prob[1:nrow(signs)] <- 1/(nrow(signs))

  # Create cumulative probabilities from the bottom up and top down

  cumul_up = 1:nrow(signs)/nrow(signs)
  cumul_down = nrow(signs):1/nrow(signs)

  # Bind and display the distribution

  T_mat = cbind(T, prob, cumul_up, cumul_down)
  T_mat
}

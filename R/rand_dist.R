#' Randomization Test Distribution
#'
#' Display the distribution of the test statistic for a single-sample
#' randomization test.
#'
#' @param x double
#' @param show.all boolean
#'
#' @return output
#' @importFrom gtools permutations
#' @export
#'
#' @examples
#' scores <- (5, 3, -7)
#' rand_dist(scores)
rand_dist <- function(x, show.all = TRUE) {

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

  # If not showing all, then condense

  if (!show.all) {
    T.c <- unique(T)
    prob.c <- 1:length(T.c)
    cumul_up.c <- 1:length(T.c)
    cumul_down.c <- 1:length(T.c)

    for (i in 1:length(T.c)) {
      prob.c[i] <- sum(prob[which(T %in% T.c[i])])
      cumul_up.c[i] <- cumul_up[max(which(T %in% T.c[i]))]
      cumul_down.c[i] <- cumul_down[min(which(T %in% T.c[i]))]
    }

  T <- T.c
  prob <- prob.c
  cumul_up <- cumul_up.c
  cumul_down <- cumul_down.c
  }

  # Bind and display the distribution

  T_mat = noquote(cbind(T,
                        formatC(prob, digits = 7, format = "f"),
                        formatC(cumul_up, digits = 7, format = "f"),
                        formatC(cumul_down, digits = 7, format = "f")))

  T_mat
}

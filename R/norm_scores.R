#' Normal Scores Generation
#'
#' Calculate normal scores for a single sample.
#'
#' @param x double with scores
#' @param two.sample Boolean indicating one or two samples
#'
#' @return double with normal scores
#' @export
#'
#' @examples
#' scores <- c(5, 9, -3, 8, -7)
#' norm_scores(scores)
norm_scores <- function(x, two.sample = FALSE) {

  if (two.sample) {

    # calculate the normal scores

    i <- 1:length(x)
    n.scores <- qnorm(i/(length(x) + 1))

    # Order the normal scores based on the original score order

    n.scores[order(x)] <- n.scores

  } else {

    # calculate the normal scores

    i <- 1:(2*length(x) + 1)
    n.scores <- qnorm(i/(2*length(x) + 2))

    # obtain the positive normal scores

    n.scores <- n.scores[(length(x)+2):(2*length(x)+1)]

    # order the normal scores based on absolute value of original scores

    n.scores[order(abs(x))] <- n.scores

    # attach signs to the normal scores

    n.scores <- n.scores*sign(x)
  }

  return(n.scores)
}

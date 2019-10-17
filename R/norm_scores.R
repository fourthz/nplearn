#' Normal Scores Generation
#'
#' Calculate normal scores for a single sample.
#'
#' @param x double
#'
#' @return double
#' @export
#'
#' @examples
#' scores <- c(5, 9, -3, 8, -7)
#' norm_scores(scores)
norm_scores <- function(x) {

  # calculate the normal scores

  i <- 1:(2*length(x) + 1)
  n.scores <- qnorm(i/(2*length(x) + 2))

  # obtain the positive normal scores

  n.scores <- n.scores[(length(x)+2):(2*length(x)+1)]

  # order the original scores based on absolute value

  x <- x[order(abs(x))]

  # attach signs to the normal scores

  n.scores <- n.scores*sign(x)

  return(n.scores)
}

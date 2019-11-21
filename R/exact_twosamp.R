#' Two-Sample Exact Test Distribution
#'
#' @param y vector of scores
#' @param n sample size of first sample
#' @param duplicate Boolean to indicate inclusion of duplicates of statistic
#'
#' @return outputs exact distribution
#' @export
#'
#' @examples
#' scores <- c(4, 5, 4, 7, 9, 10, 12, 6)
#' exact_twosamp_dist(scores, 3, duplicate = FALSE)
exact_twosamp_dist <- function(y, n, duplicate = TRUE) {

  T.val <- combn(y, n, function(x) sum(x))
  T.val <- sort(T.val)
  prob <- rep(1/choose(length(y), n), length(T.val))

  if (duplicate) {
    prob.up <- cumsum(prob)
    prob.down <- rev(prob.up)
    cbind(T.val, prob, prob.up, prob.down)
  } else {
    T.unique <- unique(T.val)
    u.prob.one <- T.unique

    for (i in 1:length(T.unique)) {
      u.prob.one[i] <- sum(T.val == T.unique[i])*prob[1]
    }

    prob.up <- cumsum(u.prob.one)
    rev.u.prob <- rev(u.prob.one)
    prob.down <- cumsum(rev.u.prob)
    prob.down <- rev(prob.down)
    prob <- u.prob.one
    cbind(T.unique, prob, prob.up, prob.down)
  }
}

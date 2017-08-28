#'
#' Arear Under Curve Score Calculation
#'
#' @param preds Predicted Values
#' @param obs Observed or Actual Values
#'
#' @return AUC Score
#'
#' @examples
#' auc(c(1,1,1),c(1,0.5,1))


auc <- function(preds,obs) {
  if (NROW(preds) != NROW(obs)){
    cat("Predictd values and Actual Values are not of same length!")
    return(0)
  }

  ## need to add factor behaviour within class
  rank <- rank(y_pred)
  n_pos <- sum(y_true == 1)
  n_neg <- sum(y_true == 0)
  auc <- (sum(rank[y_true == 1]) - n_pos * (n_pos + 1)/2)/(n_pos * n_neg)
  return(auc)
}

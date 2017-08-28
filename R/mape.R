#'
#' Mean Absolute Percentage Error Score Calculation
#'
#' @param preds Predicted Values
#' @param obs Observed or Actual Values
#'
#' @return MAPE Score
#'
#' @examples
#' mape(c(1,1,1),c(1,0.5,1))


mape <- function(preds,obs) {
  if (NROW(preds) != NROW(obs)){
    cat("Predictd values and Actual Values are not of same length!")
    return(0)
  }

  return(round(100*mean(abs((obs-preds)/obs)),2))

}

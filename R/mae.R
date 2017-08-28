#'
#' Mean Absolute Error Score Calculation
#'
#' @param preds Predicted Values
#' @param obs Observed or Actual Values
#'
#' @return MAE Score
#'
#' @examples
#' mae(c(1,1,1),c(1,0.5,1))


mae <- function(preds,obs) {
  if (NROW(preds) != NROW(obs)){
    cat("Predictd values and Actual Values are not of same length!")
    return(0)
  }

  return(mean(abs(preds-obs)))

}

#'
#' Bayesian Information Criterion Score Calculation
#'
#' @param preds Predicted Values
#' @param obs Observed or Actual Values
#'
#' @return BIC Score
#'
#' @examples
#' bic(c(1,1,1),c(1,0.5,1))


bic <- function(preds,obs,numVar) {
  if (NROW(preds) != NROW(obs)){
    cat("Predictd values and Actual Values are not of same length!")
    return(0)
  }

  if(missing(numVar) || is.null(numVar)){
    cat("Number of Variable used to build the Prediction Values is required!")
    return(0)
  }

  n <- NROW(preds)
  K <- numVar + 1
  rss <- sum((preds - obs)^2)
  bic <- n + n*log(2*pi) + n * log(rss/n) + log(n)*K
  return(bic)

}

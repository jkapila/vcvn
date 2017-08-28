#'
#' Akaike Information Criterion Score Calculation
#'
#' @param preds Predicted Values
#' @param obs Observed or Actual Values
#'
#' @return AIC Score
#'
#' @examples
#' aic(c(1,1,1),c(1,0.5,1))


aic <- function(preds,obs,numVar) {
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
  aic <- n + n*log(2*pi) + n * log(rss/n) + 2*K

  if(n/K < 40){
    aic <- aic +((2*K*(K+1))/(n-K-1))
  }

  return(aic)

}

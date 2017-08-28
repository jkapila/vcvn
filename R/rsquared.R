#'
#' R squared and Adjusted R squared Score Calculation
#'
#' @param preds Predicted Values
#' @param obs Observed or Actual Values
#' @param numVar Number of predictors used to build the model.
#'   If NULL general R squared is calculated.
#'
#' @return R-squared / Adjusted R-Squared Score
#'
#' @examples
#' rsquared(c(1,1,1),c(1,0.5,1))
#' rsquared(c(1,1,1),c(1,0.5,1),numVar = 1)


rsquared <- function(preds, obs, numVar = NULL) {
  if (NROW(preds) != NROW(obs)){
    cat("Predictd values and Actual Values are not of same length!")
    return(0)
  }

  obs_mean <- mean(obs)
  rss <- sum((preds - obs)^2)
  sst <- sum((preds - obs_mean)^2)
  rsquared <- 1 - (rss/sst)

  if (is.null(numVar)){
    # cat("R Squared: ", rsquared)
    return(rsquared)
  }else{
    # cat("R Squared: ", rsquared)
    n <- NROW(preds)
    rsquared <- 1- (((1-rsquared)*(n-1))/(n-numVar-1))
    # cat("Adj R Squared: ", rsquared)
    return(rsquared)
  }
}

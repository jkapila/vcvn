#'
#' Prediction Scoring
#'
#' @param preds Predicted Values
#' @param obs Observed or Actual Values
#' @param numVar Number of predictors used to build the model.
#'   If NULL, general R squared is calculated with no AIC and BIC.
#'
#' @return Prediction scores
#'
#' @examples
#' predScores(c(1,1,1),c(1,0.5,1))
#'

predScores <- function(preds, obs, numVar = NULL){

  # Need to add different behaviour for classification and regression

  cat("Predicted Values : ",NROW(preds),"has Mean:", mean(preds)," SD:",sd(preds),"\n")
  cat("Actual Values    : ",NROW(obs),"has Mean:", mean(obs)," SD:",sd(obs),"\n")
  cat("Number of Predictors: ",numVar,"\n")
  cat("Chi square test:\n")
  print(chisq.test(x = obs,y = preds))
  cat("Scores:\n")
  cat("\tRMSE              :",rmse(preds,obs),"\n")
  cat("\tMAE               :",mae(preds,obs),"\n")
  cat("\tMAPE              :",mape(preds,obs),"\n")
  cat("\tRsquared          :",rsquared(preds,obs),"\n")
  cat("\tAdjusted Rsquared :",rsquared(preds,obs),"\n")
  cat("\tAIC               :",aic(preds,obs,numVar),"\n")
  cat("\tBIC               :",bic(preds,obs,numVar),"\n")

}

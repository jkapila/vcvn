#'
#' Normalise the Vairables
#'
#' @param x A vector or Data frame to be normalised
#' @param y A depedent vector for depedent normalisation.
#'     Default value is NULL(indepedent normalisation).
#' @param method A method or a vector of methods to apply normalisation on varaibles.
#'     If vector not equal to num of variables "unit" normalisation is implemented for
#'     the rest. Methods implemented are
#'     1) unitMean
#'     2) unitSD
#'     3) unitMeanSD
#'     4) minMax
#'     5) boxCox
#'     6) log
#'     7) meanDiff
#'     8) medianDiff
#'     9) meanSDDiff
#'     10) medianSDDiff
#'     11) zscore (zero Mean and unit SD)
#'     12) power (only square is implemented right now)
#' @param diff.val Difference required from mean and median
#' @param na.rm A boolean for na treatment. Default is TRUE.
#' @param norm.list a list of vlaues for flexible normalisation
#' @param depedentMod Model to be used for Dependent Normalisation(lm or glm)
#' @param scores scores to be used for evaluting the normalisation
#' @details log is impleted with 1 added to the value
#'
#' @return Normalised vector or a data frame
#'
#' @examples
#' normalise(mtcars)
#'

normalise <- function(x,y=NULL,method="zscore",na.rm = TRUE,diff.val = 0,
                      norm.list = list(),depedentMod = NULL, scores = NULL){

  #Defining normalisation functions

  diffNorm <- function(x,na.rm = na.rm,additive,meanit=FALSE,medianit=FALSE,
                       minit=FALSE,maxit=FALSE){
    if(meanit){
      x <- x - mean(x,na.rm = na.rm)
    }
    if(medianit){
      x <- x - median(x,na.rm = na.rm)
    }
    if(minit){
      x <- x - min(x,na.rm = na.rm)
    }
    if(maxit){
      x <- x - min(x,na.rm = na.rm)
    }
    if(!missing(additive)){
      x <- x + additive
    }
    return(x)
  }

  divmulNorm <- function(x,na.rm = na.rm,valueImpl=NULL,divide=FALSE,
                                    sdit=FALSE,minmaxit=FALSE,transformit=FALSE,
                                    transformfunc=NULL,powerit = FALSE,powVal = 2){

    if(sdit){
      x <- x/sd(x,na.rm = na.rm)
    }
    if(minmaxit){
      vl <- max(x,na.rm = na.rm) - min(x,na.rm = na.rm)
      x <- x/vl
    }
    if(transformit){
      x <- transformfunc(x)
    }

    if(powerit){
      x <- x^powVal
    }
    if(!is.null(valueImpl) && divide ){
      x <- x/valueImpl
    }else if(!is.null(valueImpl)){
      x <- x*valueImpl
    }
    return(x)
  }

  # defining the switch statement function
  switchFUN <- function(x, method){
    switch(method,
       "zscore" = divmulNorm(diffNorm(x,additive = 0,meanit = TRUE,na.rm = na.rm),
                             sdit = TRUE,na.rm = na.rm),
       "unitMean" = diffNorm(x,additive = 1,meanit = TRUE,na.rm = na.rm),
       "unitSD" = divmulNorm(x,sdit = TRUE,na.rm = na.rm),
       "unitMeanSD" =divmulNorm(diffNorm(x,additive = 1,meanit = TRUE,na.rm = na.rm),
                               sdit = TRUE,na.rm = na.rm),
       "minmax" = divmulNorm(diffNorm(x,additive = 0,minit = TRUE,na.rm = na.rm),
                           minmaxit = TRUE,na.rm = na.rm),
       "meanDiff" = diffNorm(x,additive = -diff.val,meanit = TRUE,na.rm = na.rm),
       "meadianDiff" = diffNorm(x,additive = -diff.val,medianit = TRUE,na.rm = na.rm),
       "meanSDDiff" = divmulNorm(diffNorm(x,additive = -diff.val,meanit = TRUE,
                                          na.rm = na.rm),sdit = TRUE,na.rm = na.rm),
       "medianSDDiff" = divmulNorm(diffNorm(x,additive = -diff.val,medianit = TRUE
                                            ,na.rm = na.rm),
                                 sdit = TRUE,na.rm = na.rm),
       "log" = divmulNorm(diffNorm(x,additive = 1,na.rm = na.rm),
                          transformit = TRUE,
                        transformfunc = log,na.rm = na.rm),
       "power" = divmulNorm(x,powerit = TRUE,powVal = 2,na.rm = na.rm))
  }

  df <- sapply(x,function(col_df) switchFUN(col_df,method = method))
  df <- as.data.frame(df)
  return(df)
}

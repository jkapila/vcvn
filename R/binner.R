#'
#' Contineous Variable Binner
#'
#' @param x Variable to bin
#' @param y Conditional binnig on depedent variable
#' @param lower Lower bound of bins
#' @param upper Uper bound of bins
#' @param by speration between bins
#' @param min.obs Minimum Observation required in each bin
#' @param sep Bin bound seperator
#' @param above.char Charcter to denote value beyond upper bound
#' @param digits No of digits to be considered for binning
#'
#' @return A binned categorical vector of x
#'
#' @examples
#' binner(c(1,2,3,4,5,6,7,8,9,10,11,13,15,77,99))
#' vec <- c(1,1,1,1,1,2,2,2,2,2,2,5,5,5,6,6,7,8,9,9,10,11,13,15,77,99)
#' binner(vec)
#' binner(vec,by=7)
#' binner(vec,by=3)
#' binner(vec,by=3,upper=20)
#'
#' y <- sample(0:1,NROW(vec_),replace = T)
#' binner(vec,y)
#' binner(seq(0,100,1),by=12)

binner <- function(x, y=NULL,lower, upper, by, min.obs,
                   sep = "-", above.char = "+",digits) {
  if(missing(digits)){
    digits = 3
  }
  if(missing(min.obs)){
    min.obs <- ceiling(NROW(x)*.05)
  }
  if(missing(lower)){
    lower = min(x,na.rm = TRUE)
  }
  if(missing(upper)){
    upper <- ifelse(max(x,na.rm = TRUE) > (as.numeric(quantile(x)[4])*1.5),
                    ceiling(as.numeric(quantile(x)[4]*1.5)),
                    max(x,na.rm = TRUE))
  }
  if(lower == upper){
    print("Data cannot be categorised as it has only one value")
    return(x)
  }
  if(is.null(y)){
    if(missing(by)) {
      by <- 1
      for (i in 1:(upper-lower)){
        x_cuts <- cut(floor(x),breaks = c(seq(lower, upper, by = by), Inf),
                      right = FALSE)
        x_cuts <- data.frame(table(x_cuts))
        if (min(x_cuts[,"Freq"])>=min.obs) break
        by <- i
      }
    }
    print(paste("Binning Criteria- lower:",lower,"upper:",upper,
                "by:",by,"min.obs:",min.obs,"y is NULL:",is.null(y)))
    labs <- c(paste(seq(lower, upper - by, by = by),
                    seq(lower + by - 1, upper - 1, by = by),
                    sep = sep),
              paste(upper, above.char, sep = ""))
    x_cuts <-   cut(x, breaks = c(seq(lower, upper, by = by), Inf),
                    right = FALSE, labels = labs)
    return(x_cuts)
  }else if(!is.null(y)){
    if(missing(by)) {
      by <- 1
      for (i in 1:(upper-lower)){
        x_cuts <- cut(floor(x),breaks = c(seq(lower, upper, by = by), Inf),
                      right = FALSE)
        x_cuts <- data.frame(table(x_cuts,y))
        min_test <- tapply(x_cuts$Freq,x_cuts$x_cuts,
                           function(x) sum(x) > min.obs)
        if(any(min_test) != FALSE & min(x_cuts[,"Freq"])>=min.obs) break
        by <- i
      }
    }
    print(paste("Binning Criteria- lower:",lower,"upper:",upper,
                "by:",by,"min.obs:",min.obs,"y is NULL:",is.null(y)))
    labs <- c(paste(seq(lower, upper - by, by = by),
                    seq(lower + by - 1, upper - 1, by = by),
                    sep = sep),
              paste(upper, above.char, sep = ""))
    x_cuts <-   cut(x, breaks = c(seq(lower, upper, by = by), Inf),
                    right = FALSE, labels = labs)
    return(x_cuts)
  }
}

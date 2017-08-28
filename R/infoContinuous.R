#' Continuous variable information
#'
#' @param data A data frame for which information is to be retireved
#' @param var.specifier Additional variable name specifier
#' @param transpose Get a transpose of a resultant information.
#'  Not Suitable for very large number of variables
#'
#' @return a dta frame with information
#'
#' @examples
#' data(mtcars)
#' infoContinuous(mtcars)
#'

infoContinuous <- function(data,var.specifier = "",transpose = FALSE){
  num_cols <- c()
  for (ii in colnames(data)){
    if (is.numeric(data[,ii]) || is.integer(data[,ii]) || is.double(data[,ii]))
      num_cols <- c(num_cols,ii)
  }

  # opt <- data.frame(sapply(num_cols,function(x) c(mean(data[,x],na.rm = T),
  #                                                 median(data[,x],na.rm = T),
  #                                                 min(data[,x],na.rm = T),
  #                                                 max(data[,x],na.rm = T),
  #                                                 sd(data[,x],na.rm = T),
  #                                                 quantile(data[,x],na.rm = T),
  #                                                 round(sum(is.na(data[,x]))/nrow(data),2)*100)))
  opt <- sapply(data[,num_cols],function(x) c(mean(x,na.rm = T),
                                              median(x,na.rm = T),
                                              min(x,na.rm = T),
                                              max(x,na.rm = T),
                                              sd(x,na.rm = T),
                                              quantile(x,na.rm = T),
                                              round(sum(is.na(x))/nrow(data),2)*100))

  row_names <- c("Mean","Median","Min","Max","SD","0%","25%","50%","75%","100%","NA%")
  if (nchar(var.specifier)!=0){
    row_names <- paste(row_names,var.specifier,sep = "")
  }
  opt <- t(opt)
  opt <- apply(opt,2,function(x) round(x,4))
  colnames(opt) <- row_names

  if (transpose) opt <- t(opt)

  return(opt)
}

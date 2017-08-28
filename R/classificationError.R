#'
#' Calssification Error of a Categorical Variable
#'
#' @param target Depedent Variable
#' @param variable Indepedent Variable
#'
#' @return Calssification Error Score
#'
#' @examples
#' variable=c("A","A","B","A","B","B","B","C","C","C","C","B",
#'  "B","B","A","A","C","C","C","C")
#' target = c(1,0,0,0,2,1,1,2,1,2,2,1,1,0,1,2,1,0,2,1)
#' classificationError(target,variable)


classificationError  <- function(target,variable) {
  data_ <- data.frame(variable=variable,
                      target=target)
  if(is.factor(variable)){
    tot_tab <- data.frame(table(data_sm$variable,data_sm$target))
    tot_tab
    missclass <- 0
    p_sub <- c()
    for (i in unique(tot_tab$Var1)){
      sub_tab <- tot_tab[which(tot_tab$Var1 == i),c("Var2","Freq")]
      p_sub <- c(p_sub,(sum(sub_tab$Freq)/sum(tot_tab$Freq)))
    }
    return(1 - max(p_sub,na.rm = T))
  } else if(!is.factor(variable)){
    return(0)
  }
}

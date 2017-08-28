#'
#' Gini Impurity of a Categorical Variable
#'
#' @param target Depedent Variable
#' @param variable Indepedent Variable
#'
#' @return Gini Impurity Score
#'
#' @examples
#' variable=c("A","A","B","A","B","B","B","C","C","C","C","B",
#'  "B","B","A","A","C","C","C","C")
#' target = c(1,0,0,0,2,1,1,2,1,2,2,1,1,0,1,2,1,0,2,1)
#' giniImpurity(target,variable)


giniImpurity <- function(target,variable){
  data_ <- data.frame(variable=variable,
                      target=target)
  if(is.factor(variabel)){
    pred_parent <- data.frame(table(data_$target))
    gini_parent <- 1 - sum((pred_parent$Freq/sum(pred_parent$Freq)) ^ 2)
    print(gini_parent)

    tot_tab <- data.frame(table(data_$variable,data_$target))

    gini_variable <- 0
    for (i in unique(tot_tab$Var1)){
      sub_tab <- tot_tab[which(tot_tab$Var1 == i),]
      # print(sub_tab)
      g_sub <-  1 - sum((sub_tab$Freq/sum(sub_tab$Freq)) ^ 2)
      # print(g_sub)
      p_sub <- (sum(sub_tab$Freq)/sum(tot_tab$Freq))
      # print(p_sub)
      # print(p_sub*g_sub)
      gini_variable <- gini_variable + (p_sub*g_sub)
      # print(gini_variable)

    }
    print(gini_variable)
    print(gini_parent - gini_variable)
    return(gini_parent - gini_variable)
  }else if(!is.factor(variable)){
    return(0)
  }
}

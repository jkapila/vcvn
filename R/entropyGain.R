#'
#' Entropy Gain of a Categorical Variable
#'
#' @param target Depedent Variable
#' @param variable Indepedent Variable
#'
#' @return Entropy Gain Score
#'
#' @examples
#' variable=c("A","A","B","A","B","B","B","C","C","C","C","B",
#'  "B","B","A","A","C","C","C","C")
#' target = c(1,0,0,0,2,1,1,2,1,2,2,1,1,0,1,2,1,0,2,1)
#' entropyGain(target,variable)
#'

entropyGain  <- function(target,variable) {
  data_ <- data.frame(variable=variable,
                      target=target)
  if(is.factor(variable)){
    pred_parent <- data.frame(table(data_$target))
    pred_parent$pred <- pred_parent$Freq/sum(pred_parent$Freq)
    # print(pred_parent)
    entropy_parent <- -sum(pred_parent$pred*log2(pred_parent$pred))
    print(entropy_parent)

    tot_tab <- data.frame(table(data_$variable,data_$target))

    entropy_variable <- 0
    for (i in unique(tot_tab$Var1)){
      sub_tab <- tot_tab[which(tot_tab$Var1 == i),]
      sub_tab$pred <- sub_tab$Freq/sum(sub_tab$Freq)
      # print(sub_tab)
      g_sub <- -sum(sub_tab$pred*log2(sub_tab$pred))
      # print(g_sub)
      p_sub <- (sum(sub_tab$Freq)/sum(tot_tab$Freq))
      entropy_variable <- entropy_variable + (p_sub*g_sub)
    }
    print(entropy_variable)
    print(entropy_parent - entropy_variable)
    return(entropy_parent - entropy_variable)
  } else if(!is.factor(variable)){
    return(0)
  }
}

#'
#' Information Value of a Contineous Variable
#'
#' @param target Depedent Variable
#' @param variable Indepedent Variable
#' @param by Binning difference of contieous variable
#' @param upper Upper bound of the bins
#'
#' @return Information Value of the Variable
#'
#' @examples
#' vec <- c(1,1,1,1,1,2,2,2,2,2,2,5,5,5,6,6,7,8,9,9,10,11,13,15,77,99)
#' y <- sample(0:1,NROW(vec_),replace = T)
#'
#' informationValue(target = y,variable = vec)
#' informationValue(target = y,variable = vec,by=2)
#' informationValue(target = y,variable = vec,by=1)
#' informationValue(target = y,variable = vec,by=4)
#' informationValue(target = y,variable = vec,by=3)
#' informationValue(target = y,variable = vec,by=3,upper = 50)
#' informationValue(target = y,variable = vec,by=3,upper = 20)
#' informationValue(target = y,variable = vec,by=3,upper = 15)
#' informationValue(target = y,variable = vec,by=3,upper = 12)
#' informationValue(target = y,variable = vec,by=3,upper = 9)
#' informationValue(target = y,variable = vec,by=3,upper = 6)


informationValue <- function(target,variable,event,upper,by){
  data_ <- data.frame(variable=variable,
                      target=target)
  if(missing(event)){
    event <- max(target)
  }

  if(is.factor(variable)){
    return(0)
  }else{
    data_$binned <- binner(x=data_$variable,y=data_$target,upper = upper,by=by)
    if (identical(data_$binned,data_$variable)){
      retrun(0)
    }
    tot_tab <- data.frame(table(data_$binned,data_$target))
    tot_tab
    woe <- 0; iv <- 0
    for (i in unique(tot_tab$Var1)){
      tot_event <- sum(tot_tab[which(tot_tab$Var2 == event),"Freq"])
      tot_bad <- sum(tot_tab[which(tot_tab$Var2 != event),"Freq"])

      sub_tab <- tot_tab[which(tot_tab$Var1 == i),c("Var2","Freq")]
      # print(sub_tab)
      (dist_event <- (sub_tab[sub_tab$Var2 == event,"Freq"]/tot_event))
      (dist_bad <- (sub_tab[sub_tab$Var2 != event,"Freq"]/tot_bad))
      (woe <- log((dist_event/dist_bad),base = exp(1)))
      print(paste("woe:",woe))
      if(is.finite(woe)){
        (iv <- iv + (dist_event - dist_bad)*woe)
      }
      # print(paste("iv:",iv))
    }
    return(iv)
  }
}

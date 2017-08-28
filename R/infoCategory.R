#'
#'Information for Categorical Variables
#'
#'@param data A dat frame with variables
#'@param numFactors possible number of factors for a character variables
#'
#'@return a data frame with summary for categorical variables
#'
#'@examples
#'
#'df <- data.frame(NormalFactor = as.factor(sample(LETTERS[2:10],100,replace = T)),
#'  VeryLargeFactors = as.factor(sample(LETTERS,100,replace = T)),
#'  NormalCharacters = sample(LETTERS[2:20],100,replace = T),
#'  VeryLargeCharacters = sample(c(LETTERS,letters),100,replace = T))
#'
#'infoCategory(df)
#'infoCategory(df,length(c(LETTERS,letters)))
#'

infoCategory <- function(data, numFactors =  20){

  infoCat <- data.frame(matrix(vector(), 0, 4,
                               dimnames=list(c(),c("Variable","Level","Freq","Prec"))),
                        stringsAsFactors=F)

  for (name in colnames(data)){

    var <- data[,name]

    if (is.character(var)){
      if(length(unique(var)) > numFactors){
        cat("Data for ", name ," has ",length(unique(var))," values and
            cannot be treated as factors!")
      }else{
        var <- as.factor(var)
      }
    }

    if(is.factor(var)){

      dfc <- data.frame(table(var))
      dfc <- dfc[order(dfc$Freq,decreasing = TRUE),]
      dfc$Prec <- round( (dfc$Freq / sum(dfc$Freq))*100 ,2)
      info <- data.frame(Variable=name,
                         Level=dfc[,"var"],
                         Freq=dfc[,"Freq"],
                         Prec=dfc[,"Prec"])
      infoCat <- rbind(infoCat,info)
    }

  }
  colnames(infoCat) <- c("Variable","Level","Freq","Prec")
  return(infoCat)
}

#' The Dummy Column Generator
#'
#' @param data data containing the column for which dummy columns to be generated
#' @param name Name of Column
#' @param keep.column To keep column within the data
#' @param keep.col.name keep column name in dummy colmn
#' @param fill.value Wheteher fill value from some other column
#' @param fill.value.from Column name from whihc value to be stored in the dummy columns
#'
#' @return a data frame with dummy columns


dummyColGen <- function(data,name,keep.column=TRUE,keep.col.name=TRUE,fill.value = FALSE,
                           fill.value.from=NULL){
  data_ <- data.frame(data[,name])
  colnames(data_) <- name
  if (!keep.column)  {
    cat("Removing Column: ",name,"\n")
    data[,name] <- NULL
    cat("Columns Left :",colnames(data),"\n")
  }
  for(t in unique(data_[,name])) {
    if (keep.col.name){
      new_col <- gsub("\\s+","_",paste(name,t,sep="_"))
    }else{
      new_col <- gsub("\\s+","_",t)
    }
    if(fill.value){
      if(is.null(fill.value.from)){
        cat("No column given! filling value with 0 and 1!")
        data_[,new_col] <- ifelse(data_[,name]==t,1,0)
      }else{
        data_[,new_col] <- ifelse(data_[,name]==t,data_[,fill.value.from],0)
      }
    }else{
      data_[,new_col] <- ifelse(data_[,name]==t,1,0)
    }
  }
  # print(data_[,new_col])
  data_[,name] <- NULL
  data <- cbind(data,data_)
  cat("New Coulmns: ",colnames(data),"\n")

  return(data)
}

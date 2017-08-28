#' Making varable selection based ranking matirx
#'
#'@param variableMatrix A data frame with variable name and scored methods
#'@param candidateColumn The column which should be treated as column
#'  for candidates. If NULL first clumn will be treated as candidate column.
#'
#'@return A Ranked Matrix
#'

MakeRankedMatrix <- function(variableMatrix,candidateColumn = NULL) {
  if (is.null(candidateColumn)){
    colnames(variableMatrix)[1] <- "Candidates"
  }else {
    variableMatrix <- variableMatrix[,c(candidateColumn,colnames(variableMatrix)[
                                colnames(variableMatrix) != candidateColumn])]
    colnames(variableMatrix)[1] <- "Candidates"
  }

  for ( col_ in colnames(variableMatrix)){
    variableMatrix[,col_] <- switch(col_,
                                    "InformationValue",order(variableMatrix[,col_]),
                                    "GiniImpurity",order(variableMatrix[,col_]),
                                    "ClassificationError",order(variableMatrix[,col_]),
                                    "EntropyGain",order(variableMatrix[,col_]),
                                    "WaldTest",order(variableMatrix[,col_]),
                                    "GiniCoeeficient",order(variableMatrix[,col_]),
                                    "Concordance",order(variableMatrix[,col_]),
                                    "LogLikelihood",order(variableMatrix[,col_]))
  }

  return(variableMatrix)
}

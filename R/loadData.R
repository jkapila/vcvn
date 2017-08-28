#' Wrapper for loading multiple files in coherent manner within a folder.
#'
#' @param path Path Of the folder in which files are stored
#' @param patterns Pattern of filename to be loaded
#' @param lib.selector "readr" or "data.table" to be used
#' @param verbose print coherent output
#' @param return.df return a data.frame or list of many data.frames
#' @param recursive  Recursive search for pattern in th folder
#'
#' @return a Data Frame or a List of Data Frames.

loadData <- function(path,patterns,lib.selector = "readr",verbose = FALSE,
                     return.df=T,start.at, recursive =  FALSE) {
  require(readr)
  require(data.table)
  files <- list.files(path, pattern = '\\.csv', full.names = TRUE,recursive = recursive)
  cat("Files Available",files)
  if(missing(start.at)){
    start.at = 0
  }
  if(missing(patterns)){
    if(lib.selector == "readr"){
      tables <- lapply(files, read_csv,progress=interactive(),skip=start.at)
    }else if (lib.selector == "data.table"){
      tables <- lapply(files, fread,verbose = verbose,stringsAsFactors=FALSE,skip=start.at)
    }
  }else{
    files_ <- files
    files <- c()
    for (i in patterns){
      files <- c(files,files_[stringi::stri_detect(files_,regex = i)])
    }
    if(lib.selector == "readr"){
      tables <- lapply(files, read_csv,progress=interactive())
    }else if (lib.selector == "data.table"){
      tables <- lapply(files, fread,verbose = verbose,stringsAsFactors=FALSE)
    }else {
      tables <- lapply(files, read.csv,header = TRUE,stringsAsFactors=FALSE)
    }
  }
  print(files)
  cat("\nNo of Files Loaded are: ",length(tables))
  if (return.df){
    tables <- data.table::rbindlist(tables)
    data.table::setDF(tables)
  }
  return(tables)
}

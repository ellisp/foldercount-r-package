#' Count files
#' 
#' count the files, given a vector of extensions
#' 
#' @export
#' @param filenames vector of full path filenames to search
#' @param exts vector of file extensions eg c("doc", "docx", "ppt")
#' @param label a potential extra element to add, handy if using this function on many different folder systems
#' @return a data frame with a single row and a column for each file extension, with counts as values.  
#' If label is not NULL, there will be an additional column label.
#' @examples
#' lf <- list.files(recursive = TRUE, full.names = TRUE)
#' count_files_ext_v(lf, binary_exts)
#' count_files_ext_v(lf, text_exts)
count_files_ext_v <- function(filenames, exts, label = NULL){
  tmp <- sapply(exts, function(ext){
    sum(grepl(paste0("\\." , ext, "$"), filenames))
  })
  tmp <- data.frame(t(tmp))
  if(!is.null(label)){
    tmp$label <- label
  }
  return(tmp)
}


#' count the lines in a single file
#' 
#' @export
#' @param fname full path file name
count_lines <- function(fname){
  length(readLines(fname)) 
}


#' count the lines in all the files with a given extension
#' 
#' @export
#' @param filenames vector of files to search for files with the extension
#' @param ext a file extension eg "txt".  Should be for text files of some sort
#' @examples
#' lf <- list.files(recursive = TRUE, full.names = TRUE)
#' count_lines_ext(lf, "r")
#' count_lines_ext(lf, "sql")
count_lines_ext <- function(filenames, ext){
  fn <- filenames[grepl(paste0("\\." , ext, "$"), filenames)]
  tmp <- sapply(fn, count_lines)
  tmp_df <- data.frame(filename = names(tmp), lines = as.numeric(tmp))
  return(tmp_df)
}


#' count the lines in all the files with a vector of given extensions
#' 
#' @export
#' @param filenames a vector of full path file names to search for files of those extensions
#' @param exts a vector of file extensions eg c("txt", "r", "sas", "c")
#' @param onlyknown should the program only proceed if it recognises all the extensions as text files?
#' @param label a potential extra column to add, handy if using this function on many different folder systems
#' @examples
#' lf <- list.files(recursive = TRUE, full.names = TRUE)
#' count_lines_ext_v(lf, text_exts)
#' count_files_ext_v(lf, binary_exts)
count_lines_ext_v <- function(filenames, exts, label = NULL, onlyknown = TRUE){
  if(onlyknown & sum(exts %in% text_exts) != length(exts)){
    message(paste(exts[!exts %in% text_exts], collapse = " "))
    stop("Some of those file extensions don't look like text files to me...")
  }
  output <- NULL
  for(i in 1:length(exts)){
    tmp <- count_lines_ext(filenames = filenames, ext = exts[i])
    if(nrow(tmp) > 0){
      tmp$ext <- exts[i]
      if(is.null(output)){
        output <- tmp
      } else {
        output <- rbind(output, tmp)
      }
    }
  }
  if(!is.null(output) & !is.null(label)){
    output$label <- label
  }
  return(output)
}





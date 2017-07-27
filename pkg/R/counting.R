path <- "social_housing-master"

lf <- tolower(list.files("social_housing-master", all.files = TRUE, recursive = TRUE, full.names = TRUE))
lf <- tolower(list.files("../../blog/ellisp.github.io", all.files = TRUE, recursive = TRUE, full.names = TRUE))


# number of R files

#' Vector of common text file extensions
#' @export
textfile_exts <- c("r", "sas", "do", "sql", "c", "txt", "md", "rmd", "csv")

#' Vector of common binary file extensions (svg isn't really binary but for our purposes...)
#' @export
binary_exts <- c("doc", "docx", "ppt", "pptx", "xls", "xlsx", "xlsc", "xlsm", 
                 "rda", "rdata", "sas7bdat", "dta",
                 "png", "jpg", "jpeg", "gif", "svg", "pdf")

#' Count files
#' 
#' count the files, given a vector of extensions
#' 
#' @export
#' @param exts vector of file extensions eg c("doc", "docx", "ppt")
#' @param filenames vector of full path filenames to search
#' @examples
#' lf <- list.files(recursive = TRUE, full.names = TRUE)
#' count_files_ext_v(binary_exts, lf)
#' count_files_ext_v(textfile_exts, lf)
count_files_ext_v <- function(exts, filenames){
  sapply(exts, function(ext){
    sum(grepl(paste0("\\." , ext, "$"), filenames))
  })
}


#' count the lines in a single file
#' @param fname full path file name
count_lines <- function(fname){
  length(readLines(fname)) 
}


#' count the lines in all the files with a given extension
#' @param ext a file extension eg "txt".  Should be for text files of some sort
#' @param filenames vector of files to search for files with the extension
#' @examples
#' lf <- list.files(recursive = TRUE, full.names = TRUE)
#' count_lines_ext("r", lf)
#' count_lines_ext("sql", lf)
count_lines_ext <- function(ext, filenames){
  fn <- filenames[grepl(paste0("\\." , ext, "$"), filenames)]
  tmp <- sapply(fn, count_lines)
  tmp_df <- data.frame(filename = names(tmp), lines = as.numeric(tmp))
  return(tmp_df)
}


#' count the lines in all the files with a vector of given extensions
#' 
#' @param exts a vector of file extensions eg c("txt", "r", "sas", "c")
#' @param filenames a vector of full path file names to search for files of those extensions
#' @param onlyknown should the program only proceed if it recognises all the extensions as text files?
#' @examples
#' count_lines_ext_v(textfile_exts, lf)
#' count_files_ext_v(binary_exts, lf)
count_lines_ext_v <- function(exts, filenames, onlyknown = TRUE){
  if(onlyknown & sum(exts %in% textfile_exts) != length(exts)){
    message(paste(exts[!exts %in% textfile_exts], collapse = " "))
    stop("Some of those file extensions don't look like text files to me...")
  }
  output <- NULL
  for(i in 1:length(exts)){
    tmp <- count_lines_ext(exts[i], filenames)
    if(nrow(tmp) > 0){
      tmp$ext <- exts[i]
      if(is.null(output)){
        output <- tmp
      } else {
        output <- rbind(output, tmp)
      }
    }
  }
  return(output)
}




# shoudl be an error
count_lines_ext_v(binary_exts, lf)

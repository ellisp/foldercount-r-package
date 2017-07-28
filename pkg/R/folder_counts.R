#' File counts in multiple folders
#'
#' Count all the files of given extension, in each of a collection of folder systems
#' 
#' @export
#' @param foldernames a vector of paths to folders to search (with their subdirectories) for files
#' @param exts a vector of file extensions eg c("r", "sas", "sql") to find and count in each folder system
#' @examples
#' foldernames <- c("social_housing-master", "../forecastHybrid", "../ggseas")
#' folder_file_counts(foldernames, c("r", "sql", "exe", "pdf"))
folder_file_counts <- function(foldernames, exts){
  for(i in 1:length(foldernames)){
    exts <- tolower(exts)
    the_folder <- foldernames[i]
    lf <- tolower(list.files(the_folder, recursive = TRUE, full.names = TRUE))
    tmp <- count_files_ext_v(exts = exts, filenames = lf, label = the_folder)
    if(i == 1){
      output <- tmp
    } else {
      output <- rbind(output, tmp)
    }
  }
  return(output)
}

#' Line counts in multiple folders
#' 
#' Count the lines in all the files of given extensions, in each of a collection of folder systems
#' 
#' @export
#' @param foldernames a vector of paths to folders to search (with their subdirectories) for files
#' @param exts a vector of file extensions eg c("r", "sas", "sql") to find and count in each folder system
#' @examples
#' foldernames <- c("social_housing-master", "../forecastHybrid", "../ggseas")
#' folder_line_counts(foldernames)
folder_line_counts <- function(foldernames, exts = text_exts, onlyknown = TRUE){
for(i in 1:length(foldernames)){
    the_folder <- foldernames[i]
    lf <- tolower(list.files(the_folder, recursive = TRUE, full.names = TRUE))
    tmp <- count_lines_ext_v(exts = exts, filenames = lf, label = the_folder, onlyknown = onlyknown)
    if(i == 1){
      output <- tmp
    } else {
      output <- rbind(output, tmp)
    }
  
  }
  return(output)
}




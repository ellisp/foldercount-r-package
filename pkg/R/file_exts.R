#' Vector of common text file extensions
#' @export
text_exts <- c("r", "sas", "do", "sql", "c", "txt", "md", "rmd", "rd", "html", "js", "py", "cpp", "stan")
# have deliberately excluded CSV and SVG 

#' Vector of common binary file extensions (svg isn't really binary but for our purposes...)
#' @export
binary_exts <- c("doc", "docx", "ppt", "pptx", "xls", "xlsx", "xlsc", "xlsm", 
                 "rda", "rdata", "sas7bdat", "dta", "jar", "exe",
                 "png", "jpg", "jpeg", "gif", "svg", "pdf")

#' Vector of common data file extensions
#' @export
data_exts <- c("csv", "rda", "rdata", "sas7bdat", "dta")

#' Vector of common image file extensions
#' @export
image_exts <- c("png", "jpb", "jpeg", "gif", "svg", "pdf")

#' Vector of executable file extensions
#' @export
image_exts <- c("exe", "dll", "jar")
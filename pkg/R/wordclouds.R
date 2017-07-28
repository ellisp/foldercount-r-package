#' utility function, not for export, converting some common character combinations to spaces
#' @param x character vector
#' @param From characters to turn into spaces
MakeSpaces <- function(x, From = c("(", ")", "$", "{", "}", ",", "<-", "=", "+", "*", "[", "]", 
                                   ":", '"', "'", "#", "%", ":", ".", ";")){
  for(i in 1:length(From)){
    x <- gsub(From[i], " ", x, fixed=TRUE)
  }
  return(x)
}

#' Count words in a target folder system
#' 
#' Counts words and draws a word cloud of all files in a folder system
#' 
#' @export
#' @import tm
#' @import wordcloud
#' @import magrittr
#' @param foldername path to a folder or directory that contains some text files
#' @param plot should the word cloud be drawn
#' @param ... other parameters to pass to wordcloud
folder_wordcloud <- function(foldername, plot = TRUE, ...){
  txt <- Corpus(DirSource(foldername, recursive = TRUE)) %>%
    tm_map(content_transformer(MakeSpaces)) %>%
    tm_map(function(x) removeWords(x, stopwords("english"))) %>%
    tm_map(content_transformer(tolower)) %>%
    TermDocumentMatrix() 
  
  ap.m <- as.matrix(txt)
  ap.v <- sort(rowSums(ap.m), decreasing = TRUE)
  ap.d <- data.frame(word = names(ap.v), freq = ap.v)
  if(plot){
    print(wordcloud(ap.d$word, ap.d$freq, random.order = FALSE, ...))  
  }
  return(ap.d)
}

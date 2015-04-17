#' Computed Hash Value for Character String
#'
#' Returns the computed hash value for the given character vector.
#'
#' @name hash_table
#' @export 
#' @param str A character vector
#' @return numeric vector
#' @useDynLib envestigate C_hash_value
#'
hash_value <- function(str){
  if (!is.character(str)){
    stop("Not a character vector!")
  }
  sapply(str, function(i) .Call(C_hash_value,i))
}

#' Hash Index for String from Hash Table
#'
#' Returns the index of the bucket for the specified string and 'hash_table' object.
#'
#' @name hash_index
#' @export 
#' @param str string under consideration
#' @param ht 'hash_table' object
#' @return integer
#'
hash_index <- function(str,ht){
    hash_value(str) %% ht$size + 1
}

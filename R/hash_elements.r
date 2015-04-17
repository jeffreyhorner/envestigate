#' Hash Elements
#'
#' The elements of the 'hash_table'
#'
#' @name hash_table
#' @export 
#' @param ht 'hash_table' object.
#' @return character vector containing all strings in 'ht'.
#'
hash_elements <- function(ht){
  if (!inherits(ht,'hash_table')) stop("Not a 'hash_table' object!")

  unlist(sapply(ht$non_empty_buckets, function(i) ht$buckets[[i]]))
}

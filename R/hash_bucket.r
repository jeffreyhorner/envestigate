#' Hash Bucket From Hash Table
#'
#' Returns the full bucket contents identified  by index or a string from the hash table.
#'
#' @name hash_bucket
#' @export 
#' @param obj index of hash bucket as an object that can be coerced to integer, or a string
#' @param ht 'hash_table' object
#' @return character vector
#'
hash_bucket <- function(obj, ht){
  if (!inherits(ht,'hash_table')) stop("Not a 'hash_table' object!")

  if (is.character(obj))
    return(ht$buckets[[ hash_index(obj[1],ht) ]])

  if (!is.numeric(obj) && !is.integer(obj))
    stop("Not a string or a numeric/integer")
  idx <- as.integer(obj[1])
  if (idx < 1 || idx > ht$size)
    stop(idx, " out of bounds");
  ht$buckets[[idx]]
}

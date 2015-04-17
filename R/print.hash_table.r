#' Print Summary of Class hash_table
#'
#' Print a summary of the hash_table object
#'
#' @name print.hash_table
#' @export 
#' @param object of class 'hash_table' as returned by 'hash_table()'.
#' @return the argument ht invisibly
#'
print.hash_table <- function(ht,...){
    pht <- ht
    pht$buckets <- NULL
    pht$empty_buckets <- NULL
    pht$non_empty_buckets <- NULL
    pht$bucket_counts <- NULL
    pht$bucket_size_dist <- NULL
    class(pht) <- 'list'
    print(pht)
    invisible(ht)
}

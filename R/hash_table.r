#' Hash Table of the Environment
#'
#' Returns the hash table in a list of character vectors from the provided hashed environment.
#'
#' @name hash_table
#' @export 
#' @param env An environment.
#' @return object of class 'hash_table', a list of character vectors
#' @useDynLib envestigate C_hash_table
#'
hash_table <- function(env){
  if (is.environment(env)){
    if (!is_hashed(env)) stop("Not a hashed environment!")
  } else{
    stop("Not an environment!")
  }
  ans <- list();
  ans$buckets <- as.list(.Call(C_hash_table,env))
  ans$size <- length(ans$buckets)
  bc <- sapply(ans$buckets,length)
  ans$num_entries <- sum(bc)
  ans$num_collisions <- 
    sum(sapply(ans$buckets,function(x) if (length(x)>1) length(x)-1 else 0))
  ans$num_empty_buckets <- sum(bc==0)
  ans$empty_buckets <- which(bc==0)
  ans$num_non_empty_buckets <- ans$size - ans$num_empty_buckets
  ans$load_factor <- ans$num_non_empty_buckets/ans$size
  ans$non_empty_buckets <- which(bc!=0)
  ans$bucket_counts <- bc[which(bc!=0)]
  names(ans$bucket_counts) <- which(bc!=0)
  ans$max_bucket_length <- max(ans$bucket_counts)
  bucket_size_dist <- sapply(min(bc):max(bc),function(i) length(which(bc==i)))
  ans$bucket_size_dist <- data.frame(
    buckets = bucket_size_dist,
    size =  seq(min(bc),max(bc),by=1)
  )
  structure(ans,class=c("hash_table","list"))
}

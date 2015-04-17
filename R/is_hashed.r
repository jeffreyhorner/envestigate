#' Anser the question,"Is this environment hashed?"
#'
#' @name is_hashed
#' @export 
#' @param env An environment.
#' @return TRUE or FALSE
#' @useDynLib envestigate C_is_hashed
#'
is_hashed <- function(env){
  if (!is.environment(env)) stop("Not an environment!")
  .Call(C_is_hashed,env)
}

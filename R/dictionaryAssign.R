#' @export
`[.Dictionary` <- function(x, key){
  x$get(key)
}
#' @export
`[<-.Dictionary` <- function(x, key, value){
  x$set(key, value)
}
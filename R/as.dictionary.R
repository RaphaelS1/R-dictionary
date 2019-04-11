#' @export
as.Dictionary <- function(x,...){
  UseMethod("as.Dictionary")
}
#' @export
as.Dictionary.matrix <- function(x,...){
  Dictionary$new(x,...)
}
#' @export
as.Dictionary.xts <- function(x,...){
  Dictionary$new(x,...)
}
#' @export
as.Dictionary.data.frame <- function(x,...){
  Dictionary$new(x,...)
}
#' @export
as.Dictionary.list <- function(x,...){
  Dictionary$new(x,...)
}
#' @export
as.Dictionary.integer <- function(x,...){
  Dictionary$new(x,...)
}
#' @export
as.Dictionary.numeric <- function(x,...){
  Dictionary$new(x,...)
}
#' @export
as.Dictionary.complex <- function(x,...){
  Dictionary$new(x,...)
}
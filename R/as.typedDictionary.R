#' @export
as.TypedDictionary <- function(x,...){
  UseMethod("as.TypedDictionary")
}
#' @export
as.TypedDictionary.matrix <- function(x,...){
  TypedDictionary$new(x,...)
}
#' @export
as.TypedDictionary.xts <- function(x,...){
  TypedDictionary$new(x,...)
}
#' @export
as.TypedDictionary.data.frame <- function(x,...){
  TypedDictionary$new(x,...)
}
#' @export
as.TypedDictionary.list <- function(x,...){
  TypedDictionary$new(x,...)
}
#' @export
as.TypedDictionary.integer <- function(x,...){
  TypedDictionary$new(x,...)
}
#' @export
as.TypedDictionary.numeric <- function(x,...){
  TypedDictionary$new(x,...)
}
#' @export
as.TypedDictionary.complex <- function(x,...){
  TypedDictionary$new(x,...)
}
#' @export
as.TypedDictionary.dictionary <- function(x,...){
  TypedDictionary$new(x,...)
}
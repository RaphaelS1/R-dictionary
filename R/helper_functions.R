#' @export
coerceList = function(list,class){
  x = switch (class,
              character = as.list(as.character((unlist(list,recursive = T)))),
              integer = as.list(as.integer((unlist(list,recursive = T)))),
              numeric = as.list(as.numeric((unlist(list,recursive = T)))),
              complex = as.list(as.complex((unlist(list,recursive = T)))),
              factor = as.list(as.factor((unlist(list,recursive = T)))),
              logical = as.list(as.logical((unlist(list,recursive = T))))
  )
  return(x)
}

#' @export
checkXts = function(x){
  if("xts"%in%class(x))
    return(TRUE)
  else
    return(sprintf("Must be of type 'xts', not '%s'",class(x)))
}
#' @export
assertXts = function(x){
  if("xts"%in%class(x))
    return(invisible(1))
  else
    BBmisc::stopf("Must be of type 'xts', not '%s'",class(x))
}
#' @export
testXts = function(x){
  if("xts"%in%class(x))
    return(TRUE)
  else
    return(FALSE)
}

#' @export
assertListClassUnique = function(list){
  checkmate::assertList(list)
  classes = lapply(list, function(y) {
    if(length(class(y))>1){
      if(class(y)[length(class(y))] == "R6")
        return(class(y)[length(class(y))-1])
      else
        return(class(y)[length(class(y))])
    } else
      return(class(y))
  })
  if(length(unique(unlist(classes)))>1)
    stop("Each list item must have same class")
  else
    return(invisible(unique(classes)))
}

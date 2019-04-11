#' @export
Dictionary <- R6::R6Class("Dictionary",
                      private=list(
                        .lst = NULL,
                        .keyclass = NULL
                      ))

Dictionary$set("public","initialize",
               function(object=NULL,keys=NULL,values=NULL,keyclass=NULL,
                        typed = FALSE){
  checkmate::assert(is.null(object) & !is.null(keys) & !is.null(values),
         !is.null(object),
         .var.name = "Either object or keys and values must be provided")
  if(!is.null(keyclass)){
    checkmate::assertChoice(keyclass,c("numeric","Date"))
    private$.keyclass <- keyclass
  }
  if(is.null(object)){
    lst <- as.list(values)
    names(lst) <- keys
    object <- lst
  }
  if(testXts(object)){
    checkmate::assert(ncol(object)==1,.var.name = "xts objects must have one column only")
    object <- data.frame(object,stringsAsFactors = FALSE)
  } else if(checkmate::testMatrix(object)){
    object <- data.frame(object,stringsAsFactors = FALSE)
  } else if(inherits(object,"Dictionary")){
    object <- as.data.frame(object)
  }
  if(checkmate::testDataFrame(object)){
    checkmate::assertNumber(ncol(object),lower=1,upper=2)
    if(ncol(object)==1){
      lst <- as.list(object[,1])
      names(lst) <- row.names(object)
    } else {
      lst <- as.list(object[,2])
      names(lst) <- object[,1]
    }
    object <- lst
  } else if(checkmate::testNamed(object)){
    object <- as.list(object)
  }
  if(checkmate::testList(object)){
    checkmate::assert(!any(is.null(names(object))),.var.name = "Names must be non-NULL")
    checkmate::assert(!any(duplicated(names(list))),
           .var.name = "Names must be unique")
    private$.lst <- object
    invisible(self)
  } else
    return("Object must be one of matrix, xts, data.frame,
           named integer/numeric/complex or list")
})
Dictionary$set("public","getkeyclass",function(){
                 return(private$.keyclass)
                 })
Dictionary$set("public","setkeyclass",function(keyclass){
  checkmate::assertChoice(keyclass,c("numeric","Date"))
                 private$.keyclass <- keyclass
               })
Dictionary$set("public","keys",function(){
                 return(names(private$.lst))
               })
Dictionary$set("public","values",function(){
                 return(unlist(private$.lst,recursive = T,use.names = F))
               })
Dictionary$set("public","items",function(){
  return(unlist(private$.lst,recursive = F,use.names = T))
})
Dictionary$set("public","strprint",function(n=6L){
  items <- self$items()
  if(n>=length(self$keys()))
    string = paste0("{",paste0(self$keys(),":",items,collapse=", "),"}")
  else
    string = paste0("{",
                    paste0(head(self$keys(),n),":",head(items,n),collapse=", "),
                    "...",
                    paste0(tail(self$keys(),n),":",tail(items,n),collapse=", "),"}")

  return(string)
})
Dictionary$set("public","print",function(n=6L,...){
  cat(self$strprint(n))
  invisible(self)
})
Dictionary$set("public","summary",function(n=6L,full=FALSE,...){
  cat("Length:", self$len(),"\t")
  if(!is.null(self$getkeyclass()))
    cat("Key Class: ",self$getkeyclass(),"\t")
  if(full){
    cat("Keys: \n",self$keys())
    cat("\nValues: \n",self$values())
  } else
    cat("Items: \n",self$strprint(n))
})
Dictionary$set("public","len",function(){
  return(length(self$items()))
})
Dictionary$set("public","get",function(key){
  if(any(self$keys()%in%key))
    return(self$values()[self$keys()%in%key])
  else
    BBmisc::stopf("%s is not in this Dictionary",key)
})
Dictionary$set("public","set",function(key,value){
  if(checkmate::testList(key)){
    value = unlist(key,use.names = F)
    key = names(key)
  }
  checkmate::assert(all(key%in%self$keys()),
         .var.name = "Key(s) not in Dictionary")
  private$.lst[self$keys()%in%key] = value
  invisible(self)
})
Dictionary$set("public","del",function(key){
  private$.lst[self$keys()%in%key] = NULL
  invisible(self)
})
Dictionary$set("public","update",function(key,value){
  if(checkmate::testList(key)){
    value = unlist(key,use.names = F)
    key = names(key)
  }
  checkmate::assert(!any(key%in%self$keys()),
         .var.name = "New keys must be unique")
  lst = as.list(value)
  names(lst) = key
  private$.lst = c(private$.lst,lst)
})
Dictionary$set("public","as.data.frame",function(rownames=FALSE){
  if(rownames)
    return(data.frame(row.names=self$keys(),Values=self$values(),stringsAsFactors = FALSE))
  else
    return(data.frame(Keys=self$keys(),Values=self$values(),stringsAsFactors = FALSE))
})
Dictionary$set("public","as.matrix",function(rownames=FALSE){
  return(as.matrix(self$as.data.frame(rownames)))
})
Dictionary$set("public","as.list",function(){
  return(as.list(self$items()))
})
Dictionary$set("public","as.vector",function(){
  return(self$items())
})
Dictionary$set("public","as.xts",function(){
  return(xts::as.xts(self$items()))
})
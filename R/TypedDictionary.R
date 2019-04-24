#' @include Dictionary.R

#-------------------------------------------------------------
# TypedDictionary R6Class Definition
#-------------------------------------------------------------
#' @export
TypedDictionary <- R6::R6Class("TypedDictionary", inherit = Dictionary)

#-------------------------------------------------------------
# TypedDictionary Public Methods
#-------------------------------------------------------------
TypedDictionary$set("public","initialize",
               function(object=NULL,keys=NULL,values=NULL,keyclass=NULL){
                 if(is.null(object) & is.null(keys) & is.null(values) &
                    is.null(keyclass)){
                   invisible(self)
                 } else {
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
                   } else if(inherits(object,"dictionary")){
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
                     checkmate::assert(length(table(unique(sapply(object,class))))==1,
                                       .var.name = "All elements must be of same class")
                     checkmate::assert(!any(duplicated(names(list))),
                                       .var.name = "Names must be unique")
                     private$.lst <- object
                     private$.class <- unique(sapply(object,class))
                     invisible(self)
                   } else
                     return("Object must be one of matrix, xts, data.frame,
                            named integer/numeric/complex or list")
                 }
               })
TypedDictionary$set("public","getclass",function(){
  return(private$.class)
})
TypedDictionary$set("public","setclass",function(class){
                 keys = self$keys()
                 if(self$getclass()=="character" & class!="character"){
                   if(class %in% c("logical","integer","numeric")){
                     private$.lst = coerceList(coerceList(private$.lst,"complex"),class)
                     names(private$.list) = keys
                   }else{
                     private$.lst = coerceList(private$.lst,class)
                     names(private$.list) = keys
                   }
                 }

                 if(self$getclass()=="factor" & class!="factor"){
                   private$.lst = coerceList(private$.lst,"character")
                   names(private$.lst) = keys
                   return(self$setclass(class))
                 }

                 if(self$getclass()=="logical" & class!="logical"){
                   private$.lst = coerceList(private$.lst,class)
                   names(private$.lst) = keys
                 }

                 if(self$getclass()%in% c("complex","integer","numeric")){
                   private$.lst = coerceList(private$.lst,class)
                   names(private$.lst) = keys
                 }

                 private$.class = class
                 invisible(self)
               })
TypedDictionary$set("public","print",function(n=6L,round=5,simple = FALSE,...){
  if(is.null(self$items())){
    cat("dictionary with 0 items")
    invisible(self)
  } else{
  cat(self$strprint(n,round,simple))
  invisible(self)
  }
})
TypedDictionary$set("public","strprint",function(n=6L, round=5, simple = FALSE){
  items <- self$items()
  if(self$getclass() %in% c("numeric","integer","complex"))
    items <- round(items,round)
  if(n>=length(self$keys()))
    string = paste0("{",paste0(self$keys(),":",items,collapse=", "),"}")
  else
    string = paste0("{",
                    paste0(head(self$keys(),n),":",head(items,n),collapse=", "),
                    "...",
                    paste0(tail(self$keys(),n),":",tail(items,n),collapse=", "),"}")

  if(simple)
    return(string)
  else {
    newString = NULL
    if(!is.null(self$getkeyclass()))
      newString = paste0("Key Class: ",self$getkeyclass(),"\t")
    newString = paste0(newString, "Value Class: ",self$getclass(),
                       "\tSeries: ",string,"\n")
    return(newString)
  }
})
TypedDictionary$set("public","summary",function(n=6L,full=FALSE,round=5){
  if(is.null(self$items())){
    cat("dictionary with 0 items")
    invisible(self)
  } else{
  cat("Length:", self$len(),"\t")
  if(!is.null(self$getkeyclass()))
    cat("Key Class: ",self$getkeyclass(),"\t")
  cat("Value Class: ",self$getclass(),"\n")
  if(full){
    cat("Keys: \n",self$keys())
    cat("\nValues: \n",round(self$values(),round))
  } else
    cat("Items: \n",self$strprint(n,round,TRUE))
  }
})
TypedDictionary$set("public","set",function(key,value){
  if(checkmate::testList(key)){
    value = unlist(key,use.names = F)
    key = names(key)
  }
  checkmate::assert(all(key%in%self$keys()),
                    .var.name = "Key(s) not in dictionary")
  new_value = do.call(paste0("as.",self$getclass()),list(x=value))
  if(any(is.na(new_value))){
    BBmisc::stopf("%s is not coercable to %s",class(value),self$getclass())
  }
  private$.lst[self$keys()%in%key] = new_value
  invisible(self)
})
TypedDictionary$set("public","update",function(key,value){
  if(checkmate::testList(key)){
    value = unlist(key,use.names = F)
    key = names(key)
  }
  checkmate::assert(!any(key%in%self$keys()),
                    .var.name = "New keys must be unique")
  new_value = do.call(paste0("as.",self$getclass()),list(x=value))
  if(any(is.na(new_value) & !is.na(value))){
    BBmisc::stopf("%s is not coercable to %s",class(value),self$getclass())
  }
  lst = as.list(new_value)
  names(lst) = key
  private$.lst = c(private$.lst,lst)
  invisible(self)
})
TypedDictionary$set("public","transformer.spacing",function(keys){
  self$update(keys[!(keys %in% self$keys())],rep(NA,sum(!(keys %in% self$keys()))))
  self$reorder(newkeys = keys)
  invisible(self)
})
TypedDictionary$set("public","reorder",function(newkeys=NULL,order=NULL){
  if(is.null(order)){
    if(!is.null(newkeys))
      order = match(as.character(newkeys),self$keys())
    else if(!is.null(self$getkeyclass())){
      if(!is.null(self$getkeyclass())){
        if(self$getkeyclass() == "numeric")
          order = order(as.numeric(self$keys()))
        else
          order = order(as.Date(self$keys()))
      }else
        stop("One of newkeys, order or keyclass must be given.")
    }
  }
  private$.lst = private$.lst[order]
  invisible(self)
})

#-------------------------------------------------------------
# TypedDictionary Private Variables
#-------------------------------------------------------------
TypedDictionary$set("private",".class",NULL)

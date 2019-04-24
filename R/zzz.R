.onLoad <- function(libname, pkgname){
  print(get0(Dictionary,envir=parent.env(environment())))
  R62S3::R62S3(Dictionary,parent.env(environment()),parent.env(environment()))
  R62S3::R62S3(TypedDictionary,parent.env(environment()),parent.env(environment()))
}
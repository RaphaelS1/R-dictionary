.onLoad <- function(libname, pkgname){
  print("test")
  R62S3::R62S3(Dictionary,parent.env(environment()),parent.env(environment()))
  R62S3::R62S3(TypedDictionary,parent.env(environment()),parent.env(environment()))
}
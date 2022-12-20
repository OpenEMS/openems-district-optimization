# ---maxP_el_minus------------------------------------------------------
#' maxP_el_minus (set)
#'
#'Set the maximal electric input  of class PubGel in kW. If the maxP_el_minus is set to -1, the PubGel can consume electrical energy unlimited.
#'
#'@importFrom Root maxP_el_minus<-
#'@param object A PubGel
#'@param value A numeric
#'@return PubGel
#'@examples
#'pubgel <- new.PubGel()
#'# add the variable "P_el_minus"
#'variables(pubgel) <- "P_el_minus"
#'maxP_el_minus(pubgel) <- 10
#'@export


setMethod("maxP_el_minus<-", signature(object = "PubGel",value="numeric"), function(object, value) {
  if("P_el_minus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_el_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_el_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxP_el_minus <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
  }else{stop("The variable P_el_minus is needed")}
})
# ---minP_el_minus -------------------------------------------------
#'  minP_el_minus (set)
#'
#'Set the minimal electric input  of class PubGel in kW.
#'
#'@importFrom Root minP_el_minus<-
#'@param object A PubGel
#'@param value A numeric
#'@examples
#'pubgel <- new.PubGel()
#'#add the variable "P_el_minus"
#'variables(pubgel)<- "P_el_minus"
#'minP_el_minus(pubgel) <- 5
#'@return PubGel
#'@export

setMethod("minP_el_minus<-", signature(object = "PubGel",value="numeric"), function(object, value) {
  if("P_el_minus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "minP_el_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_el_minus has to be positiv"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_el_minus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)}else{stop("The variable P_el_minus is needed")}
})


# ---maxP_el_plus------------------------------------------------------
#' maxP_el_plus (set)
#'
#'Set the maximal electric output of class PubGel in kW. If the maxP_el_plus is set to -1, the PubGel can produce energy unlimited.
#'
#'@importFrom Root maxP_el_plus<-
#'@param object A PubGel
#'@param value A numeric
#'@return PubGel
#'@examples
#'pubgel <- new.PubGel()
#'#add the variable "P_el_plus"
#'variables(pubgel) <- "P_el_plus"
#'maxP_el_plus(pubgel) <- 10
#'@export



setMethod("maxP_el_plus<-", signature(object = "PubGel",value="numeric"), function(object, value) {
  if("P_el_plus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_el_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_el_plus has to be positiv or -1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@maxP_el_plus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
  }else{stop("The variable P_el_minus is needed")}
})
# ---minP_el_plus------------------------------------------------------
#' minP_el_plus (set)
#'
#'Set the minimal electric output of class PubGel in kW.
#'
#'@importFrom Root minP_el_plus<-
#'@param object A PubGel
#'@param value A numeric
#'@return PubGel
#'@examples
#'pubgel <- new.PubGel()
#'#add the variable "P_el_plus"
#'variables(pubgel)<- "P_el_plus"
#'minP_el_plus(pubgel) <- 5
#'@export


setMethod("minP_el_plus<-", signature(object = "PubGel",value="numeric"), function(object, value) {
  if("P_el_plus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "minP_el_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_el_plus has to be positiv"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_el_plus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
  }else{stop("The variable P_el_plus is needed")}
})


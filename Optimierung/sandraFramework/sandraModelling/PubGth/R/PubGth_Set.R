# ---maxP_th_minus------------------------------------------------------
#' maxP_th_minus (set)
#'
#'Set the maxP_th_minus of class PubGth in kW. If the maxP_th_minus is set to -1, the PubGth can consume thermal energy unlimited.
#'
#'@importFrom Root maxP_th_minus<-
#'@param object A PubGth
#'@param value A numeric
#'@return PubGth
#'@examples
#'pubgth <- new.PubGth()
#'#add variable "P_th_minus"
#'variables(pubgth) <- "P_th_minus"
#'maxP_th_minus(pubgth)<- 10
#'@export


setMethod("maxP_th_minus<-", signature(object = "PubGth",value="numeric"), function(object, value) {
 if("P_th_minus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_th_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_th_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@maxP_th_minus <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)}else{stop("The variable P_th_minus is needed")}
})
# ---minP_th_minus -------------------------------------------------
#'  minP_th_minus (set)
#'
#'Set the  minP_th_minus of class PubGth
#'
#'@importFrom Root minP_th_minus<-
#'@param object A PubGth
#'@param value A numeric
#'@return PubGth
#'@examples
#'pubgth <- new.PubGth()
#'variables(pubgth) <- "P_th_minus"
#'minP_th_minus(pubgth) <- 5
#'@export



setMethod("minP_th_minus<-", signature(object = "PubGth",value="numeric"), function(object, value) {
  if("P_th_minus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "minP_th_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_th_minus has to be >= 0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_th_minus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)}else{stop("The variable P_th_minus is needed")}
})


# ---maxP_th_plus------------------------------------------------------
#' maxP_th_plus (set)
#'
#'Set the maxP_th_plus of class PubGth in kW.If the maxP_th_plus is set to -1, the PubGth can produce thermal energy unlimited.
#'
#'@importFrom Root maxP_th_plus<-
#'@param object A PubGth
#'@param value A numeric
#'@return PubGth
#'@examples
#'pubgth <- new.PubGth()
#'variables(pubgth) <- "P_th_plus"
#'maxP_th_plus(pubgth) <- 10
#'@export


setMethod("maxP_th_plus<-", signature(object = "PubGth",value="numeric"), function(object, value) {
  if("P_th_plus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_th_plus has to be positiv or -1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@maxP_th_plus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)}else{stop("The variable P_th_plus is needed")}
})
# ---minP_th_plus------------------------------------------------------
#' minP_th_plus (set)
#'
#'Set the minP_th_plus of class PubGth
#'
#'@importFrom Root minP_th_plus<-
#'@param object A PubGth
#'@param value A numeric
#'@return PubGth
#'@examples
#'pubgth <- new.PubGth()
#'variables(pubgth) <- "P_th_plus"
#'minP_th_plus(pubgth) <- 5
#'@export



setMethod("minP_th_plus<-", signature(object = "PubGth",value="numeric"), function(object, value) {
  if("P_th_plus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "minP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_th_plus has to be  >= 0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_th_plus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)}else{stop("The variable P_th_plus is needed")}
})


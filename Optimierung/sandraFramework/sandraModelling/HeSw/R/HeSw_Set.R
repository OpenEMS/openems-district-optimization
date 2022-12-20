#----maxP_el_minus-----------------------------------------
#'maxP_el_minus (set)
#'
#'Set the maxP_el_minus of class HeSw in kW. If the maxP_el_minus is set to -1, the HeSw can consume electrical energy unlimited.
#'
#'@importFrom Root maxP_el_minus<-
#'@param object A HeSw
#'@param value A numeric
#'@return HeSw
#'@examples
#'hesw <- new.HeSw()
#'maxP_el_minus(hesw) <- 5
#'@export
setMethod("maxP_el_minus<-", signature(object = "HeSw",value="numeric"), function(object, value) {

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
    if (length(effMaxP_th_plus(object))==1){
      object@maxP_th_plus <- value * effMaxP_th_plus(object)
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})

#---effMaxP_th_plus------------------------------------------
#' effMaxP_th_plus (set)
#'
#'Set the effMaxP_th_plus of class HeSw in \% (has to be bigger than 0)
#'
#'@importFrom Root effMaxP_th_plus<-
#'@param object A HeSw
#'@param value A numeric
#'@return HeSw
#'@examples
#'hesw <- new.HeSw()
#'effMaxP_th_plus(hesw) <- 0.8
#'@export
setMethod("effMaxP_th_plus<-", signature(object = "HeSw",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "effMaxP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if(all(value > 1 | value <= 0)) {
    msg <- "effMaxP_th_plus has to be bigger than 0 and <= 1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@effMaxP_th_plus <- value
    if (length(maxP_el_minus(object))==1){
      object@maxP_th_plus <- maxP_el_minus(object) * value
    }
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})


# ---------------------------------------------------------
# the validity function of PubGth
check.PubGth <- function(object) {
  errors <- character()

  if (length(object@maxP_th_minus) == 1 &  length(object@minP_th_minus) == 1) {
    if (object@maxP_th_minus < object@minP_th_minus & object@maxP_th_minus !=-1) {
      msg <- "maxP_th_minus has to bigger than minP_th_minus or maxP_th_minus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
  }

  if (length(object@maxP_th_plus) == 1 &  length(object@minP_th_plus) == 1) {
    if (object@maxP_th_plus < object@minP_th_plus & object@maxP_th_plus !=-1) {
      msg <- "maxP_th_plus has to bigger than minP_th_plus or maxP_th_minus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
  }





  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
# ---------------------------------------------------------
#' Class PubGth
#'
#'The PubGth represents the class PublicGridHeat
#'
#'@slot maxP_th_minus  maximal input of thermal power in kW
#'@slot minP_th_minus minimal input of thermal power in kW
#'@slot maxP_th_plus  maximal output of thermal power in kW
#'@slot minP_th_plus minimal output of thermal power in kW
#'@export
setClass(
  "PubGth",
  slots = list(
    maxP_th_minus = "numeric" ,
    minP_th_minus = "numeric" ,
    maxP_th_plus = "numeric" ,
    minP_th_plus = "numeric"
  ),
  contains = "Root",
  validity = check.PubGth
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.PubGth
#'
#'Constructor of PubGth creates a new obejct of class PubGth
#'
#'@return New object of class PubGth
#'@export
new.PubGth <- function() {
  return(new(Class = "PubGth",variables = c("P_th_plus","P_th_minus")))
}

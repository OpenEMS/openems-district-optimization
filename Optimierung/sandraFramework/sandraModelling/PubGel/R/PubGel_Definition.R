# ---------------------------------------------------------
# the validity function of PubGel
check.PubGel <- function(object) {
  errors <- character()

  if (length(object@maxP_el_minus) == 1 &  length(object@minP_el_minus) == 1) {
    if (object@maxP_el_minus < object@minP_el_minus & object@maxP_el_minus != -1) {
      msg <- "maxP_el_minus has to bigger than minP_el_minus or maxP_el_minus has to be -1 (unlimited)"
      errors <- c(msg, errors)
    }
  }

  if (length(object@maxP_el_plus) == 1 &  length(object@minP_el_plus) == 1) {
    if (object@maxP_el_plus < object@minP_el_plus & object@maxP_el_plus != -1) {
      msg <- "maxP_el_plus has to bigger than minP_el_plus or maxP_el_plus has to be -1 (unlimited)"
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
#' Class PubGel
#'
#'The PubGel represents the class PublicGridPower
#'
#'@slot price_el_plus electric price (input) in €/kwh
#'@slot price_el_minus electric price  (output) in €/kwh
#'@slot maxP_el_minus  maximal input of electric power in kW
#'@slot minP_el_minus minimal input of electric power in kW
#'@slot maxP_el_plus  maximal output of electric power in kW
#'@slot minP_el_plus minimal output of electric power in kW
#'@export
setClass(
  "PubGel",
  slots = list(
    maxP_el_minus = "numeric" ,
    minP_el_minus = "numeric" ,
    maxP_el_plus = "numeric" ,
    minP_el_plus = "numeric"
  ),
  contains = "Root",
  validity = check.PubGel
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.PubGel
#'
#'Constructor of PubGel creates a new obejct of class PubGel
#'
#'@return New object of class PubGel
#'@export
new.PubGel <- function() {
  return(new(Class = "PubGel",variables = c("P_el_plus","P_el_minus")))
}

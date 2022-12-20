#----maxP_th_plus------------------------------------------
#'maxP_th_plus (get)
#'
#'Get maxP_th_plus of HeSw in kW
#'
#'@importFrom Root maxP_th_plus
#'@param object A HeSw
#'@return  maxP_th_plus of HeSw
#'@export
#'
setMethod("maxP_th_plus", signature(object = "HeSw"), function(object) {
  if (length(object@maxP_th_plus)==1) {
    return(object@maxP_th_plus)
  } else {
    print("No value! Please set maxP_el_minus and effMaxP_el_minus")
    return(object@maxP_th_plus)
  }
})
#----effMaxP_th_plus-----------------------------------------
#'effMaxP_th_plus (get)
#'
#'Get effMaxP_th_plus of HeSw in \%
#'
#'@importFrom Root effMaxP_th_plus
#'@param object A HeSw
#'@return  effMaxP_th_plus of HeSw
#'@export
#'
setMethod("effMaxP_th_plus", signature(object = "HeSw"), function(object) {
  return(object@effMaxP_th_plus)
})
#----maxP_el_minus-----------------------------------------
#'maxP_el_minus (get)
#'
#'Get maxP_el_minus of HeSw in kW
#'
#'@importFrom Root maxP_el_minus
#'@param object A HeSw
#'@return  maxP_el_minus of HeSw
#'@export
#'
setMethod("maxP_el_minus", signature(object = "HeSw"), function(object) {
  return(object@maxP_el_minus)
})

#----maxP_th_plus-------------------------
#'maxP_th_plus (get)
#'
#'Get maxP_th_plus of GaBo in kW
#'
#'@importFrom Root maxP_th_plus
#'@param object A GaBo
#'@return  maxP_th_plus of GaBo
#'@export
#'
setMethod("maxP_th_plus", signature(object = "GaBo"), function(object) {
  return(object@maxP_th_plus)
})
#----effMaxP_th_plus-------------------------
#'effMaxP_th_plus (get)
#'
#'Get effMaxP_th_plus of GaBo in \%
#'
#'@importFrom Root effMaxP_th_plus
#'@param object A GaBo
#'@return  effMaxP_th_plus of GaBo
#'@export
#'
setMethod("effMaxP_th_plus", signature(object = "GaBo"), function(object) {
  return(object@effMaxP_th_plus)
})

#----maxP_fuel_minus-------------------------
#'maxP_fuel_minus (get)
#'
#'Get maxP_fuel_minus of GaBo in kW
#'
#'@importFrom Root maxP_fuel_minus
#'@param object A GaBo
#'@return  maxP_fuel_minus of GaBo
#'@export
#'
setMethod("maxP_fuel_minus", signature(object = "GaBo"), function(object) {
  return(object@maxP_fuel_minus)
})

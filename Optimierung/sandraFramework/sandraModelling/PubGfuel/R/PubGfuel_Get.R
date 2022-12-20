#----maxP_fuel_minus-------------------------
#' maxP_fuel_minus (get)
#'
#' Get maxP_fuel_minus of PubGfuel
#'
#'@importFrom Root maxP_fuel_minus
#'@param object A PubGfuel
#'@return  maxP_fuel_minus of PubGfuel
#'@export
#'

setMethod("maxP_fuel_minus", signature(object = "PubGfuel"), function(object) {
  return(object@maxP_fuel_minus)
})

#---minP_fuel_minus--------------------------
#' minP_fuel_minus (get)
#'
#' Get minP_fuel_minus of PubGfuel
#'
#'@importFrom Root minP_fuel_minus
#'@param object A PubGfuel
#'@return  minP_fuel_minus of PubGfuel
#'@export


setMethod("minP_fuel_minus", signature(object = "PubGfuel"), function(object) {
  return(object@minP_fuel_minus)
})

#---maxP_fuel_plus--------------------------
#' maxP_fuel_plus (get)
#'
#' Get maxP_fuel_plus of PubGfuel
#'
#'@importFrom Root maxP_fuel_plus
#'@param object A PubGfuel
#'@return  maxP_fuel_plus of PubGfuel
#'@export


setMethod("maxP_fuel_plus", signature(object = "PubGfuel"), function(object) {
  return(object@maxP_fuel_plus)
})

#---minP_fuel_plus--------------------------
#' minP_fuel_plus (get)
#'
#' Get minP_fuel_plus of PubGfuel
#'
#'@importFrom Root minP_fuel_plus
#'@param object A PubGfuel
#'@return  minP_fuel_plus of PubGfuel
#'@export



setMethod("minP_fuel_plus", signature(object = "PubGfuel"), function(object) {
  return(object@minP_fuel_plus)
})


#----maxP_th_minus-------------------------
#' maxP_th_minus (get)
#'
#' Get maxP_th_minus of PubGth
#'
#'@importFrom Root maxP_th_minus
#'@param object A PubGth
#'@return  maxP_th_minus of PubGth
#'@export



setMethod("maxP_th_minus", signature(object = "PubGth"), function(object) {
  return(object@maxP_th_minus)
})

#---minP_th_minus--------------------------
#' minP_th_minus (get)
#'
#' Get minP_th_minus of PubGth
#'
#'@importFrom Root minP_th_minus
#'@param object A PubGth
#'@return  minP_th_minus of PubGth
#'@export


setMethod("minP_th_minus", signature(object = "PubGth"), function(object) {
  return(object@minP_th_minus)
})

#---maxP_th_plus--------------------------
#' maxP_th_plus (get)
#'
#' Get maxP_th_plus of PubGth
#'
#'@importFrom Root maxP_th_plus
#'@param object A PubGth
#'@return  maxP_th_plus of PubGth
#'@export



setMethod("maxP_th_plus", signature(object = "PubGth"), function(object) {
  return(object@maxP_th_plus)
})

#---minP_th_plus--------------------------
#' minP_th_plus (get)
#'
#' Get minP_th_plus of PubGth
#'
#'@importFrom Root minP_th_plus
#'@param object A PubGth
#'@return  minP_th_plus of PubGth
#'@export


setMethod("minP_th_plus", signature(object = "PubGth"), function(object) {
  return(object@minP_th_plus)
})


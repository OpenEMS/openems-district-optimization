#----maxP_el_minus-------------------------
#' maxP_el_minus (get)
#'
#' Get maxP_el_minus of PubGel
#'
#'@importFrom Root maxP_el_minus
#'@param object A PubGel
#'@return  maxP_el_minus of PubGel
#'@export
#'
setMethod("maxP_el_minus", signature(object = "PubGel"), function(object) {
    return(object@maxP_el_minus)
})

#---minP_el_minus--------------------------
#' minP_el_minus (get)
#'
#' Get minP_el_minus of PubGel
#'
#'@importFrom Root minP_el_minus
#'@param object A PubGel
#'@return  minP_el_minus of PubGel
#'@export


setMethod("minP_el_minus", signature(object = "PubGel"), function(object) {
  return(object@minP_el_minus)
})

#---maxP_el_plus--------------------------
#' maxP_el_plus (get)
#'
#' Get maxP_el_plus of PubGel
#'
#'@importFrom Root maxP_el_plus
#'@param object A PubGel
#'@return  maxP_el_plus of PubGel
#'@export


setMethod("maxP_el_plus", signature(object = "PubGel"), function(object) {
  return(object@maxP_el_plus)
})

#---minP_el_plus--------------------------
#' minP_el_plus (get)
#'
#' Get minP_el_plus of PubGel
#'
#'@importFrom Root minP_el_plus
#'@param object A PubGel
#'@return  minP_el_plus of PubGel
#'@export



setMethod("minP_el_plus", signature(object = "PubGel"), function(object) {
  return(object@minP_el_plus)
})


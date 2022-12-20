#----load_15min_th-----------------------------------------
#'load_15min_th (get)
#'
#'Get load_15min_th of Demth in kW per 15 min
#'
#'@importFrom Root load_15min_th
#'@param object An Demth
#'@return  load_15min_th of Demth
#'@export
#'

setMethod("load_15min_th", signature(object = "Demth"), function(object) {
  return(object@load_15min_th)
})


#----load_abstract_th-----------------------------------------
#'load_abstract_th (get)
#'
#'Get load_abstract_th of Demth in kW per 15 min
#'
#'@importFrom Root load_abstract_th
#'@param object An Demth
#'@return  load_abstract_th of Demth
#'@export
#'

setMethod("load_abstract_th", signature(object = "Demth"), function(object) {
  return(object@load_abstract_th)
})

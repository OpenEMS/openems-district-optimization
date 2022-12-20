#----load_15min_el-----------------------------------------
#'load_15min_el (get)
#'
#'Get load_15min_el of Demel in kW per 15 min
#'
#'@importFrom Root load_15min_el
#'@param object An Demel
#'@return  load_15min_el of Demel
#'@export
#'

setMethod("load_15min_el", signature(object = "Demel"), function(object) {
  return(object@load_15min_el)
})

#----load_anstract_el-----------------------------------------
#'load_abstract_el (get)
#'
#'Get load_abstract_el of Demel in kW
#'
#'@importFrom Root load_abstract_el
#'@param object An Demel
#'@return  load_abstract_el of Demel
#'@export
#'

setMethod("load_abstract_el", signature(object = "Demel"), function(object) {
  return(object@load_abstract_el)
})



#----load_15min_el-----------------------------------------
#'load_15min_el (get)
#'
#'Get load_15min_el of PV in kW per 15 min
#'
#'@importFrom Root load_15min_el
#'@param object An PV
#'@return  load_15min_el of PV
#'@export
#'

setMethod("load_15min_el", signature(object = "PV"), function(object) {
  return(object@load_15min_el)
})

#----load_abstract_el-----------------------------------------
#'load_abstract_el (get)
#'
#'Get load_abstract_el of PV in kW per 15 min
#'
#'@importFrom Root load_abstract_el
#'@param object An PV
#'@return  load_abstract_el of PV
#'@export
#'

setMethod("load_abstract_el", signature(object = "PV"), function(object) {
  return(object@load_abstract_el)
})


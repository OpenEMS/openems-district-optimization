#----load_15min_fuel---------------------------------------
#'load_15min_fuel (get)
#'
#'Get load_15min_fuel of Demfuel in kW per 15 min
#'
#'@importFrom Root load_15min_fuel
#'@param object An Demfuel
#'@return  load_15min_fuel of Demfuel
#'@export
#'

setMethod("load_15min_fuel", signature(object = "Demfuel"), function(object) {
  return(object@load_15min_fuel)
})


#----load_abstract_fuel---------------------------------------
#'load_abstract_fuel (get)
#'
#'Get load_abstract_fuel of Demfuel in kW per 15 min
#'
#'@importFrom Root load_abstract_fuel
#'@param object An Demfuel
#'@return  load_abstract_fuel of Demfuel
#'@export
#'

setMethod("load_abstract_fuel", signature(object = "Demfuel"), function(object) {
  return(object@load_abstract_fuel)
})

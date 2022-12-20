#----load_15min_fuel-----------------------------------------
#'load_15min_fuel (get)
#'
#'Get load_15min_fuel of Prodfuel in kW per 15 min
#'
#'@importFrom Root load_15min_fuel
#'@param object A Prodfuel
#'@return  load_15min_fuel of Prodfuel
#'@export
#'

setMethod("load_15min_fuel", signature(object = "Prodfuel"), function(object) {
  return(object@load_15min_fuel)
})

#----load_abstract_fuel-----------------------------------------
#'load_abstract_fuel (get)
#'
#'Get load_abstract_fuel of Prodfuel in kW per 15 min
#'
#'@importFrom Root load_abstract_fuel
#'@param object A Prodfuel
#'@return  load_abstract_fuel of Prodfuel
#'@export
#'

setMethod("load_abstract_fuel", signature(object = "Prodfuel"), function(object) {
  return(object@load_abstract_fuel)
})


#---Overview-----------------------------------------------
# maxP_el_minus
# maxP_fuel_plus
# maxP_th_plus
# minP_el_minus
# minP_fuel_plus
# minP_th_plus
# effMaxP_el_minus
# effMaxP_fuel_plus
# effMaxP_th_plus
# effMinP_el_minus
# effMinP_fuel_plus
# effMinP_th_plus
# load_P_th_plus
# price_maintenance
# minRuntime
# minDowntime
# initial_state
# modulated

#----------------------------------------------------------
#---maxP_el_minus (get)-------------------------------------
#'maxP_el_minus (Get)
#'
#'Get the maximal electric input power of a Electrolyseur in kW
#'
#'@importFrom Root maxP_el_minus
#'@export
setMethod("maxP_el_minus", signature(object = "Electrolyseur"),function(object){
  return(object@maxP_el_minus)
})
#---maxP_fuel_plus (get)-------------------------------------
#'maxP_fuel_plus (Get)
#'
#'Get the maximal fuel output power of a Electrolyseur in kW
#'
#'@importFrom Root maxP_fuel_plus
#'@export
setMethod("maxP_fuel_plus", signature(object = "Electrolyseur"),function(object){
  if(length(maxP_el_minus(object))*length(effMaxP_el_minus(object))==0){
    print("maxP_el_minus and effMaxP_el_minus have to be set, to get maxP_fuel_plus")
  }

  return(object@maxP_fuel_plus)
})
#---maxP_th_plus (get)-------------------------------------
#'maxP_th_plus (Get)
#'
#'Get the maximal thermal output power of a Electrolyseur in kW
#'
#'@importFrom Root maxP_th_plus
#'@export
setMethod("maxP_th_plus", signature(object = "Electrolyseur"),function(object){
  if(length(maxP_el_minus(object))*length(effMaxP_th_plus(object))*length(effMaxP_el_minus(object))==0){
    print("maxP_el_minus, effMaxP_th_plus and effMaxP_el_minus have to be set, to get maxP_th_plus")
    }

  return(object@maxP_th_plus)
})
#---minP_el_minus (get)-------------------------------------
#'minP_el_minus (Get)
#'
#'Get the minimal electric input power of a Electrolyseur in kW
#'
#'@importFrom Root minP_el_minus
#'@export
setMethod("minP_el_minus", signature(object = "Electrolyseur"),function(object){
  return(object@minP_el_minus)
})
#---minP_fuel_plus (get)-------------------------------------
#'minP_fuel_plus (Get)
#'
#'Get the minimal fuel output power of a Electrolyseur in kW
#'
#'@importFrom Root minP_fuel_plus
#'@export
setMethod("minP_fuel_plus", signature(object = "Electrolyseur"),function(object){
  if(length(minP_el_minus(object))*length(effMinP_el_minus(object))==0){
    print("minP_el_minus and effMinP_el_minus have to be set, to get minP_fuel_plus")
  }
  return(object@minP_fuel_plus)
})
#---minP_th_plus (get)-------------------------------------
#'minP_th_plus (Get)
#'
#'Get the minimal thermal output power of a Electrolyseur in kW
#'
#'@importFrom Root minP_th_plus
#'@export
setMethod("minP_th_plus", signature(object = "Electrolyseur"),function(object){
  if(length(minP_el_minus(object))*length(effMinP_th_plus(object))*length(effMinP_el_minus(object))==0){
    print("minP_el_minus, effMinP_th_plus and effMinP_el_minus have to be set, to get minP_th_plus")
    }

  return(object@minP_th_plus)
})





#---load_P_th_plus (get)---------------------------------------
#'load_P_th_plus (get)
#'
#'Get the profil of P_th_plus of the Electrolyseur
#'
#'@importFrom Root load_P_th_plus
#'
#'@export
setMethod("load_P_th_plus", signature(object = "Electrolyseur"),function(object){
  return(object@load_P_th_plus)
})

#---effMaxP_el_minus (get)---------------------------------------
#'effMaxP_el_minus (get)
#'
#'Get the electrical efficiency of a Electrolyseur at maximum load in \%
#'
#'@importFrom Root effMaxP_el_minus
#'
#'@export
setMethod("effMaxP_el_minus", signature(object = "Electrolyseur"),function(object){
  return(object@effMaxP_el_minus)
})
#---effMaxP_fuel_plus (get)---------------------------------------
#'effMaxP_fuel_plus (get)
#'
#'Get the fuel efficiency of a Electrolyseur at maximum load in \%
#'
#'
#'@importFrom Root effMaxP_fuel_plus
#'@export
setMethod("effMaxP_fuel_plus", signature(object = "Electrolyseur"),function(object){

  if(length(effMaxP_th_plus(object))*length(effMaxP_el_minus(object))==0){
    print('effMaxP_th_plus and effMaxP_el_minus have to be set, to get effMaxP_fuel_plus')
  }
  return(object@effMaxP_fuel_plus)
})
#---effMaxP_th_plus (get)---------------------------------------
#'effMaxP_th_plus (get)
#'
#'Get the thermal efficiency of a Electrolyseur at maximum load in \%
#'
#'@importFrom Root effMaxP_th_plus
#'@export
setMethod("effMaxP_th_plus", signature(object = "Electrolyseur"),function(object){
  return(object@effMaxP_th_plus)
})
#---effMinP_el_minus (get)---------------------------------------
#'effMinP_el_minus (get)
#'
#'Get the electrical efficiency of a Electrolyseur at minimum load bevor shutdown in \%
#'
#'@importFrom Root effMinP_el_minus
#'@export
setMethod("effMinP_el_minus", signature(object = "Electrolyseur"),function(object){
  return(object@effMinP_el_minus)
})
#---effMinP_fuel_plus (get)---------------------------------------
#'effMinP_fuel_plus (get)
#'
#'Get the fuel efficiency of a Electrolyseur at minimum load bevor shutdown in \%
#'
#'@importFrom Root effMinP_fuel_plus
#'@export
setMethod("effMinP_fuel_plus", signature(object = "Electrolyseur"),function(object){

  if(length(effMinP_th_plus(object))*length(effMinP_el_minus(object))==0){
    print('effMinP_th_plus and effMinP_el_minus have to be set, to get effMinP_fuel_plus')
  }
  return(object@effMinP_fuel_plus)
})
#---effMinP_th_plus (get)---------------------------------------
#'effMinP_th_plus (get)
#'
#'Get the thermal efficiency of a Electrolyseur at minimum load bevor shutdown in \%
#'
#'@importFrom Root effMinP_th_plus
#'@export
setMethod("effMinP_th_plus", signature(object = "Electrolyseur"),function(object){
  return(object@effMinP_th_plus)
})
#---price_maintenance (get)------------------------------------
#'price_maintenance (get)
#'
#'Get the price for maintenance of every produced kilowatthour of hydrogen
#'of CHP in €/kWh_th
#'
#'
#'@importFrom Root price_maintenance
#'
#'@export
setMethod("price_maintenance", signature(object = "Electrolyseur"),function(object){
  return(object@price_maintenance)
})
#---minRuntime (get)-------------------------------------
#'minRuntime (Get)
#'
#'Get the minimum runtime of the Electrolyseur
#'
#'@importFrom Root minRuntime
#'@export
setMethod("minRuntime", signature(object = "Electrolyseur"),function(object){
  return(object@minRuntime)
})
#---minDowntime (get)-------------------------------------
#'minDowntime (Get)
#'
#'Get the minimum downtime of the Electrolyseur
#'
#'@importFrom Root minDowntime
#'@export
setMethod("minDowntime", signature(object = "Electrolyseur"),function(object){
  return(object@minDowntime)
})

#---initial_state (get)-------------------------------------
#'initial_state (Get)
#'
#'Get the minimum initial_state of the Electrolyseur
#'
#'@importFrom Root initial_state
#'@export
setMethod("initial_state", signature(object = "Electrolyseur"),function(object){
  return(object@initial_state)
})

#---modulated (get)-------------------------------------
#'modulated (Get)
#'
#'Get modulated of the Electrolyseur
#'
#'@importFrom Root modulated
#'@export
setMethod("modulated", signature(object = "Electrolyseur"),function(object){
  return(object@modulated)
})

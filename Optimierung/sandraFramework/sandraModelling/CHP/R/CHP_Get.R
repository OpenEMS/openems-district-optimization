#---Overview-----------------------------------------------
# maxP_el_plus
# maxP_fuel_minus
# maxP_th_plus
# minP_el_plus
# minP_fuel_minus
# minP_th_plus
# effMaxP_el_plus
# effMaxP_fuel_minus
# effMaxP_th_plus
# effMinP_el_plus
# effMinP_fuel_minus
# effMinP_th_plus
# load_P_th_plus
# price_maintenance
# minRuntime
# minDowntime
# initial_state
# modulated
# initial_hours
# price_opChange
#----------------------------------------------------------
#---maxP_el_plus (get)-------------------------------------
#'maxP_el_plus (Get)
#'
#'Get the maximal electric output power of a CHP in kW
#'
#'@importFrom Root maxP_el_plus
#'@export
setMethod("maxP_el_plus", signature(object = "CHP"),function(object){
  return(object@maxP_el_plus)
})
#---maxP_fuel_minus (get)-------------------------------------
#'maxP_fuel_minus (Get)
#'
#'Get the maximal fuel input power of a CHP in kW
#'
#'@importFrom Root maxP_fuel_minus
#'@export
setMethod("maxP_fuel_minus", signature(object = "CHP"),function(object){
  if(length(maxP_el_plus(object))*length(effMaxP_el_plus(object))==0){
    print("maxP_el_plus and effMaxP_el_plus have to be set, to get maxP_fuel_minus")
  }

  return(object@maxP_fuel_minus)
})
#---maxP_th_plus (get)-------------------------------------
#'maxP_th_plus (Get)
#'
#'Get the maximal thermal output power of a CHP in kW
#'
#'@importFrom Root maxP_th_plus
#'@export
setMethod("maxP_th_plus", signature(object = "CHP"),function(object){
  if(length(maxP_el_plus(object))*length(effMaxP_th_plus(object))*length(effMaxP_el_plus(object))==0){
    print("maxP_el_plus, effMaxP_th_plus and effMaxP_el_plus have to be set, to get maxP_th_plus")
    }

  return(object@maxP_th_plus)
})
#---minP_el_plus (get)-------------------------------------
#'minP_el_plus (Get)
#'
#'Get the minimal electric output power of a CHP in kW
#'@importFrom Root minP_el_plus
#'@export
setMethod("minP_el_plus", signature(object = "CHP"),function(object){
  return(object@minP_el_plus)
})
#---minP_fuel_minus (get)-------------------------------------
#'minP_fuel_minus (Get)
#'
#'Get the minimal fuel input power of a CHP in kW
#'@importFrom Root minP_fuel_minus
#'@export
setMethod("minP_fuel_minus", signature(object = "CHP"),function(object){
  if(length(minP_el_plus(object))*length(effMinP_el_plus(object))==0){
    print("minP_el_plus and effMinP_el_plus have to be set, to get minP_fuel_minus")
  }
  return(object@minP_fuel_minus)
})
#---minP_th_plus (get)-------------------------------------
#'minP_th_plus (Get)
#'
#'Get the minimal thermal output power of a CHP in kW
#'
#'@importFrom Root minP_th_plus
#'@export
setMethod("minP_th_plus", signature(object = "CHP"),function(object){
  if(length(minP_el_plus(object))*length(effMinP_th_plus(object))*length(effMinP_el_plus(object))==0){
    print("minP_el_plus, effMinP_th_plus and effMinP_el_plus have to be set, to get minP_th_plus")
    }

  return(object@minP_th_plus)
})





#---load_P_th_plus (get)---------------------------------------
#'load_P_th_plus (get)
#'
#'Get the profil of P_th_plus of the CHP
#'
#'@importFrom Root load_P_th_plus
#'
#'@export
setMethod("load_P_th_plus", signature(object = "CHP"),function(object){
  return(object@load_P_th_plus)
})

#---effMaxP_el_plus (get)---------------------------------------
#'effMaxP_el_plus (get)
#'
#'Get the electrical efficiency of a CHP at maximum load in \%
#'
#'@importFrom Root effMaxP_el_plus
#'
#'@export
setMethod("effMaxP_el_plus", signature(object = "CHP"),function(object){
  return(object@effMaxP_el_plus)
})
#---effMaxP_fuel_minus (get)---------------------------------------
#'effMaxP_fuel_minus (get)
#'
#'Get the fuel efficiency of a CHP at maximum load in \%
#'
#'
#'@importFrom Root effMaxP_fuel_minus
#'@export
setMethod("effMaxP_fuel_minus", signature(object = "CHP"),function(object){

  if(length(effMaxP_th_plus(object))*length(effMaxP_el_plus(object))==0){
    print('effMaxP_th_plus and effMaxP_el_plus have to be set, to get effMaxP_fuel_minus')
  }
  return(object@effMaxP_fuel_minus)
})
#---effMaxP_th_plus (get)---------------------------------------
#'effMaxP_th_plus (get)
#'
#'Get the thermal efficiency of a CHP at maximum load in \%
#'
#'@importFrom Root effMaxP_th_plus
#'@export
setMethod("effMaxP_th_plus", signature(object = "CHP"),function(object){
  return(object@effMaxP_th_plus)
})
#---effMinP_el_plus (get)---------------------------------------
#'effMinP_el_plus (get)
#'
#'Get the electrical efficiency of a CHP at minimum load bevor shutdown in \%
#'@importFrom Root effMinP_el_plus
#'@export
setMethod("effMinP_el_plus", signature(object = "CHP"),function(object){
  return(object@effMinP_el_plus)
})
#---effMinP_fuel_minus (get)---------------------------------------
#'effMinP_fuel_minus (get)
#'
#'@importFrom Root effMinP_fuel_minus
#'Get the fuel efficiency of a CHP at minimum load bevor shutdown in \%
#'
#'@export
setMethod("effMinP_fuel_minus", signature(object = "CHP"),function(object){

  if(length(effMinP_th_plus(object))*length(effMinP_el_plus(object))==0){
    print('effMinP_th_plus and effMinP_el_plus have to be set, to get effMinP_fuel_minus')
  }
  return(object@effMinP_fuel_minus)
})
#---effMinP_th_plus (get)---------------------------------------
#'effMinP_th_plus (get)
#'
#'Get the thermal efficiency of a CHP at minimum load bevor shutdown in \%
#'
#'@importFrom Root effMinP_th_plus
#'@export
setMethod("effMinP_th_plus", signature(object = "CHP"),function(object){
  return(object@effMinP_th_plus)
})
#---price_maintenance (get)------------------------------------
#'price_maintenance (get)
#'
#'Get the price for maintenance of every produced kilowatthour of electricity
#'of CHP in €/kWh_th
#'
#'
#'@importFrom Root price_maintenance
#'
#'@export
setMethod("price_maintenance", signature(object = "CHP"),function(object){
  return(object@price_maintenance)
})
#---minRuntime (get)-------------------------------------
#'minRuntime (Get)
#'
#'Get the minimum runtime of the CHP
#'@importFrom Root minRuntime
#'
#'@export
setMethod("minRuntime", signature(object = "CHP"),function(object){
  return(object@minRuntime)
})
#---minDowntime (get)-------------------------------------
#'minDowntime (Get)
#'
#'Get the minimum downtime of the CHP
#'@importFrom Root minDowntime
#'
#'@export
setMethod("minDowntime", signature(object = "CHP"),function(object){
  return(object@minDowntime)
})

#---initial_state (get)-------------------------------------
#'initial_state (Get)
#'
#'Get the minimum initial_state of the CHP
#'@importFrom Root initial_state
#'
#'@export
setMethod("initial_state", signature(object = "CHP"),function(object){
  return(object@initial_state)
})

#---modulated (get)-------------------------------------
#'modulated (Get)
#'
#'Get modulated of the CHP
#'@importFrom Root modulated
#'
#'@export
setMethod("modulated", signature(object = "CHP"),function(object){
  return(object@modulated)
})


#---initial_hours (get)-------------------------------------
#'initial_hours (Get)
#'
#'Get the minimum initial_state of the CHP
#'@importFrom Root initial_hours
#'
#'@export
setMethod("initial_hours", signature(object = "CHP"),function(object){
  return(object@initial_hours)
})


#---price_opChange (get)-------------------------------------
#'price_opChange (Get)
#'
#'@importFrom Root price_opChange
#'
#'@export
setMethod("price_opChange", signature(object = "CHP"),function(object){
  return(object@price_opChange)
})



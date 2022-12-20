#---maxP_el_plus (get)-------------------------------------
#'maxP_el_plus (Get)
#'
#'Get the maximal electric output power of a Wado in kW
#'
#'
#'@importFrom Root maxP_el_plus
#'@export
setMethod("maxP_el_plus", signature(object = "Wado"),function(object){
  return(object@maxP_el_plus)
})
#---maxP_el_minus (get)-------------------------------------
#'maxP_el_minus (Get)
#'
#'Get the maximal electric output power of a Wado in kW
#'
#'@importFrom Root maxP_el_minus
#'@export
setMethod("maxP_el_minus", signature(object = "Wado"),function(object){
  return(object@maxP_el_minus)
})

#---maxP_fuel_plus (get)-------------------------------------
#'maxP_fuel_plus (Get)
#'
#'Get the maximal electric output power of a Wado in kW
#'
#'@importFrom Root maxP_fuel_plus
#'
#'@export
setMethod("maxP_fuel_plus", signature(object = "Wado"),function(object){
  return(object@maxP_fuel_plus)
})

#---maxP_fuel_minus (get)-------------------------------------
#'maxP_fuel_minus (Get)
#'
#'Get the maximal electric output power of a Wado in kW
#'
#'@importFrom Root maxP_fuel_minus
#'
#'@export
setMethod("maxP_fuel_minus", signature(object = "Wado"),function(object){
  return(object@maxP_fuel_minus)
})

#---maxP_th_plus (get)-------------------------------------
#'maxP_th_plus (Get)
#'
#'Get the maximal electric output power of a Wado in kW
#'
#'@importFrom Root maxP_th_plus
#'
#'@export
setMethod("maxP_th_plus", signature(object = "Wado"),function(object){
  return(object@maxP_th_plus)
})

#---maxP_th_minus (get)-------------------------------------
#'maxP_th_minus (Get)
#'
#'Get the maximal electric output power of a Wado in kW
#'
#'@importFrom Root maxP_th_minus
#'
#'@export
setMethod("maxP_th_minus", signature(object = "Wado"),function(object){
  return(object@maxP_th_minus)
})

#---minP_el_plus (get)-------------------------------------
#'minP_el_plus (Get)
#'
#'Get the minimal electric output power of a Wado in kW
#'
#'@importFrom Root minP_el_plus
#'
#'@export
setMethod("minP_el_plus", signature(object = "Wado"),function(object){
  return(object@minP_el_plus)
})
#---minP_el_minus (get)-------------------------------------
#'minP_el_minus (Get)
#'
#'Get the minimal electric output power of a Wado in kW
#'
#'@importFrom Root minP_el_minus
#'
#'@export
setMethod("minP_el_minus", signature(object = "Wado"),function(object){
  return(object@minP_el_minus)
})

#---minP_fuel_plus (get)-------------------------------------
#'minP_fuel_plus (Get)
#'
#'Get the minimal electric output power of a Wado in kW
#'
#'@importFrom Root minP_fuel_plus
#'
#'@export
setMethod("minP_fuel_plus", signature(object = "Wado"),function(object){
  return(object@minP_fuel_plus)
})

#---minP_fuel_minus (get)-------------------------------------
#'minP_fuel_minus (Get)
#'
#'Get the minimal electric output power of a Wado in kW
#'
#'@importFrom Root minP_fuel_minus
#'
#'@export
setMethod("minP_fuel_minus", signature(object = "Wado"),function(object){
  return(object@minP_fuel_minus)
})

#---minP_th_plus (get)-------------------------------------
#'minP_th_plus (Get)
#'
#'Get the minimal electric output power of a Wado in kW
#'
#'@importFrom Root minP_th_plus
#'@export
setMethod("minP_th_plus", signature(object = "Wado"),function(object){
  return(object@minP_th_plus)
})

#---minP_th_minus (get)-------------------------------------
#'minP_th_minus (Get)
#'
#'Get the minimal electric output power of a Wado in kW
#'
#'@importFrom Root minP_th_minus
#'
#'@export
setMethod("minP_th_minus", signature(object = "Wado"),function(object){
  return(object@minP_th_minus)
})

#---ts_virtual (get)-------------------------------------
#'ts_virtual (Get)
#'
#'Get if Wado has a virtual ts
#'
#'@importFrom Root ts_virtual
#'
#'@export
setMethod("ts_virtual", signature(object = "Wado"),function(object){
  return(object@ts_virtual)
})

#---Overview-----------------------------------------------
# maxP_el_plus
# minP_el_plus
# effMaxP_el_plus
# effMaxP_th_plus
# effMinP_el_plus
# effMinP_th_plus
# load_P_th_plus
# price_maintenance
# minRuntime
# minDowntime
# initial_state
# initial_stateEndurance
# modulated
# price_opChange
# initial_hours
#
#----------------------------------------------------------
#---maxP_el_plus (set)-----------------------------------
#'maxP_el_plus (set)
#'
#'Set the maximal electric output of a CHP in kW.  If the maxP_el_plus is set to -1, the CHP can produce electrical and thermal energy unlimited and CHP can consume fuel unlimited.
#'
#'@importFrom Root maxP_el_plus<-
#'@examples
#'chp<-new.CHP()
#'maxP_el_plus(chp) <- 10
#'@export
setMethod("maxP_el_plus<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "maxP_el_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_el_plus has to be positiv or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxP_el_plus <- value

    #if the electric efficiency at maximum power is available, calculate the maximal fuel power input before shutdown
    if(length(effMaxP_el_plus(object))>0){
      if(maxP_el_plus(object)==-1){object@maxP_fuel_minus<- -1
      }else{
      object@maxP_fuel_minus <- maxP_el_plus(object) / effMaxP_el_plus(object)
      }
      #if also the thermal efficiency at maximum power is available, calculate  the maximal thermal power output
      if(length(effMaxP_th_plus(object))>0){
        if(maxP_el_plus(object)==-1){object@maxP_th_plus<- -1
        }else{
        object@maxP_th_plus <- effMaxP_th_plus(object)* maxP_fuel_minus(object)}
      }
    }


    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})
#---minP_el_plus (set)-----------------------------------
#'minP_el_plus (set)
#'
#'Set the minimal electric output of a CHP in kW
#'
#'
#'@importFrom Root minP_el_plus<-
#'@examples
#'chp <- new.CHP()
#'minP_el_plus(chp) <- 5
#'@export
setMethod("minP_el_plus<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minP_el_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_el_plus has to be >= 0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_el_plus <- value

    #if the electric efficiency at minimum power is available, calculate the minimal fuel power input before shutdown
    if(length(effMinP_el_plus(object))>0){
      object@minP_fuel_minus <- minP_el_plus(object) / effMinP_el_plus(object)
      #if also the thermal efficiency at minimum power is available, calculate  the minimal thermal power output
      if(length(effMinP_th_plus(object))>0){
        object@minP_th_plus <- effMinP_th_plus(object)* minP_fuel_minus(object)
      }
    }
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})

#---effMaxP_el_plus (set)-------------------------------------
#'effMaxP_el_plus (set)
#'
#'Set the electric efficiency at maximal load of a CHP in \% (bigger than 0)
#'
#'@importFrom Root effMaxP_el_plus<-
#'
#'@examples
#'chp <- new.CHP()
#'effMaxP_el_plus(chp) <- 0.9
#'@export
setMethod("effMaxP_el_plus<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "effMaxP_el_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value >1 & value <= 0) {
    msg <- "effMaxP_el_plus has to be bigger than 0 and <= 1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@effMaxP_el_plus <- value

if(length(object@effMaxP_fuel_minus)!=0){
  object@effMaxP_th_plus <- object@effMaxP_fuel_minus - object@effMaxP_el_plus
}
    #if the maximal electric power output is available, calculate the maximal fuel power input before shutdown
    if(length(maxP_el_plus(object))>0){
      object@maxP_fuel_minus <- maxP_el_plus(object) / effMaxP_el_plus(object)
      #if also the thermal efficiency at maximum power is available, calculate  the maximal thermal power output
      if(length(effMaxP_th_plus(object))>0){
        object@maxP_th_plus <- effMaxP_th_plus(object)* maxP_fuel_minus(object)
      }
    }
    #if  the thermal efficiency at maximum power is available, calculate the fuel efficiency at maximum power
    if(length(effMaxP_th_plus(object))>0){
      object@effMaxP_fuel_minus <- effMaxP_th_plus(object)+ effMaxP_el_plus(object)
    }

    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#---effMaxP_th_plus (set)-------------------------------------
#'effMaxP_th_plus (set)
#'
#'Set the thermal efficiency at maximal load of a CHP in \% (bigger than 0)
#'
#'@importFrom Root effMaxP_th_plus<-
#'@examples
#'chp<-new.CHP()
#'effMaxP_th_plus(chp)<-0.9
#'@export
setMethod("effMaxP_th_plus<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "effMaxP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value >1 & value <= 0) {
    msg <- "effMaxP_th_plus has to be bigger than 0 and <=1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@effMaxP_th_plus <- value

    #if the efficiency at maximum power is available,  calculate the fuel efficiency at maximum power before shutdow
    if(length(effMaxP_el_plus(object))>0){
      object@effMaxP_fuel_minus <- effMaxP_th_plus(object) + effMaxP_el_plus(object)
      # if also the the maximal electric power output thermal is available,  calculate  the maximal thermal power output
      if(length(maxP_el_plus(object))>0){
        object@maxP_th_plus <- effMaxP_th_plus(object)* maxP_fuel_minus(object)
      }
    }

    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#---effMinP_el_plus (set)-------------------------------------
#'effMinP_el_plus (set)
#'
#'Set the electric efficiency at minimal load of a CHP in \% (bigger than 0)
#'
#'@importFrom Root effMinP_el_plus<-
#'@examples
#'chp <- new.CHP()
#'effMinP_el_plus(chp) <- 0.8
#'
#'@export
setMethod("effMinP_el_plus<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "effMinP_el_plus has to be of length one"
    errors <- c(msg, errors)
  }


  if (value >1 & value <= 0) {
    msg <- "effMinP_el_plus has to be bigger than 0 and <= 1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@effMinP_el_plus <- value

    #if the minimal electric power output is available, calculate the minimal fuel power input before shutdown
    if(length(minP_el_plus(object))>0){
      object@minP_fuel_minus <- minP_el_plus(object) / effMinP_el_plus(object)
      #if also the thermal efficiency at minimum power is available, calculate  the minimal thermal power output
      if(length(effMinP_th_plus)>0){
        object@minP_th_plus <- effMinP_th_plus(object)* minP_fuel_minus(object)
      }
    }

    #if  the thermal efficiency at minimum power is available, calculate the fuel efficiency at minimum power before shutdow
    if(length(effMinP_th_plus(object))>0){
      object@effMinP_fuel_minus <- effMinP_th_plus(object) + effMinP_el_plus(object)
    }

    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#---effMinP_th_plus (set)-------------------------------------
#'effMinP_th_plus (set)
#'
#'Set the thermal efficiency at minimal load of a CHP in \% (bigger than 0)
#'
#'@importFrom Root effMinP_th_plus<-
#'@examples
#'chp <- new.CHP()
#'effMinP_th_plus(chp) <- 0.9
#'@export
setMethod("effMinP_th_plus<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "effMinP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value >1 & value <= 0) {
    msg <- "effMinP_th_plus has to be bigger than 0 and  <= 1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@effMinP_th_plus <- value

    #if the efficiency at minimum power is available,  calculate the fuel efficiency at minimum power before shutdow
    if(length(effMinP_el_plus(object))>0){
      object@effMinP_fuel_minus <- effMinP_th_plus(object) + effMinP_el_plus(object)
      # if also the the minimal electric power output thermal is available,  calculate  the minimal thermal power output
      if(length(minP_el_plus(object))>0){
        object@minP_th_plus <- effMinP_th_plus(object)* minP_fuel_minus(object)
      }
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})

#----load_P_th_plus-----------------------------------------
#'load_P_th_plus (set)
#'
#'Set the profil of P_th_plus of class CHP in kW. Each row contains the value of one timestep. If the value of row x should be used for timestep x (positiv value) or not (negativ value)
#'
#'@importFrom Root load_P_th_plus<-
#'@param object A CHP
#'@param value A numeric
#'@examples
#'chp <- new.CHP()
#'load_P_th_plus(chp) <- c(15,3,2,5,2)
#'@export

setMethod("load_P_th_plus<-", signature(object = "CHP",value="numeric"), function(object, value) {

  errors<-character();


  if (length(errors)==0) {
    object@load_P_th_plus <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})


#---price_maintenance (set)-------------------------------
#'price_maintenance (set)
#'
#'Set the maintenance price for every produced kilowatthour of electricity
#'in €/kWh_el
#'
#'@importFrom Root price_maintenance<-
#'@examples
#'chp <- new.CHP()
#'price_maintenance(chp) <- 0.03
#'@export
setMethod("price_maintenance<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  te = length(object@timegrid)
  token = length(value) == 1
  if(te != 0){
    token = token | (length(value) ==  te)
  }else{
    token = TRUE
  }
  if (!token) {
    msg <- "price_maintenance has to be of length one or of the same length as timegrid"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@price_maintenance <- value
  }else{
    print(errors)
  }
  return(object)
})

#---minRuntime (set)-----------------------------------
#'minRuntime (set)
#'
#'Set the minimal runtime of the CHP
#'
#'
#'@importFrom Root minRuntime<-
#'@examples
#'chp <- new.CHP()
#'minRuntime(chp) <- 3
#'@export
setMethod("minRuntime<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minRuntime has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minRuntime has to be positive"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minRuntime <- value
  }else{
    print(errors)

  }
  return(object)
})


#---minDowntime (set)-----------------------------------
#'minDowntime (set)
#'
#'Set the minimal downtime of the CHP
#'
#'@importFrom Root minDowntime<-
#'
#'@examples
#'chp <- new.CHP()
#'minDowntime(chp) <- 2
#'@export
setMethod("minDowntime<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minDowntime has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minDowntime has to be positive"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minDowntime <- value
  }else{
    print(errors)

  }
  return(object)
})


#---initial_state (set)-----------------------------------
#'initial_state (set)
#'
#'Set the initial state of the CHP
#'
#'@importFrom Root initial_state<-
#'
#'@examples
#'chp <- new.CHP()
#'initial_state(chp) <- 0
#'@export
setMethod("initial_state<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "initial_state has to be of length one"
    errors <- c(msg, errors)
  }

  if (value != 0 & value != 1) {
    msg <- "initial_state has to be 0 or 1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@initial_state <- value
  }else{
    print(errors)

  }
  return(object)
})

#---initial_stateEndurance (set)-----------------------------------
#'initial_stateEndurance (set)
#'
#'Set the initial state of the CHP
#'
#'@importFrom Root initial_stateEndurance<-
#'
#'@examples
#'chp <- new.CHP()
#'initial_stateEndurance(chp) <- 0
#'@export
setMethod("initial_stateEndurance<-", signature(object = "CHP",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "initial_stateEndurance has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "initial_stateEndurance has to be >= 0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@initial_stateEndurance <- value
  }else{
    print(errors)

  }
  return(object)
})


#---modulated (set)-------------------------------------
#'modulated (set)
#'
#'set modulated of the CHP
#'@importFrom Root modulated<-
#'
#'@export
setMethod("modulated<-", signature(object = "CHP",value = "numeric"),function(object,value){
  object@modulated <- value
  return(object)
})

#---price_opChange (set)-------------------------------------
#'price_opChange (set)
#'
#'set price_opChange of the CHP
#'@importFrom Root price_opChange<-
#'
#'@export
setMethod("price_opChange<-", signature(object = "CHP",value = "numeric"),function(object,value){
  object@price_opChange <- value
  return(object)
})


#---initial_hours (set)-------------------------------------
#'initial_hours (set)
#'
#'set initial_hours of the CHP
#'@importFrom Root initial_hours<-
#'
#'@export
setMethod("initial_hours<-", signature(object = "CHP",value = "numeric"),function(object,value){
  object@initial_hours <- value
  return(object)
})

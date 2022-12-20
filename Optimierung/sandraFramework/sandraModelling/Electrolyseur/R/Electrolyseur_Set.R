#---Overview-----------------------------------------------
# maxP_el_minus
# minP_el_minus
# effMaxP_fuel_plus
# effMaxP_th_plus
# effMinP_fuel_plus
# effMinP_th_plus
# load_P_th_plus
# price_maintenance
# minRuntime
# minDowntime
# initial_state
# initial_stateEndurance
# modulated


#----------------------------------------------------------
#---maxP_el_minus (set)-----------------------------------
#'maxP_el_minus (set)
#'
#'Set the maximal electric input of a Electrolyseur in kW.  If the maxP_el_minus 
#'is set to -1, the electrolyseur can produce hydrogen and thermal energy 
#'unlimited and Electrolyseur can consume electrical power unlimited.
#'
#'@importFrom Root maxP_el_minus<-
#'@examples
#'electrolyseur<-new.Electrolyseur()
#'maxP_el_minus(electrolyseur) <- 10
#'@export
setMethod("maxP_el_minus<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "maxP_el_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_el_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxP_el_minus <- value

    #if the fuel efficiency at maximum power is available, calculate the maximal fuel power output before shutdown
    if(length(effMaxP_fuel_plus(object))>0){
      if(maxP_el_minus(object)==-1){object@maxP_fuel_plus<- -1
      }else{
      object@maxP_fuel_plus <- maxP_el_minus(object) * effMaxP_fuel_plus(object)
      }
      #if also the thermal efficiency at maximum power is available, calculate  the maximal thermal power output
      if(length(effMaxP_th_plus(object))>0){
        if(maxP_el_minus(object)==-1){object@maxP_th_plus<- -1
        }else{
        object@maxP_th_plus <- maxP_el_minus(object) * effMaxP_th_plus(object)}
      }
    }


    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})
#---minP_el_minus (set)-----------------------------------
#'minP_el_minus (set)
#'
#'Set the minimal electric input of Electrolyseur in kW
#'
#'
#'@importFrom Root minP_el_minus<-
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'minP_el_minus(electrolyseur) <- 5
#'@export
setMethod("minP_el_minus<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minP_el_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_el_minus has to be >= 0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_el_minus <- value

    #if the fuel efficiency at minimum power is available, calculate the minimal fuel power output before shutdown
    if(length(effMinP_fuel_plus(object))>0){
      object@minP_fuel_plus <- minP_el_minus(object) * effMinP_fuel_plus(object)
      #if also the thermal efficiency at minimum power is available, calculate  the minimal thermal power output
      if(length(effMinP_th_plus(object))>0){
        object@minP_th_plus <- minP_el_minus(object) * effMinP_th_plus(object)
      }
    }
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})

#---effMaxP_fuel_plus (set)-------------------------------------
#'effMaxP_fuel_plus (set)
#'
#'Set the fuel efficiency at maximal load of a Electrolyseur in \% (bigger than 0)
#'
#'@importFrom Root effMaxP_fuel_plus<-
#'
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'effMaxP_fuel_plus(electrolyseur) <- 0.9
#'@export
setMethod("effMaxP_fuel_plus<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "effMaxP_fuel_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value >1 & value <= 0) {
    msg <- "effMaxP_fuel_plus has to be bigger than 0 and <= 1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@effMaxP_fuel_plus <- value

    if(length(object@effMaxP_th_plus)!=0){
      object@effMaxP_el_minus <- object@effMaxP_fuel_plus + object@effMaxP_th_plus
    }

    #if the maximal electricitc input is available, calculate the maximal fuel output before shutdown
    if(length(maxP_el_minus(object))>0){
      object@maxP_fuel_plus <- maxP_el_minus(object) / effMaxP_fuel_plus(object)
      #if also the thermal efficiency at maximum power is available, calculate  the maximal thermal power output
      if(length(effMaxP_th_plus(object))>0){
        object@maxP_th_plus <- maxP_el_minus(object) / effMaxP_th_plus(object)
      }
    }
    #if  the thermal efficiency at maximum power is available, calculate the electricity efficiency at maximum power
    # if(length(effMaxP_th_plus(object))>0){
    #   object@effMaxP_el_minus <- effMaxP_fuel_plus(object) - effMaxP_th_plus(object)
    # }

    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#---effMaxP_th_plus (set)-------------------------------------
#'effMaxP_th_plus (set)
#'
#'Set the thermal efficiency at maximal load of a Electrolyseur in \% (bigger than 0)
#'
#'@importFrom Root effMaxP_th_plus<-
#'@examples
#'electrolyseur<-new.Electrolyseur()
#'effMaxP_th_plus(electrolyseur)<-0.9
#'@export
setMethod("effMaxP_th_plus<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
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
    
    if(length(object@effMaxP_fuel_plus)!=0){
      object@effMaxP_el_minus <- object@effMaxP_fuel_plus + object@effMaxP_th_plus
    }

    #if the efficiency at maximum power is available,  calculate the fuel efficiency at maximum power before shutdow
    if(length(effMaxP_el_minus(object))>0){
      #object@effMaxP_fuel_plus <- effMaxP_el_minus(object) - effMaxP_th_plus(object)
      # if also the the maximal electric power input is available,  calculate  the maximal thermal power output
      if(length(maxP_el_minus(object))>0){
        object@maxP_th_plus <- maxP_el_minus(object) * effMaxP_th_plus(object) 
      }
    }

    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#---effMinP_fuel_plus (set)-------------------------------------
#'effMinP_fuel_plus (set)
#'
#'Set the fuel efficiency at minimal load of a Electrolyseur in \% (bigger than 0)
#'
#'@importFrom Root effMinP_fuel_plus<-
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'effMinP_fuel_plus(electrolyseur) <- 0.8
#'
#'@export
setMethod("effMinP_fuel_plus<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "effMinP_fuel_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value >1 & value <= 0) {
    msg <- "effMinP_fuel_plus has to be bigger than 0 and <= 1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@effMinP_fuel_plus <- value
    
    if(length(object@effMinP_fuel_plus)!=0){
      object@effMinP_el_minus <- object@effMinP_fuel_plus + object@effMinP_th_plus
    }

    #if the minimal electircal input is available, calculate the minimal fuel power output before shutdown
    if(length(minP_el_minus(object))>0){
      object@minP_fuel_plus <- minP_el_minus(object) / effMinP_fuel_plus(object) / 
      #if also the thermal efficiency at minimum power is available, calculate  the minimal thermal power output
      if(length(effMinP_th_plus)>0){
        object@minP_th_plus <- minP_el_minus(object) / effMinP_th_plus(object)
      }
    }

    #if  the thermal efficiency at minimum power is available, calculate the electircal efficiency at minimum power before shutdow
    # if(length(effMinP_th_plus(object))>0){
    #   object@effMinP_el_minus <- effMinP_fuel_plus(object) - effMinP_th_plus(object)
    # }

    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#---effMinP_th_plus (set)-------------------------------------
#'effMinP_th_plus (set)
#'
#'Set the thermal efficiency at minimal load of a Electrolyseur in \% (bigger than 0)
#'
#'@importFrom Root effMinP_th_plus<-
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'effMinP_th_plus(electrolyseur) <- 0.9
#'@export
setMethod("effMinP_th_plus<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
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
    
    if(length(object@effMinP_fuel_plus)!=0){
      object@effMinP_el_minus <- object@effMinP_fuel_plus + object@effMinP_th_plus
    }

    #if the minimal power input is available,  calculate the thermal power output before shutdow
    if(length(minP_el_minus(object))>0){
      object@minP_th_plus <- minP_el_minus(object) / effMinP_th_plus(object)
      # if also the the minimal fuel efficiency is available,  calculate  the minimal fuel power output
      if(length(effMinP_fuel_plus(object))>0){
        object@minP_fuel_plus <-  minP_el_minus(object) / effMinP_fuel_plus(object)
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
#'Set the profil of P_th_plus of class Electrolyseur in kW. Each row 
#'contains the value of one timestep. If the value of row x should be 
#'used for timestep x (positiv value) or not (negativ value)
#'
#'@importFrom Root load_P_th_plus<-
#'@param object A Electrolyseur
#'@param value A numeric
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'load_P_th_plus(electrolyseur) <- c(15,3,2,5,2)
#'@export

setMethod("load_P_th_plus<-", signature(object = "Electrolyseur",value="numeric"), function(object, value) {

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
#'Set the maintenance price for every produced kilowatthour of hydrogen (fuel)
#'in €/kWh_fuel
#'
#'@importFrom Root price_maintenance<-
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'price_maintenance(electrolyseur) <- 0.03
#'@export
setMethod("price_maintenance<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
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
#'Set the minimal runtime of the Electrolyseur
#'
#'
#'@importFrom Root minRuntime<-
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'minRuntime(electrolyseur) <- 3
#'@export
setMethod("minRuntime<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
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
#'Set the minimal downtime of the Electrolyseur
#'
#'@importFrom Root minDowntime<-
#'
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'minDowntime(electrolyseur) <- 2
#'@export
setMethod("minDowntime<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
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
#'Set the initial state of the Electrolyseur
#'
#'@importFrom Root initial_state<-
#'
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'initial_state(electrolyseur) <- 0
#'@export
setMethod("initial_state<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
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
#'Set the initial state of the Electrolyseur
#'
#'@importFrom Root initial_stateEndurance<-
#'
#'@examples
#'electrolyseur <- new.Electrolyseur()
#'initial_stateEndurance(electrolyseur) <- 0
#'@export
setMethod("initial_stateEndurance<-", signature(object = "Electrolyseur",value="numeric"),function(object,value){
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
#'set modulated of the Electrolyseur
#'@importFrom Root modulated<-
#'
#'@export
setMethod("modulated<-", signature(object = "Electrolyseur",value = "numeric"),function(object,value){
  object@modulated <- value
  return(object)
})


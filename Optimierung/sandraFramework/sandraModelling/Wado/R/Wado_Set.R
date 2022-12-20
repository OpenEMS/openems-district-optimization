# maxP_el_plus
# maxP_el_minus
# maxP_fuel_plus
# maxP_fuel_minus
# maxP_th_plus
# maxP_th_minus
# minP_el_plus
# minP_el_minus
# minP_fuel_plus
# minP_fuel_minus
# minP_th_plus
# minP_th_minus
# ts_virtual
# variables
#-----------------------
#---maxP_el_plus (set)-----------------------------------
#'maxP_el_plus (set)
#'
#'Set the maximal electric output of a Wado in kW. If the maxP_el_plus is set to -1, the Wado can produce electrical energy unlimited.
#'
#'@importFrom Root maxP_el_plus<-
#'
#'@examples
#'wado <- new.Wado()
#'maxP_el_plus(wado) <- 10
#'@export
setMethod("maxP_el_plus<-", signature(object = "Wado",value="numeric"),function(object,value){
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
  }else{
    print(errors)

  }
  return(object)
})
#---maxP_el_minus (set)-----------------------------------
#'maxP_el_minus (set)
#'
#'Set the maximal electric output of a Wado in kW. If the maxP_el_minus is set to -1, the Wado can consume electrical energy unlimited.
#'
#'
#'@importFrom Root maxP_el_minus<-
#'@examples
#'wado <- new.Wado()
#'maxP_el_minus(wado) <- 10
#'
#'@export
setMethod("maxP_el_minus<-", signature(object = "Wado",value="numeric"),function(object,value){
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
  }else{
    print(errors)

  }
  return(object)
})
#---maxP_fuel_plus (set)-----------------------------------
#'maxP_fuel_plus (set)
#'
#'Set the maximal electric output of a Wado in kW. If the maxP_fuel_plus is set to -1, the Wado can produce fuel unlimited.
#'
#'@importFrom Root maxP_fuel_plus<-
#'@export
setMethod("maxP_fuel_plus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "maxP_fuel_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_fuel_plus has to be positiv or -1"
    errors <- c(msg, errors)
  }



  if (length(errors)==0) {
    object@maxP_fuel_plus <- value
  }else{
    print(errors)

  }
  return(object)
})
#---maxP_fuel_minus (set)-----------------------------------
#'maxP_fuel_minus (set)
#'
#'Set the maximal electric output of a Wado in kW.If the maxP_fuel_minus is set to -1, the Wado can consume fuel unlimited.
#'
#'@importFrom Root maxP_fuel_minus<-
#'@export
setMethod("maxP_fuel_minus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "maxP_fuel_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_fuel_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@maxP_fuel_minus <- value
  }else{
    print(errors)

  }
  return(object)
})
#---maxP_th_plus (set)-----------------------------------
#'maxP_th_plus (set)
#'
#'Set the maximal electric output of a Wado in kW. If the maxP_th_plus is set to -1, the Wado can produce thermal energy unlimited.
#'@importFrom Root maxP_th_plus<-
#'
#'@export
setMethod("maxP_th_plus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "maxP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_th_plus has to be positiv or -1"
    errors <- c(msg, errors)
  }



  if (length(errors)==0) {
    object@maxP_th_plus <- value
  }else{
    print(errors)

  }
  return(object)
})
#---maxP_th_minus (set)-----------------------------------
#'maxP_th_minus (set)
#'
#'Set the maximal electric output of a Wado in kW. If the maxP_th_minus is set to -1, the Wado can consume thermal energy unlimited.
#'@importFrom Root maxP_th_minus<-
#'
#'@export
setMethod("maxP_th_minus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "maxP_th_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_el_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxP_th_minus <- value
  }else{
    print(errors)

  }
  return(object)
})



#---minP_el_plus (set)-----------------------------------
#'minP_el_plus (set)
#'
#'Set the minimal electric output of a Wado in kW
#'
#'@importFrom Root minP_el_plus<-
#'
#'@export
setMethod("minP_el_plus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minP_el_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_el_plus has to be >=0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_el_plus <- value
  }else{
    print(errors)

  }
  return(object)
})
#---minP_el_minus (set)-----------------------------------
#'minP_el_minus (set)
#'
#'Set the minimal electric output of a Wado in kW
#'
#'@importFrom Root minP_el_minus<-
#'@export
setMethod("minP_el_minus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minP_el_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_el_minus has to be >=0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_el_minus <- value
  }else{
    print(errors)

  }
  return(object)
})
#---minP_fuel_plus (set)-----------------------------------
#'minP_fuel_plus (set)
#'
#'Set the minimal electric output of a Wado in kW
#'
#'@importFrom Root minP_fuel_plus<-
#'@export
setMethod("minP_fuel_plus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minP_fuel_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_fuel_plus has to be >=0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_fuel_plus <- value
  }else{
    print(errors)

  }
  return(object)
})
#---minP_fuel_minus (set)-----------------------------------
#'minP_fuel_minus (set)
#'
#'Set the minimal electric output of a Wado in kW
#'
#'@importFrom Root minP_fuel_minus<-
#'@export
setMethod("minP_fuel_minus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minP_fuel_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_fuel_minus has to be >=0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_fuel_minus <- value
  }else{
    print(errors)

  }
  return(object)
})
#---minP_th_plus (set)-----------------------------------
#'minP_th_plus (set)
#'
#'Set the minimal electric output of a Wado in kW
#'
#'@importFrom Root minP_th_plus<-
#'@export
setMethod("minP_th_plus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_th_plus has to be >=0"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_th_plus <- value
  }else{
    print(errors)

  }
  return(object)
})
#---minP_th_minus (set)-----------------------------------
#'minP_th_minus (set)
#'
#'Set the minimal electric output of a Wado in kW
#'
#'@importFrom Root minP_th_minus<-
#'@export
setMethod("minP_th_minus<-", signature(object = "Wado",value="numeric"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "minP_th_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_th_minus has to be >="
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_th_minus <- value
  }else{
    print(errors)

  }
  return(object)
})



#---ts_virtual (set)-----------------------------------
#'ts_virtual (set)
#'
#'Set if wado has a virtual ts (TRUE or FALSE)
#'
#'@importFrom Root ts_virtual<-
#'@export
setMethod("ts_virtual<-", signature(object = "Wado",value="logical"),function(object,value){
  errors<-character()
  if (length(value) != 1) {
    msg <- "ts_virtual has to be of length one"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@ts_virtual <- value
  }else{
    print(errors)

  }
  return(object)
})


#---variables (set)----------------------------------------
#'variables (set)
#'
#'Set variables of Wado
#'
#'@importFrom Root variables<-
#'@export
#'@param object A Wado
#'@param value A character or list of characters see details
#'@details
#'For variables only subsets of the list are allowed:
#'\itemize{
#'\item "Op": on/off
#'\item "P_el_plus": electric production
#'\item "P_el_minus": electric consumption
#'\item "P_th_plus": thermal production
#'\item "P_th_minus": thermal consumption
#'\item "P_fuel_plus": fuel production
#'\item "P_fuel_minus": fuel consumption
#'\item "E_el": electric energy
#'\item "E_th": thermal energy
#'\item "E_fuel": fuel energy
#'}
#'Note:
#'\itemize{
#'\item If variable "P_xy_plus" is set, also "P_xy_minus" has to be set and vise versa (where xy is el, th or fuel)
#'\item If variables, name and timegrid of a Wado are set, the coordinates of the
#'Wado are built in the form name_var1_t1,name_var1_t2,..,name_var2_t1,
#'name_var2_t2....
#'\item The coordinates can be get by calling the getter function coord()
#'}
#'@return object The new object of Wado with set variables
setMethod("variables<-", signature(object = "Wado", value = "character"), function(object, value) {

  if(length(value)==0)
  {object@variables<-"Op";
  }else{
    errors<-character();

    if (length(errors)==0) {
      #add new variables (if variables(object)<-character(), the standard variables of the class set see new.xy in xy_Definition.R)
      object@variables <- c(variables(object),value)

      if(validObject(object)==TRUE){
        if (length(timegrid(object)) * length(name(object)) > 0) {
          #object@coord <- coordi(object)
          #update coordinates via set name  (function of Root)
          name(object)<-name(object)
        }
      }else{print(errors)}

    }else{
      print(errors)
    }
  }
  return(object)
})



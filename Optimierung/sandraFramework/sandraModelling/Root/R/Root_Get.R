#----overview----------------------------------------------
# coord
# info
# timegrid
# name
# variables
#----------------------------------------------------------
#---coord (get)--------------------------------------------
#' coord (get)
#'
#'Get the coordinates of a Root. The coordinates of a Root are built up in the
#'form var1_comp1_comp2_time1, ....
#'Note: At least two components within a sandbox are necessary to built coordinates
#'
#'@export
#'@param object A Root
#'@return character vector with all the coordinates as components
setMethod("coord", signature(object = "Root"), function(object) {
  return(object@coord)
})
#---info (get)--------------------------------------------
#' info (get)
#'
#'Get the info of a Root.
#'
#'@export
#'@param object A Root
#'@return character vector with all the infoinates as components
setMethod("info", signature(object = "Root"), function(object) {
  return(object@info)
})
#---name (get)---------------------------------------------
#' name (get)
#'
#'Get name of Root
#'
#'@export
#'@param object An Root
#'@return character. The name of object Root. Note the name of the Root is unique.
setMethod("name", signature(object = "Root"), function(object) {
  return(object@name)
})
#---timegrid (get)----------------------------------------
#' timegrid (get)
#'
#'Get timegrid of Root
#'
#'@export
#'@param object An Root
#'@return integer or vector of integers. The timegrid is a vector describing the time timegrid to be considered, each component a time block in minutes
setMethod("timegrid", signature(object = "Root"), function(object) {
  return(object@timegrid)
})
#---variables (get)----------------------------------------
#' variables (get)
#'
#'Get variables of Root
#'
#'@export
#'@param object An Root
#'@return character. The variables (possible variables see details) of object Root.
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
setMethod("variables", signature(object = "Root"), function(object) {
  return(object@variables)
})




#----maxP_el_minus-------------------------
#' maxP_el_minus (get)
#'
#' Get maxP_el_minus of Root
#'
#'@param object A Root object
#'@return  maxP_el_minus of Root
#'@export
#'
setMethod("maxP_el_minus", signature(object = "Root"), function(object) {

  if(is.element("maxP_el_minus", slotNames(object)) ){
    return(object@maxP_el_minus)
  }else{
    return(NULL)
  }
})

#---minP_el_minus--------------------------
#' minP_el_minus (get)
#'
#' Get minP_el_minus of Root
#'
#'@param object A Root
#'@return  minP_el_minus of Root
#'@export


setMethod("minP_el_minus", signature(object = "Root"), function(object) {
  if(is.element("minP_el_minus", slotNames(object)) ){
    return(object@minP_el_minus)
  }else{
    return(NULL)
  }
})

#---maxP_el_plus--------------------------
#' maxP_el_plus (get)
#'
#' Get maxP_el_plus of Root
#'
#'@param object A Root
#'@return  maxP_el_plus of Root
#'@export


setMethod("maxP_el_plus", signature(object = "Root"), function(object) {
  if(is.element("maxP_el_plus",slotNames(object))){
    return(object@maxP_el_plus)
  }else{
    return(NULL)
  }
})

#---minP_el_plus--------------------------
#' minP_el_plus (get)
#'
#' Get minP_el_plus of Root
#'
#'@param object A Root
#'@return  minP_el_plus of Root
#'@export



setMethod("minP_el_plus", signature(object = "Root"), function(object) {
  if(is.element("minP_el_plus",slotNames(object))){
    return(object@minP_el_plus)
  }else{
    return(NULL)
  }
})





#----maxP_th_minus-------------------------
#' maxP_th_minus (get)
#'
#' Get maxP_th_minus of Root
#'
#'@param object A Root object
#'@return  maxP_th_minus of Root
#'@export
#'
setMethod("maxP_th_minus", signature(object = "Root"), function(object) {

  if(is.element("maxP_th_minus", slotNames(object)) ){
    return(object@maxP_th_minus)
  }else{
    return(NULL)
  }
})

#---minP_th_minus--------------------------
#' minP_th_minus (get)
#'
#' Get minP_th_minus of Root
#'
#'@param object A Root
#'@return  minP_th_minus of Root
#'@export


setMethod("minP_th_minus", signature(object = "Root"), function(object) {
  if(is.element("minP_th_minus", slotNames(object)) ){
    return(object@minP_th_minus)
  }else{
    return(NULL)
  }
})

#---maxP_th_plus--------------------------
#' maxP_th_plus (get)
#'
#' Get maxP_th_plus of Root
#'
#'@param object A Root
#'@return  maxP_th_plus of Root
#'@export


setMethod("maxP_th_plus", signature(object = "Root"), function(object) {
  if(is.element("maxP_th_plus",slotNames(object))){
    return(object@maxP_th_plus)
  }else{
    return(NULL)
  }
})

#---minP_th_plus--------------------------
#' minP_th_plus (get)
#'
#' Get minP_th_plus of Root
#'
#'@param object A Root
#'@return  minP_th_plus of Root
#'@export



setMethod("minP_th_plus", signature(object = "Root"), function(object) {
  if(is.element("minP_th_plus",slotNames(object))){
    return(object@minP_th_plus)
  }else{
    return(NULL)
  }
})


###



#----maxP_fuel_minus-------------------------
#' maxP_fuel_minus (get)
#'
#' Get maxP_fuel_minus of Root
#'
#'@param object A Root object
#'@return  maxP_fuel_minus of Root
#'@export
#'
setMethod("maxP_fuel_minus", signature(object = "Root"), function(object) {

  if(is.element("maxP_fuel_minus", slotNames(object)) ){
    return(object@maxP_fuel_minus)
  }else{
    return(NULL)
  }
})

#---minP_fuel_minus--------------------------
#' minP_fuel_minus (get)
#'
#' Get minP_fuel_minus of Root
#'
#'@param object A Root
#'@return  minP_fuel_minus of Root
#'@export


setMethod("minP_fuel_minus", signature(object = "Root"), function(object) {
  if(is.element("minP_fuel_minus", slotNames(object)) ){
    return(object@minP_fuel_minus)
  }else{
    return(NULL)
  }
})

#---maxP_fuel_plus--------------------------
#' maxP_fuel_plus (get)
#'
#' Get maxP_fuel_plus of Root
#'
#'@param object A Root
#'@return  maxP_fuel_plus of Root
#'@export


setMethod("maxP_fuel_plus", signature(object = "Root"), function(object) {
  if(is.element("maxP_fuel_plus",slotNames(object))){
    return(object@maxP_fuel_plus)
  }else{
    return(NULL)
  }
})

#---minP_fuel_plus--------------------------
#' minP_fuel_plus (get)
#'
#' Get minP_fuel_plus of Root
#'
#'@param object A Root
#'@return  minP_fuel_plus of Root
#'@export



setMethod("minP_fuel_plus", signature(object = "Root"), function(object) {
  if(is.element("minP_fuel_plus",slotNames(object))){
    return(object@minP_fuel_plus)
  }else{
    return(NULL)
  }
})





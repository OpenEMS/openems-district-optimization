#---validity-----------------------------------------------
check.Root <- function(object) {
  errors <- character();
  if(length(variables(object))>0){
    if (anyDuplicated(variables(object)) != 0) {
      msg <- "variable names may only occure once"
      errors <- c(errors, msg)
    }

    allowed <- c(
      "Op",
      "P_el_plus",
      "P_el_minus",
      "P_th_plus",
      "P_th_minus",
      "P_fuel_plus",
      "P_fuel_minus",
      "E_el",
      "E_th",
      "E_fuel"
    )
    n <- length(variables(object))
    correctVar <- logical()
    for (i in 1:n) {
      correctVar <- c(correctVar, any(variables(object)[i] == allowed))
    }
    if (all(correctVar) == FALSE) {
      msg <- "not allowed variables. the allowed variables are: Op, P_el_plus, P_el_minus, P_th_plus, P_th_minus, P_fuel_plus,P_fuel_minus, E_el, E_th, E_fuel"
      errors <- c(errors, msg)
    }
  }

  if (length(errors) == 0) {
    return(TRUE)
  } else{
    return(errors)
  }
}

#---Definition of the class--------------------------------
#' The class Root
#'
#'A Root is an abstract concept of first uncorrelated variables describing
#'electrical, thermal and fuel production or consumption as timeseries. It
#'also contains all setGeneric()-Functions and the main slots to describe the
#'identity of an object.
#'
#'@export
#'@slot name A character. The name of the Root
#'@slot timegrid A numeric. Vector describing the time timegrid to be considered, each component a time block in minutes
#'@slot variables A character. The variable names, which are used to model the Root.
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
#'@slot coord A character. The coordinates of a Root are build up in the form#'name_var1_t1,name_var1_t2,..,name_var2_t1,name_var2_t2....
#'@slot info  A list. Containing additional information to describe the object
#'@slot invest A numeric. The investment costs.
#'@slot linelossel A numeric. Line loss of P_el_minus, based on AdjacencyEff_el
#'@slot linelossfuel A numeric. Line loss of P_fuel_minus, based on AdjacencyEff_fuel
#'@slot linelossth A numeric. Line loss of P_th_minus, based on AdjacencyEff_th
setClass(
  "Root",
  slots = list(
    name = "character",
    timegrid = "numeric",
    variables = "character",
    coord = "character",
    info = "list",
    invest = "numeric",
    linelossel= "numeric",
    linelossfuel= "numeric",
    linelossth= "numeric"
  ),
  validity = check.Root
)
#---constructor--------------------------------------------
#' new.Root
#'
#'creat a new obejct of class Root
#'
#'@export
#'@return New object of class Root
new.Root <- function() {
  return(new(Class = "Root", linelossel=1,linelossfuel=1,linelossth=1))
}

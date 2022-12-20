# ---------------------------------------------------------
# the validity function of HeSw
check.HeSw <- function(object) {
  errors <- character()


  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
# ---------------------------------------------------------
#' Class HeSw
#'
#'The HeSw represents the class Heatsword
#'
#'@slot maxP_th_plus the maximal output of thermal power in kW
#'@slot effMaxP_th_plus is the degree of efficiency between electrical power and thermal provided in \%
#'@slot maxP_el_minus   maximal input of electric power in kW

#'@export
setClass(
  "HeSw",
  slots = list(
    maxP_el_minus = "numeric" ,
    effMaxP_th_plus = "numeric" ,
    maxP_th_plus = "numeric"
  ),
  contains = "Root",
  validity = check.HeSw
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.HeSw
#'
#'Constructor of HeSw creates a new obejct of class HeSw
#'
#'@return New object of class HeSw
#'@export
new.HeSw <- function() {
  return(new(Class = "HeSw", variables= c("P_th_plus", "P_el_minus")
  ))
}

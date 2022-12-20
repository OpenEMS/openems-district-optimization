#--Overview-------
# adjacency_el
# adjacency_fuel
# adjacency_th
# adjacencyEff_el
# adjacencyEff_fuel
# adjacencyEff_th
# adjacencyPrice_el
# adjacencyPrice_fuel
# adjacencyPrice_th
# adjacencyWatch_el
# adjacencyWatch_fuel
# adjacencyWatch_th
# adjacencyWatch_el_var
# adjacencyWatch_fuel_var
# adjacencyWatch_th_var
# components

#----------------------------
#---adjacency_el------------------------------------------------------
#'adjacency_el (Get)
#'
#'Get adjacency_el, the adjacency matrix of the graph of the electric Sandbox.
#'It describes the relations of all components to each other in the Sandbox and the relation of the compontes to the Sandbox itself
#'If an entry is made there is a connection between the to components. If
#'there is no entry in the adjacency matrix no energy is able to be transported
#'from one component to the other. The value qualifies the limitation of the
#'maximal load that can be transportet in kW. If the value is negative, there
#'is no limitation. Together with the adjacencyEff_el it modells the ability
#'of transporting energy from one component to another.
#'
#'@importFrom Root adjacency_el
#'@export
setMethod("adjacency_el", "Sandbox", function(object) {

  return(object@adjacency_el)
})
#---adjacency_fuel------------------------------------------------------
#'adjacency_fuel (Get)
#'
#'Get adjacency_fuel, the adjacency matrix of the graph of the fuel Sandbox.
#'It describes the relations of all components to each other in the Sandbox and the relation of the compontes to the Sandbox itself.
#'If an entry is made there is a connection between the to components. If
#'there is no entry in the adjacency matrix no energy is able to be transported
#'from one component to the other. The value qualifies the limitation of the
#'maximal load that can be transportet in kW. If the value is negative, there
#'is no limitation. Together with the adjacencyEff_fuel it modells the ability
#'of transporting energy from one component to another.
#'
#'@importFrom Root adjacency_fuel
#'@export
setMethod("adjacency_fuel", "Sandbox", function(object) {

  return(object@adjacency_fuel)
})
#---adjacency_th------------------------------------------------------
#'adjacency_th (Get)
#'
#'Get adjacency_th, the adjacency matrix of the graph of the thermal Sandbox.
#'It describes the relations of all components to each other in the Sandbox.
#'If an entry is made there is a connection between the to components. If
#'there is no entry in the adjacency matrix no energy is able to be transported
#'from one component to the other. The value qualifies the limitation of the
#'maximal load that can be transportet in kW. If the value is negative, there
#'is no limitation. Together with the adjacencyEff_th it modells the ability
#'of transporting energy from one component to another.
#'
#'@importFrom Root adjacency_th
#'@export
setMethod("adjacency_th", "Sandbox", function(object) {

  return(object@adjacency_th)
})
#---adjacencyEff_el------------------------------------------------------
#'adjacencyEff_el (Get)
#'
#'Get adjacencyEff_el, the matrix describing the efficieny of electric
#'delivery in the  Sandbox. It describes the relations of all components
#'to each other in the Sandbox and the relation of the compontes to the Sandbox itself. If an entry is made there is a connection
#'between the to components. If there is no entry in the adjacency matrix
#'no energy is able to be transported from one component to the other. The
#'value qualifies in % the efficiency of the transportation. Together with the
#'adjacency_el it modells the ability of transporting energy from one
#'component to another.
#'
#'@importFrom Root adjacencyEff_el
#'@export

setMethod("adjacencyEff_el", "Sandbox", function(object) {
  return(object@adjacencyEff_el)
})
#---adjacencyEff_fuel------------------------------------------------------
#'adjacencyEff_fuel (Get)
#'
#'Get adjacencyEff_fuel, the matrix describing the efficieny of electric
#'delivery in the  Sandbox. It describes the relations of all components
#'to each other in the Sandbox and the relation of the compontes to the Sandbox itself. If an entry is made there is a connection
#'between the to components. If there is no entry in the adjacency matrix
#'no energy is able to be transported from one component to the other. The
#'value qualifies in % the efficiency of the transportation. Together with the
#'adjacency_fuel it modells the ability of transporting energy from one
#'component to another.
#'
#'@importFrom Root adjacencyEff_fuel
#'@export

setMethod("adjacencyEff_fuel", "Sandbox", function(object) {
  return(object@adjacencyEff_fuel)
})

#---adjacencyEff_th------------------------------------------------------
#'adjacencyEff_th (Get)
#'
#'Get adjacencyEff_th, the matrix describing the efficieny of electric
#'delivery in the  Sandbox. It describes the relations of all components
#'to each other in the Sandbox and the relation of the compontes to the Sandbox itself. If an entry is made there is a connection
#'between the to components. If there is no entry in the adjacency matrix
#'no energy is able to be transported from one component to the other. The
#'value qualifies in % the efficiency of the transportation. Together with the
#'adjacency_th it modells the ability of transporting energy from one
#'component to another.
#'
#'@importFrom Root adjacencyEff_th
#'@export

setMethod("adjacencyEff_th", "Sandbox", function(object) {
  return(object@adjacencyEff_th)
})


#---adjacencyPrice_el------------------------------------------------------
#'adjacencyPrice_el (Get)
#'
#'Get adjacencyPrice_el, the matrix describing the price of electric
#'delivery in the  Sandbox. It describes the relations of all components
#'to each other in the Sandbox and the relation of the compontes to the Sandbox itself. If an entry is made there is a connection
#'between the to components. If there is no entry in the adjacency matrix
#'no energy is able to be transported from one component to the other.
#'
#'@importFrom Root adjacencyPrice_el
#'@export

setMethod("adjacencyPrice_el", "Sandbox", function(object) {
  return(object@adjacencyPrice_el)
})
#---adjacencyPrice_fuel------------------------------------------------------
#'adjacencyPrice_fuel (Get)
#'
#'Get adjacencyPrice_fuel, the matrix describing the price of fuel
#'delivery in the  Sandbox. It describes the relations of all components
#'to each other in the Sandbox and the relation of the compontes to the Sandbox itself. If an entry is made there is a connection
#'between the to components. If there is no entry in the adjacency matrix
#'no energy is able to be transported from one component to the other.
#'
#'@importFrom Root adjacencyPrice_fuel
#'@export

setMethod("adjacencyPrice_fuel", "Sandbox", function(object) {
  return(object@adjacencyPrice_fuel)
})

#---adjacencyPrice_th------------------------------------------------------
#'adjacencyPrice_th (Get)
#'
#'Get adjacencyPrice_th, the matrix describing the price of thermal
#'delivery in the  Sandbox. It describes the relations of all components
#'to each other in the Sandbox and the relation of the compontes to the Sandbox itself. If an entry is made there is a connection
#'between the to components. If there is no entry in the adjacency matrix
#'no energy is able to be transported from one component to the other.
#'
#'@importFrom Root adjacencyPrice_th
#'@export

setMethod("adjacencyPrice_th", "Sandbox", function(object) {
  return(object@adjacencyPrice_th)
})
#---adjacencyWatch_el------------------------------------------------------
#'adjacencyWatch_el (Get)
#'
#'Get adjacencyWatch_el
#'
#'@importFrom Root adjacencyWatch_el
#'@export

setMethod("adjacencyWatch_el", "Sandbox", function(object) {
  return(object@adjacencyWatch_el)
})
#---adjacencyWatch_fuel------------------------------------------------------
#'adjacencyWatch_fuel (Get)
#'
#'Get adjacencyWatch_fuel
#'
#'@importFrom Root adjacencyWatch_fuel
#'@export

setMethod("adjacencyWatch_fuel", "Sandbox", function(object) {
  return(object@adjacencyWatch_fuel)
})
#---adjacencyWatch_th------------------------------------------------------
#'adjacencyWatch_th (Get)
#'
#'Get adjacencyWatch_th
#'
#'@importFrom Root adjacencyWatch_th
#'@export

setMethod("adjacencyWatch_th", "Sandbox", function(object) {
  return(object@adjacencyWatch_th)
})
#---adjacencyWatch_el_var------------------------------------------------------
#'adjacencyWatch_el_var (Get)
#'
#'Get adjacencyWatch_el_var
#'
#'@importFrom Root adjacencyWatch_el_var
#'@export

setMethod("adjacencyWatch_el_var", "Sandbox", function(object) {
  return(object@adjacencyWatch_el_var)
})
#---adjacencyWatch_fuel_var------------------------------------------------------
#'adjacencyWatch_fuel_var (Get)
#'
#'Get adjacencyWatch_fuel_var
#'
#'@importFrom Root adjacencyWatch_fuel_var
#'@export

setMethod("adjacencyWatch_fuel_var", "Sandbox", function(object) {
  return(object@adjacencyWatch_fuel_var)
})
#---adjacencyWatch_th_var------------------------------------------------------
#'adjacencyWatch_th_var (Get)
#'
#'Get adjacencyWatch_th_var
#'
#'@importFrom Root adjacencyWatch_th_var
#'@export

setMethod("adjacencyWatch_th_var", "Sandbox", function(object) {
  return(object@adjacencyWatch_th_var)
})
#---components----------------------------------------------------
#'components (Get)
#'
#'Get the components of the Sandbox, e.g. the sandbox represents a house
#'containg the components CHP, TS, and Bat.
#'
#'@importFrom Root components
#'@export
setMethod("components", "Sandbox", function(object) {
  return(object@components)
})


#---Overview-----------------------------------------------
#----getter----
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
# auto_adjacency
# SourceTemp1
# SourceTemp2
# SourceTemp_15min
# components
# componentsDefault
# coord
# COP
# COP_nominal
# COP1
# COP2
# CurveOffset
# CurvePitch
# eff_losses
# effMaxP_el_minus
# effMaxP_el_plus
# effMaxP_fuel_minus
# effMaxP_fuel_plus
# effMaxP_th_minus
# effMaxP_th_plus
# effMinP_el_minus
# effMinP_el_plus
# effMinP_fuel_minus
# effMinP_fuel_plus
# effMinP_th_minus
# effMinP_th_plus
# EnergyStart
# EnergyEnd
# info
# load_15min_el
# load_15min_fuel
# load_15min_th
# load_P_th_plus
# maxE_el
# maxE_th
# maxP_el_minus
# maxP_el_plus
# maxP_fuel_minus
# maxP_fuel_plus
# maxP_th_minus
# maxP_th_plus
# maxTemp
# minDowntime
# minE_el
# minP_el_minus
# minP_el_plus
# minP_fuel_minus
# minP_fuel_plus
# minP_th_minus
# minP_th_plus
# minRuntime
# minTemp
# name
# nominalP_th_plus
# percentageEnd
# percentageOverheating
# percentageStart
# price_maintenance
# timegrid
# ts_virtual
# variables
# volume

#----setter----
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
# SourceTemp1
# SourceTemp2
# SourceTemp_15min
# components
# COP
# COP1
# COP2
# eff_losses
# effMaxP_el_minus
# effMaxP_el_plus
# effMaxP_fuel_minus
# effMaxP_fuel_plus
# effMaxP_th_minus
# effMaxP_th_plus
# effMinP_el_minus
# effMinP_el_plus
# effMinP_fuel_minus
# effMinP_fuel_plus
# effMinP_th_minus
# effMinP_th_plus
# Energy_el
# Energy_fuel
# Energy_th
# finalcoordinates
# info
# load_15min_el
# load_15min_fuel
# load_15min_th
# load_P_th_plus
# maxE_el
# maxE_th
# maxP_el_minus
# maxP_el_plus
# maxP_fuel_minus
# maxP_fuel_plus
# maxP_th_minus
# maxP_th_plus
# maxTemp
# minDowntime
# minE_el
# minP_el_minus
# minP_el_plus
# minP_fuel_minus
# minP_fuel_plus
# minP_th_minus
# minP_th_plus
# minRuntime
# minTemp
# name
# nominalP_th_plus
# percentageEnd
# percentageOverheating
# percentageStart
# price_maintenance
# timegrid
# ts_virtual
# variables
# volume


#----Methods----
# as.Root
# component
# compoClass
# compoNames
# coordComp
# default
# E_el
# E_fuel
# E_th
# finalcoordinates
# lineloss
# load_el
# load_fuel
# load_th
# maxE_el
# maxE_fuel
# maxE_th
# maxP_el_minus
# maxP_el_plus
# maxP_fuel_minus
# maxP_fuel_plus
# maxP_th_minus
# maxP_th_plus
# minE_el
# minE_fuel
# minE_th
# minP_el_minus
# minP_el_plus
# minP_fuel_minus
# minP_fuel_plus
# minP_th_minus
# minP_th_plus
# Op
# profile_source
# P_el
# P_el_from
# P_el_minus
# P_el_plus
# P_el_to
# P_fuel
# P_fuel_from
# P_fuel_minus
# P_fuel_plus
# P_fuel_to
# P_th
# P_th_from
# P_th_minus
# P_th_plus
# P_th_to
# removeComp
# startE_th
# strdefault
# tau

#----Util----
# standardGenericVar
# Tminus
# Tend
# transform_loadprofile_sum
# transform_loadprofile_mean

#----LM.Methods----
# LM.Adja
# LM.Adja_Price
# LM.Adja_Lim_el
# LM.Adja_Lim_fuel
# LM.Adja_Lim_th
# LM.Adja_Wat_el
# LM.Adja_Wat_fuel
# LM.Adja_Wat_th
# LM.ComponentsDA
# LM.DA
# LM.effP_el
# LM.effP_fuel
# LM.effP_th
# LM.E_el
# LM.E_fuel
# LM.E_th
# LM.maxE_el
# LM.maxE_fuel
# LM.maxE_th
# LM.maxP_el_minus
# LM.maxP_el_minus_Op
# LM.maxP_el_plus
# LM.maxP_el_plus_Op
# LM.maxP_fuel_minus
# LM.maxP_fuel_minus_Op
# LM.maxP_fuel_plus
# LM.maxP_fuel_plus_Op
# LM.maxP_th_plus
# LM.maxP_th_plus_nominal
# LM.maxP_th_plus_Op
# LM.maxP_th_minus
# LM.maxP_th_minus_Op
# LM.minDowntime
# LM.minRuntime
# LM.minP_el_minus
# LM.minP_el_plus
# LM.minP_fuel_minus
# LM.minP_fuel_plus
# LM.minP_th_plus
# LM.minP_th_minus
# LM.minE_el
# LM.minE_fuel
# LM.minE_th
# LM.P_th_plus
# LM.Profile_el_minus
# LM.Profile_el_plus
# LM.Profile_fuel_minus
# LM.Profile_fuel_plus
# LM.Profile_th_minus
# LM.Profile_th_plus
# LM.Transform_elth_th
# LM.Transform_el_fuel
# LM.Transform_el_th
# LM.Transform_el_th_COP
# LM.Transform_fuel_el
# LM.Transform_fuel_th
# LM.Tunnel_el
# LM.Tunnel_fuel
# LM.Tunnel_th
#----------------------------------------------------------
#---getter-------------------------------------------------


#'@export
setGeneric("lifeTime",function(object)
  standardGeneric("lifeTime"))
#'@export
setGeneric("fullChargingCycles",function(object)
  standardGeneric("fullChargingCycles"))
#'@export
setGeneric("adjacency_el", function(object)
  standardGeneric("adjacency_el"))

#'@export
setGeneric("adjacency_fuel", function(object)
  standardGeneric("adjacency_fuel"))

#'@export
setGeneric("adjacency_th", function(object)
  standardGeneric("adjacency_th"))

#'@export
setGeneric("adjacencyEff_el", function(object)
  standardGeneric("adjacencyEff_el"))

#'@export
setGeneric("adjacencyEff_fuel", function(object)
  standardGeneric("adjacencyEff_fuel"))

#'@export
setGeneric("adjacencyEff_th", function(object)
  standardGeneric("adjacencyEff_th"))

#'@export
setGeneric("adjacencyPrice_el", function(object)
  standardGeneric("adjacencyPrice_el"))

#'@export
setGeneric("adjacencyPrice_fuel", function(object)
  standardGeneric("adjacencyPrice_fuel"))

#'@export
setGeneric("adjacencyPrice_th", function(object)
  standardGeneric("adjacencyPrice_th"))

#'@export
setGeneric("adjacencyWatch_el", function(object)
  standardGeneric("adjacencyWatch_el"))

#'@export
setGeneric("adjacencyWatch_fuel", function(object)
  standardGeneric("adjacencyWatch_fuel"))

#'@export
setGeneric("adjacencyWatch_th", function(object)
  standardGeneric("adjacencyWatch_th"))

#'@export
setGeneric("adjacencyWatch_el_var", function(object)
  standardGeneric("adjacencyWatch_el_var"))

#'@export
setGeneric("adjacencyWatch_fuel_var", function(object)
  standardGeneric("adjacencyWatch_fuel_var"))

#'@export
setGeneric("adjacencyWatch_th_var", function(object)
  standardGeneric("adjacencyWatch_th_var"))

#'@export
setGeneric("auto_adjacency", function(object)
  standardGeneric("auto_adjacency"))

#'@export
setGeneric("SourceTemp1", function(object)
  standardGeneric("SourceTemp1"))

#'@export
setGeneric("SourceTemp2", function(object)
  standardGeneric("SourceTemp2"))

#'@export
setGeneric("SourceTemp_15min", function(object)
  standardGeneric("SourceTemp_15min"))

#'@export
setGeneric("SourceTemp_abstract", function(object)
  standardGeneric("SourceTemp_abstract"))

#'@export
setGeneric("components", function(object)
  standardGeneric("components"))

#'@export
setGeneric("coord", function(object)
  standardGeneric("coord"))

#'@export
setGeneric("COP", function(object)
  standardGeneric("COP"))

#'@export
setGeneric("COP_nominal", function(object)
  standardGeneric("COP_nominal"))

#'@export
setGeneric("COP1", function(object)
  standardGeneric("COP1"))

#'@export
setGeneric("COP2", function(object)
  standardGeneric("COP2"))

#'@export
setGeneric("CurveOffset", function(object)
  standardGeneric("CurveOffset"))

#'@export
setGeneric("CurvePitch", function(object)
  standardGeneric("CurvePitch"))

#'@export
setGeneric("eff_losses", function(object)
  standardGeneric("eff_losses"))

#'@export
setGeneric("effMaxP_el_minus", function(object)
  standardGeneric("effMaxP_el_minus"))

#'@export
setGeneric("effMaxP_el_plus", function(object)
  standardGeneric("effMaxP_el_plus"))

#'@export
setGeneric("effMaxP_fuel_minus", function(object)
  standardGeneric("effMaxP_fuel_minus"))

#'@export
setGeneric("effMaxP_fuel_plus", function(object)
  standardGeneric("effMaxP_fuel_plus"))

#'@export
setGeneric("effMaxP_th_minus", function(object)
  standardGeneric("effMaxP_th_minus"))

#'@export
setGeneric("effMaxP_th_plus", function(object)
  standardGeneric("effMaxP_th_plus"))

#'@export
setGeneric("effMinP_el_minus", function(object)
  standardGeneric("effMinP_el_minus"))

#'@export
setGeneric("effMinP_el_plus", function(object)
  standardGeneric("effMinP_el_plus"))

#'@export
setGeneric("effMinP_fuel_minus", function(object)
  standardGeneric("effMinP_fuel_minus"))

#'@export
setGeneric("effMinP_fuel_plus", function(object)
  standardGeneric("effMinP_fuel_plus"))

#'@export
setGeneric("effMinP_th_minus", function(object)
  standardGeneric("effMinP_th_minus"))

#'@export
setGeneric("effMinP_th_plus", function(object)
  standardGeneric("effMinP_th_plus"))

#'@export
setGeneric("EnergyStart", function(object)
  standardGeneric("EnergyStart"))

#'@export
setGeneric("EnergyEnd", function(object)
  standardGeneric("EnergyEnd"))

# #'@export
# setGeneric("Energy_el", function(object)
#   standardGeneric("Energy_el"))

# #'@export
# setGeneric("Energy_th", function(object)
#   standardGeneric("Energy_th"))
#
# #'@export
# setGeneric("Energy_fuel", function(object)
#   standardGeneric("Energy_fuel"))

#'@export
setGeneric("timegrid", function(object)
  standardGeneric("timegrid"))

#'@export
setGeneric("info", function(object)
  standardGeneric("info"))

#'@export
setGeneric("load_15min_el", function(object)
  standardGeneric("load_15min_el"))

#'@export
setGeneric("load_abstract_el", function(object)
  standardGeneric("load_abstract_el"))

#'@export
setGeneric("load_15min_fuel", function(object)
  standardGeneric("load_15min_fuel"))

#'@export
setGeneric("load_abstract_fuel", function(object)
  standardGeneric("load_abstract_fuel"))

#'@export
setGeneric("load_15min_th", function(object)
  standardGeneric("load_15min_th"))

#'@export
setGeneric("load_abstract_th", function(object)
  standardGeneric("load_abstract_th"))

#'@export
setGeneric("load_P_th_plus", function(object)
  standardGeneric("load_P_th_plus"))
# #'@export
#setGeneric("maxP_el", function(object)
#  standardGeneric("maxP_el"))
# #'@export
#setGeneric("maxP_fuel", function(object)
#  standardGeneric("maxP_fuel"))
# #'@export
#setGeneric("maxP_th", function(object)
#  standardGeneric("maxP_th"))

#'@export
setGeneric("maxE_el", function(object)
  standardGeneric("maxE_el"))

#'@export
setGeneric("maxE_th", function(object)
  standardGeneric("maxE_th"))

#'@export
setGeneric("maxP_el_minus", function(object)
  standardGeneric("maxP_el_minus"))

#'@export
setGeneric("maxP_el_plus", function(object)
  standardGeneric("maxP_el_plus"))

#'@export
setGeneric("maxP_fuel_minus", function(object)
  standardGeneric("maxP_fuel_minus"))

#'@export
setGeneric("maxP_fuel_plus", function(object)
  standardGeneric("maxP_fuel_plus"))

#'@export
setGeneric("maxP_th_minus", function(object)
  standardGeneric("maxP_th_minus"))

#'@export
setGeneric("maxP_th_plus", function(object)
  standardGeneric("maxP_th_plus"))

#'@export
setGeneric("maxTemp", function(object)
  standardGeneric("maxTemp"))


#'@export
setGeneric("minDowntime", function(object)
  standardGeneric("minDowntime"))

#'@export
setGeneric("minE_el", function(object)
  standardGeneric("minE_el"))

#'@export
setGeneric("minP_el_minus", function(object)
  standardGeneric("minP_el_minus"))

#'@export
setGeneric("minP_el_plus", function(object)
  standardGeneric("minP_el_plus"))

#'@export
setGeneric("minP_fuel_minus", function(object)
  standardGeneric("minP_fuel_minus"))

#'@export
setGeneric("minP_fuel_plus", function(object)
  standardGeneric("minP_fuel_plus"))

#'@export
setGeneric("minP_th_minus", function(object)
  standardGeneric("minP_th_minus"))

#'@export
setGeneric("minP_th_plus", function(object)
  standardGeneric("minP_th_plus"))

#'@export
setGeneric("minRuntime", function(object)
  standardGeneric("minRuntime"))

#'@export
setGeneric("minTemp", function(object)
  standardGeneric("minTemp"))

#'@export
setGeneric("name", function(object)
  standardGeneric("name"))

#'@export
setGeneric("nominalP_th_plus", function(object)
  standardGeneric("nominalP_th_plus"))

#'@export
setGeneric("percentageEnd", function(object)
  standardGeneric("percentageEnd"))

#'@export
setGeneric("percentageOverheating", function(object)
  standardGeneric("percentageOverheating"))

#'@export
setGeneric("percentageStart", function(object)
  standardGeneric("percentageStart"))

#'@export
setGeneric("price_maintenance", function(object)
  standardGeneric("price_maintenance"))

#'@export
setGeneric("ts_virtual", function(object)
  standardGeneric("ts_virtual"))

#'@export
setGeneric("variables", function(object)
  standardGeneric("variables"))

#'@export
setGeneric("volume", function(object)
  standardGeneric("volume"))

#'@export
setGeneric("initial_state", function(object)
  standardGeneric("initial_state"))

#'@export
setGeneric("initial_stateEndurance", function(object)
  standardGeneric("initial_stateEndurance"))
  
#'@export
setGeneric("modulated", function(object)
  standardGeneric("modulated"))

#'@export
setGeneric("price_opChange", function(object)
  standardGeneric("price_opChange"))
#'@export
setGeneric("initial_hours", function(object)
  standardGeneric("initial_hours"))

#---setter -------------------------------------------------

#'@export
setGeneric("lifeTime<-",function(object,value)
  standardGeneric("lifeTime<-"))
#'@export
setGeneric("fullChargingCycles<-",function(object,value)
  standardGeneric("fullChargingCycles<-"))

#'@export
setGeneric("adjacency_el<-", function(object, value)
  standardGeneric("adjacency_el<-"))

#'@export
setGeneric("adjacency_fuel<-", function(object, value)
  standardGeneric("adjacency_fuel<-"))

#'@export
setGeneric("adjacency_th<-", function(object, value)
  standardGeneric("adjacency_th<-"))

#'@export
setGeneric("adjacencyEff_el<-", function(object, value)
  standardGeneric("adjacencyEff_el<-"))

#'@export
setGeneric("adjacencyEff_fuel<-", function(object, value)
  standardGeneric("adjacencyEff_fuel<-"))

#'@export
setGeneric("adjacencyEff_th<-", function(object, value)
  standardGeneric("adjacencyEff_th<-"))

#'@export
setGeneric("adjacencyPrice_el<-", function(object, value)
  standardGeneric("adjacencyPrice_el<-"))

#'@export
setGeneric("adjacencyPrice_fuel<-", function(object, value)
  standardGeneric("adjacencyPrice_fuel<-"))

#'@export
setGeneric("adjacencyPrice_th<-", function(object, value)
  standardGeneric("adjacencyPrice_th<-"))

#'@export
setGeneric("adjacencyWatch_el<-", function(object, value)
  standardGeneric("adjacencyWatch_el<-"))

#'@export
setGeneric("adjacencyWatch_fuel<-", function(object, value)
  standardGeneric("adjacencyWatch_fuel<-"))

#'@export
setGeneric("adjacencyWatch_th<-", function(object, value)
  standardGeneric("adjacencyWatch_th<-"))

#'@export
setGeneric("adjacencyWatch_el_var<-", function(object, value)
  standardGeneric("adjacencyWatch_el_var<-"))

#'@export
setGeneric("adjacencyWatch_fuel_var<-", function(object, value)
  standardGeneric("adjacencyWatch_fuel_var<-"))

#'@export
setGeneric("adjacencyWatch_th_var<-", function(object, value)
  standardGeneric("adjacencyWatch_th_var<-"))

#'@export
setGeneric("SourceTemp1<-", function(object, value)
  standardGeneric("SourceTemp1<-"))

#'@export
setGeneric("SourceTemp2<-", function(object, value)
  standardGeneric("SourceTemp2<-"))

#'@export
setGeneric("SourceTemp_15min<-", function(object, value)
  standardGeneric("SourceTemp_15min<-"))

#'@export
setGeneric("SourceTemp_abstract<-", function(object, value)
  standardGeneric("SourceTemp_abstract<-"))

#'@export
setGeneric("components<-", function(object, value)
  standardGeneric("components<-"))

#'@export
setGeneric("COP<-", function(object, value)
  standardGeneric("COP<-"))


#'@export
setGeneric("COP1<-", function(object, value)
  standardGeneric("COP1<-"))

#'@export
setGeneric("COP2<-", function(object, value)
  standardGeneric("COP2<-"))

#'@export
setGeneric("eff_losses<-", function(object, value)
  standardGeneric("eff_losses<-"))

#'@export
setGeneric("effMaxP_el_minus<-", function(object, value)
  standardGeneric("effMaxP_el_minus<-"))

#'@export
setGeneric("effMaxP_el_plus<-", function(object, value)
  standardGeneric("effMaxP_el_plus<-"))

#'@export
setGeneric("effMaxP_fuel_minus<-", function(object, value)
  standardGeneric("effMaxP_fuel_minus<-"))

#'@export
setGeneric("effMaxP_fuel_plus<-", function(object, value)
  standardGeneric("effMaxP_fuel_plus<-"))

#'@export
setGeneric("effMaxP_th_minus<-", function(object, value)
  standardGeneric("effMaxP_th_minus<-"))

#'@export
setGeneric("effMaxP_th_plus<-", function(object, value)
  standardGeneric("effMaxP_th_plus<-"))

#'@export
setGeneric("effMinP_el_minus<-", function(object, value)
  standardGeneric("effMinP_el_minus<-"))

#'@export
setGeneric("effMinP_el_plus<-", function(object, value)
  standardGeneric("effMinP_el_plus<-"))

#'@export
setGeneric("effMinP_fuel_minus<-", function(object, value)
  standardGeneric("effMinP_fuel_minus<-"))

#'@export
setGeneric("effMinP_fuel_plus<-", function(object, value)
  standardGeneric("effMinP_fuel_plus<-"))

#'@export
setGeneric("effMinP_th_minus<-", function(object, value)
  standardGeneric("effMinP_th_minus<-"))

#'@export
setGeneric("effMinP_th_plus<-", function(object, value)
  standardGeneric("effMinP_th_plus<-"))


# #'@export
# setGeneric("Energy_el<-", function(object, value)
#   standardGeneric("Energy_el<-"))
#
#
# #'@export
# setGeneric("Energy_fuel<-", function(object, value)
#   standardGeneric("Energy_fuel<-"))
#
# #'@export
# setGeneric("Energy_th<-", function(object, value)
#   standardGeneric("Energy_th<-"))



#'@export
setGeneric("info<-", function(object, value)
  standardGeneric("info<-"))

#'@export
setGeneric("load_15min_el<-", function(object, value)
  standardGeneric("load_15min_el<-"))

#'@export
setGeneric("load_abstract_el<-", function(object, value)
  standardGeneric("load_abstract_el<-"))

#'@export
setGeneric("load_15min_fuel<-", function(object, value)
  standardGeneric("load_15min_fuel<-"))

#'@export
setGeneric("load_abstract_fuel<-", function(object, value)
  standardGeneric("load_abstract_fuel<-"))

#'@export
setGeneric("load_15min_th<-", function(object, value)
  standardGeneric("load_15min_th<-"))

#'@export
setGeneric("load_abstract_th<-", function(object, value)
  standardGeneric("load_abstract_th<-"))

#'@export
setGeneric("load_P_th_plus<-", function(object, value)
  standardGeneric("load_P_th_plus<-"))

#'@export
setGeneric("maxE_el<-", function(object, value)
  standardGeneric("maxE_el<-"))

#'@export
setGeneric("maxE_th<-", function(object, value)
  standardGeneric("maxE_th<-"))

#'@export
setGeneric("maxE_fuel<-", function(object, value)
  standardGeneric("maxE_fuel<-"))

#'@export
setGeneric("cal<-", function(object, value)
  standardGeneric("cal<-"))

#'@export
setGeneric("maxP_el_minus<-", function(object, value)
  standardGeneric("maxP_el_minus<-"))

#'@export
setGeneric("maxP_el_plus<-", function(object, value)
  standardGeneric("maxP_el_plus<-"))

#'@export
setGeneric("maxP_fuel_minus<-", function(object, value)
  standardGeneric("maxP_fuel_minus<-"))

#'@export
setGeneric("maxP_fuel_plus<-", function(object, value)
  standardGeneric("maxP_fuel_plus<-"))

#'@export
setGeneric("maxP_th_minus<-", function(object, value)
  standardGeneric("maxP_th_minus<-"))

#'@export
setGeneric("maxP_th_plus<-", function(object, value)
  standardGeneric("maxP_th_plus<-"))

#'@export
setGeneric("maxTemp<-", function(object, value)
  standardGeneric("maxTemp<-"))


#'@export
setGeneric("minDowntime<-", function(object, value)
  standardGeneric("minDowntime<-"))

#'@export
setGeneric("minE_el<-", function(object, value)
  standardGeneric("minE_el<-"))

#'@export
setGeneric("minP_el_minus<-", function(object, value)
  standardGeneric("minP_el_minus<-"))

#'@export
setGeneric("minP_el_plus<-", function(object, value)
  standardGeneric("minP_el_plus<-"))

#'@export
setGeneric("minP_fuel_minus<-", function(object, value)
  standardGeneric("minP_fuel_minus<-"))

#'@export
setGeneric("minP_fuel_plus<-", function(object, value)
  standardGeneric("minP_fuel_plus<-"))

#'@export
setGeneric("minP_th_minus<-", function(object, value)
  standardGeneric("minP_th_minus<-"))

#'@export
setGeneric("minP_th_plus<-", function(object, value)
  standardGeneric("minP_th_plus<-"))

#'@export
setGeneric("minRuntime<-", function(object, value)
  standardGeneric("minRuntime<-"))

#'@export
setGeneric("minTemp<-", function(object, value)
  standardGeneric("minTemp<-"))

#'@export
setGeneric("name<-", function(object, value)
  standardGeneric("name<-"))

#'@export
setGeneric("percentageEnd<-", function(object, value)
  standardGeneric("percentageEnd<-"))

#'@export
setGeneric("percentageOverheating<-", function(object, value)
  standardGeneric("percentageOverheating<-"))

#'@export
setGeneric("percentageStart<-", function(object, value)
  standardGeneric("percentageStart<-"))


#'@export
setGeneric("price_maintenance<-", function(object, value)
  standardGeneric("price_maintenance<-"))

#'@export
setGeneric("timegrid<-", function(object, value)
  standardGeneric("timegrid<-"))

#'@export
setGeneric("ts_virtual<-", function(object, value)
  standardGeneric("ts_virtual<-"))

#'@export
setGeneric("variables<-", function(object, value)
  standardGeneric("variables<-"))

#'@export
setGeneric("volume<-", function(object, value)
  standardGeneric("volume<-"))

#'@export
setGeneric("initial_state<-", function(object, value)
  standardGeneric("initial_state<-"))
  
#'@export
setGeneric("initial_stateEndurance<-", function(object, value)
  standardGeneric("initial_stateEndurance<-"))

#'@export
setGeneric("modulated<-", function(object,value)
  standardGeneric("modulated<-"))
#'@export
setGeneric("price_opChange<-", function(object,value)
  standardGeneric("price_opChange<-"))
#'@export
setGeneric("initial_hours<-", function(object,value)
  standardGeneric("initial_hours<-"))

#---Methods------------------------------------------------
#'@export
setGeneric("as.Root", function(object)
  standardGeneric("as.Root"))

#'@export
setGeneric("component", function(object, value)
  standardGeneric("component"))

#'@export
setGeneric("compoClass",function(object) standardGeneric("compoClass"))

#'@export
setGeneric("compoNames", function(object)
  standardGeneric("compoNames"))

#'@export
setGeneric("coordComp", function(object)
  standardGeneric("coordComp"))

#'@export
setGeneric("default", function(object)
  standardGeneric("default"))

#'@export
setGeneric("E_el", function(object, value,steps)
  standardGenericVar("E_el"))

#'@export
setGeneric("E_fuel", function(object, value, steps)
  standardGenericVar("E_fuel"))

#'@export
setGeneric("E_th", function(object, value, steps)
  standardGenericVar("E_th"))

#'@export
setGeneric("finalcoordinates", function(object)
  standardGeneric("finalcoordinates"))

#'@export
setGeneric("lineloss", function(object)
  standardGeneric("lineloss"))

#'@export
setGeneric("load_el", function(object)
  standardGeneric("load_el"))

#'@export
setGeneric("load_fuel", function(object)
  standardGeneric("load_fuel"))

#'@export
setGeneric("load_th", function(object)
  standardGeneric("load_th"))

#'@export
setGeneric("maxE_el", function(object)
  standardGeneric("maxE_el"))

#'@export
setGeneric("maxE_fuel", function(object)
  standardGeneric("maxE_fuel"))

#'@export
setGeneric("maxE_th", function(object)
  standardGeneric("maxE_th"))

#'@export
setGeneric("maxP_el_minus", function(object)
  standardGeneric("maxP_el_minus"))

#'@export
setGeneric("maxP_el_plus", function(object)
  standardGeneric("maxP_el_plus"))

#'@export
setGeneric("maxP_fuel_minus", function(object)
  standardGeneric("maxP_fuel_minus"))

#'@export
setGeneric("maxP_fuel_plus", function(object)
  standardGeneric("maxP_fuel_plus"))

#'@export
setGeneric("maxP_th_minus", function(object)
  standardGeneric("maxP_th_minus"))

#'@export
setGeneric("maxP_th_plus", function(object)
  standardGeneric("maxP_th_plus"))

#'@export
setGeneric("minE_el", function(object)
  standardGeneric("minE_el"))

#'@export
setGeneric("minE_fuel", function(object)
  standardGeneric("minE_fuel"))

#'@export
setGeneric("minE_th", function(object)
  standardGeneric("minE_th"))

#'@export
setGeneric("minP_el_minus", function(object)
  standardGeneric("minP_el_minus"))

#'@export
setGeneric("minP_el_plus", function(object)
  standardGeneric("minP_el_plus"))

#'@export
setGeneric("minP_fuel_minus", function(object)
  standardGeneric("minP_fuel_minus"))

#'@export
setGeneric("minP_fuel_plus", function(object)
  standardGeneric("minP_fuel_plus"))

#'@export
setGeneric("minP_th_minus", function(object)
  standardGeneric("minP_th_minus"))

#'@export
setGeneric("minP_th_plus", function(object)
  standardGeneric("minP_th_plus"))


#'@export
setGeneric("cal",function(object)
  standardGeneric("cal"))

#'@export
setGeneric("Op", function(object, value, steps)
  standardGeneric("Op"))

#'@export
setGeneric("profile_source", function(object)
  standardGeneric("profile_source"))

#'@export
setGeneric("P_el", function(object, i , j)
  standardGenericVar("P_el"))

#'@export
setGeneric("P_el_from", function(object, i)
  standardGenericVar("P_el_from"))

#'@export
setGeneric("P_el_minus", function(object, value, steps)
  standardGenericVar("P_el_minus"))

#'@export
setGeneric("P_el_plus", function(object, value, steps)
  standardGenericVar("P_el_plus"))
#'@export
setGeneric("P_el_to", function(object, i)
  standardGenericVar("P_el_to"))

#'@export
setGeneric("P_fuel", function(object, i , j)
  standardGenericVar("P_fuel"))

#'@export
setGeneric("P_fuel_from", function(object, i)
  standardGenericVar("P_fuel_plus"))

#'@export
setGeneric("P_fuel_minus", function(object, value, steps)
  standardGenericVar("P_fuel_minus"))

#'@export
setGeneric("P_fuel_plus", function(object, value, steps)
  standardGenericVar("P_fuel_plus"))
#'@export
setGeneric("P_fuel_to", function(object, i)
  standardGenericVar("P_fuel_to"))

#'@export
setGeneric("P_th", function(object, i , j)
  standardGenericVar("P_th"))

#'@export
setGeneric("P_th_from", function(object, i)
  standardGenericVar("P_th_from"))

#'@export
setGeneric("P_th_minus", function(object, value, steps)
  standardGenericVar("P_th_minus"))

#'@export
setGeneric("P_th_plus", function(object, value, steps)
  standardGenericVar("P_th_plus"))

#'@export
setGeneric("P_th_to", function(object, i)
  standardGenericVar("P_th_to"))

#'@export
setGeneric("removeComp",function(object,value)
  standardGeneric("removeComp"))

# #'@export
# setGeneric("startE_th", function(object)
#   standardGeneric("startE_th"))

#'@export
setGeneric("strdefault", function(object)
  standardGeneric("strdefault"))



#'@export
setGeneric("tau", function(object)
  standardGeneric("tau"))

#---Util---------------------------------------------------
#'@export
setGeneric("standardGenericVar",function(x)
  standardGeneric("standardGenericVar"))

#'@export
setGeneric("Tminus",function(object)
  standardGeneric("Tminus"))

#'@export
setGeneric("Tend",function(object)
  standardGeneric("Tend"))

#'@export
setGeneric("transform_loadprofile_mean",function(Profil, timestep)
  standardGeneric("transform_loadprofile_mean"))

#'@export
setGeneric("transform_loadprofile_sum",function(Profil, timestep)
  standardGeneric("transform_loadprofile_sum"))


#---LM.Methods---------------------------------------------
#'@export
setGeneric("LM.Adja_Price", function(object)
  standardGeneric("LM.Adja_Price"))

#  # @export
#  setGeneric("LM.Adja_Con_el", function(object)
#    standardGeneric("LM.Adja_Con_el"))
#  # @export
#  setGeneric("LM.Adja_Con_fuel", function(object)
#    standardGeneric("LM.Adja_Con_fuel"))
#
#  # @export
#  setGeneric("LM.Adja_Con_th", function(object)
#    standardGeneric("LM.Adja_Con_th"))

#'@export
setGeneric("LM.Adja_Lim_el", function(object)
  standardGeneric("LM.Adja_Lim_el"))

#'@export
setGeneric("LM.Adja_Lim_fuel", function(object)
  standardGeneric("LM.Adja_Lim_fuel"))

#'@export
setGeneric("LM.Adja_Lim_th", function(object)
  standardGeneric("LM.Adja_Lim_th"))

#'@export
setGeneric("LM.Adja_Wat_el", function(object)
  standardGeneric("LM.Adja_Wat_el"))

#'@export
setGeneric("LM.Adja_Wat_fuel", function(object)
  standardGeneric("LM.Adja_Wat_fuel"))

#'@export
setGeneric("LM.Adja_Wat_th", function(object)
  standardGeneric("LM.Adja_Wat_th"))

#'@export
setGeneric("LM.ComponentsConstraints", function(object)
  standardGeneric("LM.ComponentsConstraints"))

#'@export
setGeneric("LM.Constraints", function(object)
  standardGeneric("LM.Constraints"))

#'@export
setGeneric("LM.effP_el", function(object)
  standardGeneric("LM.effP_el"))

#'@export
setGeneric("LM.effP_fuel", function(object)
  standardGeneric("LM.effP_fuel"))

#'@export
setGeneric("LM.effP_th", function(object)
  standardGeneric("LM.effP_th"))

#'@export
setGeneric("LM.Adja", function(object)
  standardGeneric("LM.Adja"))

#'@export
setGeneric("LM.E_el", function(object)
  standardGeneric("LM.E_el"))

#'@export
setGeneric("LM.E_fuel", function(object)
  standardGeneric("LM.E_fuel"))

#'@export
setGeneric("LM.E_th", function(object)
  standardGeneric("LM.E_th"))

#'@export
setGeneric("LM.maxE_el", function(object)
  standardGeneric("LM.maxE_el"))

#'@export
setGeneric("LM.maxE_fuel", function(object)
  standardGeneric("LM.maxE_fuel"))

#'@export
setGeneric("LM.maxE_th", function(object)
  standardGeneric("LM.maxE_th"))

# #'@export
# setGeneric("LM.maxP_el", function(object)
#   standardGeneric("LM.maxP_el"))

#'@export
setGeneric("LM.maxP_el_minus", function(object)
  standardGeneric("LM.maxP_el_minus"))

#'@export
setGeneric("LM.maxP_el_minus_Op", function(object)
  standardGeneric("LM.maxP_el_minus_Op"))

#'@export
setGeneric("LM.minP_el_minus_Op", function(object)
  standardGeneric("LM.minP_el_minus_Op"))

#'@export
setGeneric("LM.maxP_el_plus", function(object)
  standardGeneric("LM.maxP_el_plus"))

#'@export
setGeneric("LM.maxP_el_plus_Op", function(object)
  standardGeneric("LM.maxP_el_plus_Op"))

#'@export
setGeneric("LM.minP_el_plus_Op", function(object)
  standardGeneric("LM.minP_el_plus_Op"))

#'@export
setGeneric("LM.maxP_fuel", function(object)
standardGeneric("LM.maxP_fuel"))

#'@export
setGeneric("LM.maxP_fuel_minus", function(object)
  standardGeneric("LM.maxP_fuel_minus"))

#'@export
setGeneric("LM.maxP_fuel_minus_Op", function(object)
  standardGeneric("LM.maxP_fuel_minus_Op"))

#'@export
setGeneric("LM.minP_fuel_minus_Op", function(object)
  standardGeneric("LM.minP_fuel_minus_Op"))

#'@export
setGeneric("LM.maxP_fuel_plus", function(object)
  standardGeneric("LM.maxP_fuel_plus"))

#'@export
setGeneric("LM.maxP_fuel_plus_Op", function(object)
  standardGeneric("LM.maxP_fuel_plus_Op"))

#'@export
setGeneric("LM.minP_fuel_plus_Op", function(object)
  standardGeneric("LM.minP_fuel_plus_Op"))
# #'@export
# setGeneric("LM.maxP_th", function(object)
#   standardGeneric("LM.maxP_th"))

#'@export
setGeneric("LM.maxP_th_minus", function(object)
  standardGeneric("LM.maxP_th_minus"))

#'@export
setGeneric("LM.maxP_th_minus_Op", function(object)
  standardGeneric("LM.maxP_th_minus_Op"))

#'@export
setGeneric("LM.maxP_th_plus", function(object)
  standardGeneric("LM.maxP_th_plus"))

#'@export
setGeneric("LM.maxP_th_plus_nominal", function(object)
  standardGeneric("LM.maxP_th_plus_nominal"))

#'@export
setGeneric("LM.maxP_th_plus_Op", function(object)
  standardGeneric("LM.maxP_th_plus_Op"))

#'@export
setGeneric("LM.minDowntime", function(object)
  standardGeneric("LM.minDowntime"))

#'@export
setGeneric("LM.minRuntime", function(object)
  standardGeneric("LM.minRuntime"))

#'@export
setGeneric("LM.minP_th_plus_Op", function(object)
  standardGeneric("LM.minP_th_plus_Op"))

#'@export
setGeneric("LM.minP_th_minus_Op", function(object)
  standardGeneric("LM.minP_th_minus_Op"))

#'@export
setGeneric("LM.minP_el_minus", function(object)
  standardGeneric("LM.minP_el_minus"))

#'@export
setGeneric("LM.minP_el_plus", function(object)
  standardGeneric("LM.minP_el_plus"))

#'@export
setGeneric("LM.minP_fuel_minus", function(object)
  standardGeneric("LM.minP_fuel_minus"))

#'@export
setGeneric("LM.minP_fuel_plus", function(object)
  standardGeneric("LM.minP_fuel_plus"))

#'@export
setGeneric("LM.minP_th_minus", function(object)
  standardGeneric("LM.minP_th_minus"))

#'@export
setGeneric("LM.minP_th_plus", function(object)
  standardGeneric("LM.minP_th_plus"))

#'@export
setGeneric("LM.minE_el", function(object)
  standardGeneric("LM.minE_el"))

#'@export
setGeneric("LM.minE_fuel", function(object)
  standardGeneric("LM.minE_fuel"))

#'@export
setGeneric("LM.minE_th", function(object)
  standardGeneric("LM.minE_th"))

#'@export
setGeneric("LM.P_th_plus", function(object)
  standardGeneric("LM.P_th_plus"))

#'@export
setGeneric("LM.Profile", function(object)
  standardGeneric("LM.Profile"))

#'@export
setGeneric("LM.Profile_el_minus", function(object)
  standardGeneric("LM.Profile_el_minus"))

#'@export
setGeneric("LM.Profile_el_plus", function(object)
  standardGeneric("LM.Profile_el_plus"))

#'@export
setGeneric("LM.Profile_fuel_minus", function(object)
  standardGeneric("LM.Profile_fuel_minus"))

#'@export
setGeneric("LM.Profile_fuel_plus", function(object)
  standardGeneric("LM.Profile_fuel_plus"))

#'@export
setGeneric("LM.Profile_th_minus", function(object)
  standardGeneric("LM.Profile_th_minus"))

#'@export
setGeneric("LM.Profile_th_plus", function(object)
  standardGeneric("LM.Profile_th_plus"))

#'@export
setGeneric("LM.Transform_elth_th", function(object)
  standardGeneric("LM.Transform_elth_th"))

#'@export
setGeneric("LM.Transform_el_fuel", function(object)
  standardGeneric("LM.Transform_el_fuel"))

#'@export
setGeneric("LM.Transform_el_th", function(object)
  standardGeneric("LM.Transform_el_th"))

#'@export
setGeneric("LM.Transform_el_th_COP", function(object)
  standardGeneric("LM.Transform_el_th_COP"))



#'@export
setGeneric("LM.Transform_fuel_el", function(object)
  standardGeneric("LM.Transform_fuel_el"))

#'@export
setGeneric("LM.Transform_fuel_th", function(object)
  standardGeneric("LM.Transform_fuel_th"))

#'@export
setGeneric("LM.Tunnel_el", function(object)
  standardGeneric("LM.Tunnel_el"))

#'@export
setGeneric("LM.Tunnel_fuel", function(object)
  standardGeneric("LM.Tunnel_fuel"))

#'@export
setGeneric("LM.Tunnel_th", function(object)
  standardGeneric("LM.Tunnel_th"))

#'@export
setGeneric("LM.Flexa",function(object,schedule,direction,which_time,cheap,flexMax,cost_flex)
  standardGeneric("LM.Flexa"))


#'@export
setGeneric("LM.FlexaIndividual", function(object,schedule,direction,which_time,cheap,flexMax)
  standardGeneric("LM.FlexaIndividual"))


#'@export
setGeneric("LM.Schedule", function(object,schedule,which_time,call,cheap)
  standardGeneric("LM.Schedule"))

#'@export
setGeneric("LM.ScheduleIndividual", function(object,schedule,which_time,cheap)
  standardGeneric("LM.ScheduleIndividual"))


#'@export
setGeneric("LM.Shave", function(object,which_time,compo_names,cost_shave)
  standardGeneric("LM.Shave"))

#'@export
setGeneric("FlexMax", function(object)
  standardGeneric("FlexMax"))


#'FlexMax
#'
#'@export
#'
setMethod("FlexMax", signature(object = "Root"), function(object) {
print("FlexMax!")})

#'@export
setGeneric("LM.maxP_el", function(object)
  standardGeneric("LM.maxP_el"))


#'@export
setGeneric("LM.maxP_th", function(object)
  standardGeneric("LM.maxP_th"))

#'@export
setGeneric("LM.maxP_fuel", function(object)
  standardGeneric("LM.maxP_fuel"))

#'@export
setGeneric("LM.maxChargingCycles", function(object)
  standardGeneric("LM.maxChargingCycles"))

#'@export
setGeneric("LM.modulated", function(object)
  standardGeneric("LM.modulated"))

#'@export
setGeneric("LM.Schedule_new", function(object, schedules)
  standardGeneric("LM.Schedule_new"))
#'@export
setGeneric("LM.Schedule_slack", function(object, schedules)
  standardGeneric("LM.Schedule_slack"))

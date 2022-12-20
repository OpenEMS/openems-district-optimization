library(sandraModelling)
library(FS)

fs = new.FS()
sb = new.Sandbox()
volume(fs)<- 4000
volume(fs)
cal(fs)<- 0.006
cal(fs)
maxE_fuel(fs)
percentageStart(fs)<-0.5
percentageStart(fs)
EnergyStart(fs)
percentageEnd(fs)<-0.5
maxP_fuel_minus(fs)<--1
maxP_fuel_plus(fs)<--1
eff_losses(fs)<-1
timegrid(fs)<- c(60,60)
timegrid(fs)
variables(fs)
fs@coord<-coordiii(fs)
LM.E_fuel(fs)
LM.maxE_fuel(fs)
LM.maxP_fuel(fs)
LM.maxP_fuel_plus(fs)
LM.maxP_fuel_minus(fs)
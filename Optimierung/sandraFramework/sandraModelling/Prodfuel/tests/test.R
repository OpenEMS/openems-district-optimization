library(sandraModelling)
pf = New("Prodfuel")

load_15min_fuel(pf)<-c(10,20,30)
print(load_15min_fuel(pf))
load_15min_fuel(pf)<-numeric()
load_abstract_fuel(pf)<-c(10,20,30)
print(load_abstract_fuel(pf))





To install a small version of Sandy in R and use the optimization of a simple model just do the following steps:

1. Install R
2. Install in R the packages "devtools" and "Rgplk" (if there occur errors see the comments of file "Installation_Script_Sandra_Small.R")
3. Install all packages of Sandy (run file "Installation_Script_Sandra_Small.R")


To test or run an optimization run the python script "Main_Runtime.py". More information about that:
1. The data for demand and production is presented in the file "LoadProfiles.CSV"
2. The energy system is shown in the file "EnergyModel.jpg"
3. The code for optimizing the energy system is presented in the file "Function_Optimization.R". It is a function, in which the model is defined. Only the demand/production, the initial values for chp and thermal storage as well as additional values for the optimization grid has to be passed.
4. In the file "Main_Runtime.py" a simulation loop is presented, which calls the optimization function in "Function_Optimization.R" for an hourly optimization. Therefore, data from the "LoadProfile.CSV" of 24 hours is passed to it and an optimized scheduel is returned.
5. A simple plot for the heat sector of the results is presented in the file "Main_Runtime.py"



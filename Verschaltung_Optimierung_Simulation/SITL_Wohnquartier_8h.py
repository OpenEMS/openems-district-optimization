import os
import numpy as N                                       
import rpy2.robjects.numpy2ri
import matplotlib.pyplot as plt 
import pandas as pd
import rpy2.robjects as robjects
from fmpy import simulate_fmu
from fmpy import *
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter
from datetime import datetime

now = datetime.now()

current_time = now.strftime("%H:%M:%S")
print("Current Time =", current_time)

pandas2ri.activate()
rpy2.robjects.numpy2ri.activate()

def my_callback(time, recorder):
    print(time/3600)	
    return True


os.chdir('C:/.../Verschaltung_Optimierung_Simulation') # Pfad anpassen
#robjects.r('source("SITL_MAGGIE_8h.R")')
#robjects.r('source("SITL_MAGGIE_8h_CO2optimiert.R")')
robjects.r('source("SITL_MAGGIE_8h_Preisoptimiert.R")')
r_func_Optimierung = robjects.globalenv['Optimierung']


fmi_version='2.0'
fmi_type='CoSimulation'
#fmi_type='ModelExchange'
output=['u__boiler_fuel__1__thermal_power_plus__1',
        'u__chp__1__electric_power_plus__1',
        'u__heatpump__1__thermal_power_plus__1',
        'u__demand_el__1__electrical_power_minus__1',
        'u__demand_th__1__thermal_power_minus__1',
        'u__demand_th__2__thermal_power_minus__1',
        'u__demand_th__3__thermal_power_minus__1',
        'u__demand_th__4__thermal_power_minus__1', 
        'u__demand_th__5__thermal_power_minus__1',
        'u__demand_th__6__thermal_power_minus__1',
        'u__demand_th__7__thermal_power_minus__1',
        'u__demand_th__8__thermal_power_minus__1',
        'u__demand_th__9__thermal_power_minus__1',
        'u__demand_th__10__thermal_power_minus__1',
        'u__demand_th__11__thermal_power_minus__1',
        'u__demand_th__12__thermal_power_minus__1',
        'u__pv__1__electric_power_plus__1',
        'u__pv__2__electric_power_plus__1',
        'u__pv__3__electric_power_plus__1',
        'u__source_th__1__tp__1',
        'y__boiler_fuel__1__status__1',
        'y__boiler_fuel__1__thermal_power_plus__1',
        'y__chp__1__electric_power_plus__1',
        'y__chp__1__status__1',
        'y__chp__1__thermal_power_plus__1',
        'y__CO2__der',
        'y__CO2__sum',
        'y__cost__der',
        'y__cost__sum',
        'y__demand_el__1__electric_power_minus__1',
        'y__demand_th__1__thermal_power_minus__1',
        'y__demand_th__1__tp__1',
        'y__demand_th__1__tp__2',
        'y__demand_th__10__thermal_power_minus__1',
        'y__demand_th__10__tp__1',
        'y__demand_th__10__tp__2',
        'y__demand_th__11__thermal_power_minus__1',
        'y__demand_th__11__tp__1',
        'y__demand_th__11__tp__2',
        'y__demand_th__12__thermal_power_minus__1',
        'y__demand_th__12__tp__1',
        'y__demand_th__12__tp__2',
        'y__demand_th__2__thermal_power_minus__1',
        'y__demand_th__2__tp__1',
        'y__demand_th__2__tp__2',
        'y__demand_th__3__thermal_power_minus__1',
        'y__demand_th__3__tp__1',
        'y__demand_th__3__tp__2',
        'y__demand_th__4__thermal_power_minus__1',
        'y__demand_th__4__tp__1',
        'y__demand_th__4__tp__2',
        'y__demand_th__5__thermal_power_minus__1',
        'y__demand_th__5__tp__1',
        'y__demand_th__5__tp__2',
        'y__demand_th__6__thermal_power_minus__1',
        'y__demand_th__6__tp__1',
        'y__demand_th__6__tp__2',
        'y__demand_th__7__thermal_power_minus__1',
        'y__demand_th__7__tp__1',
        'y__demand_th__7__tp__2',
        'y__demand_th__8__thermal_power_minus__1',
        'y__demand_th__8__tp__1',
        'y__demand_th__8__tp__2',
        'y__demand_th__9__thermal_power_minus__1',
        'y__demand_th__9__tp__1',
        'y__demand_th__9__tp__2',
        'y__heatpump__1__electric_power_minus__1',
        'y__heatpump__1__status__1',
        'y__heatpump__1__thermal_power_plus__1',
        'y__pv__1__electric_power_plus__1',
        'y__pv__2__electric_power_plus__1',
        'y__pv__3__electric_power_plus__1',
        'y__storage_th__1__tp__1',
        'y__storage_th__1__tp__2',
        'y__storage_th__1__tp__3',
        'y__storage_th__1__tp__4',
        'y__storage_th__2__tp__1',
        'y__storage_th__2__tp__2',
        'y__storage_th__2__tp__3',
        'y__storage_th__2__tp__4',
        'y__storage_th__3__tp__1',
        'y__storage_th__3__tp__2',
        'y__storage_th__3__tp__3',
        'y__storage_th__3__tp__4',
        'y__storage_th__4__tp__1',
        'y__storage_th__4__tp__2',
        'y__storage_th__4__tp__3',
        'y__storage_th__4__tp__4',
        'y__storage_th__5__tp__1',
        'y__storage_th__5__tp__2',
        'y__storage_th__5__tp__3',
        'y__storage_th__5__tp__4',
        'y__storage_th__6__tp__1',
        'y__storage_th__6__tp__2',
        'y__storage_th__6__tp__3',
        'y__storage_th__6__tp__4',
        'y__storage_th__7__tp__1',
        'y__storage_th__7__tp__2',
        'y__storage_th__7__tp__3',
        'y__storage_th__7__tp__4',
        'y__storage_th__7__tp__5',
        'y__storage_th__8__tp__1',
        'y__storage_th__8__tp__2']
solver='CVode'
events=True
fmi_logging=False
show_plot=True

fmu = 'FMU_mit_synchroner_Pufferbeladung.fmu'
# fmu = 'C:/.../Verschaltung_Optimierung_Simulation/FMU_ohne_synchroner_Pufferbeladung.fmu' # Pfad anpassen
dump(fmu)


# Lastprofile

Lastprofile_MAGGIE = pd.read_csv('Lastprofile_MAGGIE_kW_berichtigt.csv', decimal=',', sep=';')  

Sekunden = Lastprofile_MAGGIE['Sekunden']         
PV_Sued = Lastprofile_MAGGIE['PV_Sued_kW'] 
PV_Ost = Lastprofile_MAGGIE['PV_Ost_kW'] 
PV_West = Lastprofile_MAGGIE['PV_West_kW']
Strombedarf = Lastprofile_MAGGIE['Strombedarf_kW'] 
TWWH1 = Lastprofile_MAGGIE['TWWH1_kW']
TWWH2 = Lastprofile_MAGGIE['TWWH2_kW']
TWWH3 = Lastprofile_MAGGIE['TWWH3_kW']
TWWH4 = Lastprofile_MAGGIE['TWWH4_kW']
TWWH5 = Lastprofile_MAGGIE['TWWH5_kW']
TWWH6 = Lastprofile_MAGGIE['TWWH6_kW']
HeizwaermeH1 = Lastprofile_MAGGIE['HeizwaermeH1_kW']
HeizwaermeH2 = Lastprofile_MAGGIE['HeizwaermeH2_kW']
HeizwaermeH3 = Lastprofile_MAGGIE['HeizwaermeH3_kW']
HeizwaermeH4 = Lastprofile_MAGGIE['HeizwaermeH4_kW']
HeizwaermeH5 = Lastprofile_MAGGIE['HeizwaermeH5_kW']
HeizwaermeH6 = Lastprofile_MAGGIE['HeizwaermeH6_kW']
Spotpreis = Lastprofile_MAGGIE['u_Spot_EuroMWh'] * 0   # Spot deaktiviert
Temperatur = Lastprofile_MAGGIE['Temperatur_degC']
    
#----Temperaturniveaus festlegen
NT = 55 # Vorlauf Niedertemperatur
HT = 80 # Vorlauf Hochtemperatur
Base = 30 # Rücklauf

####
### Modell-Parameter festlegen
#
init__controll_value = 0  # 0 = wärmegeführt, 1 = nur extern, 2 = Mischung
init__controll_temp__heatpump = 55
init__controll_temp__boiler_fuel = 50
init__control_chp_on = 72
init__control_chp_off = 57
init__controll_chp_offset = 3
init__control_heatpump_on = 42
init__control_heatpump_off = 37
init__controll_heatpump_offset = 3
init__control_stroage_th__1_6_on = 54 # entspricht 60 grad, da neue Hysterese auf gemittelte Temperatur
init__control_stroage_th__1_6_off = 41 # entspricht 45 grad, da neue Hysterese auf gemittelte Temperatur


init__chp__1__status__1 = 'false'
init__heatpump__1__status__1 = 'false'
init__storage_th__1__tp__1 = 35
init__storage_th__1__tp__2 = 45
init__storage_th__1__tp__3 = 65
init__storage_th__1__tp__4 = 75
init__storage_th__2__tp__1 = 35
init__storage_th__2__tp__2 = 45
init__storage_th__2__tp__3 = 65
init__storage_th__2__tp__4 = 75
init__storage_th__3__tp__1 = 35
init__storage_th__3__tp__2 = 45
init__storage_th__3__tp__3 = 65
init__storage_th__3__tp__4 = 75
init__storage_th__4__tp__1 = 35
init__storage_th__4__tp__2 = 45
init__storage_th__4__tp__3 = 65
init__storage_th__4__tp__4 = 75
init__storage_th__5__tp__1 = 35
init__storage_th__5__tp__2 = 45
init__storage_th__5__tp__3 = 65
init__storage_th__5__tp__4 = 75
init__storage_th__6__tp__1 = 35
init__storage_th__6__tp__2 = 45
init__storage_th__6__tp__3 = 65
init__storage_th__6__tp__4 = 75
init__storage_th__7__tp__1 = 50
init__storage_th__7__tp__2 = 60
init__storage_th__7__tp__3 = 65
init__storage_th__7__tp__4 = 70
init__storage_th__7__tp__5 = 80
init__storage_th__8__tp__1 = 30
init__storage_th__8__tp__2 = 50


#######################
TS1_E_Init = 0.5
TS2_E_Init = 0.5
TS3_E_Init = 0.5
TS4_E_Init = 0.5
TS5_E_Init = 0.5
TS6_E_Init = 0.5
TS7_E_Init = 0.5
TS8_E_Init = 0.5
BHKW_initialstate = 0

storage_th__1__E_Init = (560 * TS1_E_Init * (HT-Base) / 860) / (560 * (HT-Base) / 860)
storage_th__2__E_Init = (560 * TS2_E_Init * (HT-Base) / 860) / (560 * (HT-Base) / 860)
storage_th__3__E_Init = (560 * TS3_E_Init * (HT-Base) / 860) / (560 * (HT-Base) / 860) 
storage_th__4__E_Init = (560 * TS4_E_Init * (HT-Base) / 860) / (560 * (HT-Base) / 860) 
storage_th__5__E_Init = (560 * TS5_E_Init * (HT-Base) / 860) / (560 * (HT-Base) / 860) 
storage_th__6__E_Init = (560 * TS6_E_Init * (HT-Base) / 860) / (560 * (HT-Base) / 860) 
storage_th__7__E_Init = (1000 * TS7_E_Init * (HT-Base) / 860) / (1000 * (HT-Base) / 860) 
storage_th__8__E_Init = (500 * TS8_E_Init * (NT-Base) / 860) / (500 * (NT-Base) / 860) 


Cost_Init = 0
CO2_Init = 0

#### Simulationsdauer und Starttag anpassen

Tag_Start = 302 #326 #310 #145
Tag = Tag_Start
Anzahl_Tage = 8
Tage_Ende = Tag_Start+Anzahl_Tage

    
if 1 == 0: # If-Abfrage anpassen, um Bereich zu aktivieren/deaktivieren

    # Jahressimulation ohne Optimierung

    Stunde_Beginn = 24*Tag-24
    Stunde_Ende = 24*(Tag+Anzahl_Tage-1)

    Start_time = Stunde_Beginn*3600
    End_time = Stunde_Ende*3600 #31536000
    Stepnumber = 24*Anzahl_Tage*4

    t = Sekunden[Stunde_Beginn*4:Stunde_Ende*4]
    u__demand_el__1__electrical_power_minus__1 = Strombedarf[Stunde_Beginn*4:Stunde_Ende*4]
    u__demand_th__1__thermal_power_minus__1 = TWWH1[Stunde_Beginn*4:Stunde_Ende*4]*0.035*0.45*0.5
    u__demand_th__2__thermal_power_minus__1 = TWWH2[Stunde_Beginn*4:Stunde_Ende*4]*0.035*0.45*0.5
    u__demand_th__3__thermal_power_minus__1 = TWWH3[Stunde_Beginn*4:Stunde_Ende*4]*0.035*0.45*0.5
    u__demand_th__4__thermal_power_minus__1 = TWWH4[Stunde_Beginn*4:Stunde_Ende*4]*0.035*0.45*0.5
    u__demand_th__5__thermal_power_minus__1 = TWWH5[Stunde_Beginn*4:Stunde_Ende*4]*0.035*0.45*0.5
    u__demand_th__6__thermal_power_minus__1 = TWWH6[Stunde_Beginn*4:Stunde_Ende*4]*0.035*0.45*0.5
    u__demand_th__7__thermal_power_minus__1 = HeizwaermeH1[Stunde_Beginn*4:Stunde_Ende*4]
    u__demand_th__8__thermal_power_minus__1 = HeizwaermeH2[Stunde_Beginn*4:Stunde_Ende*4]
    u__demand_th__9__thermal_power_minus__1 = HeizwaermeH3[Stunde_Beginn*4:Stunde_Ende*4]
    u__demand_th__10__thermal_power_minus__1 = HeizwaermeH4[Stunde_Beginn*4:Stunde_Ende*4]
    u__demand_th__11__thermal_power_minus__1 = HeizwaermeH5[Stunde_Beginn*4:Stunde_Ende*4]
    u__demand_th__12__thermal_power_minus__1 = HeizwaermeH6[Stunde_Beginn*4:Stunde_Ende*4]
    u__pv__1__electric_power_plus__1 = PV_Ost[Stunde_Beginn*4:Stunde_Ende*4]
    u__pv__2__electric_power_plus__1 = PV_Sued[Stunde_Beginn*4:Stunde_Ende*4]
    u__pv__3__electric_power_plus__1 = PV_West[Stunde_Beginn*4:Stunde_Ende*4]
    u__source_th__1__tp__1 = Temperatur[Stunde_Beginn*4:Stunde_Ende*4]
    u__spot = Spotpreis[Stunde_Beginn*4:Stunde_Ende*4] * 0
    
    u__boiler_fuel__1__thermal_power_plus__1 = N.linspace(0,0,Stepnumber)
    u__chp__1__electric_power_plus__1 = N.linspace(0,0,Stepnumber)
    u__heatpump__1__thermal_power_plus__1 = N.linspace(0,0,Stepnumber)
    
    u__cost = N.linspace(Cost_Init,Cost_Init,Stepnumber)
    u__CO2 = N.linspace(CO2_Init,CO2_Init,Stepnumber)

    # Umwandeln auf Leistungen (Keine Interpolation)
    a = N.delete(N.sort(t.append(t+1)),(1))
    a = N.append(a,a[len(a)-1]+900-1)
    t = N.array(a) 
    u__demand_el__1__electrical_power_minus__1 = N.kron(u__demand_el__1__electrical_power_minus__1,(1,1))
    u__demand_th__1__thermal_power_minus__1 = N.kron(u__demand_th__1__thermal_power_minus__1,(1,1))
    u__demand_th__2__thermal_power_minus__1 = N.kron(u__demand_th__2__thermal_power_minus__1,(1,1))
    u__demand_th__3__thermal_power_minus__1 = N.kron(u__demand_th__3__thermal_power_minus__1,(1,1))
    u__demand_th__4__thermal_power_minus__1 = N.kron(u__demand_th__4__thermal_power_minus__1,(1,1))
    u__demand_th__5__thermal_power_minus__1 = N.kron(u__demand_th__5__thermal_power_minus__1,(1,1))
    u__demand_th__6__thermal_power_minus__1 = N.kron(u__demand_th__6__thermal_power_minus__1,(1,1))
    u__demand_th__7__thermal_power_minus__1 = N.kron(u__demand_th__7__thermal_power_minus__1,(1,1))
    u__demand_th__8__thermal_power_minus__1 = N.kron(u__demand_th__8__thermal_power_minus__1,(1,1))
    u__demand_th__9__thermal_power_minus__1 = N.kron(u__demand_th__9__thermal_power_minus__1,(1,1))
    u__demand_th__10__thermal_power_minus__1 = N.kron(u__demand_th__10__thermal_power_minus__1,(1,1))
    u__demand_th__11__thermal_power_minus__1 = N.kron(u__demand_th__11__thermal_power_minus__1,(1,1))
    u__demand_th__12__thermal_power_minus__1 = N.kron(u__demand_th__12__thermal_power_minus__1,(1,1))
    u__pv__1__electric_power_plus__1 = N.kron(u__pv__1__electric_power_plus__1,(1,1))
    u__pv__2__electric_power_plus__1 = N.kron(u__pv__2__electric_power_plus__1,(1,1))
    u__pv__3__electric_power_plus__1 = N.kron(u__pv__3__electric_power_plus__1,(1,1))
    u__source_th__1__tp__1 = N.kron(u__source_th__1__tp__1,(1,1))
    u__spot = N.kron(u__spot,(1,1))
    u__cost = N.kron(u__cost,(1,1))
    u__CO2 = N.kron(u__CO2,(1,1))
    
    u__boiler_fuel__1__thermal_power_plus__1 = N.kron(u__boiler_fuel__1__thermal_power_plus__1,(1,1))
    u__chp__1__electric_power_plus__1 = N.kron(u__chp__1__electric_power_plus__1,(1,1))
    u__heatpump__1__thermal_power_plus__1 = N.kron(u__heatpump__1__thermal_power_plus__1,(1,1))


    tttt = {'time':t,
            'u__boiler_fuel__1__thermal_power_plus__1':u__boiler_fuel__1__thermal_power_plus__1,
            'u__chp__1__electric_power_plus__1':u__chp__1__electric_power_plus__1,
            'u__heatpump__1__thermal_power_plus__1':u__heatpump__1__thermal_power_plus__1,
            'u__demand_el__1__electrical_power_minus__1':u__demand_el__1__electrical_power_minus__1,
            'u__demand_th__1__thermal_power_minus__1':u__demand_th__1__thermal_power_minus__1,
            'u__demand_th__2__thermal_power_minus__1':u__demand_th__2__thermal_power_minus__1,
            'u__demand_th__3__thermal_power_minus__1':u__demand_th__3__thermal_power_minus__1,
            'u__demand_th__4__thermal_power_minus__1':u__demand_th__4__thermal_power_minus__1,
            'u__demand_th__5__thermal_power_minus__1':u__demand_th__5__thermal_power_minus__1,
            'u__demand_th__6__thermal_power_minus__1':u__demand_th__6__thermal_power_minus__1,
            'u__demand_th__7__thermal_power_minus__1':u__demand_th__7__thermal_power_minus__1,
            'u__demand_th__8__thermal_power_minus__1':u__demand_th__8__thermal_power_minus__1,
            'u__demand_th__9__thermal_power_minus__1':u__demand_th__9__thermal_power_minus__1,
            'u__demand_th__10__thermal_power_minus__1':u__demand_th__10__thermal_power_minus__1,
            'u__demand_th__11__thermal_power_minus__1':u__demand_th__11__thermal_power_minus__1,
            'u__demand_th__12__thermal_power_minus__1':u__demand_th__12__thermal_power_minus__1,
            'u__pv__1__electric_power_plus__1':u__pv__1__electric_power_plus__1,
            'u__pv__2__electric_power_plus__1':u__pv__2__electric_power_plus__1,
            'u__pv__3__electric_power_plus__1':u__pv__3__electric_power_plus__1,
            'u__source_th__1__tp__1':u__source_th__1__tp__1,
            'u__misc__1__spot__1':u__spot,
            'u__misc__1__cost__1':u__cost,
            'u__misc__1__CO2__1':u__CO2}

    Profile = pd.DataFrame(tttt,
                            columns=['time',
                                     'u__boiler_fuel__1__thermal_power_plus__1',
                                     'u__chp__1__electric_power_plus__1',
                                     'u__heatpump__1__thermal_power_plus__1',
                                     'u__demand_el__1__electrical_power_minus__1',
                                     'u__demand_th__1__thermal_power_minus__1',
                                     'u__demand_th__2__thermal_power_minus__1',
                                     'u__demand_th__3__thermal_power_minus__1',
                                     'u__demand_th__4__thermal_power_minus__1',
                                     'u__demand_th__5__thermal_power_minus__1',
                                     'u__demand_th__6__thermal_power_minus__1',
                                     'u__demand_th__7__thermal_power_minus__1',
                                     'u__demand_th__8__thermal_power_minus__1',
                                     'u__demand_th__9__thermal_power_minus__1',
                                     'u__demand_th__10__thermal_power_minus__1',
                                     'u__demand_th__11__thermal_power_minus__1',
                                     'u__demand_th__12__thermal_power_minus__1',
                                     'u__pv__1__electric_power_plus__1',
                                     'u__pv__2__electric_power_plus__1',
                                     'u__pv__3__electric_power_plus__1',
                                     'u__source_th__1__tp__1',
                                     'u__misc__1__spot__1',
                                     'u__misc__1__cost__1',
                                     'u__misc__1__CO2__1'])

    Profile.to_csv (r'export_dataframe1.csv',     
                index = False, header=True)
    
    input_array = N.genfromtxt('export_dataframe1.csv', delimiter=',', names=True)                   # Lastprofile werden wieder geladen
        
    print('---------------------------------------------------')
    print('Start wärmgeführte Simulation')
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    print("Current Time =", current_time)
    print('---------------------------------------------------')

    res = simulate_fmu(
                filename=fmu,
                validate=False,
                fmi_type=fmi_type,
                start_time = Start_time,
                stop_time = End_time,
                solver=solver,
                step_size=900,
                relative_tolerance=1e-6,
                output_interval=900,
                record_events=events,
                start_values={#'Start_time':Start_time,
                              #'End_time':End_time,
                              'init__controll_value':0,
                              'init__controll_temp__heatpump':init__controll_temp__heatpump,
                              'init__controll_temp__boiler_fuel':init__controll_temp__boiler_fuel,
                              'init__control_chp_on':init__control_chp_on,
                              'init__control_chp_off':init__control_chp_off,
                              'init__controll_chp_offset':init__controll_chp_offset,
                              'init__control_heatpump_on':init__control_heatpump_on,
                              'init__control_heatpump_off':init__control_heatpump_off,
                              'init__controll_heatpump_offset':init__controll_heatpump_offset,
                              'init__control_stroage_th__1_6_on':init__control_stroage_th__1_6_on,
                              'init__control_stroage_th__1_6_off':init__control_stroage_th__1_6_off,
                              'init__chp__1__status__1':init__chp__1__status__1,
                              'init__heatpump__1__status__1':init__heatpump__1__status__1,
                              'init__storage_th__1__tp__1':init__storage_th__1__tp__1,
                              'init__storage_th__1__tp__2':init__storage_th__1__tp__2,
                              'init__storage_th__1__tp__3':init__storage_th__1__tp__3,
                              'init__storage_th__1__tp__4':init__storage_th__1__tp__4,
                              'init__storage_th__2__tp__1':init__storage_th__2__tp__1,
                              'init__storage_th__2__tp__2':init__storage_th__2__tp__2,
                              'init__storage_th__2__tp__3':init__storage_th__2__tp__3,
                              'init__storage_th__2__tp__4':init__storage_th__2__tp__4,
                              'init__storage_th__3__tp__1':init__storage_th__3__tp__1,
                              'init__storage_th__3__tp__2':init__storage_th__3__tp__2,
                              'init__storage_th__3__tp__3':init__storage_th__3__tp__3,
                              'init__storage_th__3__tp__4':init__storage_th__3__tp__4,
                              'init__storage_th__4__tp__1':init__storage_th__4__tp__1,
                              'init__storage_th__4__tp__2':init__storage_th__4__tp__2,
                              'init__storage_th__4__tp__3':init__storage_th__4__tp__3,
                              'init__storage_th__4__tp__4':init__storage_th__4__tp__4,
                              'init__storage_th__5__tp__1':init__storage_th__5__tp__1,
                              'init__storage_th__5__tp__2':init__storage_th__5__tp__2,
                              'init__storage_th__5__tp__3':init__storage_th__5__tp__3,
                              'init__storage_th__5__tp__4':init__storage_th__5__tp__4,
                              'init__storage_th__6__tp__1':init__storage_th__6__tp__1,
                              'init__storage_th__6__tp__2':init__storage_th__6__tp__2,
                              'init__storage_th__6__tp__3':init__storage_th__6__tp__3,
                              'init__storage_th__6__tp__4':init__storage_th__6__tp__4,
                              'init__storage_th__7__tp__1':init__storage_th__7__tp__1,
                              'init__storage_th__7__tp__2':init__storage_th__7__tp__2,
                              'init__storage_th__7__tp__3':init__storage_th__7__tp__3,
                              'init__storage_th__7__tp__4':init__storage_th__7__tp__4,
                              'init__storage_th__7__tp__5':init__storage_th__7__tp__5,
                              'init__storage_th__8__tp__1':init__storage_th__8__tp__1,
                              'init__storage_th__8__tp__2':init__storage_th__8__tp__2},
                input=input_array,
                output=output,
                fmi_call_logger=lambda s: print('[FMI] ' + s) if fmi_logging else None,
                step_finished=my_callback)
                #timeout=10*60) # in sconds
    
    mtr_res2 = N.array([res['time'],                                            # 0
                        res['u__boiler_fuel__1__thermal_power_plus__1'],        # 1
                        res['u__chp__1__electric_power_plus__1'],               # 2
                        res['u__demand_el__1__electrical_power_minus__1'],      # 3
                        res['u__demand_th__1__thermal_power_minus__1'],         # 4
                        res['u__demand_th__10__thermal_power_minus__1'],        # 5
                        res['u__demand_th__11__thermal_power_minus__1'],        # 6
                        res['u__demand_th__12__thermal_power_minus__1'],        # 7
                        res['u__demand_th__2__thermal_power_minus__1'],         # 8
                        res['u__demand_th__3__thermal_power_minus__1'],         # 9
                        res['u__demand_th__4__thermal_power_minus__1'],         # 10
                        res['u__demand_th__5__thermal_power_minus__1'],         # 11
                        res['u__demand_th__6__thermal_power_minus__1'],         # 12
                        res['u__demand_th__7__thermal_power_minus__1'],         # 13
                        res['u__demand_th__8__thermal_power_minus__1'],         # 14
                        res['u__demand_th__9__thermal_power_minus__1'],         # 15
                        res['u__heatpump__1__thermal_power_plus__1'],           # 16
                        res['u__pv__1__electric_power_plus__1'],                # 17
                        res['u__pv__2__electric_power_plus__1'],                # 18
                        res['u__pv__3__electric_power_plus__1'],                # 19
                        res['u__source_th__1__tp__1'],                          # 20
                        res['y__boiler_fuel__1__status__1'],                    # 21
                        res['y__boiler_fuel__1__thermal_power_plus__1'],        # 22
                        res['y__chp__1__electric_power_plus__1'],               # 23
                        res['y__chp__1__status__1'],                            # 24
                        res['y__chp__1__thermal_power_plus__1'],                # 25
                        res['y__CO2__der'],                                     # 26
                        res['y__CO2__sum'],                                     # 27
                        res['y__cost__der'],                                    # 28
                        res['y__cost__sum'],                                    # 29
                        res['y__demand_el__1__electric_power_minus__1'],        # 30
                        res['y__demand_th__1__thermal_power_minus__1'],         # 31
                        res['y__demand_th__1__tp__1'],                          # 32
                        res['y__demand_th__1__tp__2'],                          # 33
                        res['y__demand_th__10__thermal_power_minus__1'],        # 34    
                        res['y__demand_th__10__tp__1'],                         # 35
                        res['y__demand_th__10__tp__2'],                         # 36
                        res['y__demand_th__11__thermal_power_minus__1'],        # 37
                        res['y__demand_th__11__tp__1'],                         # 38
                        res['y__demand_th__11__tp__2'],                         # 39
                        res['y__demand_th__12__thermal_power_minus__1'],        # 40
                        res['y__demand_th__12__tp__1'],                         # 41
                        res['y__demand_th__12__tp__2'],                         # 42
                        res['y__demand_th__2__thermal_power_minus__1'],         # 43
                        res['y__demand_th__2__tp__1'],                          # 44
                        res['y__demand_th__2__tp__2'],                          # 45
                        res['y__demand_th__3__thermal_power_minus__1'],         # 46
                        res['y__demand_th__3__tp__1'],                          # 47
                        res['y__demand_th__3__tp__2'],                          # 48
                        res['y__demand_th__4__thermal_power_minus__1'],         # 49
                        res['y__demand_th__4__tp__1'],                          # 50
                        res['y__demand_th__4__tp__2'],                          # 51
                        res['y__demand_th__5__thermal_power_minus__1'],         # 52
                        res['y__demand_th__5__tp__1'],                          # 53
                        res['y__demand_th__5__tp__2'],                          # 54
                        res['y__demand_th__6__thermal_power_minus__1'],         # 55
                        res['y__demand_th__6__tp__1'],                          # 56
                        res['y__demand_th__6__tp__2'],                          # 57
                        res['y__demand_th__7__thermal_power_minus__1'],         # 58
                        res['y__demand_th__7__tp__1'],                          # 59
                        res['y__demand_th__7__tp__2'],                          # 60
                        res['y__demand_th__8__thermal_power_minus__1'],         # 61
                        res['y__demand_th__8__tp__1'],                          # 62
                        res['y__demand_th__8__tp__2'],                          # 63
                        res['y__demand_th__9__thermal_power_minus__1'],         # 64
                        res['y__demand_th__9__tp__1'],                          # 65
                        res['y__demand_th__9__tp__2'],                          # 66
                        res['y__heatpump__1__electric_power_minus__1'],         # 67
                        res['y__heatpump__1__status__1'],                       # 68
                        res['y__heatpump__1__thermal_power_plus__1'],           # 69
                        res['y__pv__1__electric_power_plus__1'],                # 70
                        res['y__pv__2__electric_power_plus__1'],                # 71
                        res['y__pv__3__electric_power_plus__1'],                # 72
                        res['y__storage_th__1__tp__1'],                         # 73
                        res['y__storage_th__1__tp__2'],                         # 74
                        res['y__storage_th__1__tp__3'],                         # 75
                        res['y__storage_th__1__tp__4'],                         # 76
                        res['y__storage_th__2__tp__1'],                         # 77
                        res['y__storage_th__2__tp__2'],                         # 78
                        res['y__storage_th__2__tp__3'],                         # 79
                        res['y__storage_th__2__tp__4'],                         # 80
                        res['y__storage_th__3__tp__1'],                         # 81
                        res['y__storage_th__3__tp__2'],                         # 82
                        res['y__storage_th__3__tp__3'],                         # 83
                        res['y__storage_th__3__tp__4'],                         # 84
                        res['y__storage_th__4__tp__1'],                         # 85
                        res['y__storage_th__4__tp__2'],                         # 86
                        res['y__storage_th__4__tp__3'],                         # 87
                        res['y__storage_th__4__tp__4'],                         # 88
                        res['y__storage_th__5__tp__1'],                         # 89
                        res['y__storage_th__5__tp__2'],                         # 90
                        res['y__storage_th__5__tp__3'],                         # 91
                        res['y__storage_th__5__tp__4'],                         # 92
                        res['y__storage_th__6__tp__1'],                         # 93
                        res['y__storage_th__6__tp__2'],                         # 94
                        res['y__storage_th__6__tp__3'],                         # 95
                        res['y__storage_th__6__tp__4'],                         # 96
                        res['y__storage_th__7__tp__1'],                         # 97
                        res['y__storage_th__7__tp__2'],                         # 98
                        res['y__storage_th__7__tp__3'],                         # 99
                        res['y__storage_th__7__tp__4'],                         # 100
                        res['y__storage_th__7__tp__5'],                         # 101
                        res['y__storage_th__8__tp__1'],                         # 102
                        res['y__storage_th__8__tp__2']])                        # 103
    
    
    res_8760_waerme = mtr_res2
    res_8760_waerme_transponiert = N.transpose(res_8760_waerme)
    #
    N.savetxt("res_8760_waerme.csv", res_8760_waerme, delimiter=",")                          
    N.savetxt("res_8760_waerme_transponiert.csv", res_8760_waerme_transponiert, delimiter=",") 
    
    t_plot_waerme = res_8760_waerme[0,:]

    print('---------------------------------------------------')
    print('Wärmgeführte Simulation abgeschlossen')
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    print("Current Time =", current_time)

print('---------------------------------------------------')
print('Start Optimierungsschleife')
print('---------------------------------------------------')



# Jahressimulation mit Optimierung

SimTimeEnd = 0
Readjustments = 0


if 1 == 1: # If-Abfrage anpassen, um Bereich zu aktivieren/deaktivieren
    while Tag<Tage_Ende: 
        # Beginn while-Schleife
        Teiltag = 1
        while Teiltag <= 3:
            
            if Teiltag==1:
                Stunde_Beginn = 24*Tag-24
                Stunde_Ende = 24*Tag-16
            elif Teiltag==2:
                Stunde_Beginn = 24*Tag-24+8
                Stunde_Ende = 24*Tag-8
            elif Teiltag==3:
                Stunde_Beginn = 24*Tag-24+16
                Stunde_Ende = 24*Tag
            
            Start_time = (Stunde_Beginn)*3600
            End_time = Stunde_Ende*3600 #15*96*60*365
            Stepnumber = Stunde_Ende-Stunde_Beginn
               
            # Lastprofile initialisiseren auf 15 min raster (1 Tag gesamt)
            t = Sekunden[Stunde_Beginn*4:Stunde_Ende*4]
            #u__boiler_fuel__1__thermal_power_plus__1 = N.linspace(0,0,Stepnumber)
            #u__chp__1__electric_power_plus__1 = N.linspace(0,0,Stepnumber)
            #u__heatpump__1__thermal_power_plus__1 = N.linspace(0,0,Stepnumber)
            u__demand_el__1__electrical_power_minus__1 = Strombedarf[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__1__thermal_power_minus__1 = TWWH1[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__2__thermal_power_minus__1 = TWWH2[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__3__thermal_power_minus__1 = TWWH3[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__4__thermal_power_minus__1 = TWWH4[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__5__thermal_power_minus__1 = TWWH5[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__6__thermal_power_minus__1 = TWWH6[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__7__thermal_power_minus__1 = HeizwaermeH1[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__8__thermal_power_minus__1 = HeizwaermeH2[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__9__thermal_power_minus__1 = HeizwaermeH3[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__10__thermal_power_minus__1 = HeizwaermeH4[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__11__thermal_power_minus__1 = HeizwaermeH5[Stunde_Beginn*4:Stunde_Ende*4]
            u__demand_th__12__thermal_power_minus__1 = HeizwaermeH6[Stunde_Beginn*4:Stunde_Ende*4]
            u__pv__1__electric_power_plus__1 = PV_Ost[Stunde_Beginn*4:Stunde_Ende*4]
            u__pv__2__electric_power_plus__1 = PV_Sued[Stunde_Beginn*4:Stunde_Ende*4]
            u__pv__3__electric_power_plus__1 = PV_West[Stunde_Beginn*4:Stunde_Ende*4]
            u__source_th__1__tp__1 = Temperatur[Stunde_Beginn*4:Stunde_Ende*4]
            u__spot = Spotpreis[Stunde_Beginn*4:Stunde_Ende*4]
            u__cost = N.linspace(Cost_Init,Cost_Init,Stepnumber)
            u__CO2 = N.linspace(CO2_Init,CO2_Init,Stepnumber)
            
            Scale_NT = 1.5#1.2
            Scale_HT = 1.2#0.9
            # Aufteilen der Wärmebedarfe in Hochtemperatur und Niedertemperatur
            TWWH1_NT = (NT-Base)/(HT-Base)*TWWH1[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            TWWH2_NT = (NT-Base)/(HT-Base)*TWWH2[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            TWWH3_NT = (NT-Base)/(HT-Base)*TWWH3[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            TWWH4_NT = (NT-Base)/(HT-Base)*TWWH4[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            TWWH5_NT = (NT-Base)/(HT-Base)*TWWH5[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            TWWH6_NT = (NT-Base)/(HT-Base)*TWWH6[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            HeizwaermeH1_NT = (NT-Base)/(HT-Base)*HeizwaermeH1[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            HeizwaermeH2_NT = (NT-Base)/(HT-Base)*HeizwaermeH2[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            HeizwaermeH3_NT = (NT-Base)/(HT-Base)*HeizwaermeH3[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            HeizwaermeH4_NT = (NT-Base)/(HT-Base)*HeizwaermeH4[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            HeizwaermeH5_NT = (NT-Base)/(HT-Base)*HeizwaermeH5[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            HeizwaermeH6_NT = (NT-Base)/(HT-Base)*HeizwaermeH6[Stunde_Beginn*4:Stunde_Ende*4] * Scale_NT
            TWWH1_HT = (HT-NT)/(HT-Base)*TWWH1[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            TWWH2_HT = (HT-NT)/(HT-Base)*TWWH2[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            TWWH3_HT = (HT-NT)/(HT-Base)*TWWH3[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            TWWH4_HT = (HT-NT)/(HT-Base)*TWWH4[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            TWWH5_HT = (HT-NT)/(HT-Base)*TWWH5[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            TWWH6_HT = (HT-NT)/(HT-Base)*TWWH6[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            HeizwaermeH1_HT = (HT-NT)/(HT-Base)*HeizwaermeH1[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            HeizwaermeH2_HT = (HT-NT)/(HT-Base)*HeizwaermeH2[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            HeizwaermeH3_HT = (HT-NT)/(HT-Base)*HeizwaermeH3[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            HeizwaermeH4_HT = (HT-NT)/(HT-Base)*HeizwaermeH4[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            HeizwaermeH5_HT = (HT-NT)/(HT-Base)*HeizwaermeH5[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            HeizwaermeH6_HT = (HT-NT)/(HT-Base)*HeizwaermeH6[Stunde_Beginn*4:Stunde_Ende*4] * Scale_HT
            Strom_ges = Strombedarf[Stunde_Beginn*4:Stunde_Ende*4] 
            Strom_PV_O = PV_Ost[Stunde_Beginn*4:Stunde_Ende*4] 
            Strom_PV_S = PV_Sued[Stunde_Beginn*4:Stunde_Ende*4] 
            Strom_PV_W = PV_West[Stunde_Beginn*4:Stunde_Ende*4]
            Temp_Luft = Temperatur[Stunde_Beginn*4:Stunde_Ende*4]
            Spot = Spotpreis[Stunde_Beginn*4:Stunde_Ende*4]
            
            # Zeitgitter anpassen
            #Strom_ges = N.kron(Strom_ges,(1,1,1))
            #TWWH1_NT = N.kron(TWWH1_NT,(1,1,1))
            #TWWH2_NT = N.kron(TWWH2_NT,(1,1,1))
            #TWWH3_NT = N.kron(TWWH3_NT,(1,1,1))
            #TWWH4_NT = N.kron(TWWH4_NT,(1,1,1))
            #TWWH5_NT = N.kron(TWWH5_NT,(1,1,1))
            #TWWH6_NT = N.kron(TWWH6_NT,(1,1,1))
            #HeizwaermeH1_NT = N.kron(HeizwaermeH1_NT,(1,1,1))
            #HeizwaermeH2_NT = N.kron(HeizwaermeH2_NT,(1,1,1))
            #HeizwaermeH3_NT = N.kron(HeizwaermeH3_NT,(1,1,1))
            #HeizwaermeH4_NT = N.kron(HeizwaermeH4_NT,(1,1,1))
            #HeizwaermeH5_NT = N.kron(HeizwaermeH5_NT,(1,1,1))
            #HeizwaermeH6_NT = N.kron(HeizwaermeH6_NT,(1,1,1))
            #TWWH1_HT = N.kron(TWWH1_HT,(1,1,1))
            #TWWH2_HT = N.kron(TWWH2_HT,(1,1,1))
            #TWWH3_HT = N.kron(TWWH3_HT,(1,1,1))
            #TWWH4_HT = N.kron(TWWH4_HT,(1,1,1))
            #TWWH5_HT = N.kron(TWWH5_HT,(1,1,1))
            #TWWH6_HT = N.kron(TWWH6_HT,(1,1,1))
            #HeizwaermeH1_HT = N.kron(HeizwaermeH1_HT,(1,1,1))
            #HeizwaermeH2_HT = N.kron(HeizwaermeH2_HT,(1,1,1))
            #HeizwaermeH3_HT = N.kron(HeizwaermeH3_HT,(1,1,1))
            #HeizwaermeH4_HT = N.kron(HeizwaermeH4_HT,(1,1,1))
            #HeizwaermeH5_HT = N.kron(HeizwaermeH5_HT,(1,1,1))
            #HeizwaermeH6_HT = N.kron(HeizwaermeH6_HT,(1,1,1))
            #PV_Ost = N.kron(Strom_PV_O,(1,1,1))
            #PV_Sued = N.kron(Strom_PV_S,(1,1,1))
            #PV_West = N.kron(Strom_PV_W,(1,1,1))
            #Temperatur = N.kron(Temp_Luft,(1,1,1))
            #Spotpreis = N.kron(Spot,(1,1,1))
            
            # Umwandeln auf R-Format
            Strombedarf_R = robjects.FloatVector(Strom_ges)
            TWWH1_NT_R = robjects.FloatVector(TWWH1_NT)
            TWWH2_NT_R = robjects.FloatVector(TWWH2_NT)
            TWWH3_NT_R = robjects.FloatVector(TWWH3_NT)
            TWWH4_NT_R = robjects.FloatVector(TWWH4_NT)
            TWWH5_NT_R = robjects.FloatVector(TWWH5_NT)
            TWWH6_NT_R = robjects.FloatVector(TWWH6_NT)
            HeizwaermeH1_NT_R = robjects.FloatVector(HeizwaermeH1_NT)
            HeizwaermeH2_NT_R = robjects.FloatVector(HeizwaermeH2_NT)
            HeizwaermeH3_NT_R = robjects.FloatVector(HeizwaermeH3_NT)
            HeizwaermeH4_NT_R = robjects.FloatVector(HeizwaermeH4_NT)
            HeizwaermeH5_NT_R = robjects.FloatVector(HeizwaermeH5_NT)
            HeizwaermeH6_NT_R = robjects.FloatVector(HeizwaermeH6_NT)
            TWWH1_HT_R = robjects.FloatVector(TWWH1_HT)
            TWWH2_HT_R = robjects.FloatVector(TWWH2_HT)
            TWWH3_HT_R = robjects.FloatVector(TWWH3_HT)
            TWWH4_HT_R = robjects.FloatVector(TWWH4_HT)
            TWWH5_HT_R = robjects.FloatVector(TWWH5_HT)
            TWWH6_HT_R = robjects.FloatVector(TWWH6_HT)
            HeizwaermeH1_HT_R = robjects.FloatVector(HeizwaermeH1_HT)
            HeizwaermeH2_HT_R = robjects.FloatVector(HeizwaermeH2_HT)
            HeizwaermeH3_HT_R = robjects.FloatVector(HeizwaermeH3_HT)
            HeizwaermeH4_HT_R = robjects.FloatVector(HeizwaermeH4_HT)
            HeizwaermeH5_HT_R = robjects.FloatVector(HeizwaermeH5_HT)
            HeizwaermeH6_HT_R = robjects.FloatVector(HeizwaermeH6_HT)
            PV_Ost_R = robjects.FloatVector(Strom_PV_O)
            PV_Sued_R = robjects.FloatVector(Strom_PV_S)
            PV_West_R = robjects.FloatVector(Strom_PV_W)
            Temperatur_R = robjects.FloatVector(Temp_Luft)
            Spotpreis_R = robjects.FloatVector(Spot)
            
            Anzahl_Zeitschritte = Stepnumber*4*3
            Zeitintervall = 5 # in Minuten
            
            Solver = 1 # 1=Rglpk, 2=lpsolve
        
            print('Start Optimierung')
        
            matrix_R = r_func_Optimierung(PV_Ost_R,  
                                          PV_Sued_R,
                                          PV_West_R,
                                          Strombedarf_R,
                                          Spotpreis_R,
                                          TWWH1_NT_R,
                                          TWWH2_NT_R,
                                          TWWH3_NT_R,
                                          TWWH4_NT_R,
                                          TWWH5_NT_R,
                                          TWWH6_NT_R,
                                          TWWH1_HT_R,
                                          TWWH2_HT_R,
                                          TWWH3_HT_R,
                                          TWWH4_HT_R,
                                          TWWH5_HT_R,
                                          TWWH6_HT_R,
                                          HeizwaermeH1_NT_R,
                                          HeizwaermeH2_NT_R,
                                          HeizwaermeH3_NT_R,
                                          HeizwaermeH4_NT_R,
                                          HeizwaermeH5_NT_R,
                                          HeizwaermeH6_NT_R,
                                          HeizwaermeH1_HT_R,
                                          HeizwaermeH2_HT_R,
                                          HeizwaermeH3_HT_R,
                                          HeizwaermeH4_HT_R,
                                          HeizwaermeH5_HT_R,
                                          HeizwaermeH6_HT_R,
                                          Temperatur_R,
                                          Anzahl_Zeitschritte, Zeitintervall, Solver, 
                                          TS1_E_Init,
                                          TS2_E_Init,
                                          TS3_E_Init,
                                          TS4_E_Init,
                                          TS5_E_Init,
                                          TS6_E_Init,
                                          TS7_E_Init,
                                          TS8_E_Init,
                                          BHKW_initialstate)
            
            with localconverter(robjects.default_converter + pandas2ri.converter):
                matrix = robjects.conversion.rpy2py(matrix_R)
            
            u__chp__1__electric_power_plus__1 = matrix['Pel_CHP1_PubGel1'] + matrix['Pel_CHP1_HePu1'] + matrix['Pel_CHP1_Demel1'] 
            u__boiler_fuel__1__thermal_power_plus__1 = matrix['Pfuel_PubGfuel1_GaBo1']*0.9
            u__heatpump__1__thermal_power_plus__1 = matrix['Pth_HePu1_TS8']
            u__cost = N.linspace(Cost_Init,Cost_Init,Stepnumber*4) 
            u__CO2 = N.linspace(CO2_Init,CO2_Init,Stepnumber*4)
            
            # Anpassen auf 15 min Werte
            #u__chp__1__electric_power_plus__1 = N.kron(u__chp__1__electric_power_plus__1,(1,1)) 
            #u__boiler_fuel__1__thermal_power_plus__1 = N.kron(u__boiler_fuel__1__thermal_power_plus__1,(1,1,1,1)) 
            #u__heatpump__1__thermal_power_plus__1 = N.kron(u__heatpump__1__thermal_power_plus__1,(1,1,1,1)) 
            #u__cost = N.linspace(Cost_Init,Cost_Init,Stepnumber)
            
               
            # Umwandeln auf Leistungen (Keine Interpolation)
           
            a = N.sort(t)
            aa = N.linspace(a[0],a[len(a)-1]+900,(len(a)*3)-2+3)
            aaa = pd.Series(aa)
            aaaa = N.delete(N.sort(aaa.append(aaa+1)),(1))
            aaaaa = N.delete(aaaa,(len(aaaa)-1))
            t = N.array(aaaaa) 
            
            u__chp__1__electric_power_plus__1 = N.kron(u__chp__1__electric_power_plus__1,(1,1))       # Kronecker-Produkt = jedes Element A wird mit jedem Element in B multipliziert (Riesenmatrix)
            u__boiler_fuel__1__thermal_power_plus__1 = N.kron(u__boiler_fuel__1__thermal_power_plus__1,(1,1))         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__heatpump__1__thermal_power_plus__1 = N.kron(u__heatpump__1__thermal_power_plus__1,(1,1))         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_el__1__electrical_power_minus__1 = N.kron(u__demand_el__1__electrical_power_minus__1,(1,1,1,1,1,1))       # Kronecker-Produkt = jedes Element A wird mit jedem Element in B multipliziert (Riesenmatrix)
            u__demand_th__1__thermal_power_minus__1 = N.kron(u__demand_th__1__thermal_power_minus__1,(1,1,1,1,1,1))*0.035*0.45*0.5         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__2__thermal_power_minus__1 = N.kron(u__demand_th__2__thermal_power_minus__1,(1,1,1,1,1,1))*0.035*0.45*0.5         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__3__thermal_power_minus__1 = N.kron(u__demand_th__3__thermal_power_minus__1,(1,1,1,1,1,1))*0.035*0.45*0.5         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__4__thermal_power_minus__1 = N.kron(u__demand_th__4__thermal_power_minus__1,(1,1,1,1,1,1))*0.035*0.45*0.5         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__5__thermal_power_minus__1 = N.kron(u__demand_th__5__thermal_power_minus__1,(1,1,1,1,1,1))*0.035*0.45*0.5         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__6__thermal_power_minus__1 = N.kron(u__demand_th__6__thermal_power_minus__1,(1,1,1,1,1,1))*0.035*0.45*0.5         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__7__thermal_power_minus__1 = N.kron(u__demand_th__7__thermal_power_minus__1,(1,1,1,1,1,1))         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__8__thermal_power_minus__1 = N.kron(u__demand_th__8__thermal_power_minus__1,(1,1,1,1,1,1))         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__9__thermal_power_minus__1 = N.kron(u__demand_th__9__thermal_power_minus__1,(1,1,1,1,1,1))         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__10__thermal_power_minus__1 = N.kron(u__demand_th__10__thermal_power_minus__1,(1,1,1,1,1,1))         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__11__thermal_power_minus__1 = N.kron(u__demand_th__11__thermal_power_minus__1,(1,1,1,1,1,1))         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__demand_th__12__thermal_power_minus__1 = N.kron(u__demand_th__12__thermal_power_minus__1,(1,1,1,1,1,1))         # Frage: Wozu genau ist diese Riesenmatrix nützlich? (Alle möglichen Optionen für Optimierung erzeugen und Probieren?)
            u__pv__1__electric_power_plus__1 = N.kron(u__pv__1__electric_power_plus__1,(1,1,1,1,1,1))                       # Inputs anpassen pv,el,th
            u__pv__2__electric_power_plus__1 = N.kron(u__pv__2__electric_power_plus__1,(1,1,1,1,1,1))                       # Inputs anpassen pv,el,th
            u__pv__3__electric_power_plus__1 = N.kron(u__pv__3__electric_power_plus__1,(1,1,1,1,1,1))                       # Inputs anpassen pv,el,th
            u__source_th__1__tp__1 = N.kron(u__source_th__1__tp__1,(1,1,1,1,1,1))   
            u__spot = N.kron(u__spot,(1,1,1,1,1,1))   
            u__cost = N.kron(u__cost,(1,1,1,1,1,1)) 
            u__CO2 = N.kron(u__CO2,(1,1,1,1,1,1)) 
                
            plt.plot(u__heatpump__1__thermal_power_plus__1, '--', label='WP', linewidth=2)
            plt.plot(u__chp__1__electric_power_plus__1, '--', label='BHKW', linewidth=2)
            plt.plot(u__boiler_fuel__1__thermal_power_plus__1, '--', label='Gasboiler', linewidth=2)
            plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
            plt.grid(True)
            plt.title('Ausgabe_Optimierung')
            plt.ylabel('Leistung [kW]') 
            plt.show()
            
            
            tttt = {'time':t,
                    'u__boiler_fuel__1__thermal_power_plus__1':u__boiler_fuel__1__thermal_power_plus__1,
                    'u__chp__1__electric_power_plus__1':u__chp__1__electric_power_plus__1,
                    'u__heatpump__1__thermal_power_plus__1':u__heatpump__1__thermal_power_plus__1,
                    'u__demand_el__1__electrical_power_minus__1':u__demand_el__1__electrical_power_minus__1,
                    'u__demand_th__1__thermal_power_minus__1':u__demand_th__1__thermal_power_minus__1,
                    'u__demand_th__2__thermal_power_minus__1':u__demand_th__2__thermal_power_minus__1,
                    'u__demand_th__3__thermal_power_minus__1':u__demand_th__3__thermal_power_minus__1,
                    'u__demand_th__4__thermal_power_minus__1':u__demand_th__4__thermal_power_minus__1,
                    'u__demand_th__5__thermal_power_minus__1':u__demand_th__5__thermal_power_minus__1,
                    'u__demand_th__6__thermal_power_minus__1':u__demand_th__6__thermal_power_minus__1,
                    'u__demand_th__7__thermal_power_minus__1':u__demand_th__7__thermal_power_minus__1,
                    'u__demand_th__8__thermal_power_minus__1':u__demand_th__8__thermal_power_minus__1,
                    'u__demand_th__9__thermal_power_minus__1':u__demand_th__9__thermal_power_minus__1,
                    'u__demand_th__10__thermal_power_minus__1':u__demand_th__10__thermal_power_minus__1,
                    'u__demand_th__11__thermal_power_minus__1':u__demand_th__11__thermal_power_minus__1,
                    'u__demand_th__12__thermal_power_minus__1':u__demand_th__12__thermal_power_minus__1,
                    'u__pv__1__electric_power_plus__1':u__pv__1__electric_power_plus__1,
                    'u__pv__2__electric_power_plus__1':u__pv__2__electric_power_plus__1,
                    'u__pv__3__electric_power_plus__1':u__pv__3__electric_power_plus__1,
                    'u__source_th__1__tp__1':u__source_th__1__tp__1,
                    'u__misc__1__spot__1':u__spot,
                    'u__misc__1__cost__1':u__cost,
                    'u__misc__1__CO2__1':u__CO2}
        
            Profile = pd.DataFrame(tttt,
                                    columns=['time',
                                             'u__boiler_fuel__1__thermal_power_plus__1',
                                             'u__chp__1__electric_power_plus__1',
                                             'u__heatpump__1__thermal_power_plus__1',
                                             'u__demand_el__1__electrical_power_minus__1',
                                             'u__demand_th__1__thermal_power_minus__1',
                                             'u__demand_th__2__thermal_power_minus__1',
                                             'u__demand_th__3__thermal_power_minus__1',
                                             'u__demand_th__4__thermal_power_minus__1',
                                             'u__demand_th__5__thermal_power_minus__1',
                                             'u__demand_th__6__thermal_power_minus__1',
                                             'u__demand_th__7__thermal_power_minus__1',
                                             'u__demand_th__8__thermal_power_minus__1',
                                             'u__demand_th__9__thermal_power_minus__1',
                                             'u__demand_th__10__thermal_power_minus__1',
                                             'u__demand_th__11__thermal_power_minus__1',
                                             'u__demand_th__12__thermal_power_minus__1',
                                             'u__pv__1__electric_power_plus__1',
                                             'u__pv__2__electric_power_plus__1',
                                             'u__pv__3__electric_power_plus__1',
                                             'u__source_th__1__tp__1',
                                             'u__misc__1__spot__1',
                                             'u__misc__1__cost__1',
                                             'u__misc__1__CO2__1'])
        
        
            Profile.to_csv (r'export_dataframe.csv', 
                        index = False, header=True)
        
            input_array = N.genfromtxt('export_dataframe.csv', delimiter=',', names=True)
                
            print('Start Simulation')
            
            now_SimulationStart = datetime.now()
        
        
            # while-loop. If simulation takes to long (timeout-time at simulate_fmu), the siimulation will
            # be abroaded and startet at the next 15 min timegrid with the initial values of the abroaded
            # simulation
            SimTimeStart = Start_time
            
            #while SimTimeStart<(Start_time+86400): #SimTimeEnd<End_time: 
             
                
            res = simulate_fmu(
                        filename=fmu,
                        validate=False,
                        fmi_type=fmi_type,
                        start_time = SimTimeStart,
                        stop_time = End_time,
                        solver=solver,
                        step_size=900,
                        relative_tolerance=1e-6,
                        output_interval=900,
                        record_events=events,
                        start_values={#'Start_time':Start_time,
                                      #'End_time':End_time,
                                      'init__controll_value':1,
                                      'init__controll_temp__heatpump':init__controll_temp__heatpump,
                                      'init__controll_temp__boiler_fuel':init__controll_temp__boiler_fuel,
                                      'init__control_chp_on':init__control_chp_on,
                                      'init__control_chp_off':init__control_chp_off,
                                      'init__controll_chp_offset':init__controll_chp_offset,
                                      'init__control_heatpump_on':init__control_heatpump_on,
                                      'init__control_heatpump_off':init__control_heatpump_off,
                                      'init__controll_heatpump_offset':init__controll_heatpump_offset,
                                      'init__control_stroage_th__1_6_on':init__control_stroage_th__1_6_on,
                                      'init__control_stroage_th__1_6_off':init__control_stroage_th__1_6_off,
                                      'init__chp__1__status__1':init__chp__1__status__1,
                                      'init__heatpump__1__status__1':init__heatpump__1__status__1,
                                      'init__storage_th__1__tp__1':init__storage_th__1__tp__1,
                                      'init__storage_th__1__tp__2':init__storage_th__1__tp__2,
                                      'init__storage_th__1__tp__3':init__storage_th__1__tp__3,
                                      'init__storage_th__1__tp__4':init__storage_th__1__tp__4,
                                      'init__storage_th__2__tp__1':init__storage_th__2__tp__1,
                                      'init__storage_th__2__tp__2':init__storage_th__2__tp__2,
                                      'init__storage_th__2__tp__3':init__storage_th__2__tp__3,
                                      'init__storage_th__2__tp__4':init__storage_th__2__tp__4,
                                      'init__storage_th__3__tp__1':init__storage_th__3__tp__1,
                                      'init__storage_th__3__tp__2':init__storage_th__3__tp__2,
                                      'init__storage_th__3__tp__3':init__storage_th__3__tp__3,
                                      'init__storage_th__3__tp__4':init__storage_th__3__tp__4,
                                      'init__storage_th__4__tp__1':init__storage_th__4__tp__1,
                                      'init__storage_th__4__tp__2':init__storage_th__4__tp__2,
                                      'init__storage_th__4__tp__3':init__storage_th__4__tp__3,
                                      'init__storage_th__4__tp__4':init__storage_th__4__tp__4,
                                      'init__storage_th__5__tp__1':init__storage_th__5__tp__1,
                                      'init__storage_th__5__tp__2':init__storage_th__5__tp__2,
                                      'init__storage_th__5__tp__3':init__storage_th__5__tp__3,
                                      'init__storage_th__5__tp__4':init__storage_th__5__tp__4,
                                      'init__storage_th__6__tp__1':init__storage_th__6__tp__1,
                                      'init__storage_th__6__tp__2':init__storage_th__6__tp__2,
                                      'init__storage_th__6__tp__3':init__storage_th__6__tp__3,
                                      'init__storage_th__6__tp__4':init__storage_th__6__tp__4,
                                      'init__storage_th__7__tp__1':init__storage_th__7__tp__1,
                                      'init__storage_th__7__tp__2':init__storage_th__7__tp__2,
                                      'init__storage_th__7__tp__3':init__storage_th__7__tp__3,
                                      'init__storage_th__7__tp__4':init__storage_th__7__tp__4,
                                      'init__storage_th__7__tp__5':init__storage_th__7__tp__5,
                                      'init__storage_th__8__tp__1':init__storage_th__8__tp__1,
                                      'init__storage_th__8__tp__2':init__storage_th__8__tp__2},
                        input=input_array,
                        output=output,
                        fmi_call_logger=lambda s: print('[FMI] ' + s) if fmi_logging else None,
                        step_finished=my_callback,
                        timeout=10*60) # in sconds
            
            mtr_res = N.array([res['time'],                                            # 0
                                res['u__boiler_fuel__1__thermal_power_plus__1'],        # 1
                                res['u__chp__1__electric_power_plus__1'],               # 2
                                res['u__demand_el__1__electrical_power_minus__1'],      # 3
                                res['u__demand_th__1__thermal_power_minus__1'],         # 4
                                res['u__demand_th__10__thermal_power_minus__1'],        # 5
                                res['u__demand_th__11__thermal_power_minus__1'],        # 6
                                res['u__demand_th__12__thermal_power_minus__1'],        # 7
                                res['u__demand_th__2__thermal_power_minus__1'],         # 8
                                res['u__demand_th__3__thermal_power_minus__1'],         # 9
                                res['u__demand_th__4__thermal_power_minus__1'],         # 10
                                res['u__demand_th__5__thermal_power_minus__1'],         # 11
                                res['u__demand_th__6__thermal_power_minus__1'],         # 12
                                res['u__demand_th__7__thermal_power_minus__1'],         # 13
                                res['u__demand_th__8__thermal_power_minus__1'],         # 14
                                res['u__demand_th__9__thermal_power_minus__1'],         # 15
                                res['u__heatpump__1__thermal_power_plus__1'],           # 16
                                res['u__pv__1__electric_power_plus__1'],                # 17
                                res['u__pv__2__electric_power_plus__1'],                # 18
                                res['u__pv__3__electric_power_plus__1'],                # 19
                                res['u__source_th__1__tp__1'],                          # 20
                                res['y__boiler_fuel__1__status__1'],                    # 21
                                res['y__boiler_fuel__1__thermal_power_plus__1'],        # 22
                                res['y__chp__1__electric_power_plus__1'],               # 23
                                res['y__chp__1__status__1'],                            # 24
                                res['y__chp__1__thermal_power_plus__1'],                # 25
                                res['y__CO2__der'],                                     # 26
                                res['y__CO2__sum'],                                     # 27
                                res['y__cost__der'],                                    # 28
                                res['y__cost__sum'],                                    # 29
                                res['y__demand_el__1__electric_power_minus__1'],        # 30
                                res['y__demand_th__1__thermal_power_minus__1'],         # 31
                                res['y__demand_th__1__tp__1'],                          # 32
                                res['y__demand_th__1__tp__2'],                          # 33
                                res['y__demand_th__10__thermal_power_minus__1'],        # 34    
                                res['y__demand_th__10__tp__1'],                         # 35
                                res['y__demand_th__10__tp__2'],                         # 36
                                res['y__demand_th__11__thermal_power_minus__1'],        # 37
                                res['y__demand_th__11__tp__1'],                         # 38
                                res['y__demand_th__11__tp__2'],                         # 39
                                res['y__demand_th__12__thermal_power_minus__1'],        # 40
                                res['y__demand_th__12__tp__1'],                         # 41
                                res['y__demand_th__12__tp__2'],                         # 42
                                res['y__demand_th__2__thermal_power_minus__1'],         # 43
                                res['y__demand_th__2__tp__1'],                          # 44
                                res['y__demand_th__2__tp__2'],                          # 45
                                res['y__demand_th__3__thermal_power_minus__1'],         # 46
                                res['y__demand_th__3__tp__1'],                          # 47
                                res['y__demand_th__3__tp__2'],                          # 48
                                res['y__demand_th__4__thermal_power_minus__1'],         # 49
                                res['y__demand_th__4__tp__1'],                          # 50
                                res['y__demand_th__4__tp__2'],                          # 51
                                res['y__demand_th__5__thermal_power_minus__1'],         # 52
                                res['y__demand_th__5__tp__1'],                          # 53
                                res['y__demand_th__5__tp__2'],                          # 54
                                res['y__demand_th__6__thermal_power_minus__1'],         # 55
                                res['y__demand_th__6__tp__1'],                          # 56
                                res['y__demand_th__6__tp__2'],                          # 57
                                res['y__demand_th__7__thermal_power_minus__1'],         # 58
                                res['y__demand_th__7__tp__1'],                          # 59
                                res['y__demand_th__7__tp__2'],                          # 60
                                res['y__demand_th__8__thermal_power_minus__1'],         # 61
                                res['y__demand_th__8__tp__1'],                          # 62
                                res['y__demand_th__8__tp__2'],                          # 63
                                res['y__demand_th__9__thermal_power_minus__1'],         # 64
                                res['y__demand_th__9__tp__1'],                          # 65
                                res['y__demand_th__9__tp__2'],                          # 66
                                res['y__heatpump__1__electric_power_minus__1'],         # 67
                                res['y__heatpump__1__status__1'],                       # 68
                                res['y__heatpump__1__thermal_power_plus__1'],           # 69
                                res['y__pv__1__electric_power_plus__1'],                # 70
                                res['y__pv__2__electric_power_plus__1'],                # 71
                                res['y__pv__3__electric_power_plus__1'],                # 72
                                res['y__storage_th__1__tp__1'],                         # 73
                                res['y__storage_th__1__tp__2'],                         # 74
                                res['y__storage_th__1__tp__3'],                         # 75
                                res['y__storage_th__1__tp__4'],                         # 76
                                res['y__storage_th__2__tp__1'],                         # 77
                                res['y__storage_th__2__tp__2'],                         # 78
                                res['y__storage_th__2__tp__3'],                         # 79
                                res['y__storage_th__2__tp__4'],                         # 80
                                res['y__storage_th__3__tp__1'],                         # 81
                                res['y__storage_th__3__tp__2'],                         # 82
                                res['y__storage_th__3__tp__3'],                         # 83
                                res['y__storage_th__3__tp__4'],                         # 84
                                res['y__storage_th__4__tp__1'],                         # 85
                                res['y__storage_th__4__tp__2'],                         # 86
                                res['y__storage_th__4__tp__3'],                         # 87
                                res['y__storage_th__4__tp__4'],                         # 88
                                res['y__storage_th__5__tp__1'],                         # 89
                                res['y__storage_th__5__tp__2'],                         # 90
                                res['y__storage_th__5__tp__3'],                         # 91
                                res['y__storage_th__5__tp__4'],                         # 92
                                res['y__storage_th__6__tp__1'],                         # 93
                                res['y__storage_th__6__tp__2'],                         # 94
                                res['y__storage_th__6__tp__3'],                         # 95
                                res['y__storage_th__6__tp__4'],                         # 96
                                res['y__storage_th__7__tp__1'],                         # 97
                                res['y__storage_th__7__tp__2'],                         # 98
                                res['y__storage_th__7__tp__3'],                         # 99
                                res['y__storage_th__7__tp__4'],                         # 100
                                res['y__storage_th__7__tp__5'],                         # 101
                                res['y__storage_th__8__tp__1'],                         # 102
                                res['y__storage_th__8__tp__2']])                        # 103
            
            if SimTimeEnd == 0:
                res_8760_opt = mtr_res
            else:
                res_8760_opt = N.c_[res_8760_opt,mtr_res]
        
            
            Speicher = N.transpose(res_8760_opt)
            
            N.savetxt("res_8760_opt_einzeln.csv", res_8760_opt, delimiter=";")
            N.savetxt("res_8760_opt_einzeln_transponiert.csv", Speicher, delimiter=";")
            
            SimTimeEnd = res['time'][len(res['time'])-1]
            
            # If there is an abroad, a new simulation has to be startet with SimTimeEnd as new Start_time
            #if SimTimeEnd!= End_time: # (SimTimeStart+900): #
            #    SimTimeStart = SimTimeStart + 900 # 10 seconds more as new start point
            #    Readjustments = Readjustments + 1
            #    print('readjustment nr. ', Readjustments, ' at simulation time: ', SimTimeEnd/3600)
             
                
            #if (SimTimeStart % 86400) == 0:
            #    print('readjustment nr. ', Readjustments, ' at simulation time: ', SimTimeEnd/3600)
            
            
            #SimTimeStart  = SimTimeStart + 900
        
            # Initialisierungswerte für neue Optimierung und Simulation bestimmen 
            chp__1__status__1__End = res['y__chp__1__status__1'] 
            init__chp__1__status__1 = chp__1__status__1__End[len(res['y__chp__1__status__1'])-1]
        
            boiler_fuel__1__status__1__End = res['y__boiler_fuel__1__status__1'] 
            init__boiler_fuel__1__status__1 = boiler_fuel__1__status__1__End[len(res['y__boiler_fuel__1__status__1'])-1]
        
            heatpump__1__status__1__End = res['y__heatpump__1__status__1'] 
            init__heatpump__1__status__1 = heatpump__1__status__1__End[len(res['y__heatpump__1__status__1'])-1]
        
            storage_th__1__tp__1__End = res['y__storage_th__1__tp__1'] 
            storage_th__1__tp__2__End = res['y__storage_th__1__tp__2'] 
            storage_th__1__tp__3__End = res['y__storage_th__1__tp__3'] 
            storage_th__1__tp__4__End = res['y__storage_th__1__tp__4'] 
            init__storage_th__1__tp__1 = storage_th__1__tp__1__End[len(res['y__storage_th__1__tp__1'])-1]
            init__storage_th__1__tp__2 = storage_th__1__tp__2__End[len(res['y__storage_th__1__tp__2'])-1]
            init__storage_th__1__tp__3 = storage_th__1__tp__3__End[len(res['y__storage_th__1__tp__3'])-1]
            init__storage_th__1__tp__4 = storage_th__1__tp__4__End[len(res['y__storage_th__1__tp__4'])-1]
        
            storage_th__2__tp__1__End = res['y__storage_th__2__tp__1'] 
            storage_th__2__tp__2__End = res['y__storage_th__2__tp__2'] 
            storage_th__2__tp__3__End = res['y__storage_th__2__tp__3'] 
            storage_th__2__tp__4__End = res['y__storage_th__2__tp__4'] 
            init__storage_th__2__tp__1 = storage_th__2__tp__1__End[len(res['y__storage_th__2__tp__1'])-1]
            init__storage_th__2__tp__2 = storage_th__2__tp__2__End[len(res['y__storage_th__2__tp__2'])-1]
            init__storage_th__2__tp__3 = storage_th__2__tp__3__End[len(res['y__storage_th__2__tp__3'])-1]
            init__storage_th__2__tp__4 = storage_th__2__tp__4__End[len(res['y__storage_th__2__tp__4'])-1]
        
            storage_th__3__tp__1__End = res['y__storage_th__3__tp__1'] 
            storage_th__3__tp__2__End = res['y__storage_th__3__tp__2'] 
            storage_th__3__tp__3__End = res['y__storage_th__3__tp__3'] 
            storage_th__3__tp__4__End = res['y__storage_th__3__tp__4'] 
            init__storage_th__3__tp__1 = storage_th__3__tp__1__End[len(res['y__storage_th__3__tp__1'])-1]
            init__storage_th__3__tp__2 = storage_th__3__tp__2__End[len(res['y__storage_th__3__tp__2'])-1]
            init__storage_th__3__tp__3 = storage_th__3__tp__3__End[len(res['y__storage_th__3__tp__3'])-1]
            init__storage_th__3__tp__4 = storage_th__3__tp__4__End[len(res['y__storage_th__3__tp__4'])-1]
        
            storage_th__4__tp__1__End = res['y__storage_th__4__tp__1'] 
            storage_th__4__tp__2__End = res['y__storage_th__4__tp__2'] 
            storage_th__4__tp__3__End = res['y__storage_th__4__tp__3'] 
            storage_th__4__tp__4__End = res['y__storage_th__4__tp__4'] 
            init__storage_th__4__tp__1 = storage_th__4__tp__1__End[len(res['y__storage_th__4__tp__1'])-1]
            init__storage_th__4__tp__2 = storage_th__4__tp__2__End[len(res['y__storage_th__4__tp__2'])-1]
            init__storage_th__4__tp__3 = storage_th__4__tp__3__End[len(res['y__storage_th__4__tp__3'])-1]
            init__storage_th__4__tp__4 = storage_th__4__tp__4__End[len(res['y__storage_th__4__tp__4'])-1]
        
            storage_th__5__tp__1__End = res['y__storage_th__5__tp__1'] 
            storage_th__5__tp__2__End = res['y__storage_th__5__tp__2'] 
            storage_th__5__tp__3__End = res['y__storage_th__5__tp__3'] 
            storage_th__5__tp__4__End = res['y__storage_th__5__tp__4'] 
            init__storage_th__5__tp__1 = storage_th__5__tp__1__End[len(res['y__storage_th__5__tp__1'])-1]
            init__storage_th__5__tp__2 = storage_th__5__tp__2__End[len(res['y__storage_th__5__tp__2'])-1]
            init__storage_th__5__tp__3 = storage_th__5__tp__3__End[len(res['y__storage_th__5__tp__3'])-1]
            init__storage_th__5__tp__4 = storage_th__5__tp__4__End[len(res['y__storage_th__5__tp__4'])-1]
        
            storage_th__6__tp__1__End = res['y__storage_th__6__tp__1'] 
            storage_th__6__tp__2__End = res['y__storage_th__6__tp__2'] 
            storage_th__6__tp__3__End = res['y__storage_th__6__tp__3'] 
            storage_th__6__tp__4__End = res['y__storage_th__6__tp__4'] 
            init__storage_th__6__tp__1 = storage_th__6__tp__1__End[len(res['y__storage_th__6__tp__1'])-1]
            init__storage_th__6__tp__2 = storage_th__6__tp__2__End[len(res['y__storage_th__6__tp__2'])-1]
            init__storage_th__6__tp__3 = storage_th__6__tp__3__End[len(res['y__storage_th__6__tp__3'])-1]
            init__storage_th__6__tp__4 = storage_th__6__tp__4__End[len(res['y__storage_th__6__tp__4'])-1]
        
            storage_th__7__tp__1__End = res['y__storage_th__7__tp__1'] 
            storage_th__7__tp__2__End = res['y__storage_th__7__tp__2'] 
            storage_th__7__tp__3__End = res['y__storage_th__7__tp__3'] 
            storage_th__7__tp__4__End = res['y__storage_th__7__tp__4'] 
            storage_th__7__tp__5__End = res['y__storage_th__7__tp__5'] 
            init__storage_th__7__tp__1 = storage_th__7__tp__1__End[len(res['y__storage_th__7__tp__1'])-1]
            init__storage_th__7__tp__2 = storage_th__7__tp__2__End[len(res['y__storage_th__7__tp__2'])-1]
            init__storage_th__7__tp__3 = storage_th__7__tp__3__End[len(res['y__storage_th__7__tp__3'])-1]
            init__storage_th__7__tp__4 = storage_th__7__tp__4__End[len(res['y__storage_th__7__tp__4'])-1]
            init__storage_th__7__tp__5 = storage_th__7__tp__5__End[len(res['y__storage_th__7__tp__5'])-1]
        
            storage_th__8__tp__1__End = res['y__storage_th__8__tp__1'] 
            storage_th__8__tp__2__End = res['y__storage_th__8__tp__2'] 
            init__storage_th__8__tp__1 = storage_th__8__tp__1__End[len(res['y__storage_th__8__tp__1'])-1]
            init__storage_th__8__tp__2 = storage_th__8__tp__2__End[len(res['y__storage_th__8__tp__2'])-1]
            
            # while end
                
            now_SimulationEnd = datetime.now()
            
            SimulationDuration = now_SimulationEnd-now_SimulationStart
            
            print('------------------------------------------')
            print('Sim:', Tag-Tag_Start+1, 'von', Anzahl_Tage+1, '/ Duration: ',SimulationDuration, '/ Readjustments: ', Readjustments)
            print('------------------------------------------')   
                
            storage_th__1__tp__Mean = (init__storage_th__1__tp__1 + init__storage_th__1__tp__2 + init__storage_th__1__tp__3 + init__storage_th__1__tp__4) / 4
            storage_th__2__tp__Mean = (init__storage_th__2__tp__1 + init__storage_th__2__tp__2 + init__storage_th__2__tp__3 + init__storage_th__2__tp__4) / 4
            storage_th__3__tp__Mean = (init__storage_th__3__tp__1 + init__storage_th__3__tp__2 + init__storage_th__3__tp__3 + init__storage_th__3__tp__4) / 4
            storage_th__4__tp__Mean = (init__storage_th__4__tp__1 + init__storage_th__4__tp__2 + init__storage_th__4__tp__3 + init__storage_th__4__tp__4) / 4
            storage_th__5__tp__Mean = (init__storage_th__5__tp__1 + init__storage_th__5__tp__2 + init__storage_th__5__tp__3 + init__storage_th__5__tp__4) / 4
            storage_th__6__tp__Mean = (init__storage_th__6__tp__1 + init__storage_th__6__tp__2 + init__storage_th__6__tp__3 + init__storage_th__6__tp__4) / 4
            storage_th__7__tp__Mean = (init__storage_th__7__tp__1 + init__storage_th__7__tp__2 + init__storage_th__7__tp__3 + init__storage_th__7__tp__4 + init__storage_th__7__tp__5) / 5
            storage_th__8__tp__Mean = (init__storage_th__8__tp__1 + init__storage_th__8__tp__2) / 2
        
            storage_th__1__thermal_state_End = 560 * (storage_th__1__tp__Mean-55) / 860
            storage_th__2__thermal_state_End = 560 * (storage_th__2__tp__Mean-55) / 860
            storage_th__3__thermal_state_End = 560 * (storage_th__3__tp__Mean-55) / 860
            storage_th__4__thermal_state_End = 560 * (storage_th__4__tp__Mean-55) / 860
            storage_th__5__thermal_state_End = 560 * (storage_th__5__tp__Mean-55) / 860
            storage_th__6__thermal_state_End = 560 * (storage_th__6__tp__Mean-55) / 860
            storage_th__7__thermal_state_End = 1000 * (storage_th__7__tp__Mean-55) / 860
            storage_th__8__thermal_state_End = 500 * (storage_th__8__tp__Mean-30) / 860
        
            TS1_E_Init = min(max(storage_th__1__thermal_state_End / (560 * (HT - NT) / 860),0),1)
            TS2_E_Init = min(max(storage_th__2__thermal_state_End / (560 * (HT - NT) / 860),0),1)
            TS3_E_Init = min(max(storage_th__3__thermal_state_End / (560 * (HT - NT) / 860),0),1)
            TS4_E_Init = min(max(storage_th__4__thermal_state_End / (560 * (HT - NT) / 860),0),1)
            TS5_E_Init = min(max(storage_th__5__thermal_state_End / (560 * (HT - NT) / 860),0),1)
            TS6_E_Init = min(max(storage_th__6__thermal_state_End / (560 * (HT - NT) / 860),0),1)   
            TS7_E_Init = min(max(storage_th__7__thermal_state_End / (1000 * (HT - NT) / 860),0),1)   
            TS8_E_Init = min(max(storage_th__8__thermal_state_End / (500 * (NT - Base) / 860),0),1)   
            
            BHKW_initialstate = res['y__chp__1__status__1']
            BHKW_initialstate = BHKW_initialstate[len(res['y__chp__1__status__1'])-1] * 1
    
            Cost_Init = res['y__cost__sum']
            Cost_Init = Cost_Init[len(res['y__cost__sum'])-1]
            
            CO2_Init = res['y__CO2__sum']
            CO2_Init = CO2_Init[len(res['y__CO2__sum'])-1]
        
           # if Tag == 1:
           #     res_8760_opt = mtr_res
           #     mtr_8760_opt = matrix
           # else:
           #     res_8760_opt = N.c_[res_8760_opt,mtr_res]
           #     mtr_8760_opt = mtr_8760_opt.append(matrix)
            Teiltag = Teiltag+1
       
        Tag = Tag+1
        Tag 

print('---------------------------------------------------')
print('Ende Optimierungsschleife')
now = datetime.now()
current_time = now.strftime("%H:%M:%S")
print("Current Time =", current_time)
print('---------------------------------------------------')


# Ende while-Schleife

N.savetxt("res_8760_opt.csv", res_8760_opt, delimiter=";")
##


#res_8760_opt = N.genfromtxt('res_8760_opt.csv', delimiter=';', names=True) 
#res_8760_waerme = N.genfromtxt('res_8760_waerme.csv', delimiter=',', names=True) 




t_plot_FMU = res_8760_opt[0,:]
t_plot_warme = res_8760_waerme[0,:]

PSP1_waerme = (res_8760_waerme[1,:] + res_8760_waerme[2,:] + res_8760_waerme[3,:] + res_8760_waerme[4,:] + res_8760_waerme[5,:] + res_8760_waerme[6,:] + res_8760_waerme[7,:] + res_8760_waerme[8,:]) / 8
PSP1_opt = (res_8760_opt[1,:] + res_8760_opt[2,:] + res_8760_opt[3,:] + res_8760_opt[4,:] + res_8760_opt[5,:] + res_8760_opt[6,:] + res_8760_opt[7,:] + res_8760_opt[8,:]) / 8


## Auswertungen
# 
#### Wärmesektor ####
#
## EZA's Wärmegeführt
#
Trinkwarmwasser = (res_8760_waerme[4,:] + res_8760_waerme[8,:] + res_8760_waerme[9,:] + res_8760_waerme[10,:] + res_8760_waerme[11,:] + res_8760_waerme[12,:])/0.035
Raumwaerme = res_8760_waerme[13,:] + res_8760_waerme[14,:] + res_8760_waerme[15,:] + res_8760_waerme[5,:] + res_8760_waerme[6,:] + res_8760_waerme[7,:]


plt.plot(t_plot_waerme/3600, Raumwaerme , '-', label='RW', linewidth=1)
plt.plot(t_plot_waerme/3600, Trinkwarmwasser , '-', label='TWW', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Wärmebedarf (Wärmegeführt)')
plt.ylabel('Leistung [kW]') 
plt.show()

plt.plot(t_plot_waerme/3600, res_8760_waerme[69,:] , '-', label='WP', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[25,:] , '-', label='BHKW', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[22,:] , '-', label='Boiler', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Wärmeerzeugung (Wärmegeführt)')
plt.ylabel('Leistung [kW]') 
plt.show()

plt.plot(t_plot_FMU/3600, res_8760_opt[69,:] , '-', label='WP', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[25,:] , '-', label='BHKW', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[22,:] , '-', label='Boiler', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Wärmeerzeugung (Optimiert)')
plt.ylabel('Leistung [kW]') 
plt.show()

plt.plot(t_plot_FMU/3600, res_8760_opt[16,:] , '-', label='Input', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[69,:] , '-', label='Output', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Wärmepumpe (optimiert)')
plt.ylabel('Leistung [kW]') 
plt.show()

plt.plot(t_plot_FMU/3600, res_8760_opt[2,:] , '-', label='Input', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[23,:] , '-', label='Output', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('BHKW (optimiert)')
plt.ylabel('Leistung [kW]') 
plt.show()

plt.plot(t_plot_FMU/3600, res_8760_opt[1,:] , '-', label='Input', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[22,:] , '-', label='Output', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Gaskessel (optimiert)')
plt.ylabel('Leistung [kW]') 
plt.show()
#
## Demand und Gesamterzeugung (Wärmegeführt)
#

plt.plot(t_plot_waerme/3600, res_8760_waerme[29,:] , '-', label='Kosten wärme', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[29,:] , '-', label='Kosten opt', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Vergleich Kosten')
plt.ylabel('€') 
plt.show()


#
plt.plot(t_plot_waerme/3600, res_8760_waerme[27,:] , '-', label='CO2 wärme', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[27,:] , '-', label='CO2 opt', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Vergleich CO2')
plt.ylabel('t') 
plt.show()



plt.plot(t_plot_waerme/3600, res_8760_waerme[76,:] , '-', label='PS 1 oben', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[80,:] , '-', label='PS 2 oben', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[84,:] , '-', label='PS 3 oben', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[88,:] , '-', label='PS 4 oben', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[92,:] , '-', label='PS 5 oben', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[96,:] , '-', label='PS 6 oben', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[73,:] , '-', label='PS 1 unten', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[77,:] , '-', label='PS 2 unten', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[81,:] , '-', label='PS 3 unten', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[85,:] , '-', label='PS 4 unten', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[89,:] , '-', label='PS 5 unten', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[93,:] , '-', label='PS 6 unten', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Vergleich dezentrale Puffer (wärmegeführt)')
plt.ylabel('in °C') 
plt.show()

plt.plot(t_plot_waerme/3600, res_8760_waerme[97,:] , '-', label='PS zentral oben', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[101,:] , '-', label='PS zentral unten', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[103,:] , '-', label='PS WP oben', linewidth=1)
plt.plot(t_plot_waerme/3600, res_8760_waerme[102,:] , '-', label='PS WP unten', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Vergleich zentrale Puffer (wärmegeführt)')
plt.ylabel('in °C') 
plt.show()


plt.plot(t_plot_FMU/3600, res_8760_opt[76,:] , '-', label='PS 1 oben', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[80,:] , '-', label='PS 2 oben', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[84,:] , '-', label='PS 3 oben', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[88,:] , '-', label='PS 4 oben', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[92,:] , '-', label='PS 5 oben', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[96,:] , '-', label='PS 6 oben', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[73,:] , '-', label='PS 1 unten', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[77,:] , '-', label='PS 2 unten', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[81,:] , '-', label='PS 3 unten', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[85,:] , '-', label='PS 4 unten', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[89,:] , '-', label='PS 5 unten', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[93,:] , '-', label='PS 6 unten', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Vergleich dezentrale Puffer oberer Fühler (optimiert)')
plt.ylabel('in °C') 
plt.show()

plt.plot(t_plot_FMU/3600, res_8760_opt[97,:] , '-', label='PS zentral oben', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[101,:] , '-', label='PS zentral unten', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[103,:] , '-', label='PS WP oben', linewidth=1)
plt.plot(t_plot_FMU/3600, res_8760_opt[102,:] , '-', label='PS WP unten', linewidth=1)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.grid(True)
plt.title('Vergleich zentrale Puffer (optimiert)')
plt.ylabel('in °C') 
plt.show()


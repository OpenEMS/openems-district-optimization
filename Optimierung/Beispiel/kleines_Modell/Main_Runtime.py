# -*- coding: utf-8 -*-
"""
Created on Sat Jan 23 19:14:15 2021

@author: JW
"""

import os
import numpy as N
import matplotlib.pyplot as plt
import pandas as pd
import rpy2.robjects as robjects
import rpy2.robjects.numpy2ri
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter
from datetime import datetime

pandas2ri.activate()
rpy2.robjects.numpy2ri.activate()


robjects.r('source("Function_Optimization.R")')
r_func_Optimierung = robjects.globalenv['Optimierung']

plot = True

LoadProfiles = pd.read_csv('LoadProfiles.csv', thousands='.', decimal=',', sep=';')

Sekunden = LoadProfiles['Sekunden']
Strombedarf = LoadProfiles['DemandPower']
Waermebedarf = LoadProfiles['DemandHeat']
PV_Erzeugung = LoadProfiles['ProductionPV']

Tag_Start = 160
Anzahl_Tage = 10

Stunde_Beginn = 24*Tag_Start-24
Stunde_Ende = (Tag_Start+Anzahl_Tage-1)*24 
Stunde_Stop = Stunde_Ende

# Optimierungsparameter festlegen
BHKW_Init = 0 # 0 = off, 1 = on
Puffer_Init = 0.5 
Fahrplan = BHKW_Init

while Stunde_Beginn<=Stunde_Stop: 

    # Beginn while-Schleife            
    Stunde_Ende = Stunde_Beginn+24
           
    # Lastprofile initialisiseren
    t = Sekunden[Stunde_Beginn*4:Stunde_Ende*4]
    Strom = Strombedarf[Stunde_Beginn*4:Stunde_Ende*4]
    Waerme = Waermebedarf[Stunde_Beginn*4:Stunde_Ende*4]
    PV_Prod = PV_Erzeugung[Stunde_Beginn*4:Stunde_Ende*4]

    # Umwandeln auf R-Format
    Strom_R = robjects.FloatVector(Strom)
    Waerme_R = robjects.FloatVector(Waerme)
    PV_Prod_R = robjects.FloatVector(PV_Prod)
    
    Anzahl_Zeitschritte = 24
    Zeitintervall = 60 # in Minuten
        
    matrix_R = r_func_Optimierung(Strom_R,
                                  Waerme_R,
                                  PV_Prod_R,
                                  Puffer_Init,
                                  BHKW_Init,
                                  Anzahl_Zeitschritte, Zeitintervall)


    with localconverter(robjects.default_converter + pandas2ri.converter):
        matrix = robjects.conversion.rpy2py(matrix_R)
    
    R_Gasboiler_th = N.kron(matrix['Pth_GaBo1_TS1'],(1,1,1,1,1,1,1,1)) 
    R_BHKW_th = N.kron(matrix['Pth_CHP1_TS1'],(1,1,1,1,1,1,1,1)) 
    R_Bedarf_th = N.kron(matrix['Pth_TS1_Demth1'],(1,1,1,1,1,1,1,1))  
    R_Puffer_th = N.kron(matrix['Eth_TS1'],(1,1,1,1,1,1,1,1)) 
    R_kuQuell_th = N.kron(matrix['Pth_PubGth1_TS1'],(1,1,1,1,1,1,1,1)) 
    R_PV_el = N.kron((matrix['Pel_PV1_PubGel1']+matrix['Pel_PV1_Demel1']),(1,1,1,1,1,1,1,1))  
    R_BHKW_el = N.kron((matrix['Pel_CHP1_PubGel1']+matrix['Pel_CHP1_Demel1']),(1,1,1,1,1,1,1,1))  
    R_Bedarf_el = N.kron((matrix['Pel_CHP1_Demel1']+matrix['Pel_PV1_Demel1']+matrix['Pel_PubGel1_Demel1']),(1,1,1,1,1,1,1,1)) 
    R_Netz_el = N.kron(matrix['Pel_PubGel1_Demel1'],(1,1,1,1,1,1,1,1))  
    
    
    
    BHKW_Init = R_BHKW_th[2]
    
    Fahrplan = N.append(Fahrplan, BHKW_Init)
    
    if BHKW_Init > 0:
        BHKW_Init = 1
    else:
        BHKW_Init = 0
    
    Puffer_Init = R_Puffer_th[2]/69.77
    if Puffer_Init > 1:
        Puffer_Init = 1
    elif Puffer_Init < 0:
        Puffer_Init = 0

    if plot == True:
        t = N.linspace(Stunde_Beginn, Stunde_Ende, len(R_BHKW_th))
        
        plt.plot(t, R_Gasboiler_th, '--', label='Gasboiler', linewidth=2)
        plt.plot(t, R_BHKW_th, '--', label='BHKW', linewidth=2)
        plt.plot(t, R_Bedarf_th, '--', label='Bedarf', linewidth=2)
        plt.plot(t, R_kuQuell_th, '-', label='Künstliche Quelle', linewidth=1)
        plt.plot(t, R_Puffer_th, '--', label='Speicherfüllstand', linewidth=1) 
        plt.ylim((0,100))
        plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
        plt.grid(True)
        plt.title('Ausgabe_Optimierung')
        plt.ylabel('Leistung [kW], Füllstand [kWh]') 
        plt.show()

    Stunde_Beginn  = Stunde_Beginn  + 1
    


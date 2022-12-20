# Simulation
## Introduction

The energy system in the Maggie research project was set up in Modelica and exported as a functional mock-up unit in order to be able to use it in the interconnections with the optimization.

In order to keep the Git repository lean and targeted, the simulation model and all the necessary load profiles, input parameters and required libraries were outsourced. These can be found at the following link:
<Link einfügen>

## Requiered tools and packages
To run the simulation model, OpenModelica or Dymola is used. Dymola is chargeable, but shows strengths in the performance of the simulation compared to OpenModelica.

In the simulation model, the paths to the external files are stored as absolute paths. These paths currently point to "C:/Simulationen/Grundlagen". If a different storage location is selected, all paths must be adjusted accordingly.

Two external libraries are used:
* AixLib
https://github.com/RWTH-EBC/AixLib

* BuildingSystems
https://github.com/UdK-VPT/BuildingSystems/tree/v2.0.0-beta2

Since the "FastVAC" part of the AixLib used is no longer maintained, a corresponding version of the library was frozen and continued to be worked on.

Therefore, in addition to the two libraries, it must also be ensured that the Modelica standard library 3.2.3 is used.
https://github.com/modelica/ModelicaStandardLibrary/tree/v3.2.3

The two external libraries can be found in the external files folder along with the simulation model and all input profiles.



## License

MIT

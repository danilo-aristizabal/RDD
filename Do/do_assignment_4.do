*** Done by Danilo Esteban Aristizabal Giraldo
*** Assignment 4 - Causal Inference class
*** Replication of Punishment and Deterrence: Evidence from Drunk Driving (Hansen, 2015)

cls 
clear all

* Working directory
global pc = "C:\Users\DANILO\Github\RDD"
global data = "$pc/Data"
global figures = "$pc/Figures"
global figures = "$pc/Tables"

* Load database
use "$data/hansen_dwi.dta", clear 

* Create a dummy equaling 1 if bac1>= 0.08 and 0 otherwise
gen dummy = 1 if bac1>=0.08
recode dummy (.=0)
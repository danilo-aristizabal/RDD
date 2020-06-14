*** Done by Danilo Esteban Aristizabal Giraldo
*** Assignment 4 - Causal Inference class
*** Replication of Punishment and Deterrence: Evidence from Drunk Driving (Hansen, 2015)

cls 
clear all

* Working directory
global pc = "C:\Users\DANILO\Github\RDD"
global data = "$pc/Data"
global figures = "$pc/Figures"
global tables = "$pc/Tables"

* Load database
use "$data/hansen_dwi.dta", clear 

* Create a dummy equaling 1 if bac1>= 0.08 and 0 otherwise
gen DUI = 1 if bac1>=0.08
recode DUI (.=0)

* Now we we replicate figure 1
gr two (hist bac1, frequency width(0.00001) bcolor(gray)) ///
(scatteri 0 0.08 1700 0.08, c(l) m(i)), graphregion(fcolor(white)) ///
ytitle("Frequency", margin(samll)) ///
xtitle("BAC", margin(samll)) ///
title("BAC histogram") legend(off)
graph export "$figures/figure1.pdf", replace

*** We test for manipualtion of the BAC test
* It seems there is manipulation in BAC test at the threshold 0.08
rddensity bac1, c(0.08)
* outreg2 using "$pc/tables/density_test.xls", replace

rddensity bac1, c(0.08) plot

* We do it for several points of BAC test
* And it seems there is no manipulation at different thresholds
rddensity bac1, c(0.02)
rddensity bac1, c(0.03)
rddensity bac1, c(0.04)
rddensity bac1, c(0.05)
rddensity bac1, c(0.06)
rddensity bac1, c(0.07)
rddensity bac1, c(0.09)
rddensity bac1, c(0.10)

*** Now reproduce table 2 of Hansen (2015)
* first create the interacion
gen DUI_BAC = DUI*bac1

*Rescale the cuttoff
gen bac1centered = bac1-0.08

* Create the table 2
capture erase "$tables/table_2.txt"
foreach outcome in male white aged acc {

        summ `outcome'
		local mean_c = round(`r(mean)',0.001)

		* RD for all covariates 
		rdrobust `outcome' bac1 if (bac1centered>=-0.05 & bac1centered<=0.05), c(0.08) h(0.05) p(1) kernel(uniform)
		outreg2 using "$tables/table_2.xls", append dec(4) ctitle("`outcome'") addtext(Controls, No, Mean, `mean_c')
} 


*** Reproducing figure 2, Panel A-D 
* First I do the linear fit
cmogram acc bac1 if bac1<0.2, cut(0.08) scatter line(0.08) lfitci ///
title("Accident at scene") leg graphopts(mfc(gs16))
graph export "$figures/figure2_A.pdf", replace

cmogram male bac1 if bac1<0.2, cut(0.08) scatter line(0.08) lfitci ///
title("Male") leg graphopts(mfc(gs16))
graph export "$figures/figure2_B.pdf", replace

cmogram aged bac1 if bac1<0.2, cut(0.08) scatter line(0.08) lfitci ///
title("Age") leg graphopts(mfc(gs16))
graph export "$figures/figure2_C.pdf", replace

cmogram white bac1 if bac1<0.2, cut(0.08) scatter line(0.08) lfitci ///
title("White") leg graphopts(mfc(gs16))
graph export "$figures/figure2_D.pdf", replace

*graph combine A B C D, r(1) ycommon name(figure2, replace) graphregion(color(white))
*graph export  "$figures/figure2.pdf", as(pdf) replace

* Second I do the quadratic fit
cmogram acc bac1 if bac1<0.2, cut(0.08) scatter line(0.08) qfitci ///
title("Accident at scene") leg graphopts(mfc(gs16)) 
graph export "$figures/figure2_qfit_A.pdf", replace

cmogram male bac1 if bac1<0.2, cut(0.08) scatter line(0.08) qfitci ///
title("Male") leg graphopts(mfc(gs16)) 
graph export "$figures/figure2_qfit_B.pdf", replace

cmogram aged bac1 if bac1<0.2, cut(0.08) scatter line(0.08) qfitci ///
title("Age") leg graphopts(mfc(gs16))
graph export "$figures/figure2_qfit_C.pdf", replace

cmogram white bac1 if bac1<0.2, cut(0.08) scatter line(0.08) qfitci ///
title("White") leg graphopts(mfc(gs16))
graph export "$figures/figure2_qfit_D.pdf", replace

*** Now I estimate equation 1 of Hansen's paper
* Set the controls
global controls "male white aged acc"

* Create bac1centered squared
gen bac1centeredsq = bac1centered*bac1centered 

summ recidivism
local mean_c = round(`r(mean)',0.001)
		
capture erase "$tables/table_3.txt"
* Using a bandwith of 0.05
reg recidivism DUI bac1centered $controls if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_1A

reg recidivism DUI bac1centered DUI#c.bac1centered $controls if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_2A

reg recidivism DUI bac1centered bac1centeredsq DUI#c.bac1centered DUI#c.bac1centeredsq if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_3A

* Using a bandwith of 0.025
reg recidivism DUI bac1centered $controls if (bac1>=-0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_4A

reg recidivism DUI bac1centered DUI#c.bac1centered $controls if (bac1>=-0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_5A

reg recidivism DUI bac1centered bac1centeredsq DUI#c.bac1centered DUI#c.bac1centeredsq if (bac1>=-0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_6A

esttab model_1A model_2A model_3A model_4A model_5A model_6A ///
using "$tables/table3.xls", replace keep(DUI)							///
b(3) se(3) star(* 0.10 * 0.05 ** 0.01) nonum nonotes nolines compress obslast	///
stats(ymean N, label("Mean" "Observations") ///
fmt(3 3 0))

*** Now, Point 8
cmogram recidivism bac1 if bac1<=0.15, cut(0.08) scatter line(0.08) lfitci ///
title("All offenders") leg graphopts(mfc(gs16))
graph export "$figures/figure3_A.pdf", replace

cmogram recidivism bac1 if bac1<=0.15, cut(0.08) scatter line(0.08) qfitci ///
title("All offenders") leg graphopts(mfc(gs16))
graph export "$figures/figure3_A_qfit.pdf", replace



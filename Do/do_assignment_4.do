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

		* Media del grupo de control en pre-tratamiento
        summ `outcome'
		local mean_c = round(`r(mean)',0.001)

        * Distancia expresada como porcentaje del salario minimo
		rdrobust `outcome' bac1 if (bac1centered>=-0.05 & bac1centered<=0.05), c(0.08) h(0.05) p(1) kernel(uniform)
		outreg2 using "$tables/table_2.xls", append dec(4) ctitle("`outcome'") addtext(Controls, No, Mean, `mean_c')
} 


* Look at covariates using reg command
capture erase "$tables/table_2_reg.txt"
foreach outcome in male white aged acc {

		* Media del grupo de control en pre-tratamiento
        summ `outcome'
		local mean_c = round(`r(mean)',0.001)

        * Distancia expresada como porcentaje del salario minimo
		reg `outcome' bac1 if (bac1>= 0.08-0.05 & bac1<= 0.08+0.05), r
		outreg2 using "$tables/table_2_reg.xls", append dec(4) ctitle("`outcome'") addtext(Controls, No, Mean, `mean_c')
}


*** Reproducing figure 2, Panel A-D 
* First I do the linear fit
cmogram acc bac1, cut(0.08) scatter line(0.08) lfit ///
title("Accident at scene") leg name(A, replace)
graph export "$figures/figure2_A.pdf", replace

cmogram male bac1, cut(0.08) scatter line(0.08) lfit ///
title("Male") graphopts(legend(on))
graph export "$figures/figure2_B.pdf", replace

cmogram aged bac1, cut(0.08) scatter line(0.08) lfit ///
title("Age") graphopts(legend(on))
graph export "$figures/figure2_C.pdf", replace

cmogram white bac1, cut(0.08) scatter line(0.08) lfit ///
title("White") graphopts(legend(on))
graph export "$figures/figure2_D.pdf", replace

graph combine A B C D, r(1) ycommon name(figure2, replace) graphregion(color(white))
graph export  "$figures/figure2.pdf", as(pdf) replace

xsize(20) ysize(10)

* Second I do the quadratic fit
cmogram acc bac1, cut(0.08) scatter line(0.08) qfitci ///
title("Accident at scene") leg 

cmogram male bac1, cut(0.08) scatter line(0.08) qfitci ///
title("Male") leg 

cmogram aged bac1, cut(0.08) scatter line(0.08) qfitci ///
title("Age") leg 

cmogram white bac1, cut(0.08) scatter line(0.08) qfitci ///
title("White") leg 






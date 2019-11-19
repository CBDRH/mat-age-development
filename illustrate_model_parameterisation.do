* Stata Version SE 14.1


*********************
*** Simulate data ***
*********************
clear
set obs 100000

* Aboriginality
gen aboriginal = rbinomial(1,.06)
label variable aboriginal "Aboriginal status"

* Age distribution
gen matage = floor(rnormal(28,6))
replace matage = floor(rnormal(25,8)) if aboriginal==1
drop if ! inrange(matage,15,39)
label variable matage "Maternal age"

* Risk of vulnerability
gen risk = exp(-1.8 + 0.70 - 0.015*(matage-30)) if aboriginal==1 & inrange(matage,15,29)
replace risk = exp(-1.8 - 0.060*(matage-30)) if aboriginal==0 & inrange(matage,15,29)
replace risk = exp(-1.8 + 0.70*aboriginal) if inrange(matage,30,34)
replace risk = exp(-1.8 + 0.70 - 0.005*(matage-34)) if aboriginal==1 & inrange(matage,35,39)
replace risk = exp(-1.8 + 0.030*(matage-34)) if aboriginal==0 & inrange(matage,35,39)
label variable risk "Latent risk of vulnerability"

* Developmental vulnerability
gen dv1 = rpoisson(risk)
replace dv1 = 1 if dv1 >1
label variable dv1 "Vulnerable on 1+ domains"


*************************************
**** Visualise the simulated data ***
*************************************

preserve

collapse (mean) mean=dv1 (count) n=dv1 ,by(aboriginal matage)
bysort aboriginal: egen prop = pc(n) 

gr tw ///
(bar prop matage if aboriginal==0, yaxis(1) col(navy) barw(1)) ///  
(bar prop matage if aboriginal==1, yaxis(1) col(maroon) barw(0.75)) ///
(scatter mean matage if aboriginal==0, yaxis(2) m(square) mcol(blue)) ///
(scatter mean matage if aboriginal==1, yaxis(2) m(circle) mcol(red)), ///
yscale(range(0(2)8) axis(1) alt) yscale(range(0(.1).6) axis(2) alt) ///
ylabel(0(2)8, axis(1) nogrid) ylabel(0(.1).6, axis(2)) ///
ytitle("Developmentally vulnerable (%)", axis(2)) ///
ytitle("Births (%)", axis(1)) ///
xtitle("Maternal age at childbirth (years)", axis(1)) ///
title("Risk of developmental vulnerabilty by Aboriginality" "(Simulated data)")  ///
legend(order(1 "Non-Aboriginal births %" 2 "Aboriginal births %" ///
3 "Non-Aboriginal risk" 4 "Aboriginal risk") rows(1) pos(12) ring(0) size(vsmall) symx(*.3) ) 

restore



**************************************************
*** Create variables to parameterise the model ***
**************************************************

* Dummy indicator for maternal age >=35
gen a1 = inrange(matage,35,39)

* Recoding of maternal age
gen a2 = matage-30 if inrange(matage,15,29)
replace a2 = 0 if inrange(matage,30,34)
replace a2 = matage-34 if inrange(matage,35,39)



*********************
*** Fit the model ***
*********************

* Modified Poisson model
glm dv1 i.aboriginal i.a1#c.a2#i.aboriginal, ///
family(poisson) link(log) vce(robust)

* Store predicted values
predict p


******************************************
*** Plot observed versus fitted values ***
******************************************

bysort aboriginal matage: egen prop_dv1 = mean(dv1)
egen tag = tag(aboriginal matage)

gr tw ///
(scatter prop_dv1 matage if aboriginal==0, m(square) mcol(blue)) ///
(scatter prop_dv1 matage if aboriginal==1, m(circle) mcol(red)) ///
(scatter p matage if aboriginal==0, m(i) lc(blue) connect(L)) ///
(scatter p matage if aboriginal==1, m(i) lc(red)  connect(L)) if tag==1, ///
ytitle("Developmentally vulnerable (%)") ///
xtitle("Maternal age at childbirth (years)", axis(1)) ///
yscale(range(0(.1).5)) ylabel(0(.1).5) ///
xline(30, lc(gs10)) xline(34, lc(gs10)) ///
title("Observed versus fitted values" ("Simulated data")) ///
legend(order(1 "Non-Aboriginal observed" 2 "Aboriginal observed" ///
3 "Non-Aboriginal fitted" 4 "Aboriginal fitted") rows(1) pos(12) ring(0) size(vsmall) symx(*.3))


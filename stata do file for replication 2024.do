
clear

local path "yourpath"

use "`path'\lifeladder2019.dta"
merge m:1 Country using "`path'\hdi2019.dta"
keep if _merge==3
drop _merge
merge 1:1 Country using "`path'\democracy.dta"
drop _merge
drop if HDI==.
merge 1:1 id using "`path'\sdggoaldata.dta"
drop if _merge==2
drop _merge

****generate standardized  data and indices***************
****indices*****
egen zhdi=std(HDI) ,m(3) s(1)
egen zwhr=std(LifeLadder), m(3) s(1)
egen zsdg=std(SDGIndexScore), m(3) s(1)


*****generate CES functions*****

gen sigma=1.0000001
gen sigma1=0.1
gen sigma3=3

****use Lifeladder index **************
gen ceslsdual=((1/2)^(1/sigma)*zhdi^((sigma-1)/sigma)+(1/2)^(1/sigma)*zwhr^((sigma-1)/sigma))^(sigma/(sigma-1))
gsort - ceslsdual, generate(ceslsdualrank)
gen ceslsdual1=((1/2)^(1/sigma1)*zhdi^((sigma1-1)/sigma1)+(1/2)^(1/sigma1)*zwhr^((sigma1-1)/sigma1))^(sigma1/(sigma1-1))
gsort - ceslsdual1, generate(ceslsdual1rank)
gen ceslsdual3=((1/2)^(1/sigma3)*zhdi^((sigma3-1)/sigma3)+(1/2)^(1/sigma3)*zwhr^((sigma3-1)/sigma3))^(sigma3/(sigma3-1))
gsort - ceslsdual3, generate(ceslsdual3rank)

****use sdg index with whr**************
gen cesgdual=((1/2)^(1/sigma)*zsdg^((sigma-1)/sigma)+(1/2)^(1/sigma)*zwhr^((sigma-1)/sigma))^(sigma/(sigma-1))
gsort - cesgdual, generate(cesgdualrank)
gen cesgdual1=((1/2)^(1/sigma1)*zsdg^((sigma1-1)/sigma1)+(1/2)^(1/sigma1)*zwhr^((sigma1-1)/sigma1))^(sigma1/(sigma1-1))
gsort - cesgdual1, generate(cesgdual1rank)
gen cesgdual3=((1/2)^(1/sigma3)*zsdg^((sigma3-1)/sigma3)+(1/2)^(1/sigma3)*zwhr^((sigma3-1)/sigma3))^(sigma3/(sigma3-1))
gsort - cesgdual3, generate(cesgdual3rank)


*** sort create rank and break ties according to GDP*********
gsort - HDI LGDP, generate(hdirank)
gsort - SDGIndexScore LGDP, generate(sdgrank)
gsort - zwhr LGDP, generate(whrrank)


*** Generate rank differntials*********************
generate difs=ceslsdual1rank-hdirank

generate difshighlow= ceslsdual3rank-ceslsdual1rank
gener adifshighlow=abs( difshighlow)

generate difghighlow= cesgdual3rank-cesgdual1rank
gener adifghighlow=abs( difghighlow)

***** generate standard errors for significance test ****

egen stdadifshighlow=sd(adifghighlow)
egen stdadifghighlow=sd(adifghighlow)
gener sigadifshighlow=1 if adifshighlow>=2* stdadifshighlow
gener sigadifghighlow=1 if adifghighlow>=2* stdadifghighlow

***** graphs********


***** twoway plots********

***Figure II HDI versus WHR********
label variable hdirank "HDI rank"
label variable whrrank "WHR rank"
twoway  (scatter hdirank whrrank) (line whrrank whrrank)


*** Figure III HDI versus dual********
label variable ceslsdual3rank "Dual necessity rank (s=3)"
label variable ceslsdual1rank "Dual necessity rank (s=0.1)"

twoway (scatter ceslsdual3rank hdirank ) (scatter ceslsdual1rank hdirank) (line hdirank hdirank)

**** Figure IV differences in country rankings according to CES function with high and low substitution by HDI ranking****
label variable difshighlow "Difference between high and low subsitution ranking"
twoway (scatter difshighlow hdirank )


*** Figure V SDG versus WHR********
label variable sdgrank "SDG rank"
label variable whrrank "WHR rank"
twoway  (scatter sdgrank whrrank) (line whrrank whrrank)

*** Figure VI SDG versus dual********
label variable cesgdual3rank "Dual necessity rank (s=3)"
label variable cesgdual1rank "Dual necessity rank (s=0.1)"

twoway (scatter cesgdual3rank sdgrank ) (scatter cesgdual1rank sdgrank) (line sdgrank sdgrank)

**** Figure VII differences in country rankings according to CES function with high and low substitution by SDG index ranking
label variable difghighlow "Difference between high and low subsitution ranking"
twoway (scatter difghighlow sdgrank ) 

*****correlation matrices*****
xtile whrquart = whrrank, nq(4)
sort whrquart
ktau hdirank whrrank
by whrquart: ktau hdirank whrrank
ktau sdgrank whrrank
by whrquart: ktau sdgrank whrrank

*****Quartile differences*****
xtile hdiquart = hdirank, nq(4)
sort hdiquart
gen difshighlowabs=abs(difshighlow)
by hdiquart: tabstat difshighlowabs, s(mean)
xtile sdgquart = sdgrank, nq(4)
sort sdgquart
gen difghighlowabs=abs(difghighlow)
by sdgquart: tabstat difghighlowabs, s(mean)

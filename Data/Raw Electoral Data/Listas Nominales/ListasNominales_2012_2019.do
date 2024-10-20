/////////////////////////////////////////
//////LISTAS NOMINALES 2012-2019/////////
//////SALVADOR ASCENCIO//////////////////
//////JUL 2019///////////////////////////
/////////////////////////////////////////


///////////
///2012////
///////////
clear
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2012"
local txtfiles : dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2012" files "*.txt"
foreach file of local txtfiles {
 insheet using "`file'", clear
 local name : subinstr local file ".txt" ""
 gen file = "`name'" 
 
 save "`name'.dta", replace
}

local statafiles: dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2012" files "*.dta"
append using `statafiles' 
save LN2012.dta , replace

gen year=2012
gen month=substr(file,8,2)
destring month, replace
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+municipio
save LN2012.dta , replace


///////////
///2013////
///////////
clear
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2013"
local txtfiles : dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2013" files "*.txt"
foreach file of local txtfiles {
 insheet using "`file'", clear
 local name : subinstr local file ".txt" ""
 gen file = "`name'"
 
 save "`name'.dta", replace
}

local statafiles: dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2013" files "*.dta"
append using `statafiles' 
save LN2013.dta , replace

gen year=2013
gen month=substr(file,8,2)
destring month, replace
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+municipio
save LN2013.dta , replace


///////////
///2014////
///////////
clear
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2014"
local txtfiles : dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2014" files "*.txt"
foreach file of local txtfiles {
 insheet using "`file'", clear
 local name : subinstr local file ".txt" ""
 gen file = "`name'" 
 
 save "`name'.dta", replace
}

local statafiles: dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2014" files "*.dta"
append using `statafiles' 
save LN2014.dta , replace

gen year=2014
gen month=substr(file,8,2)
destring month, replace
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+municipio
save LN2014.dta , replace


///////////
///2015////
///////////
clear
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2015"
local txtfiles : dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2015" files "*.txt"
foreach file of local txtfiles {
 insheet using "`file'", clear
 local name : subinstr local file ".txt" ""
 gen file = "`name'" 
 
 save "`name'.dta", replace
}

local statafiles: dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2015" files "*.dta"
append using `statafiles' 
save LN2015.dta , replace

gen year=2015
gen month=substr(file,8,2)
destring month, replace
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+municipio
save LN2015.dta , replace


///////////
///2016////
///////////
clear
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2016"
local txtfiles : dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2016" files "*.txt"
foreach file of local txtfiles {
 insheet using "`file'", clear 
 local name : subinstr local file ".txt" ""
 gen file = "`name'" 
 
 save "`name'.dta", replace
}

local statafiles: dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2016" files "*.dta"
append using `statafiles' 
save LN2016.dta , replace

gen year=2016
gen month=substr(file,8,2)
destring month, replace
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+municipio
save LN2016.dta , replace


///////////
///2017////
///////////
clear
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2017"
local txtfiles : dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2017" files "*.txt"
foreach file of local txtfiles {
 insheet using "`file'", clear
 local name : subinstr local file ".txt" ""
 gen file = "`name'" 
 
 save "`name'.dta", replace
}

local statafiles: dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2017" files "*.dta"
append using `statafiles' 
save LN2017.dta , replace

gen year=2017
gen month=substr(file,8,2)
destring month, replace
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+municipio
save LN2017.dta , replace



///////////
///2018////
///////////
clear
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2018"
local txtfiles : dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2018" files "*.txt"
foreach file of local txtfiles {
 insheet using "`file'", clear
 local name : subinstr local file ".txt" ""
 gen file = "`name'" 
 
 save "`name'.dta", replace
}

local statafiles: dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2018" files "*.dta"
append using `statafiles' 
save LN2018.dta , replace

gen year=2018
gen month=substr(file,8,2)
destring month, replace
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+municipio
save LN2018.dta , replace


///////////
///2019////
///////////
clear
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2019"
local txtfiles : dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2019" files "*.txt"
foreach file of local txtfiles {
 insheet using "`file'", clear
 local name : subinstr local file ".txt" ""
 gen file = "`name'" 
 
 save "`name'.dta", replace
}

local statafiles: dir "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales\LN 2012-2019\2019" files "*.dta"
append using `statafiles' 
save LN2019.dta , replace

gen year=2019
gen month=substr(file,8,2)
destring month, replace
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+municipio
save LN2019.dta , replace


















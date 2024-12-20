////////////////////////////////////////////
////////YUCATAN PRECINCT PANEL UPDATE///////
////////JUL 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Yucatan"

import excel "communal candidates 2015.xlsx", sheet("Ayuntamientos") firstrow clear
rename CC* comun*
save "merge.dta", replace

import excel "Ayuntamientos_Yuc_2015.xlsx", sheet("REGIDORES") clear firstrow 

merge m:1 municipality using "merge.dta"
erase "merge.dta"

gen   uniqueid= 0
replace uniqueid=31001 if municipality =="ABALÁ"
replace uniqueid=31002 if municipality =="ACANCEH"
replace uniqueid=31003 if municipality =="AKIL"
replace uniqueid=31004 if municipality =="BACA"
replace uniqueid=31005 if municipality =="BOKOBÁ"
replace uniqueid=31006 if municipality =="BUCTZOTZ"
replace uniqueid=31007 if municipality =="CACALCHÉN"
replace uniqueid=31008 if municipality =="CALOTMUL"
replace uniqueid=31009 if municipality =="CANSAHCAB"
replace uniqueid=31010 if municipality =="CANTAMAYEC"
replace uniqueid=31011 if municipality =="CELESTÚN"
replace uniqueid=31012 if municipality =="CENOTILLO"
replace uniqueid=31016 if municipality =="CHACSINKÍN"
replace uniqueid=31017 if municipality =="CHANKOM"
replace uniqueid=31018 if municipality =="CHAPAB"
replace uniqueid=31019 if municipality =="CHEMAX"
replace uniqueid=31021 if municipality =="CHICHIMILÁ"
replace uniqueid=31020 if municipality =="CHICXULUB PUEBLO"
replace uniqueid=31022 if municipality =="CHIKINDZONOT"
replace uniqueid=31023 if municipality =="CHOCHOLÁ"
replace uniqueid=31024 if municipality =="CHUMAYEL"
replace uniqueid=31013 if municipality =="CONKAL"
replace uniqueid=31014 if municipality =="CUNCUNUL"
replace uniqueid=31015 if municipality =="CUZAMÁ"
replace uniqueid=31025 if municipality =="DZÁN"
replace uniqueid=31026 if municipality =="DZEMUL"
replace uniqueid=31027 if municipality =="DZIDZANTÚN"
replace uniqueid=31028 if municipality =="DZILAM BRAVO"
replace uniqueid=31029 if municipality =="DZILAM GONZÁLEZ"
replace uniqueid=31030 if municipality =="DZITÁS"
replace uniqueid=31031 if municipality =="DZONCAUICH"
replace uniqueid=31032 if municipality =="ESPITA"
replace uniqueid=31033 if municipality =="HALACHÓ"
replace uniqueid=31034 if municipality =="HOCABÁ"
replace uniqueid=31035 if municipality =="HOCTÚN"
replace uniqueid=31036 if municipality =="HOMÚN"
replace uniqueid=31037 if municipality =="HUHÍ"
replace uniqueid=31038 if municipality =="HUNUCMÁ"
replace uniqueid=31039 if municipality =="IXIL"
replace uniqueid=31040 if municipality =="IZAMAL"
replace uniqueid=31041 if municipality =="KANASÍN"
replace uniqueid=31042 if municipality =="KANTUNIL"
replace uniqueid=31043 if municipality =="KAUA"
replace uniqueid=31044 if municipality =="KINCHIL"
replace uniqueid=31045 if municipality =="KOPOMÁ"
replace uniqueid=31046 if municipality =="MAMA"
replace uniqueid=31047 if municipality =="MANÍ"
replace uniqueid=31048 if municipality =="MAXCANÚ"
replace uniqueid=31049 if municipality =="MAYAPAN"
replace uniqueid=31050 if municipality =="MERIDA"
replace uniqueid=31051 if municipality =="MOCOCHÁ"
replace uniqueid=31052 if municipality =="MOTUL"
replace uniqueid=31053 if municipality =="MUNA"
replace uniqueid=31054 if municipality =="MUXUPIP"
replace uniqueid=31055 if municipality =="OPICHEN"
replace uniqueid=31056 if municipality =="OXKUTZCAB"
replace uniqueid=31057 if municipality =="PANABÁ"
replace uniqueid=31058 if municipality =="PETO"
replace uniqueid=31059 if municipality =="PROGRESO"
replace uniqueid=31060 if municipality =="QUINTANA ROO"
replace uniqueid=31061 if municipality =="RÍO LAGARTOS"
replace uniqueid=31062 if municipality =="SACALUM"
replace uniqueid=31063 if municipality =="SAMAHIL"
replace uniqueid=31065 if municipality =="SAN FELIPE"
replace uniqueid=31064 if municipality =="SANAHCAT"
replace uniqueid=31066 if municipality =="SANTA ELENA"
replace uniqueid=31067 if municipality =="SEYÉ"
replace uniqueid=31068 if municipality =="SINANCHE"
replace uniqueid=31069 if municipality =="SOTUTA"
replace uniqueid=31070 if municipality =="SUCILÁ"
replace uniqueid=31071 if municipality =="SUDZAL"
replace uniqueid=31072 if municipality =="SUMA DE HIDALGO"
replace uniqueid=31073 if municipality =="TAHDZIÚ"
replace uniqueid=31074 if municipality =="TAHMEK"
replace uniqueid=31075 if municipality =="TEABO"
replace uniqueid=31076 if municipality =="TECOH"
replace uniqueid=31077 if municipality =="TEKAL DE VENEGAS"
replace uniqueid=31078 if municipality =="TEKANTÓ"
replace uniqueid=31079 if municipality =="TEKAX"
replace uniqueid=31080 if municipality =="TEKIT"
replace uniqueid=31081 if municipality =="TEKOM"
replace uniqueid=31082 if municipality =="TELCHAC PUEBLO"
replace uniqueid=31083 if municipality =="TELCHAC PUERTO"
replace uniqueid=31084 if municipality =="TEMAX"
replace uniqueid=31085 if municipality =="TEMOZÓN"
replace uniqueid=31086 if municipality =="TEPAKÁN"
replace uniqueid=31087 if municipality =="TETIZ"
replace uniqueid=31088 if municipality =="TEYA"
replace uniqueid=31089 if municipality =="TICUL"
replace uniqueid=31090 if municipality =="TIMUCUY"
replace uniqueid=31091 if municipality =="TINUM"
replace uniqueid=31092 if municipality =="TIXCACALCUPUL"
replace uniqueid=31093 if municipality =="TIXKOKOB"
replace uniqueid=31094 if municipality =="TIXMÉHUAC"
replace uniqueid=31095 if municipality =="TIXPÉHUAL"
replace uniqueid=31096 if municipality =="TIZIMÍN"
replace uniqueid=31097 if municipality =="TUNKÁS"
replace uniqueid=31098 if municipality =="TZUCACAB"
replace uniqueid=31099 if municipality =="UAYMA"
replace uniqueid=31100 if municipality =="UCÚ"
replace uniqueid=31101 if municipality =="UMÁN"
replace uniqueid=31102 if municipality =="VALLADOLID"
replace uniqueid=31103 if municipality =="XOCCHEL"
replace uniqueid=31104 if municipality =="YAXCABÁ"
replace uniqueid=31105 if municipality =="YAXKUKUL"
replace uniqueid=31106 if municipality =="YOBAÍN"

drop if section==.

collapse (sum) PAN-CI_1 total (first) comun*, by (municipality uniqueid section)

g PRI_PANAL=.
replace PRI_PANAL=PRI + PANAL + CC1 if comun1=="PRI_PANAL"

g PRI_PANAL_PES=.
replace PRI_PANAL_PES=PRI + PANAL + PES + CC1 if comun1=="PRI_PANAL_PES"

g PRI_PANAL_PH_PES=.
replace PRI_PANAL_PH_PES=PRI + PANAL + PES + PH + CC1 if comun1=="PRI_PANAL_PH_PES"

g PRI_PES=.
replace PRI_PES=PRI + PES + CC1 if comun1=="PRI_PES"

g PRI_PH=.
replace PRI_PH=PRI + PH + CC1 if comun1=="PRI_PH"

g PRI_PH_PES=.
replace PRI_PH_PES=PRI + PES + PH + CC1 if comun1=="PRI_PH_PES"

g PRI_PVEM=.
replace PRI_PVEM=PRI + PVEM + CC1 if comun1=="PRI_PVEM"

g PRI_PVEM_PANAL=.
replace PRI_PVEM_PANAL=PRI + PANAL + PVEM + CC1 if comun1=="PRI_PVEM_PANAL"

g PRI_PVEM_PANAL_PES=.
replace PRI_PVEM_PANAL_PES=PRI + PANAL + PVEM + PES + CC1 if comun1=="PRI_PVEM_PANAL_PES"

g PRI_PVEM_PANAL_PH=.
replace PRI_PVEM_PANAL_PH=PRI + PANAL + PVEM + PH + CC1 if comun1=="PRI_PVEM_PANAL_PH"

g PRI_PVEM_PANAL_PH_PES=.
replace PRI_PVEM_PANAL_PH_PES=PRI + PANAL + PVEM + PES + PH + CC1 if comun1=="PRI_PVEM_PANAL_PH_PES"

g PRI_PVEM_PES=.
replace PRI_PVEM_PES=PRI + PVEM + PES + CC1 if comun1=="PRI_PVEM_PES"

g PRI_PVEM_PH_PES=.
replace PRI_PVEM_PH_PES=PRI + PVEM + PES + PH + CC1 if comun1=="PRI_PVEM_PH_PES"

drop PRI
foreach x in PVEM PANAL PH PES {
	replace `x'=. if strpos(comun1,"`x'")>0
}

g PAN_PANAL=.
replace PAN_PANAL= PAN + PANAL + CC2 if comun2=="PAN_PANAL"

g PAN_PRD=.
replace PAN_PRD= PAN + PRD + CC2 if comun2=="PAN_PRD"

g PAN_PRD_PT=.
replace PAN_PRD_PT= PAN + PRD + PT + CC2 if comun2=="PAN_PRD_PT"

g PRD_PT=.
replace PRD_PT= PRD + PT + CC2 if comun2=="PRD_PT"

g PRD_PT_PANAL=.
replace PRD_PT_PANAL= PAN + PRD + PT + PANAL + CC2 if comun2=="PRD_PT_PANAL"


foreach var in PANAL PRD PT {
	replace `var'=. if strpos(comun2,"`var'")>0
}
replace PAN=. if comun2=="PAN_PANAL" | comun2=="PAN_PRD" | comun2=="PAN_PRD_PT"

drop comun* CC*

order PRI_PANAL-PRD_PT_PANAL, b(CI_1)

egen valid= rowtotal(PAN PRD PVEM PT MC PANAL MORENA PH PES PRI_PANAL PRI_PANAL_PES PRI_PANAL_PH_PES PRI_PES PRI_PH PRI_PH_PES PRI_PVEM PRI_PVEM_PANAL PRI_PVEM_PANAL_PES ///
	PRI_PVEM_PANAL_PH PRI_PVEM_PANAL_PH_PES PRI_PVEM_PES PRI_PVEM_PH_PES PAN_PANAL PAN_PRD PAN_PRD_PT PRD_PT PRD_PT_PANAL CI_1)

foreach var in PAN PRD PVEM PT MC PANAL MORENA PH PES PRI_PANAL PRI_PANAL_PES PRI_PANAL_PH_PES PRI_PES PRI_PH PRI_PH_PES PRI_PVEM PRI_PVEM_PANAL PRI_PVEM_PANAL_PES ///
	PRI_PVEM_PANAL_PH PRI_PVEM_PANAL_PH_PES PRI_PVEM_PES PRI_PVEM_PH_PES PAN_PANAL PAN_PRD PAN_PRD_PT PRD_PT PRD_PT_PANAL CI_1 valid total {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PH i_PES i_PRI_PANAL i_PRI_PANAL_PES i_PRI_PANAL_PH_PES i_PRI_PES i_PRI_PH i_PRI_PH_PES i_PRI_PVEM i_PRI_PVEM_PANAL i_PRI_PVEM_PANAL_PES ///
	i_PRI_PVEM_PANAL_PH i_PRI_PVEM_PANAL_PH_PES i_PRI_PVEM_PES i_PRI_PVEM_PH_PES i_PAN_PANAL i_PAN_PRD i_PAN_PRD_PT i_PRD_PT i_PRD_PT_PANAL i_CI_1, ///
	gen(PAN_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PH_r PES_r PRI_PANAL_r PRI_PANAL_PES_r PRI_PANAL_PH_PES_r PRI_PES_r PRI_PH_r PRI_PH_PES_r PRI_PVEM_r PRI_PVEM_PANAL_r PRI_PVEM_PANAL_PES_r ///
	PRI_PVEM_PANAL_PH_r PRI_PVEM_PANAL_PH_PES_r PRI_PVEM_PES_r PRI_PVEM_PH_PES_r PAN_PANAL_r PAN_PRD_r PAN_PRD_PT_r PRD_PT_r PRD_PT_PANAL_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRD PVEM PT MC PANAL MORENA PH PES PRI_PANAL PRI_PANAL_PES PRI_PANAL_PH_PES PRI_PES PRI_PH PRI_PH_PES PRI_PVEM PRI_PVEM_PANAL PRI_PVEM_PANAL_PES ///
	PRI_PVEM_PANAL_PH PRI_PVEM_PANAL_PH_PES PRI_PVEM_PES PRI_PVEM_PH_PES PAN_PANAL PAN_PRD PAN_PRD_PT PRD_PT PRD_PT_PANAL CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1"

g year=2015
g month="June"
g STATE="YUCATAN"

preserve
use "..\Listas Nominales\LN 2012-2019\2015\LN2015.dta", clear
*Lista nominal at the last date before the election (June 30 2015) 
keep entidad municipio seccion lista file year month
keep if entidad==31 & month==6
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN15_YUC.dta, replace
restore

merge 1:1 section using LN15_YUC.dta
drop if _merge==2
drop _merge
erase LN15_YUC.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g mun_turnout = mun_total/mun_listanominal
g turnout = total/listanominal

order STATE municipality uniqueid section year month *

save Yucatan_Section_2015.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-valid listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Yucatan_2015.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

import excel "Ayuntamientos_Yuc_2018.xlsx", sheet("RESULTADOS") clear firstrow

drop uniqueid
gen   uniqueid= 0
replace uniqueid=31001 if municipality =="ABALA"
replace uniqueid=31002 if municipality =="ACANCEH"
replace uniqueid=31003 if municipality =="AKIL"
replace uniqueid=31004 if municipality =="BACA"
replace uniqueid=31005 if municipality =="BOKOBA"
replace uniqueid=31006 if municipality =="BUCTZOTZ"
replace uniqueid=31007 if municipality =="CACALCHEN"
replace uniqueid=31008 if municipality =="CALOTMUL"
replace uniqueid=31009 if municipality =="CANSAHCAB"
replace uniqueid=31010 if municipality =="CANTAMAYEC"
replace uniqueid=31011 if municipality =="CELESTUN"
replace uniqueid=31012 if municipality =="CENOTILLO"
replace uniqueid=31016 if municipality =="CHACSINKIN"
replace uniqueid=31017 if municipality =="CHANKOM"
replace uniqueid=31018 if municipality =="CHAPAB"
replace uniqueid=31019 if municipality =="CHEMAX"
replace uniqueid=31021 if municipality =="CHICHIMILA"
replace uniqueid=31020 if municipality =="CHICXULUB PUEBLO"
replace uniqueid=31022 if municipality =="CHIKINDZONOT"
replace uniqueid=31023 if municipality =="CHOCHOLA"
replace uniqueid=31024 if municipality =="CHUMAYEL"
replace uniqueid=31013 if municipality =="CONKAL"
replace uniqueid=31014 if municipality =="CUNCUNUL"
replace uniqueid=31015 if municipality =="CUZAMA"
replace uniqueid=31025 if municipality =="DZAN"
replace uniqueid=31026 if municipality =="DZEMUL"
replace uniqueid=31027 if municipality =="DZIDZANTUN"
replace uniqueid=31028 if municipality =="DZILAM DE BRAVO"
replace uniqueid=31029 if municipality =="DZILAM GONZALEZ"
replace uniqueid=31030 if municipality =="DZITAS"
replace uniqueid=31031 if municipality =="DZONCAUICH"
replace uniqueid=31032 if municipality =="ESPITA"
replace uniqueid=31033 if municipality =="HALACHO"
replace uniqueid=31034 if municipality =="HOCABA"
replace uniqueid=31035 if municipality =="HOCTUN"
replace uniqueid=31036 if municipality =="HOMUN"
replace uniqueid=31037 if municipality =="HUHI"
replace uniqueid=31038 if municipality =="HUNUCMA"
replace uniqueid=31039 if municipality =="IXIL"
replace uniqueid=31040 if municipality =="IZAMAL"
replace uniqueid=31041 if municipality =="KANASIN"
replace uniqueid=31042 if municipality =="KANTUNIL"
replace uniqueid=31043 if municipality =="KAUA"
replace uniqueid=31044 if municipality =="KINCHIL"
replace uniqueid=31045 if municipality =="KOPOMA"
replace uniqueid=31046 if municipality =="MAMA"
replace uniqueid=31047 if municipality =="MANI"
replace uniqueid=31048 if municipality =="MAXCANU"
replace uniqueid=31049 if municipality =="MAYAPAN"
replace uniqueid=31050 if municipality =="MERIDA"
replace uniqueid=31051 if municipality =="MOCOCHA"
replace uniqueid=31052 if municipality =="MOTUL"
replace uniqueid=31053 if municipality =="MUNA"
replace uniqueid=31054 if municipality =="MUXUPIP"
replace uniqueid=31055 if municipality =="OPICHEN"
replace uniqueid=31056 if municipality =="OXKUTZCAB"
replace uniqueid=31057 if municipality =="PANABA"
replace uniqueid=31058 if municipality =="PETO"
replace uniqueid=31059 if municipality =="PROGRESO"
replace uniqueid=31060 if municipality =="QUINTANA ROO"
replace uniqueid=31061 if municipality =="RIO LAGARTOS"
replace uniqueid=31062 if municipality =="SACALUM"
replace uniqueid=31063 if municipality =="SAMAHIL"
replace uniqueid=31065 if municipality =="SAN FELIPE"
replace uniqueid=31064 if municipality =="SANAHCAT"
replace uniqueid=31066 if municipality =="SANTA ELENA"
replace uniqueid=31067 if municipality =="SEYE"
replace uniqueid=31068 if municipality =="SINANCHE"
replace uniqueid=31069 if municipality =="SOTUTA"
replace uniqueid=31070 if municipality =="SUCILA"
replace uniqueid=31071 if municipality =="SUDZAL"
replace uniqueid=31072 if municipality =="SUMA"
replace uniqueid=31073 if municipality =="TAHDZIU"
replace uniqueid=31074 if municipality =="TAHMEK"
replace uniqueid=31075 if municipality =="TEABO"
replace uniqueid=31076 if municipality =="TECOH"
replace uniqueid=31077 if municipality =="TEKAL DE VENEGAS"
replace uniqueid=31078 if municipality =="TEKANTO"
replace uniqueid=31079 if municipality =="TEKAX"
replace uniqueid=31080 if municipality =="TEKIT"
replace uniqueid=31081 if municipality =="TEKOM"
replace uniqueid=31082 if municipality =="TELCHAC PUEBLO"
replace uniqueid=31083 if municipality =="TELCHAC PUERTO"
replace uniqueid=31084 if municipality =="TEMAX"
replace uniqueid=31085 if municipality =="TEMOZON"
replace uniqueid=31086 if municipality =="TEPAKAN"
replace uniqueid=31087 if municipality =="TETIZ"
replace uniqueid=31088 if municipality =="TEYA"
replace uniqueid=31089 if municipality =="TICUL"
replace uniqueid=31090 if municipality =="TIMUCUY"
replace uniqueid=31091 if municipality =="TINUM"
replace uniqueid=31092 if municipality =="TIXCACALCUPUL"
replace uniqueid=31093 if municipality =="TIXKOKOB"
replace uniqueid=31094 if municipality =="TIXMEHUAC"
replace uniqueid=31095 if municipality =="TIXPEUAL"
replace uniqueid=31096 if municipality =="TIZIMIN"
replace uniqueid=31097 if municipality =="TUNKAS"
replace uniqueid=31098 if municipality =="TZUCACAB"
replace uniqueid=31099 if municipality =="UAYMA"
replace uniqueid=31100 if municipality =="UCU"
replace uniqueid=31101 if municipality =="UMAN"
replace uniqueid=31102 if municipality =="VALLADOLID"
replace uniqueid=31103 if municipality =="XOCCHEL"
replace uniqueid=31104 if municipality =="YAXCABA"
replace uniqueid=31105 if municipality =="YAXKUKUL"
replace uniqueid=31106 if municipality =="YOBAIN"

collapse (sum) PAN-CI_1 listanominal total, by (municipality uniqueid section)

preserve
import excel "computosRegidores2018-130918.xlsx", sheet("REGIDURIAS") cellrange(A3:AE109) firstrow clear
keep MUNICIPIO PANPRDMOV-MORENAPES
foreach x of varlist PANPRDMOV-MORENAPES {
	replace `x' = "1" if `x'!="NA"
	replace `x' = "0" if `x'=="NA"
}
destring _all, replace
rename * coal_*
rename coal_MUNICIPIO municipality
replace coal_PANPRD = 0 if coal_PANPRDMOV==1
replace coal_PANMOV = 0 if coal_PANPRDMOV==1
replace coal_PRDMC = 0 if coal_PANPRDMOV==1
rename coal_PANPRDMOV coal_PANPRDMC
rename coal_PANMOV coal_PANMC
replace coal_PRIPVEM = 0 if coal_PRIPVEMPANAL==1
replace coal_PRIPANAL = 0 if coal_PRIPVEMPANAL==1
replace coal_PVEMPANAL = 0 if coal_PRIPVEMPANAL==1
replace coal_PTMORENA = 0 if coal_PTMORENAPES==1
replace coal_PTPES = 0 if coal_PTMORENAPES==1
drop coal_MORENAPES
replace municipality = subinstr(municipality, "Á", "A", .)
replace municipality = subinstr(municipality, "É", "E", .)
replace municipality = subinstr(municipality, "Í", "I", .)
replace municipality = subinstr(municipality, "Ó", "O", .)
replace municipality = subinstr(municipality, "Ú", "U", .)
save coalindicators.dta, replace
restore

replace municipality = "SUMA DE HIDALGO" if municipality=="SUMA"
merge m:1 municipality using coalindicators.dta
drop _merge
erase coalindicators.dta

rename *NVA_ALIANZA *PANAL

replace PAN_PRD_MC = PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC if coal_PANPRDMC==1
replace PAN_PRD_MC = . if coal_PANPRDMC==0
replace PAN=. if coal_PANPRDMC==1
replace PRD=. if coal_PANPRDMC==1
replace MC=. if coal_PANPRDMC==1
replace PAN_PRD=. if coal_PANPRDMC==1
replace PAN_MC=. if coal_PANPRDMC==1
drop PRD_MC

replace PAN_PRD=PAN_PRD + PAN + PRD if coal_PANPRD==1
replace PAN_PRD=. if coal_PANPRD==0
replace PAN=. if coal_PANPRD==1
replace PRD=. if coal_PANPRD==1

replace PAN_MC=PAN_MC + PAN + MC if coal_PANMC==1
replace PAN_MC=. if coal_PANMC==0
replace PAN=. if coal_PANMC==1
replace MC=. if coal_PANMC==1

replace PAN_PANAL=PAN_PANAL + PAN + PANAL if coal_PANPANAL==1
replace PAN_PANAL=. if coal_PANPANAL==0
replace PAN=. if coal_PANPANAL==1
replace PANAL=. if coal_PANPANAL==1

replace PRI_PVEM_PANAL=PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL if coal_PRIPVEMPANAL==1
replace PRI_PVEM_PANAL=. if coal_PRIPVEMPANAL==0
replace PRI_PVEM=. if coal_PRIPVEMPANAL==1
replace PRI_PANAL=. if coal_PRIPVEMPANAL==1
replace PVEM_PANAL=. if coal_PRIPVEMPANAL==1
replace PRI=. if coal_PRIPVEMPANAL==1
replace PVEM=. if coal_PRIPVEMPANAL==1
replace PANAL=. if coal_PRIPVEMPANAL==1

replace PRI_PANAL=PRI_PANAL + PRI + PANAL if coal_PRIPANAL==1
replace PRI_PANAL=. if coal_PRIPANAL==0
replace PRI=. if coal_PRIPANAL==1
replace PANAL=. if coal_PRIPANAL==1

replace PVEM_PANAL=PVEM_PANAL + PVEM + PANAL if coal_PVEMPANAL==1
replace PVEM_PANAL=. if coal_PVEMPANAL==0
replace PVEM=. if coal_PVEMPANAL==1
replace PANAL=. if coal_PVEMPANAL==1

replace PRI_PVEM=PRI_PVEM + PRI + PVEM if coal_PRIPVEM==1
replace PRI_PVEM=. if coal_PRIPVEM==0
replace PRI=. if coal_PRIPVEM==1
replace PVEM=. if coal_PRIPVEM==1

replace PT_MORENA_PES = PT_MORENA_PES + PT_MORENA + PT_PES + MORENA_PES + PT + MORENA + PES if coal_PTMORENAPES==1
replace PT_MORENA_PES = . if coal_PTMORENAPES==0
replace PT =. if coal_PTMORENAPES==1
replace MORENA=. if coal_PTMORENAPES==1
replace PES=. if coal_PTMORENAPES==1
drop MORENA_PES  

replace PT_MORENA=PT + MORENA + PT_MORENA if coal_PTMORENA==1
replace PT_MORENA=. if coal_PTMORENA==0
replace PT=. if coal_PVEMPANAL==1
replace MORENA=. if coal_PVEMPANAL==1

replace PT_PES=PT + PES + PT_PES if coal_PTPES==1
replace PT_PES=. if coal_PTPES==0
replace PT=. if coal_PTPES==1
replace PES=. if coal_PTPES==1

drop coal_*
     
egen valid = rowtotal(PAN PRI PRD PVEM PT MC PANAL MORENA PES PAN_PRD_MC PAN_PRD PAN_MC PAN_PANAL PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL PT_MORENA_PES PT_MORENA PT_PES CI_1)

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES PAN_PRD_MC PAN_PRD PAN_MC PAN_PANAL PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL PT_MORENA_PES PT_MORENA PT_PES CI_1 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
	replace mun_`var' = . if `var'==.
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PES i_PAN_PRD_MC i_PAN_PRD i_PAN_MC i_PAN_PANAL i_PRI_PVEM_PANAL i_PRI_PVEM i_PRI_PANAL i_PVEM_PANAL i_PT_MORENA_PES i_PT_MORENA i_PT_PES i_CI_1, ///
	gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PES_r PAN_PRD_MC_r PAN_PRD_r PAN_MC_r PAN_PANAL_r PRI_PVEM_PANAL_r PRI_PVEM_r PRI_PANAL_r PVEM_PANAL_r PT_MORENA_PES_r PT_MORENA_r PT_PES_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES PAN_PRD_MC PAN_PRD PAN_MC PAN_PANAL PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL PT_MORENA_PES PT_MORENA PT_PES CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1"

g year=2018
g month="July"
g STATE="YUCATAN"

save Yucatan_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) listanominal-valid (first) STATE year month winner second third mun_turnout, by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Yucatan_2018.dta", replace
 
****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

clear
append using Yucatan_Section_2015.dta
append using Yucatan_Section_2018.dta

erase Yucatan_Section_2015.dta
erase Yucatan_Section_2018.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}
tab winner_counter
count if winner_counter==0
drop winner_counter

save Yucatan_Section_15_18.dta, replace

use "..\..\Precinct\Yucatan_ALL.dta", clear
append using Yucatan_Section_15_18.dta

erase Yucatan_Section_15_18.dta

replace municipality = upper(municipality)

save Yucatan_ALL_SALVADOR.dta, replace 

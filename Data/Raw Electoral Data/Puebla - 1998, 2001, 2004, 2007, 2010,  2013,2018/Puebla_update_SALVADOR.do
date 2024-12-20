////////////////////////////////////////////
////////PUEBLA PRECINCT PANEL UPDATE////////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Puebla"

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************
*2014 extraordinary election for Acajete and Cuapiaxtla de Madero 

import excel "FINAL_Ayu_Ext_2014.xlsx", sheet("2014") clear firstrow

replace uniqueid=21001 if municipality =="Acajete"
replace uniqueid=21038 if municipality =="Cuapiaxtla de Madero"
replace municipality = "ACAJETE EXTRAORDINARIO" if municipality=="Acajete"
replace municipality = "CUAPIAXTLA DE MADERO EXTRAORDINARIO" if municipality=="Cuapiaxtla de Madero"

collapse (sum) listanominal total MC-PRI_PVEM, by (municipality section uniqueid)

egen valid=rowtotal(PRI_PVEM PAN_PRD_PANAL_CP_PSI MC)

foreach var in PRI_PVEM PAN_PRD_PANAL_CP_PSI MC total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout=valid/listanominal
g mun_turnout=mun_valid/mun_listanominal

rowranks i_PRI_PVEM i_PAN_PRD_PANAL_CP_PSI i_MC, gen(PRI_PVEM_r PAN_PRD_PANAL_CP_PSI_r MC_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI_PVEM PAN_PRD_PANAL_CP_PSI MC {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

gen year=2014
gen month="July"
gen STATE="PUEBLA"

save Puebla_Section_2014.dta, replace

collapse (first) winner, by (municipality uniqueid)
rename winner incumbent

save incumbents2018b.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Puebla_Section_2014.dta, clear
collapse (sum) listanominal-valid (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "D:\Dropbox\Salvador Ascencio\Update Municipal\Puebla_2014.dta", replace

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

import excel "Ayuntamientos_2018.xlsx", sheet("Ayuntamientos") clear firstrow 

replace municipality = proper(municipality)
drop uniqueid
gen   uniqueid= 0
replace uniqueid=21001 if municipality =="Acajete"
replace uniqueid=21002 if municipality =="Acateno"
replace uniqueid=21003 if municipality =="Acatlan"
replace uniqueid=21004 if municipality =="Acatzingo"
replace uniqueid=21005 if municipality =="Acteopan"
replace uniqueid=21006 if municipality =="Ahuacatlan"
replace uniqueid=21007 if municipality =="Ahuatlan"
replace uniqueid=21008 if municipality =="Ahuazotepec"
replace uniqueid=21009 if municipality =="Ahuehuetitla"
replace uniqueid=21010 if municipality =="Ajalpan"
replace uniqueid=21011 if municipality =="Albino Zertuche"
replace uniqueid=21012 if municipality =="Aljojuca"
replace uniqueid=21013 if municipality =="Altepexi"
replace uniqueid=21014 if municipality =="Amixtlan"
replace uniqueid=21015 if municipality =="Amozoc"
replace uniqueid=21016 if municipality =="Aquixtla"
replace uniqueid=21017 if municipality =="Atempan"
replace uniqueid=21018 if municipality =="Atexcal"
replace uniqueid=21080 if municipality =="Atlequizayan"
replace uniqueid=21019 if municipality =="Atlixco"
replace uniqueid=21020 if municipality =="Atoyatempan"
replace uniqueid=21021 if municipality =="Atzala"
replace uniqueid=21022 if municipality =="Atzitzihuacan"
replace uniqueid=21023 if municipality =="Atzitzintla"
replace uniqueid=21024 if municipality =="Axutla"
replace uniqueid=21025 if municipality =="Ayotoxco De Guerrero"
replace uniqueid=21026 if municipality =="Calpan"
replace uniqueid=21027 if municipality =="Caltepec"
replace uniqueid=21028 if municipality =="Camocuautla"
replace uniqueid=21099 if municipality =="CaÑAda Morelos"
replace uniqueid=21029 if municipality =="Caxhuacan"
replace uniqueid=21045 if municipality =="Chalchicomula De Sesma"
replace uniqueid=21046 if municipality =="Chapulco"
replace uniqueid=21047 if municipality =="Chiautla"
replace uniqueid=21048 if municipality =="Chiautzingo"
replace uniqueid=21050 if municipality =="Chichiquila"
replace uniqueid=21049 if municipality =="Chiconcuautla"
replace uniqueid=21051 if municipality =="Chietla"
replace uniqueid=21052 if municipality =="Chigmecatitlan"
replace uniqueid=21053 if municipality =="Chignahuapan"
replace uniqueid=21054 if municipality =="Chignautla"
replace uniqueid=21055 if municipality =="Chila"
replace uniqueid=21056 if municipality =="Chila De La Sal"
replace uniqueid=21058 if municipality =="Chilchotla"
replace uniqueid=21059 if municipality =="Chinantla"
replace uniqueid=21030 if municipality =="Coatepec"
replace uniqueid=21031 if municipality =="Coatzingo"
replace uniqueid=21032 if municipality =="Cohetzala"
replace uniqueid=21033 if municipality =="Cohuecan"
replace uniqueid=21034 if municipality =="Coronango"
replace uniqueid=21035 if municipality =="Coxcatlan"
replace uniqueid=21036 if municipality =="Coyomeapan"
replace uniqueid=21037 if municipality =="Coyotepec"
replace uniqueid=21038 if municipality =="Cuapiaxtla De Madero"
replace uniqueid=21039 if municipality =="Cuautempan"
replace uniqueid=21040 if municipality =="Cuautinchan"
replace uniqueid=21041 if municipality =="Cuautlancingo"
replace uniqueid=21042 if municipality =="Cuayuca De Andrade"
replace uniqueid=21043 if municipality =="Cuetzalan Del Progreso"
replace uniqueid=21044 if municipality =="Cuyoaco"
replace uniqueid=21060 if municipality =="Domingo Arenas"
replace uniqueid=21061 if municipality =="Eloxochitlan"
replace uniqueid=21062 if municipality =="Epatlan"
replace uniqueid=21063 if municipality =="Esperanza"
replace uniqueid=21064 if municipality =="Francisco Z. Mena"
replace uniqueid=21065 if municipality =="General Felipe Angeles"
replace uniqueid=21066 if municipality =="Guadalupe"
replace uniqueid=21067 if municipality =="Guadalupe Victoria"
replace uniqueid=21068 if municipality =="Hermenegildo Galeana"
replace uniqueid=21057 if municipality =="Honey"
replace uniqueid=21069 if municipality =="Huaquechula"
replace uniqueid=21070 if municipality =="Huatlatlauca"
replace uniqueid=21071 if municipality =="Huauchinango"
replace uniqueid=21072 if municipality =="Huehuetla"
replace uniqueid=21073 if municipality =="Huehuetlan El Chico"
replace uniqueid=21150 if municipality =="Huehuetlan El Grande"
replace uniqueid=21074 if municipality =="Huejotzingo"
replace uniqueid=21075 if municipality =="Hueyapan"
replace uniqueid=21076 if municipality =="Hueytamalco"
replace uniqueid=21077 if municipality =="Hueytlalpan"
replace uniqueid=21078 if municipality =="Huitzilan De Serdan"
replace uniqueid=21079 if municipality =="Huitziltepec"
replace uniqueid=21081 if municipality =="Ixcamilpa De Guerrero"
replace uniqueid=21082 if municipality =="Ixcaquixtla"
replace uniqueid=21083 if municipality =="Ixtacamaxtitlan"
replace uniqueid=21084 if municipality =="Ixtepec"
replace uniqueid=21085 if municipality =="Izucar De Matamoros"
replace uniqueid=21086 if municipality =="Jalpan"
replace uniqueid=21087 if municipality =="Jolalpan"
replace uniqueid=21088 if municipality =="Jonotla"
replace uniqueid=21089 if municipality =="Jopala"
replace uniqueid=21090 if municipality =="Juan C. Bonilla"
replace uniqueid=21091 if municipality =="Juan Galindo"
replace uniqueid=21092 if municipality =="Juan N. Mendez"
replace uniqueid=21095 if municipality =="La Magdalena Tlatlauquitepec"
replace uniqueid=21093 if municipality =="Lafragua"
replace uniqueid=21094 if municipality =="Libres"
replace uniqueid=21118 if municipality =="Los Reyes De Juarez"
replace uniqueid=21096 if municipality =="Mazapiltepec De Juarez"
replace uniqueid=21097 if municipality =="Mixtla"
replace uniqueid=21098 if municipality =="Molcaxac"
replace uniqueid=21100 if municipality =="Naupan"
replace uniqueid=21101 if municipality =="Nauzontla"
replace uniqueid=21102 if municipality =="Nealtican"
replace uniqueid=21103 if municipality =="Nicolas Bravo"
replace uniqueid=21104 if municipality =="Nopalucan"
replace uniqueid=21105 if municipality =="Ocotepec"
replace uniqueid=21106 if municipality =="Ocoyucan"
replace uniqueid=21107 if municipality =="Olintla"
replace uniqueid=21108 if municipality =="Oriental"
replace uniqueid=21109 if municipality =="Pahuatlan"
replace uniqueid=21110 if municipality =="Palmar De Bravo"
replace uniqueid=21111 if municipality =="Pantepec"
replace uniqueid=21112 if municipality =="Petlalcingo"
replace uniqueid=21113 if municipality =="Piaxtla"
replace uniqueid=21114 if municipality =="Puebla"
replace uniqueid=21115 if municipality =="Quecholac"
replace uniqueid=21116 if municipality =="Quimixtlan"
replace uniqueid=21117 if municipality =="Rafael Lara Grajales"
replace uniqueid=21119 if municipality =="San Andres Cholula"
replace uniqueid=21120 if municipality =="San Antonio CaÃ‘Ada"
replace uniqueid=21121 if municipality =="San Diego La Mesa Tochimiltzingo"
replace uniqueid=21122 if municipality =="San Felipe Teotlalcingo"
replace uniqueid=21123 if municipality =="San Felipe Tepatlan"
replace uniqueid=21124 if municipality =="San Gabriel Chilac"
replace uniqueid=21125 if municipality =="San Gregorio Atzompa"
replace uniqueid=21126 if municipality =="San Jeronimo Tecuanipan"
replace uniqueid=21127 if municipality =="San Jeronimo Xayacatlan"
replace uniqueid=21128 if municipality =="San Jose Chiapa"
replace uniqueid=21129 if municipality =="San Jose Miahuatlan"
replace uniqueid=21130 if municipality =="San Juan Atenco"
replace uniqueid=21131 if municipality =="San Juan Atzompa"
replace uniqueid=21132 if municipality =="San Martin Texmelucan"
replace uniqueid=21133 if municipality =="San Martin Totoltepec"
replace uniqueid=21134 if municipality =="San Matias Tlalancaleca"
replace uniqueid=21135 if municipality =="San Miguel Ixitlan"
replace uniqueid=21136 if municipality =="San Miguel Xoxtla"
replace uniqueid=21137 if municipality =="San Nicolas Buenos Aires"
replace uniqueid=21138 if municipality =="San Nicolas De Los Ranchos"
replace uniqueid=21139 if municipality =="San Pablo Anicano"
replace uniqueid=21140 if municipality =="San Pedro Cholula"
replace uniqueid=21141 if municipality =="San Pedro Yeloixtlahuaca"
replace uniqueid=21142 if municipality =="San Salvador El Seco"
replace uniqueid=21143 if municipality =="San Salvador El Verde"
replace uniqueid=21144 if municipality =="San Salvador Huixcolotla"
replace uniqueid=21145 if municipality =="San Sebastian Tlacotepec"
replace uniqueid=21146 if municipality =="Santa Catarina Tlaltempan"
replace uniqueid=21147 if municipality =="Santa Ines Ahuatempan"
replace uniqueid=21148 if municipality =="Santa Isabel Cholula"
replace uniqueid=21149 if municipality =="Santiago Miahuatlan"
replace uniqueid=21151 if municipality =="Santo Tomas Hueyotlipan"
replace uniqueid=21152 if municipality =="Soltepec"
replace uniqueid=21153 if municipality =="Tecali De Herrera"
replace uniqueid=21154 if municipality =="Tecamachalco"
replace uniqueid=21155 if municipality =="Tecomatlan"
replace uniqueid=21156 if municipality =="Tehuacan"
replace uniqueid=21157 if municipality =="Tehuitzingo"
replace uniqueid=21158 if municipality =="Tenampulco"
replace uniqueid=21159 if municipality =="Teopantlan"
replace uniqueid=21160 if municipality =="Teotlalco"
replace uniqueid=21161 if municipality =="Tepanco De Lopez"
replace uniqueid=21162 if municipality =="Tepango De Rodriguez"
replace uniqueid=21163 if municipality =="Tepatlaxco De Hidalgo"
replace uniqueid=21164 if municipality =="Tepeaca"
replace uniqueid=21165 if municipality =="Tepemaxalco"
replace uniqueid=21166 if municipality =="Tepeojuma"
replace uniqueid=21167 if municipality =="Tepetzintla"
replace uniqueid=21168 if municipality =="Tepexco"
replace uniqueid=21169 if municipality =="Tepexi De Rodriguez"
replace uniqueid=21170 if municipality =="Tepeyahualco"
replace uniqueid=21171 if municipality =="Tepeyahualco De Cuauhtemoc"
replace uniqueid=21172 if municipality =="Tetela De Ocampo"
replace uniqueid=21173 if municipality =="Teteles De Avila Castillo"
replace uniqueid=21174 if municipality =="Teziutlan"
replace uniqueid=21175 if municipality =="Tianguismanalco"
replace uniqueid=21176 if municipality =="Tilapa"
replace uniqueid=21179 if municipality =="Tlachichuca"
replace uniqueid=21177 if municipality =="Tlacotepec De Benito Juarez"
replace uniqueid=21178 if municipality =="Tlacuilotepec"
replace uniqueid=21180 if municipality =="Tlahuapan"
replace uniqueid=21181 if municipality =="Tlaltenango"
replace uniqueid=21182 if municipality =="Tlanepantla"
replace uniqueid=21183 if municipality =="Tlaola"
replace uniqueid=21184 if municipality =="Tlapacoya"
replace uniqueid=21185 if municipality =="Tlapanala"
replace uniqueid=21186 if municipality =="Tlatlauquitepec"
replace uniqueid=21187 if municipality =="Tlaxco"
replace uniqueid=21188 if municipality =="Tochimilco"
replace uniqueid=21189 if municipality =="Tochtepec"
replace uniqueid=21190 if municipality =="Totoltepec De Guerrero"
replace uniqueid=21191 if municipality =="Tulcingo"
replace uniqueid=21192 if municipality =="Tuzamapan De Galeana"
replace uniqueid=21193 if municipality =="Tzicatlacoyan"
replace uniqueid=21194 if municipality =="Venustiano Carranza"
replace uniqueid=21195 if municipality =="Vicente Guerrero"
replace uniqueid=21196 if municipality =="Xayacatlan De Bravo"
replace uniqueid=21197 if municipality =="Xicotepec"
replace uniqueid=21198 if municipality =="Xicotlan"
replace uniqueid=21199 if municipality =="Xiutetelco"
replace uniqueid=21200 if municipality =="Xochiapulco"
replace uniqueid=21201 if municipality =="Xochiltepec"
replace uniqueid=21202 if municipality =="Xochitlan De Vicente Suarez"
replace uniqueid=21203 if municipality =="Xochitlan Todos Santos"
replace uniqueid=21204 if municipality =="Yaonahuac"
replace uniqueid=21205 if municipality =="Yehualtepec"
replace uniqueid=21206 if municipality =="Zacapala"
replace uniqueid=21207 if municipality =="Zacapoaxtla"
replace uniqueid=21208 if municipality =="Zacatlan"
replace uniqueid=21209 if municipality =="Zapotitlan"
replace uniqueid=21210 if municipality =="Zapotitlan De Mendez"
replace uniqueid=21211 if municipality =="Zaragoza"
replace uniqueid=21212 if municipality =="Zautla"
replace uniqueid=21213 if municipality =="Zihuateutla"
replace uniqueid=21214 if municipality =="Zinacatepec"
replace uniqueid=21215 if municipality =="Zongozotla"
replace uniqueid=21216 if municipality =="Zoquiapan"
replace uniqueid=21217 if municipality =="Zoquitlan"

replace municipality = upper(municipality)

preserve
import excel "puebla_coalitions_2018.xlsx", sheet("Sheet1") firstrow clear
save "merge.dta", replace
restore

merge m:1 municipality using "merge.dta"
drop _merge
erase "merge.dta"

foreach x of varlist cc_* {
	replace `x' = 0 if `x'==.
}

rename NA PANAL
rename PRI_NA PRI_PANAL
rename NA_PCPP PANAL_PCPP

rename (CANDIDATURAINDEPENDIENTE CANDIDATURAINDEPENDIENTE2) (CI_1 CI_2)

rename TOTAL total

collapse (sum) PAN-CI_2 total (first) cc*, by(municipality uniqueid section)

egen PAN_PRD_MC_PCPP_PSI2 = rowtotal(PAN PRD MC PCPP PSI PAN_PRD_MC_PCPP_PSI PAN_PRD_MC_PCPP PAN_PRD_MC_PSI PAN_PRD_PCPP_PSI PAN_MC_PCPP_PSI PRD_MC_PCPP_PSI PAN_PRD_MC PAN_PRD_PCPP PAN_PRD_PSI PAN_MC_PCPP PAN_MC_PSI /// 
	PAN_PCPP_PSI PRD_MC_PCPP PRD_MC_PSI PRD_PCPP_PSI MC_PCPP_PSI PAN_PRD PAN_MC PAN_PCPP PAN_PSI PRD_MC PRD_PCPP PRD_PSI MC_PCPP MC_PSI PCPP_PSI) if cc_PAN_PRD_MC_PCPP_PSI==1
foreach x of varlist PAN PRD MC PCPP PSI PAN_PRD_MC_PCPP_PSI PAN_PRD_MC_PCPP PAN_PRD_MC_PSI PAN_PRD_PCPP_PSI PAN_MC_PCPP_PSI PRD_MC_PCPP_PSI PAN_PRD_MC PAN_PRD_PCPP PAN_PRD_PSI PAN_MC_PCPP PAN_MC_PSI ///
	PAN_PCPP_PSI PRD_MC_PCPP PRD_MC_PSI PRD_PCPP_PSI MC_PCPP_PSI PAN_PRD PAN_MC PAN_PCPP PAN_PSI PRD_MC PRD_PCPP PRD_PSI MC_PCPP MC_PSI PCPP_PSI {
	replace `x' = . if cc_PAN_PRD_MC_PCPP_PSI==1
}

egen PAN_PRD_MC_PSI2 = rowtotal(PAN PRD MC PSI PAN_PRD_MC_PSI PAN_PRD_MC PAN_PRD_PSI PAN_MC_PSI PRD_MC_PSI PAN_PRD PAN_MC PAN_PSI PRD_MC PRD_PSI MC_PSI) if cc_PAN_PRD_MC_PSI==1
foreach x of varlist PAN PRD MC PSI PAN_PRD_MC_PSI PAN_PRD_MC PAN_PRD_PSI PAN_MC_PSI PRD_MC_PSI PAN_PRD PAN_MC PAN_PSI PRD_MC PRD_PSI MC_PSI {
	replace `x' = . if cc_PAN_PRD_MC_PSI==1
}

egen PAN_PRD_PCPP_PSI2 = rowtotal(PAN PRD PCPP PSI PAN_PRD_PCPP_PSI PAN_PRD_PCPP PAN_PRD_PSI PAN_PCPP_PSI PRD_PCPP_PSI PAN_PRD PAN_PCPP PAN_PSI PRD_PCPP PRD_PSI PCPP_PSI) if cc_PAN_PRD_PCPP_PSI==1
foreach x of varlist PAN PRD PCPP PSI PAN_PRD_PCPP_PSI PAN_PRD_PCPP PAN_PRD_PSI PAN_PCPP_PSI PRD_PCPP_PSI PAN_PRD PAN_PCPP PAN_PSI PRD_PCPP PRD_PSI PCPP_PSI {
	replace `x' = . if cc_PAN_PRD_PCPP_PSI==1
}

egen PAN_PRD_MC_PCPP2 = rowtotal(PAN PRD MC PCPP PAN_PRD_MC_PCPP PAN_PRD_MC PAN_PRD_PCPP PAN_MC_PCPP PRD_MC_PCPP PAN_PRD PAN_MC PAN_PCPP PRD_MC PRD_PCPP MC_PCPP) if cc_PAN_PRD_MC_PCPP==1
foreach x of varlist PAN PRD MC PCPP PAN_PRD_MC_PCPP PAN_PRD_MC PAN_PRD_PCPP PAN_MC_PCPP PRD_MC_PCPP PAN_PRD PAN_MC PAN_PCPP PRD_MC PRD_PCPP MC_PCPP {
	replace `x' = . if cc_PAN_PRD_MC_PCPP==1
}

egen PAN_MC_PCPP_PSI2 = rowtotal(PAN MC PCPP PSI PAN_MC_PCPP_PSI PAN_MC_PCPP PAN_MC_PSI PAN_PCPP_PSI MC_PCPP_PSI PAN_MC PAN_PCPP PAN_PSI MC_PCPP MC_PSI PCPP_PSI) if cc_PAN_MC_PCPP_PSI==1
foreach x of varlist PAN MC PCPP PSI PAN_MC_PCPP_PSI PAN_MC_PCPP PAN_MC_PSI PAN_PCPP_PSI MC_PCPP_PSI PAN_MC PAN_PCPP PAN_PSI MC_PCPP MC_PSI PCPP_PSI {
	replace `x' = . if cc_PAN_MC_PCPP_PSI==1
}

egen PAN_PRD_MC2 = rowtotal(PAN PRD MC PAN_PRD_MC PAN_PRD PAN_MC PRD_MC) if cc_PAN_PRD_MC==1
foreach x of varlist PAN PRD MC PAN_PRD_MC PAN_PRD PAN_MC PRD_MC {
	replace `x' = . if cc_PAN_PRD_MC==1
}

egen PAN_PCPP_PSI2 = rowtotal(PAN PCPP PSI PAN_PCPP_PSI PAN_PCPP PAN_PSI PCPP_PSI) if cc_PAN_PCPP_PSI==1
foreach x of varlist PAN PCPP PSI PAN_PCPP_PSI PAN_PCPP PAN_PSI PCPP_PSI {
	replace `x' = . if cc_PAN_PCPP_PSI==1
}

egen PAN_MC_PSI2 = rowtotal(PAN MC PSI PAN_MC_PSI PAN_MC PAN_PSI MC_PSI) if cc_PAN_MC_PSI==1
foreach x of varlist PAN MC PSI PAN_MC_PSI PAN_MC PAN_PSI MC_PSI {
	replace `x' = . if cc_PAN_MC_PSI==1
}

egen PAN_MC_PCPP2 = rowtotal(PAN MC PCPP PAN_MC_PCPP PAN_MC PAN_PCPP MC_PCPP) if cc_PAN_MC_PCPP==1
foreach x of varlist PAN MC PCPP PAN_MC_PCPP PAN_MC PAN_PCPP MC_PCPP {
	replace `x' = . if cc_PAN_MC_PCPP==1
}

egen PAN_PRD_PSI2 = rowtotal(PAN PRD PSI PAN_PRD_PSI PAN_PRD PAN_PSI PRD_PSI) if cc_PAN_PRD_PSI==1
foreach x of varlist PAN PRD PSI PAN_PRD_PSI PAN_PRD PAN_PSI PRD_PSI {
	replace `x' = . if cc_PAN_PRD_PSI==1
}

egen PAN_PRD_PCPP2 = rowtotal(PAN PRD PCPP PAN_PRD_PCPP PAN_PRD PAN_PCPP PRD_PCPP) if cc_PAN_PRD_PCPP==1
foreach x of varlist PAN PRD PCPP PAN_PRD_PCPP PAN_PRD PAN_PCPP PRD_PCPP {
	replace `x' = . if cc_PAN_PRD_PCPP==1
}

egen PAN_PRD2 = rowtotal(PAN PRD PAN_PRD) if cc_PAN_PRD==1
foreach x of varlist PAN PRD PAN_PRD {
	replace `x' = . if cc_PAN_PRD==1
}

egen PAN_PSI2 = rowtotal(PAN PSI PAN_PSI) if cc_PAN_PSI==1
foreach x of varlist PAN PSI PAN_PSI {
	replace `x' = . if cc_PAN_PSI==1
}

egen PAN_MC2 = rowtotal(PAN MC PAN_MC) if cc_PAN_MC==1
foreach x of varlist PAN MC PAN_MC {
	replace `x' = . if cc_PAN_MC==1
}

egen PAN_PCPP2 = rowtotal(PAN PCPP PAN_PCPP) if cc_PAN_PCPP==1
foreach x of varlist PAN PCPP PAN_PCPP {
	replace `x' = . if cc_PAN_PCPP==1
}

egen PRD_MC_PSI2 = rowtotal(PRD MC PSI PRD_MC_PSI PRD_MC PRD_PSI MC_PSI) if cc_PRD_MC_PSI==1
foreach x of varlist PRD MC PSI PRD_MC_PSI PRD_MC PRD_PSI MC_PSI {
	replace `x' = . if cc_PRD_MC_PSI==1
}

egen PRD_MC_PCPP2 = rowtotal(PRD MC PCPP PRD_MC_PCPP PRD_MC PRD_PCPP MC_PCPP) if cc_PRD_MC_PCPP==1
foreach x of varlist PRD MC PCPP PRD_MC_PCPP PRD_MC PRD_PCPP MC_PCPP {
	replace `x' = . if cc_PRD_MC_PCPP==1
}

egen PRD_PCPP_PSI2 = rowtotal(PRD PCPP PSI PRD_PCPP_PSI PRD_PCPP PRD_PSI PCPP_PSI) if cc_PRD_PCPP_PSI==1
foreach x of varlist PRD PCPP PSI PRD_PCPP_PSI PRD_PCPP PRD_PSI PCPP_PSI {
	replace `x' = . if cc_PRD_PCPP_PSI==1
}

egen PRD_MC2 = rowtotal(PRD MC PRD_MC) if cc_PRD_MC==1
foreach x of varlist PRD MC PRD_MC {
	replace `x' = . if cc_PRD_MC==1
}

egen PRD_PCPP2 = rowtotal(PRD PCPP PRD_PCPP) if cc_PRD_PCPP==1
foreach x of varlist PRD PCPP PRD_PCPP {
	replace `x' = . if cc_PRD_PCPP==1
}

egen PRD_PSI2 = rowtotal(PRD PSI PRD_PSI) if cc_PRD_PSI==1
foreach x of varlist PRD PSI PRD_PSI {
	replace `x' = . if cc_PRD_PSI==1
}

egen MC_PCPP_PSI2 = rowtotal(MC PCPP PSI MC_PCPP_PSI MC_PCPP MC_PSI PCPP_PSI) if cc_MC_PCPP_PSI==1
foreach x of varlist MC PCPP PSI MC_PCPP_PSI MC_PCPP MC_PSI PCPP_PSI {
	replace `x' = . if cc_MC_PCPP_PSI==1
}

egen MC_PCPP2 = rowtotal(MC PCPP MC_PCPP) if cc_MC_PCPP==1
foreach x of varlist MC PCPP MC_PCPP {
	replace `x' = . if cc_MC_PCPP==1
}

egen MC_PSI2 = rowtotal(MC PSI MC_PSI) if cc_MC_PSI==1
foreach x of varlist MC PSI MC_PSI {
	replace `x' = . if cc_MC_PSI==1
}

egen PANAL_PCPP2 = rowtotal(PANAL PCPP PANAL_PCPP) if cc_PANAL_PCPP==1
foreach x of varlist PANAL PCPP PANAL_PCPP {
	replace `x' = . if cc_PANAL_PCPP==1
}

egen PVEM_PCPP_PSI2 = rowtotal(PVEM PCPP PSI PVEM_PCPP_PSI PVEM_PCPP PVEM_PSI PCPP_PSI) if cc_PVEM_PCPP_PSI==1
foreach x of varlist PVEM PCPP PSI PVEM_PCPP_PSI PVEM_PCPP PVEM_PSI PCPP_PSI {
	replace `x' = . if cc_PVEM_PCPP_PSI==1
}

egen PVEM_PSI2 = rowtotal(PVEM PSI PVEM_PSI) if cc_PVEM_PSI==1
foreach x of varlist PVEM PSI PVEM_PSI {
	replace `x' = . if cc_PVEM_PSI==1
}

egen PVEM_PCPP2 = rowtotal(PVEM PCPP PVEM_PCPP) if cc_PVEM_PCPP==1
foreach x of varlist PVEM PCPP PVEM_PCPP {
	replace `x' = . if cc_PVEM_PCPP==1
}

egen PCPP_PSI2 = rowtotal(PCPP PSI PCPP_PSI) if cc_PCPP_PSI==1
foreach x of varlist PCPP PSI PCPP_PSI {
	replace `x' = . if cc_PCPP_PSI==1
}

egen PRI_PANAL2 = rowtotal(PRI PANAL PRI_PANAL) if cc_PRI_PANAL==1
foreach x of varlist PRI PANAL PRI_PANAL {
	replace `x' = . if cc_PRI_PANAL==1
}

foreach x in PAN_PRD_MC_PCPP_PSI PAN_PRD_MC_PSI PAN_PRD_PCPP_PSI PAN_PRD_MC_PCPP PAN_MC_PCPP_PSI PAN_PRD_MC PAN_PCPP_PSI PAN_MC_PSI PAN_MC_PCPP PAN_PRD_PSI PAN_PRD_PCPP PAN_PRD PAN_PSI PAN_MC PAN_PCPP ///
	PRD_MC PRD_MC_PSI PRD_MC_PCPP PRD_PCPP_PSI PRD_PCPP PRD_PSI MC_PCPP_PSI MC_PCPP MC_PSI PANAL_PCPP PVEM_PSI PVEM_PCPP_PSI PVEM_PCPP PCPP_PSI PRI_PANAL {
	drop `x'
	rename `x'2 `x'
}

replace PT_MORENA_PES = PT_MORENA_PES + PT_MORENA + PT_PES + MORENA_PES + PT + MORENA + PES
drop PT MORENA PES PT_MORENA PT_PES MORENA_PES

drop PRD_MC_PCPP_PSI

order PAN_PRD_MC_PCPP_PSI-PRI_PANAL, b(CI_1)

egen valid=rowtotal(PAN PRI PRD PVEM MC PANAL PCPP PSI PT_MORENA_PES PAN_PRD_MC_PCPP_PSI PAN_PRD_MC_PSI PAN_PRD_PCPP_PSI PAN_PRD_MC_PCPP PAN_MC_PCPP_PSI PAN_PRD_MC PAN_PCPP_PSI PAN_MC_PSI PAN_MC_PCPP ///
	PAN_PRD_PSI PAN_PRD_PCPP PAN_PRD PAN_PSI PAN_MC PAN_PCPP PRD_MC_PSI PRD_MC_PCPP PRD_PCPP_PSI PRD_MC PRD_PCPP PRD_PSI MC_PCPP_PSI MC_PCPP MC_PSI PANAL_PCPP PVEM_PCPP_PSI PVEM_PSI PVEM_PCPP PCPP_PSI PRI_PANAL CI_1 CI_2)

foreach var in PAN PRI PRD PVEM MC PANAL PCPP PSI PT_MORENA_PES PAN_PRD_MC_PCPP_PSI PAN_PRD_MC_PSI PAN_PRD_PCPP_PSI PAN_PRD_MC_PCPP PAN_MC_PCPP_PSI PAN_PRD_MC PAN_PCPP_PSI PAN_MC_PSI PAN_MC_PCPP PAN_PRD_PSI PAN_PRD_PCPP ///
	PAN_PRD PAN_PSI PAN_MC PAN_PCPP PRD_MC_PSI PRD_MC_PCPP PRD_PCPP_PSI PRD_MC PRD_PCPP PRD_PSI MC_PCPP_PSI MC_PCPP MC_PSI PANAL_PCPP PVEM_PCPP_PSI PVEM_PSI PVEM_PCPP PCPP_PSI PRI_PANAL CI_1 CI_2 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PVEM i_MC i_PANAL i_PCPP i_PSI i_PT_MORENA_PES i_PAN_PRD_MC_PCPP_PSI i_PAN_PRD_MC_PSI i_PAN_PRD_PCPP_PSI i_PAN_PRD_MC_PCPP i_PAN_MC_PCPP_PSI i_PAN_PRD_MC i_PAN_PCPP_PSI i_PAN_MC_PSI i_PAN_MC_PCPP i_PAN_PRD_PSI i_PAN_PRD_PCPP i_PAN_PRD ///
	i_PAN_PSI i_PAN_MC i_PAN_PCPP i_PRD_MC_PSI i_PRD_MC_PCPP i_PRD_PCPP_PSI i_PRD_MC i_PRD_PCPP i_PRD_PSI i_MC_PCPP_PSI i_MC_PCPP i_MC_PSI i_PANAL_PCPP i_PVEM_PCPP_PSI i_PVEM_PSI i_PVEM_PCPP i_PCPP_PSI i_PRI_PANAL i_CI_1 i_CI_2, ///
	gen(PAN_r PRI_r PRD_r PVEM_r MC_r PANAL_r PCPP_r PSI_r PT_MORENA_PES_r PAN_PRD_MC_PCPP_PSI_r PAN_PRD_MC_PSI_r PAN_PRD_PCPP_PSI_r PAN_PRD_MC_PCPP_r PAN_MC_PCPP_PSI_r PAN_PRD_MC_r PAN_PCPP_PSI_r PAN_MC_PSI_r PAN_MC_PCPP_r PAN_PRD_PSI_r PAN_PRD_PCPP_r PAN_PRD_r ///
	PAN_PSI_r PAN_MC_r PAN_PCPP_r PRD_MC_PSI_r PRD_MC_PCPP_r PRD_PCPP_PSI_r PRD_MC_r PRD_PCPP_r PRD_PSI_r MC_PCPP_PSI_r MC_PCPP_r MC_PSI_r PANAL_PCPP_r PVEM_PCPP_PSI_r PVEM_PSI_r PVEM_PCPP_r PCPP_PSI_r PRI_PANAL_r CI_1_r CI_2_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM MC PANAL PCPP PSI PT_MORENA_PES PAN_PRD_MC_PCPP_PSI PAN_PRD_MC_PSI PAN_PRD_PCPP_PSI PAN_PRD_MC_PCPP PAN_MC_PCPP_PSI PAN_PRD_MC PAN_PCPP_PSI PAN_MC_PSI PAN_MC_PCPP ///
	PAN_PRD_PSI PAN_PRD_PCPP PAN_PRD PAN_PSI PAN_MC PAN_PCPP PRD_MC_PSI PRD_MC_PCPP PRD_PCPP_PSI PRD_MC PRD_PCPP PRD_PSI MC_PCPP_PSI MC_PCPP MC_PSI PANAL_PCPP PVEM_PCPP_PSI PVEM_PSI PVEM_PCPP PCPP_PSI PRI_PANAL CI_1 CI_2 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" |  winner=="CI_2" 
replace second="Independent" if second=="CI_1"  | second=="CI_2"  
replace third="Independent" if third=="CI_1" | third=="CI_2"

gen year=2018
gen month="July"
gen STATE="PUEBLA"

gen incumbent_vote = .

drop cc*

preserve
use "..\Listas Nominales\ListadoNominalPREP2018.dta", clear
keep if STATE=="PUEBLA"
save PUE_LN18.dta, replace
restore

merge 1:1 section using PUE_LN18.dta
drop if _merge==2
drop _merge 
erase PUE_LN18.dta

rename ListadoNominalINE listanominal

gen turnout=total/listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 
gen mun_turnout =  mun_total/mun_listanominal

save "Puebla_Section_2018.dta", replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-valid listanominal (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Puebla_2018.dta", replace

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

foreach x in "AHUAZOTEPEC" "CAÑADA MORELOS" "MAZAPILTEPEC DE JUAREZ" "OCOYUCAN" "TEPEOJUMA" {
	import delimited "20190609_0800_CW_PRESIDENTE_MUNICIPAL\PRESIDENTE_MUNICIPAL_`x'_2019.csv", varnames(1) clear
	save "`x'.dta", replace
}
foreach x in "AHUAZOTEPEC" "CAÑADA MORELOS" "MAZAPILTEPEC DE JUAREZ" "OCOYUCAN" {
	append using "`x'.dta"
	erase "`x'.dta"
}
erase "TEPEOJUMA.dta"

rename seccion section
drop if section==.

rename lista_nominal_casilla listanominal
rename total_votos_calculados total

g PT_PVEM_MORENA_PES = pt_pvem_morena_pes + pt_pvem_morena + pt_pvem_pes + pt_morena_pes + pvem_morena_pes + pt_pvem + pt_morena + pt_pes + pvem_morena + pvem_pes + morena_pes + pvem + pt + morena ///
	if municipio=="AHUAZOTEPEC" | municipio=="CAÑADA MORELOS" | municipio=="OCOYUCAN"
g PAN_CPP = pan + cpp + pan_cpp if municipio=="AHUAZOTEPEC"
g PAN_PRD_MC_CPP = pan_prd_mc_cpp + pan_prd_mc + pan_prd_cpp + pan_mc_cpp + prd_mc_cpp + pan_prd + pan_mc + pan_cpp + prd_mc + prd_cpp + mc_cpp + pan + prd + mc + cpp if municipio=="CAÑADA MORELOS"
g PRI_CPP = pri + cpp + pri_cpp if municipio=="MAZAPILTEPEC DE JUAREZ"
g PRD_MC_CPP = prd_mc_cpp + prd_mc + prd_cpp + mc_cpp + prd + mc + cpp if municipio=="OCOYUCAN"
g PT_MORENA_PES = pt_morena_pes + pt_morena + pt_pes + morena_pes + pt + morena + pes if municipio=="TEPEOJUMA"

quietly foreach x of varlist pt_pvem_morena_pes pt_pvem_morena pt_pvem_pes pt_morena_pes pvem_morena_pes pt_pvem pt_morena pt_pes pvem_morena pvem_pes morena_pes pes pvem pt morena pan cpp pan_cpp {
	replace `x' = . if municipio=="AHUAZOTEPEC"
}

quietly foreach x of varlist pt_pvem_morena_pes pt_pvem_morena pt_pvem_pes pt_morena_pes pvem_morena_pes pt_pvem pt_morena pt_pes pvem_morena pvem_pes morena_pes pes pvem pt morena ///
	pan_prd_mc_cpp pan_prd_mc pan_prd_cpp pan_mc_cpp prd_mc_cpp pan_prd pan_mc pan_cpp prd_mc prd_cpp mc_cpp pan prd mc cpp {
	replace `x' = . if municipio=="CAÑADA MORELOS"
}

quietly foreach x of varlist pri cpp pri_cpp {
	replace `x' = . if municipio=="MAZAPILTEPEC DE JUAREZ"
}

quietly foreach x of varlist pt_pvem_morena_pes pt_pvem_morena pt_pvem_pes pt_morena_pes pvem_morena_pes pt_pvem pt_morena pt_pes pvem_morena pvem_pes morena_pes pes pvem pt morena prd_mc_cpp prd_mc prd_cpp mc_cpp prd mc cpp {
	replace `x' = . if municipio=="OCOYUCAN"
}

quietly foreach x of varlist pt_morena_pes pt_morena pt_pes morena_pes pt morena pes {
	replace `x' = . if municipio=="TEPEOJUMA"
}

missings dropvars pri-PT_MORENA_PES, force

rename pan PAN
rename pri PRI
rename pt PT
rename pvem PVEM
rename psi PSI
rename nap NAP
rename mc MC
rename morena MORENA
rename pes PES

g uniqueid = .
replace uniqueid = 21008 if municipio=="AHUAZOTEPEC"
replace uniqueid = 21099 if municipio=="CAÑADA MORELOS"
replace uniqueid = 21096 if municipio=="MAZAPILTEPEC DE JUAREZ"
replace uniqueid = 21106 if municipio=="OCOYUCAN"
replace uniqueid = 21166 if municipio=="TEPEOJUMA"
g municipality = municipio + " EXTRAORDINARIO"

collapse (sum) PRI-PES PAN-PT_MORENA_PES total listanominal, by (municipality uniqueid section)

g turnout = total/listanominal

egen valid = rowtotal(PRI PT PVEM MC MORENA PES PAN PSI NAP PT_PVEM_MORENA_PES PAN_CPP PAN_PRD_MC_CPP PRI_CPP PRD_MC_CPP PT_MORENA_PES)

foreach var in PRI PT PVEM MC MORENA PES PAN PSI NAP PT_PVEM_MORENA_PES PAN_CPP PAN_PRD_MC_CPP PRI_CPP PRD_MC_CPP PT_MORENA_PES total listanominal valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen inv_mun_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal

rowranks inv_mun_PRI inv_mun_PT inv_mun_PVEM inv_mun_MC inv_mun_MORENA inv_mun_PES inv_mun_PAN inv_mun_PSI inv_mun_NAP inv_mun_PT_PVEM_MORENA_PES inv_mun_PAN_CPP inv_mun_PAN_PRD_MC_CPP inv_mun_PRI_CPP inv_mun_PRD_MC_CPP ///
	inv_mun_PT_MORENA_PES, gen(PRI_r PT_r PVEM_r MC_r MORENA_r PES_r PAN_r PSI_r NAP_r PT_PVEM_MORENA_PES_r PAN_CPP_r PAN_PRD_MC_CPP_r PRI_CPP_r PRD_MC_CPP_r PT_MORENA_PES_r)
drop inv_mun_*

g winner = ""
foreach x in PRI PT PVEM MC MORENA PES PAN PSI NAP PT_PVEM_MORENA_PES PAN_CPP PAN_PRD_MC_CPP PRI_CPP PRD_MC_CPP PT_MORENA_PES {
	replace winner = "`x'" if `x'_r==1  
}
drop *_r

gen year = 2019
gen month ="June"

save Puebla_Section_2019.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

clear
append using Puebla_Section_2014.dta
append using Puebla_Section_2018.dta
append using Puebla_Section_2019.dta

erase Puebla_Section_2014.dta
erase Puebla_Section_2018.dta
erase Puebla_Section_2019.dta

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

save Puebla_Section_14_18.dta, replace

clear
use "..\..\Precinct\Puebla_ALL.dta"
append using Puebla_Section_14_18.dta

replace municipality = upper(municipality)

save Puebla_ALL_SALVADOR.dta, replace 

erase Puebla_Section_14_18.dta

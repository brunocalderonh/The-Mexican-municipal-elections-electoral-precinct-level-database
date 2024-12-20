capture cd "D:\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Puebla - 1998, 2001, 2004, 2007, 2010,  2013"
capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Puebla - 1998, 2001, 2004, 2007, 2010,  2013"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1998_No_LN.csv, clear

rename municipio  municipality
rename seccin section
drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan -  total , replace

collapse (sum)  pan -  total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pcp  PCP

drop    noreg nulos 

gen   uniqueid= 0
replace uniqueid=21001 if municipality =="ACAJETE"
replace uniqueid=21002 if municipality =="ACATENO"
replace uniqueid=21003 if municipality =="ACATLAN"
replace uniqueid=21004 if municipality =="ACATZINGO"
replace uniqueid=21005 if municipality =="ACTEOPAN"
replace uniqueid=21006 if municipality =="AHUACATLAN"
replace uniqueid=21007 if municipality =="AHUATLAN"
replace uniqueid=21008 if municipality =="AHUAZOTEPEC"
replace uniqueid=21009 if municipality =="AHUEHUETITLA"
replace uniqueid=21010 if municipality =="AJALPAN"
replace uniqueid=21011 if municipality =="ALBINO ZERTUCHE"
replace uniqueid=21012 if municipality =="ALJOJUCA"
replace uniqueid=21013 if municipality =="ALTEPEXI"
replace uniqueid=21014 if municipality =="AMIXTLAN"
replace uniqueid=21015 if municipality =="AMOZOC"
replace uniqueid=21016 if municipality =="AQUIXTLA"
replace uniqueid=21017 if municipality =="ATEMPAN"
replace uniqueid=21018 if municipality =="ATEXCAL"
replace uniqueid=21080 if municipality =="ATLEQUIZAYAN"
replace uniqueid=21019 if municipality =="ATLIXCO"
replace uniqueid=21020 if municipality =="ATOYATEMPAN"
replace uniqueid=21021 if municipality =="ATZALA"
replace uniqueid=21022 if municipality =="ATZITZIHUACAN"
replace uniqueid=21023 if municipality =="ATZITZINTLA"
replace uniqueid=21024 if municipality =="AXUTLA"
replace uniqueid=21025 if municipality =="AYOTOXCO DE GUERRERO"
replace uniqueid=21026 if municipality =="CALPAN"
replace uniqueid=21027 if municipality =="CALTEPEC"
replace uniqueid=21028 if municipality =="CAMOCUAUTLA"
replace uniqueid=21099 if municipality =="CA袮DA MORELOS" | strpos(muni, "ADA MORELOS")>0
replace uniqueid=21029 if municipality =="CAXHUACAN"
replace uniqueid=21045 if municipality =="CHALCHICOMULA DE SESMA"
replace uniqueid=21046 if municipality =="CHAPULCO"
replace uniqueid=21047 if municipality =="CHIAUTLA"
replace uniqueid=21048 if municipality =="CHIAUTZINGO"
replace uniqueid=21050 if municipality =="CHICHIQUILA"
replace uniqueid=21049 if municipality =="CHICONCUAUTLA"
replace uniqueid=21051 if municipality =="CHIETLA"
replace uniqueid=21052 if municipality =="CHIGMECATITLAN"
replace uniqueid=21053 if municipality =="CHIGNAHUAPAN"
replace uniqueid=21054 if municipality =="CHIGNAUTLA"
replace uniqueid=21055 if municipality =="CHILA"
replace uniqueid=21056 if municipality =="CHILA DE LA SAL"
replace uniqueid=21058 if municipality =="CHILCHOTLA"
replace uniqueid=21059 if municipality =="CHINANTLA"
replace uniqueid=21030 if municipality =="COATEPEC"
replace uniqueid=21031 if municipality =="COATZINGO"
replace uniqueid=21032 if municipality =="COHETZALA"
replace uniqueid=21033 if municipality =="COHUECAN"
replace uniqueid=21034 if municipality =="CORONANGO"
replace uniqueid=21035 if municipality =="COXCATLAN"
replace uniqueid=21036 if municipality =="COYOMEAPAN"
replace uniqueid=21037 if municipality =="COYOTEPEC"
replace uniqueid=21038 if municipality =="CUAPIAXTLA DE MADERO"
replace uniqueid=21039 if municipality =="CUAUTEMPAN"
replace uniqueid=21040 if municipality =="CUAUTINCHAN"
replace uniqueid=21041 if municipality =="CUAUTLANCINGO"
replace uniqueid=21042 if municipality =="CUAYUCA DE ANDRADE"
replace uniqueid=21043 if municipality =="CUETZALAN DEL PROGRESO"
replace uniqueid=21044 if municipality =="CUYOACO"
replace uniqueid=21060 if municipality =="DOMINGO ARENAS"
replace uniqueid=21061 if municipality =="ELOXOCHITLAN"
replace uniqueid=21062 if municipality =="EPATLAN"
replace uniqueid=21063 if municipality =="ESPERANZA"
replace uniqueid=21064 if municipality =="FRANCISCO Z. MENA"
replace uniqueid=21065 if municipality =="GENERAL FELIPE ANGELES"
replace uniqueid=21066 if municipality =="GUADALUPE"
replace uniqueid=21067 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=21068 if municipality =="HERMENEGILDO GALEANA"
replace uniqueid=21057 if municipality =="HONEY"
replace uniqueid=21069 if municipality =="HUAQUECHULA"
replace uniqueid=21070 if municipality =="HUATLATLAUCA"
replace uniqueid=21071 if municipality =="HUAUCHINANGO"
replace uniqueid=21072 if municipality =="HUEHUETLA"
replace uniqueid=21073 if municipality =="HUEHUETLAN EL CHICO"
replace uniqueid=21150 if municipality =="HUEHUETLAN EL GRANDE"
replace uniqueid=21074 if municipality =="HUEJOTZINGO"
replace uniqueid=21075 if municipality =="HUEYAPAN"
replace uniqueid=21076 if municipality =="HUEYTAMALCO"
replace uniqueid=21077 if municipality =="HUEYTLALPAN"
replace uniqueid=21078 if municipality =="HUITZILAN DE SERDAN"
replace uniqueid=21079 if municipality =="HUITZILTEPEC"
replace uniqueid=21081 if municipality =="IXCAMILPA DE GUERRERO"
replace uniqueid=21082 if municipality =="IXCAQUIXTLA"
replace uniqueid=21083 if municipality =="IXTACAMAXTITLAN"
replace uniqueid=21084 if municipality =="IXTEPEC"
replace uniqueid=21085 if municipality =="IZUCAR DE MATAMOROS"
replace uniqueid=21086 if municipality =="JALPAN"
replace uniqueid=21087 if municipality =="JOLALPAN"
replace uniqueid=21088 if municipality =="JONOTLA"
replace uniqueid=21089 if municipality =="JOPALA"
replace uniqueid=21090 if municipality =="JUAN C. BONILLA"
replace uniqueid=21091 if municipality =="JUAN GALINDO"
replace uniqueid=21092 if municipality =="JUAN N. MENDEZ"
replace uniqueid=21095 if municipality =="MAGDALENA TLATLAUQUITEPEC"
replace uniqueid=21093 if municipality =="LAFRAGUA"
replace uniqueid=21094 if municipality =="LIBRES"
replace uniqueid=21118 if municipality =="LOS REYES DE JUAREZ"
replace uniqueid=21096 if municipality =="MAZAPILTEPEC DE JUAREZ"
replace uniqueid=21097 if municipality =="MIXTLA"
replace uniqueid=21098 if municipality =="MOLCAXAC"
replace uniqueid=21100 if municipality =="NAUPAN"
replace uniqueid=21101 if municipality =="NAUZONTLA"
replace uniqueid=21102 if municipality =="NEALTICAN"
replace uniqueid=21103 if municipality =="NICOLAS BRAVO"
replace uniqueid=21104 if municipality =="NOPALUCAN"
replace uniqueid=21105 if municipality =="OCOTEPEC"
replace uniqueid=21106 if municipality =="OCOYUCAN"
replace uniqueid=21107 if municipality =="OLINTLA"
replace uniqueid=21108 if municipality =="ORIENTAL"
replace uniqueid=21109 if municipality =="PAHUATLAN"
replace uniqueid=21110 if municipality =="PALMAR DE BRAVO"
replace uniqueid=21111 if municipality =="PANTEPEC"
replace uniqueid=21112 if municipality =="PETLALCINGO"
replace uniqueid=21113 if municipality =="PIAXTLA"
replace uniqueid=21114 if municipality =="PUEBLA"
replace uniqueid=21115 if municipality =="QUECHOLAC"
replace uniqueid=21116 if municipality =="QUIMIXTLAN"
replace uniqueid=21117 if municipality =="RAFAEL LARA GRAJALES"
replace uniqueid=21119 if municipality =="SAN ANDRES CHOLULA"
replace uniqueid=21120 if municipality =="SAN ANTONIO CA袮DA" | strpos(municipality, "SAN ANTONIO CA")>0
replace uniqueid=21121 if municipality =="SAN DIEGO LA MESA TOCHIMILTZINGO"
replace uniqueid=21122 if municipality =="SAN FELIPE TEOTLALCINGO"
replace uniqueid=21123 if municipality =="SAN FELIPE TEPATLAN"
replace uniqueid=21124 if municipality =="SAN GABRIEL CHILAC"
replace uniqueid=21125 if municipality =="SAN GREGORIO ATZOMPA"
replace uniqueid=21126 if municipality =="SAN JERONIMO TECUANIPAN"
replace uniqueid=21127 if municipality =="SAN JERONIMO XAYACATLAN"
replace uniqueid=21128 if municipality =="SAN JOSE CHIAPA"
replace uniqueid=21129 if municipality =="SAN JOSE MIAHUATLAN"
replace uniqueid=21130 if municipality =="SAN JUAN ATENCO"
replace uniqueid=21131 if municipality =="SAN JUAN ATZOMPA"
replace uniqueid=21132 if municipality =="SAN MARTIN TEXMELUCAN"
replace uniqueid=21133 if municipality =="SAN MARTIN TOTOLTEPEC"
replace uniqueid=21134 if municipality =="SAN MATIAS TLALANCALECA"
replace uniqueid=21135 if municipality =="SAN MIGUEL IXITLAN"
replace uniqueid=21136 if municipality =="SAN MIGUEL XOXTLA"
replace uniqueid=21137 if municipality =="SAN NICOLAS BUENOS AIRES"
replace uniqueid=21138 if municipality =="SAN NICOLAS DE LOS RANCHOS"
replace uniqueid=21139 if municipality =="SAN PABLO ANICANO"
replace uniqueid=21140 if municipality =="SAN PEDRO CHOLULA"
replace uniqueid=21141 if municipality =="SAN PEDRO YELOIXTLAHUACA"
replace uniqueid=21142 if municipality =="SAN SALVADOR EL SECO"
replace uniqueid=21143 if municipality =="SAN SALVADOR EL VERDE"
replace uniqueid=21144 if municipality =="SAN SALVADOR HUIXCOLOTLA"
replace uniqueid=21145 if municipality =="SAN SEBASTIAN TLACOTEPEC"
replace uniqueid=21146 if municipality =="SANTA CATARINA TLALTEMPAN"
replace uniqueid=21147 if municipality =="SANTA INES AHUATEMPAN"
replace uniqueid=21148 if municipality =="SANTA ISABEL CHOLULA"
replace uniqueid=21149 if municipality =="SANTIAGO MIAHUATLAN"
replace uniqueid=21151 if municipality =="SANTO TOMAS HUEYOTLIPAN"
replace uniqueid=21152 if municipality =="SOLTEPEC"
replace uniqueid=21153 if municipality =="TECALI DE HERRERA"
replace uniqueid=21154 if municipality =="TECAMACHALCO"
replace uniqueid=21155 if municipality =="TECOMATLAN"
replace uniqueid=21156 if municipality =="TEHUACAN"
replace uniqueid=21157 if municipality =="TEHUITZINGO"
replace uniqueid=21158 if municipality =="TENAMPULCO"
replace uniqueid=21159 if municipality =="TEOPANTLAN"
replace uniqueid=21160 if municipality =="TEOTLALCO"
replace uniqueid=21161 if municipality =="TEPANCO DE LOPEZ"
replace uniqueid=21162 if municipality =="TEPANGO DE RODRIGUEZ"
replace uniqueid=21163 if municipality =="TEPATLAXCO DE HIDALGO"
replace uniqueid=21164 if municipality =="TEPEACA"
replace uniqueid=21165 if municipality =="TEPEMAXALCO"
replace uniqueid=21166 if municipality =="TEPEOJUMA"
replace uniqueid=21167 if municipality =="TEPETZINTLA"
replace uniqueid=21168 if municipality =="TEPEXCO"
replace uniqueid=21169 if municipality =="TEPEXI DE RODRIGUEZ"
replace uniqueid=21170 if municipality =="TEPEYAHUALCO"
replace uniqueid=21171 if municipality =="TEPEYAHUALCO DE CUAUHTEMOC"
replace uniqueid=21172 if municipality =="TETELA DE OCAMPO"
replace uniqueid=21173 if municipality =="TETELES DE AVILA CASTILLO"
replace uniqueid=21174 if municipality =="TEZIUTLAN"
replace uniqueid=21175 if municipality =="TIANGUISMANALCO"
replace uniqueid=21176 if municipality =="TILAPA"
replace uniqueid=21179 if municipality =="TLACHICHUCA"
replace uniqueid=21177 if municipality =="TLACOTEPEC DE BENITO JUAREZ"
replace uniqueid=21178 if municipality =="TLACUILOTEPEC"
replace uniqueid=21180 if municipality =="TLAHUAPAN"
replace uniqueid=21181 if municipality =="TLALTENANGO"
replace uniqueid=21182 if municipality =="TLANEPANTLA"
replace uniqueid=21183 if municipality =="TLAOLA"
replace uniqueid=21184 if municipality =="TLAPACOYA"
replace uniqueid=21185 if municipality =="TLAPANALA"
replace uniqueid=21186 if municipality =="TLATLAUQUITEPEC"
replace uniqueid=21187 if municipality =="TLAXCO"
replace uniqueid=21188 if municipality =="TOCHIMILCO"
replace uniqueid=21189 if municipality =="TOCHTEPEC"
replace uniqueid=21190 if municipality =="TOTOLTEPEC DE GUERRERO"
replace uniqueid=21191 if municipality =="TULCINGO DE VALLE"
replace uniqueid=21192 if municipality =="TUZAMAPAN DE GALEANA"
replace uniqueid=21193 if municipality =="TZICATLACOYAN"
replace uniqueid=21194 if municipality =="VENUSTIANO CARRANZA"
replace uniqueid=21195 if municipality =="VICENTE GUERRERO"
replace uniqueid=21196 if municipality =="XAYACATLAN DE BRAVO"
replace uniqueid=21197 if municipality =="XICOTEPEC"
replace uniqueid=21198 if municipality =="XICOTLAN"
replace uniqueid=21199 if municipality =="XIUTETELCO"
replace uniqueid=21200 if municipality =="XOCHIAPULCO"
replace uniqueid=21201 if municipality =="XOCHILTEPEC"
replace uniqueid=21202 if municipality =="XOCHITLAN DE VICENTE SUAREZ"
replace uniqueid=21203 if municipality =="XOCHITLAN TODOS SANTOS"
replace uniqueid=21204 if municipality =="YAONAHUAC"
replace uniqueid=21205 if municipality =="YEHUALTEPEC"
replace uniqueid=21206 if municipality =="ZACAPALA"
replace uniqueid=21207 if municipality =="ZACAPOAXTLA"
replace uniqueid=21208 if municipality =="ZACATLAN"
replace uniqueid=21209 if municipality =="ZAPOTITLAN SALINAS"
replace uniqueid=21210 if municipality =="ZAPOTITLAN DE MENDEZ"
replace uniqueid=21211 if municipality =="ZARAGOZA"
replace uniqueid=21212 if municipality =="ZAUTLA"
replace uniqueid=21213 if municipality =="ZIHUATEUTLA"
replace uniqueid=21214 if municipality =="ZINACATEPEC"
replace uniqueid=21215 if municipality =="ZONGOZOTLA"
replace uniqueid=21216 if municipality =="ZOQUIAPAN"
replace uniqueid=21217 if municipality =="ZOQUITLAN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PCP)

foreach var in PAN PRI PRD PT PVEM PCP total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 21
g seccion = section
capture merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
keep if month==9 & year==1998
drop if _merge==2
drop _merge ed seccion year month
rename lista listanominal

gen turnout =  total/listanominal

foreach var in listanominal {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PCP, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PCP_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r==1 
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PCP" if PCP_r==1
drop *_r

gen year = 1998
gen month ="November"

save Puebla_Section_1998.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2001.csv, clear

rename municipio  municipality
rename seccion section
rename votaciontotal total
drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan -  listanominal , replace

g missing = listanominal==.

collapse (sum)  pan -  listanominal missing, by (municipality section)

g ed = 21
g seccion = section
capture merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
keep if month==9 & year==2001
drop if _merge==2
drop _merge ed seccion year month

replace listanominal = lista if missing>=1
drop lista missing

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename cdppn PC
rename psn  PSN
rename pas  PAS

gen turnout =  total/listanominal

drop    noreg nulos 

gen   uniqueid= 0
replace uniqueid=21001 if municipality =="ACAJETE"
replace uniqueid=21002 if municipality =="ACATENO"
replace uniqueid=21003 if municipality =="ACATLAN"
replace uniqueid=21004 if municipality =="ACATZINGO"
replace uniqueid=21005 if municipality =="ACTEOPAN"
replace uniqueid=21006 if municipality =="AHUACATLAN"
replace uniqueid=21007 if municipality =="AHUATLAN"
replace uniqueid=21008 if municipality =="AHUAZOTEPEC"
replace uniqueid=21009 if municipality =="AHUEHUETITLA"
replace uniqueid=21010 if municipality =="AJALPAN"
replace uniqueid=21011 if municipality =="ALBINO ZERTUCHE"
replace uniqueid=21012 if municipality =="ALJOJUCA"
replace uniqueid=21013 if municipality =="ALTEPEXI"
replace uniqueid=21014 if municipality =="AMIXTLAN"
replace uniqueid=21015 if municipality =="AMOZOC"
replace uniqueid=21016 if municipality =="AQUIXTLA"
replace uniqueid=21017 if municipality =="ATEMPAN"
replace uniqueid=21018 if municipality =="ATEXCAL"
replace uniqueid=21080 if municipality =="ATLEQUIZAYAN"
replace uniqueid=21019 if municipality =="ATLIXCO"
replace uniqueid=21020 if municipality =="ATOYATEMPAN"
replace uniqueid=21021 if municipality =="ATZALA"
replace uniqueid=21022 if municipality =="ATZITZIHUACAN"
replace uniqueid=21023 if municipality =="ATZITZINTLA"
replace uniqueid=21024 if municipality =="AXUTLA"
replace uniqueid=21025 if municipality =="AYOTOXCO DE GUERRERO"
replace uniqueid=21026 if municipality =="CALPAN"
replace uniqueid=21027 if municipality =="CALTEPEC"
replace uniqueid=21028 if municipality =="CAMOCUAUTLA"
replace uniqueid=21099 if municipality =="CA袮DA MORELOS" | strpos(muni, "ADA MORELOS")>0
replace uniqueid=21029 if municipality =="CAXHUACAN"
replace uniqueid=21045 if municipality =="CHALCHICOMULA DE SESMA"
replace uniqueid=21046 if municipality =="CHAPULCO"
replace uniqueid=21047 if municipality =="CHIAUTLA"
replace uniqueid=21048 if municipality =="CHIAUTZINGO"
replace uniqueid=21050 if municipality =="CHICHIQUILA"
replace uniqueid=21049 if municipality =="CHICONCUAUTLA"
replace uniqueid=21051 if municipality =="CHIETLA"
replace uniqueid=21052 if municipality =="CHIGMECATITLAN"
replace uniqueid=21053 if municipality =="CHIGNAHUAPAN"
replace uniqueid=21054 if municipality =="CHIGNAUTLA"
replace uniqueid=21055 if municipality =="CHILA"
replace uniqueid=21056 if municipality =="CHILA DE LA SAL"
replace uniqueid=21058 if municipality =="CHILCHOTLA"
replace uniqueid=21059 if municipality =="CHINANTLA"
replace uniqueid=21030 if municipality =="COATEPEC"
replace uniqueid=21031 if municipality =="COATZINGO"
replace uniqueid=21032 if municipality =="COHETZALA"
replace uniqueid=21033 if municipality =="COHUECAN"
replace uniqueid=21034 if municipality =="CORONANGO"
replace uniqueid=21035 if municipality =="COXCATLAN"
replace uniqueid=21036 if municipality =="COYOMEAPAN"
replace uniqueid=21037 if municipality =="COYOTEPEC"
replace uniqueid=21038 if municipality =="CUAPIAXTLA DE MADERO"
replace uniqueid=21039 if municipality =="CUAUTEMPAN"
replace uniqueid=21040 if municipality =="CUAUTINCHAN"
replace uniqueid=21041 if municipality =="CUAUTLANCINGO"
replace uniqueid=21042 if municipality =="CUAYUCA DE ANDRADE"
replace uniqueid=21043 if municipality =="CUETZALAN DEL PROGRESO"
replace uniqueid=21044 if municipality =="CUYOACO"
replace uniqueid=21060 if municipality =="DOMINGO ARENAS"
replace uniqueid=21061 if municipality =="ELOXOCHITLAN"
replace uniqueid=21062 if municipality =="EPATLAN"
replace uniqueid=21063 if municipality =="ESPERANZA"
replace uniqueid=21064 if municipality =="FRANCISCO Z. MENA"
replace uniqueid=21065 if municipality =="GENERAL FELIPE ANGELES"
replace uniqueid=21066 if municipality =="GUADALUPE"
replace uniqueid=21067 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=21068 if municipality =="HERMENEGILDO GALEANA"
replace uniqueid=21057 if municipality =="HONEY"
replace uniqueid=21069 if municipality =="HUAQUECHULA"
replace uniqueid=21070 if municipality =="HUATLATLAUCA"
replace uniqueid=21071 if municipality =="HUAUCHINANGO"
replace uniqueid=21072 if municipality =="HUEHUETLA"
replace uniqueid=21073 if municipality =="HUEHUETLAN EL CHICO"
replace uniqueid=21150 if municipality =="HUEHUETLAN EL GRANDE"
replace uniqueid=21074 if municipality =="HUEJOTZINGO"
replace uniqueid=21075 if municipality =="HUEYAPAN"
replace uniqueid=21076 if municipality =="HUEYTAMALCO"
replace uniqueid=21077 if municipality =="HUEYTLALPAN"
replace uniqueid=21078 if municipality =="HUITZILAN DE SERDAN"
replace uniqueid=21079 if municipality =="HUITZILTEPEC"
replace uniqueid=21081 if municipality =="IXCAMILPA DE GUERRERO"
replace uniqueid=21082 if municipality =="IXCAQUIXTLA"
replace uniqueid=21083 if municipality =="IXTACAMAXTITLAN"
replace uniqueid=21084 if municipality =="IXTEPEC"
replace uniqueid=21085 if municipality =="IZUCAR DE MATAMOROS"
replace uniqueid=21086 if municipality =="JALPAN"
replace uniqueid=21087 if municipality =="JOLALPAN"
replace uniqueid=21088 if municipality =="JONOTLA"
replace uniqueid=21089 if municipality =="JOPALA"
replace uniqueid=21090 if municipality =="JUAN C. BONILLA"
replace uniqueid=21091 if municipality =="JUAN GALINDO"
replace uniqueid=21092 if municipality =="JUAN N. MENDEZ"
replace uniqueid=21095 if municipality =="LA MAGDALENA TLATLAUQUITEPEC"
replace uniqueid=21093 if municipality =="LAFRAGUA"
replace uniqueid=21094 if municipality =="LIBRES"
replace uniqueid=21118 if municipality =="LOS REYES DE JUAREZ"
replace uniqueid=21096 if municipality =="MAZAPILTEPEC DE JUAREZ"
replace uniqueid=21097 if municipality =="MIXTLA"
replace uniqueid=21098 if municipality =="MOLCAXAC"
replace uniqueid=21100 if municipality =="NAUPAN"
replace uniqueid=21101 if municipality =="NAUZONTLA"
replace uniqueid=21102 if municipality =="NEALTICAN"
replace uniqueid=21103 if municipality =="NICOLAS BRAVO"
replace uniqueid=21104 if municipality =="NOPALUCAN"
replace uniqueid=21105 if municipality =="OCOTEPEC"
replace uniqueid=21106 if municipality =="OCOYUCAN"
replace uniqueid=21107 if municipality =="OLINTLA"
replace uniqueid=21108 if municipality =="ORIENTAL"
replace uniqueid=21109 if municipality =="PAHUATLAN"
replace uniqueid=21110 if municipality =="PALMAR DE BRAVO"
replace uniqueid=21111 if municipality =="PANTEPEC"
replace uniqueid=21112 if municipality =="PETLALCINGO"
replace uniqueid=21113 if municipality =="PIAXTLA"
replace uniqueid=21114 if municipality =="PUEBLA"
replace uniqueid=21115 if municipality =="QUECHOLAC"
replace uniqueid=21116 if municipality =="QUIMIXTLAN"
replace uniqueid=21117 if municipality =="RAFAEL LARA GRAJALES"
replace uniqueid=21119 if municipality =="SAN ANDRES CHOLULA"
replace uniqueid=21120 if municipality =="SAN ANTONIO CA袮DA" | strpos(municipality, "SAN ANTONIO CA")>0
replace uniqueid=21121 if municipality =="SAN DIEGO LA MESA TOCHIMILTZINGO"
replace uniqueid=21122 if municipality =="SAN FELIPE TEOTLALCINGO"
replace uniqueid=21123 if municipality =="SAN FELIPE TEPATLAN"
replace uniqueid=21124 if municipality =="SAN GABRIEL CHILAC"
replace uniqueid=21125 if municipality =="SAN GREGORIO ATZOMPA"
replace uniqueid=21126 if municipality =="SAN JERONIMO TECUANIPAN"
replace uniqueid=21127 if municipality =="SAN JERONIMO XAYACATLAN"
replace uniqueid=21128 if municipality =="SAN JOSE CHIAPA"
replace uniqueid=21129 if municipality =="SAN JOSE MIAHUATLAN"
replace uniqueid=21130 if municipality =="SAN JUAN ATENCO"
replace uniqueid=21131 if municipality =="SAN JUAN ATZOMPA"
replace uniqueid=21132 if municipality =="SAN MARTIN TEXMELUCAN"
replace uniqueid=21133 if municipality =="SAN MARTIN TOTOLTEPEC"
replace uniqueid=21134 if municipality =="SAN MATIAS TLALANCALECA"
replace uniqueid=21135 if municipality =="SAN MIGUEL IXITLAN"
replace uniqueid=21136 if municipality =="SAN MIGUEL XOXTLA"
replace uniqueid=21137 if municipality =="SAN NICOLAS BUENOS AIRES"
replace uniqueid=21138 if municipality =="SAN NICOLAS DE LOS RANCHOS"
replace uniqueid=21139 if municipality =="SAN PABLO ANICANO"
replace uniqueid=21140 if municipality =="SAN PEDRO CHOLULA"
replace uniqueid=21141 if municipality =="SAN PEDRO YELOIXTLAHUACA"
replace uniqueid=21142 if municipality =="SAN SALVADOR EL SECO"
replace uniqueid=21143 if municipality =="SAN SALVADOR EL VERDE"
replace uniqueid=21144 if municipality =="SAN SALVADOR HUIXCOLOTLA"
replace uniqueid=21145 if municipality =="SAN SEBASTIAN TLACOTEPEC"
replace uniqueid=21146 if municipality =="SANTA CATARINA TLALTEMPAN"
replace uniqueid=21147 if municipality =="SANTA INES AHUATEMPAN"
replace uniqueid=21148 if municipality =="SANTA ISABEL CHOLULA"
replace uniqueid=21149 if municipality =="SANTIAGO MIAHUATLAN"
replace uniqueid=21151 if municipality =="SANTO TOMAS HUEYOTLIPAN"
replace uniqueid=21152 if municipality =="SOLTEPEC"
replace uniqueid=21153 if municipality =="TECALI DE HERRERA"
replace uniqueid=21154 if municipality =="TECAMACHALCO"
replace uniqueid=21155 if municipality =="TECOMATLAN"
replace uniqueid=21156 if municipality =="TEHUACAN"
replace uniqueid=21157 if municipality =="TEHUITZINGO"
replace uniqueid=21158 if municipality =="TENAMPULCO"
replace uniqueid=21159 if municipality =="TEOPANTLAN"
replace uniqueid=21160 if municipality =="TEOTLALCO"
replace uniqueid=21161 if municipality =="TEPANCO DE LOPEZ"
replace uniqueid=21162 if municipality =="TEPANGO DE RODRIGUEZ"
replace uniqueid=21163 if municipality =="TEPATLAXCO DE HIDALGO"
replace uniqueid=21164 if municipality =="TEPEACA"
replace uniqueid=21165 if municipality =="TEPEMAXALCO"
replace uniqueid=21166 if municipality =="TEPEOJUMA"
replace uniqueid=21167 if municipality =="TEPETZINTLA"
replace uniqueid=21168 if municipality =="TEPEXCO"
replace uniqueid=21169 if municipality =="TEPEXI DE RODRIGUEZ"
replace uniqueid=21170 if municipality =="TEPEYAHUALCO"
replace uniqueid=21171 if municipality =="TEPEYAHUALCO DE CUAUHTEMOC"
replace uniqueid=21172 if municipality =="TETELA DE OCAMPO"
replace uniqueid=21173 if municipality =="TETELES DE AVILA CASTILLO"
replace uniqueid=21174 if municipality =="TEZIUTLAN"
replace uniqueid=21175 if municipality =="TIANGUISMANALCO"
replace uniqueid=21176 if municipality =="TILAPA"
replace uniqueid=21179 if municipality =="TLACHICHUCA"
replace uniqueid=21177 if municipality =="TLACOTEPEC DE BENITO JUAREZ"
replace uniqueid=21178 if municipality =="TLACUILOTEPEC"
replace uniqueid=21180 if municipality =="TLAHUAPAN"
replace uniqueid=21181 if municipality =="TLALTENANGO"
replace uniqueid=21182 if municipality =="TLANEPANTLA"
replace uniqueid=21183 if municipality =="TLAOLA"
replace uniqueid=21184 if municipality =="TLAPACOYA"
replace uniqueid=21185 if municipality =="TLAPANALA"
replace uniqueid=21186 if municipality =="TLATLAUQUITEPEC"
replace uniqueid=21187 if municipality =="TLAXCO"
replace uniqueid=21188 if municipality =="TOCHIMILCO"
replace uniqueid=21189 if municipality =="TOCHTEPEC"
replace uniqueid=21190 if municipality =="TOTOLTEPEC DE GUERRERO"
replace uniqueid=21191 if municipality =="TULCINGO"
replace uniqueid=21192 if municipality =="TUZAMAPAN DE GALEANA"
replace uniqueid=21193 if municipality =="TZICATLACOYAN"
replace uniqueid=21194 if municipality =="VENUSTIANO CARRANZA"
replace uniqueid=21195 if municipality =="VICENTE GUERRERO"
replace uniqueid=21196 if municipality =="XAYACATLAN DE BRAVO"
replace uniqueid=21197 if municipality =="XICOTEPEC"
replace uniqueid=21198 if municipality =="XICOTLAN"
replace uniqueid=21199 if municipality =="XIUTETELCO"
replace uniqueid=21200 if municipality =="XOCHIAPULCO"
replace uniqueid=21201 if municipality =="XOCHILTEPEC"
replace uniqueid=21202 if municipality =="XOCHITLAN DE VICENTE SUAREZ"
replace uniqueid=21203 if municipality =="XOCHITLAN TODOS SANTOS"
replace uniqueid=21204 if municipality =="YAONAHUAC"
replace uniqueid=21205 if municipality =="YEHUALTEPEC"
replace uniqueid=21206 if municipality =="ZACAPALA"
replace uniqueid=21207 if municipality =="ZACAPOAXTLA"
replace uniqueid=21208 if municipality =="ZACATLAN"
replace uniqueid=21209 if municipality =="ZAPOTITLAN"
replace uniqueid=21210 if municipality =="ZAPOTITLAN DE MENDEZ"
replace uniqueid=21211 if municipality =="ZARAGOZA"
replace uniqueid=21212 if municipality =="ZAUTLA"
replace uniqueid=21213 if municipality =="ZIHUATEUTLA"
replace uniqueid=21214 if municipality =="ZINACATEPEC"
replace uniqueid=21215 if municipality =="ZONGOZOTLA"
replace uniqueid=21216 if municipality =="ZOQUIAPAN"
replace uniqueid=21217 if municipality =="ZOQUITLAN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSN PAS)

foreach var in PAN PRI PRD PT PVEM PC PSN PAS total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSN inv_mun_PAS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PSN_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r==1 
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PAS" if PAS_r==1
drop *_r

gen year = 2001
gen month ="November"

save Puebla_Section_2001.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Resultados por casilla elecci贸n 2002 Molcaxac.xlsx", sheet("aytos") cellrange(A8:V16) firstrow clear

rename secci贸n section
drop if section==.

g municipality = "MOLCAXAZ EXTRAORDINARIO"
g uniqueid = 21098

rename F PAN
rename G PRI
rename H PRD
rename K PC
rename M PAS
rename CiudadanosenListaNominal listanominal
rename Votaci贸nTotal total

collapse (sum) PAN PRI PRD PC PAS total listanominal, by (municipality uniqueid section)

g turnout = total/listanominal

egen valid = rowtotal(PAN PRI PRD PC PAS)

foreach var in PAN PRI PRD PC PAS total listanominal valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen inv_mun_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PC inv_mun_PAS, gen(PAN_r PRI_r PRD_r PC_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r==1 
replace winner = "PC" if PC_r==1
replace winner = "PAS" if PAS_r==1
drop *_r

gen year = 2002
gen month ="June"

save Puebla_Section_2002.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2004.csv, clear

rename municipio  municipality
rename seccion section
rename votaciontotal total

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan - conv total listanominal , replace

collapse (sum)  pan -  conv total listanominal , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename conv  PC

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=21001 if municipality =="ACAJETE"
replace uniqueid=21002 if municipality =="ACATENO"
replace uniqueid=21003 if municipality =="ACATLAN"
replace uniqueid=21004 if municipality =="ACATZINGO"
replace uniqueid=21005 if municipality =="ACTEOPAN"
replace uniqueid=21006 if municipality =="AHUACATLAN"
replace uniqueid=21007 if municipality =="AHUATLAN"
replace uniqueid=21008 if municipality =="AHUAZOTEPEC"
replace uniqueid=21009 if municipality =="AHUEHUETITLA"
replace uniqueid=21010 if municipality =="AJALPAN"
replace uniqueid=21011 if municipality =="ALBINO ZERTUCHE"
replace uniqueid=21012 if municipality =="ALJOJUCA"
replace uniqueid=21013 if municipality =="ALTEPEXI"
replace uniqueid=21014 if municipality =="AMIXTLAN"
replace uniqueid=21015 if municipality =="AMOZOC"
replace uniqueid=21016 if municipality =="AQUIXTLA"
replace uniqueid=21017 if municipality =="ATEMPAN"
replace uniqueid=21018 if municipality =="ATEXCAL"
replace uniqueid=21080 if municipality =="ATLEQUIZAYAN"
replace uniqueid=21019 if municipality =="ATLIXCO"
replace uniqueid=21020 if municipality =="ATOYATEMPAN"
replace uniqueid=21021 if municipality =="ATZALA"
replace uniqueid=21022 if municipality =="ATZITZIHUACAN"
replace uniqueid=21023 if municipality =="ATZITZINTLA"
replace uniqueid=21024 if municipality =="AXUTLA"
replace uniqueid=21025 if municipality =="AYOTOXCO DE GUERRERO"
replace uniqueid=21026 if municipality =="CALPAN"
replace uniqueid=21027 if municipality =="CALTEPEC"
replace uniqueid=21028 if municipality =="CAMOCUAUTLA"
replace uniqueid=21099 if municipality =="CA袮DA MORELOS" | strpos(muni, "ADA MORELOS")>0
replace uniqueid=21029 if municipality =="CAXHUACAN"
replace uniqueid=21045 if municipality =="CHALCHICOMULA DE SESMA"
replace uniqueid=21046 if municipality =="CHAPULCO"
replace uniqueid=21047 if municipality =="CHIAUTLA"
replace uniqueid=21048 if municipality =="CHIAUTZINGO"
replace uniqueid=21050 if municipality =="CHICHIQUILA"
replace uniqueid=21049 if municipality =="CHICONCUAUTLA"
replace uniqueid=21051 if municipality =="CHIETLA"
replace uniqueid=21052 if municipality =="CHIGMECATITLAN"
replace uniqueid=21053 if municipality =="CHIGNAHUAPAN"
replace uniqueid=21054 if municipality =="CHIGNAUTLA"
replace uniqueid=21055 if municipality =="CHILA"
replace uniqueid=21056 if municipality =="CHILA DE LA SAL"
replace uniqueid=21058 if municipality =="CHILCHOTLA"
replace uniqueid=21059 if municipality =="CHINANTLA"
replace uniqueid=21030 if municipality =="COATEPEC"
replace uniqueid=21031 if municipality =="COATZINGO"
replace uniqueid=21032 if municipality =="COHETZALA"
replace uniqueid=21033 if municipality =="COHUECAN"
replace uniqueid=21034 if municipality =="CORONANGO"
replace uniqueid=21035 if municipality =="COXCATLAN"
replace uniqueid=21036 if municipality =="COYOMEAPAN"
replace uniqueid=21037 if municipality =="COYOTEPEC"
replace uniqueid=21038 if municipality =="CUAPIAXTLA DE MADERO"
replace uniqueid=21039 if municipality =="CUAUTEMPAN"
replace uniqueid=21040 if municipality =="CUAUTINCHAN"
replace uniqueid=21041 if municipality =="CUAUTLANCINGO"
replace uniqueid=21042 if municipality =="CUAYUCA DE ANDRADE"
replace uniqueid=21043 if municipality =="CUETZALAN DEL PROGRESO"
replace uniqueid=21044 if municipality =="CUYOACO"
replace uniqueid=21060 if municipality =="DOMINGO ARENAS"
replace uniqueid=21061 if municipality =="ELOXOCHITLAN"
replace uniqueid=21062 if municipality =="EPATLAN"
replace uniqueid=21063 if municipality =="ESPERANZA"
replace uniqueid=21064 if municipality =="FRANCISCO Z. MENA"
replace uniqueid=21065 if municipality =="GENERAL FELIPE ANGELES"
replace uniqueid=21066 if municipality =="GUADALUPE"
replace uniqueid=21067 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=21068 if municipality =="HERMENEGILDO GALEANA"
replace uniqueid=21057 if municipality =="HONEY"
replace uniqueid=21069 if municipality =="HUAQUECHULA"
replace uniqueid=21070 if municipality =="HUATLATLAUCA"
replace uniqueid=21071 if municipality =="HUAUCHINANGO"
replace uniqueid=21072 if municipality =="HUEHUETLA"
replace uniqueid=21073 if municipality =="HUEHUETLAN EL CHICO"
replace uniqueid=21150 if municipality =="HUEHUETLAN EL GRANDE"
replace uniqueid=21074 if municipality =="HUEJOTZINGO"
replace uniqueid=21075 if municipality =="HUEYAPAN"
replace uniqueid=21076 if municipality =="HUEYTAMALCO"
replace uniqueid=21077 if municipality =="HUEYTLALPAN"
replace uniqueid=21078 if municipality =="HUITZILAN DE SERDAN"
replace uniqueid=21079 if municipality =="HUITZILTEPEC"
replace uniqueid=21081 if municipality =="IXCAMILPA DE GUERRERO"
replace uniqueid=21082 if municipality =="IXCAQUIXTLA"
replace uniqueid=21083 if municipality =="IXTACAMAXTITLAN"
replace uniqueid=21084 if municipality =="IXTEPEC"
replace uniqueid=21085 if municipality =="IZUCAR DE MATAMOROS"
replace uniqueid=21086 if municipality =="JALPAN"
replace uniqueid=21087 if municipality =="JOLALPAN"
replace uniqueid=21088 if municipality =="JONOTLA"
replace uniqueid=21089 if municipality =="JOPALA"
replace uniqueid=21090 if municipality =="JUAN C. BONILLA"
replace uniqueid=21091 if municipality =="JUAN GALINDO"
replace uniqueid=21092 if municipality =="JUAN N. MENDEZ"
replace uniqueid=21095 if municipality =="LA MAGDALENA TLATLAUQUITEPEC"
replace uniqueid=21093 if municipality =="LAFRAGUA"
replace uniqueid=21094 if municipality =="LIBRES"
replace uniqueid=21118 if municipality =="LOS REYES DE JUAREZ"
replace uniqueid=21096 if municipality =="MAZAPILTEPEC DE JUAREZ"
replace uniqueid=21097 if municipality =="MIXTLA"
replace uniqueid=21098 if municipality =="MOLCAXAC"
replace uniqueid=21100 if municipality =="NAUPAN"
replace uniqueid=21101 if municipality =="NAUZONTLA"
replace uniqueid=21102 if municipality =="NEALTICAN"
replace uniqueid=21103 if municipality =="NICOLAS BRAVO"
replace uniqueid=21104 if municipality =="NOPALUCAN"
replace uniqueid=21105 if municipality =="OCOTEPEC"
replace uniqueid=21106 if municipality =="OCOYUCAN"
replace uniqueid=21107 if municipality =="OLINTLA"
replace uniqueid=21108 if municipality =="ORIENTAL"
replace uniqueid=21109 if municipality =="PAHUATLAN"
replace uniqueid=21110 if municipality =="PALMAR DE BRAVO"
replace uniqueid=21111 if municipality =="PANTEPEC"
replace uniqueid=21112 if municipality =="PETLALCINGO"
replace uniqueid=21113 if municipality =="PIAXTLA"
replace uniqueid=21114 if municipality =="PUEBLA"
replace uniqueid=21115 if municipality =="QUECHOLAC"
replace uniqueid=21116 if municipality =="QUIMIXTLAN"
replace uniqueid=21117 if municipality =="RAFAEL LARA GRAJALES"
replace uniqueid=21119 if municipality =="SAN ANDRES CHOLULA"
replace uniqueid=21120 if municipality =="SAN ANTONIO CA袮DA" | strpos(municipality, "SAN ANTONIO CA")>0
replace uniqueid=21121 if municipality =="SAN DIEGO LA MESA TOCHIMILTZINGO"
replace uniqueid=21122 if municipality =="SAN FELIPE TEOTLALCINGO"
replace uniqueid=21123 if municipality =="SAN FELIPE TEPATLAN"
replace uniqueid=21124 if municipality =="SAN GABRIEL CHILAC"
replace uniqueid=21125 if municipality =="SAN GREGORIO ATZOMPA"
replace uniqueid=21126 if municipality =="SAN JERONIMO TECUANIPAN"
replace uniqueid=21127 if municipality =="SAN JERONIMO XAYACATLAN"
replace uniqueid=21128 if municipality =="SAN JOSE CHIAPA"
replace uniqueid=21129 if municipality =="SAN JOSE MIAHUATLAN"
replace uniqueid=21130 if municipality =="SAN JUAN ATENCO"
replace uniqueid=21131 if municipality =="SAN JUAN ATZOMPA"
replace uniqueid=21132 if municipality =="SAN MARTIN TEXMELUCAN"
replace uniqueid=21133 if municipality =="SAN MARTIN TOTOLTEPEC"
replace uniqueid=21134 if municipality =="SAN MATIAS TLALANCALECA"
replace uniqueid=21135 if municipality =="SAN MIGUEL IXITLAN"
replace uniqueid=21136 if municipality =="SAN MIGUEL XOXTLA"
replace uniqueid=21137 if municipality =="SAN NICOLAS BUENOS AIRES"
replace uniqueid=21138 if municipality =="SAN NICOLAS DE LOS RANCHOS"
replace uniqueid=21139 if municipality =="SAN PABLO ANICANO"
replace uniqueid=21140 if municipality =="SAN PEDRO CHOLULA"
replace uniqueid=21141 if municipality =="SAN PEDRO YELOIXTLAHUACA"
replace uniqueid=21142 if municipality =="SAN SALVADOR EL SECO"
replace uniqueid=21143 if municipality =="SAN SALVADOR EL VERDE"
replace uniqueid=21144 if municipality =="SAN SALVADOR HUIXCOLOTLA"
replace uniqueid=21145 if municipality =="SAN SEBASTIAN TLACOTEPEC"
replace uniqueid=21146 if municipality =="SANTA CATARINA TLALTEMPAN"
replace uniqueid=21147 if municipality =="SANTA INES AHUATEMPAN"
replace uniqueid=21148 if municipality =="SANTA ISABEL CHOLULA"
replace uniqueid=21149 if municipality =="SANTIAGO MIAHUATLAN"
replace uniqueid=21151 if municipality =="SANTO TOMAS HUEYOTLIPAN"
replace uniqueid=21152 if municipality =="SOLTEPEC"
replace uniqueid=21153 if municipality =="TECALI DE HERRERA"
replace uniqueid=21154 if municipality =="TECAMACHALCO"
replace uniqueid=21155 if municipality =="TECOMATLAN"
replace uniqueid=21156 if municipality =="TEHUACAN"
replace uniqueid=21157 if municipality =="TEHUITZINGO"
replace uniqueid=21158 if municipality =="TENAMPULCO"
replace uniqueid=21159 if municipality =="TEOPANTLAN"
replace uniqueid=21160 if municipality =="TEOTLALCO"
replace uniqueid=21161 if municipality =="TEPANCO DE LOPEZ"
replace uniqueid=21162 if municipality =="TEPANGO DE RODRIGUEZ"
replace uniqueid=21163 if municipality =="TEPATLAXCO DE HIDALGO"
replace uniqueid=21164 if municipality =="TEPEACA"
replace uniqueid=21165 if municipality =="TEPEMAXALCO"
replace uniqueid=21166 if municipality =="TEPEOJUMA"
replace uniqueid=21167 if municipality =="TEPETZINTLA"
replace uniqueid=21168 if municipality =="TEPEXCO"
replace uniqueid=21169 if municipality =="TEPEXI DE RODRIGUEZ"
replace uniqueid=21170 if municipality =="TEPEYAHUALCO"
replace uniqueid=21171 if municipality =="TEPEYAHUALCO DE CUAUHTEMOC"
replace uniqueid=21172 if municipality =="TETELA DE OCAMPO"
replace uniqueid=21173 if municipality =="TETELES DE AVILA CASTILLO"
replace uniqueid=21174 if municipality =="TEZIUTLAN"
replace uniqueid=21175 if municipality =="TIANGUISMANALCO"
replace uniqueid=21176 if municipality =="TILAPA"
replace uniqueid=21179 if municipality =="TLACHICHUCA"
replace uniqueid=21177 if municipality =="TLACOTEPEC DE BENITO JUAREZ"
replace uniqueid=21178 if municipality =="TLACUILOTEPEC"
replace uniqueid=21180 if municipality =="TLAHUAPAN"
replace uniqueid=21181 if municipality =="TLALTENANGO"
replace uniqueid=21182 if municipality =="TLANEPANTLA"
replace uniqueid=21183 if municipality =="TLAOLA"
replace uniqueid=21184 if municipality =="TLAPACOYA"
replace uniqueid=21185 if municipality =="TLAPANALA"
replace uniqueid=21186 if municipality =="TLATLAUQUITEPEC"
replace uniqueid=21187 if municipality =="TLAXCO"
replace uniqueid=21188 if municipality =="TOCHIMILCO"
replace uniqueid=21189 if municipality =="TOCHTEPEC"
replace uniqueid=21190 if municipality =="TOTOLTEPEC DE GUERRERO"
replace uniqueid=21191 if municipality =="TULCINGO"
replace uniqueid=21192 if municipality =="TUZAMAPAN DE GALEANA"
replace uniqueid=21193 if municipality =="TZICATLACOYAN"
replace uniqueid=21194 if municipality =="VENUSTIANO CARRANZA"
replace uniqueid=21195 if municipality =="VICENTE GUERRERO"
replace uniqueid=21196 if municipality =="XAYACATLAN DE BRAVO"
replace uniqueid=21197 if municipality =="XICOTEPEC"
replace uniqueid=21198 if municipality =="XICOTLAN"
replace uniqueid=21199 if municipality =="XIUTETELCO"
replace uniqueid=21200 if municipality =="XOCHIAPULCO"
replace uniqueid=21201 if municipality =="XOCHILTEPEC"
replace uniqueid=21202 if municipality =="XOCHITLAN DE VICENTE SUAREZ"
replace uniqueid=21203 if municipality =="XOCHITLAN TODOS SANTOS"
replace uniqueid=21204 if municipality =="YAONAHUAC"
replace uniqueid=21205 if municipality =="YEHUALTEPEC"
replace uniqueid=21206 if municipality =="ZACAPALA"
replace uniqueid=21207 if municipality =="ZACAPOAXTLA"
replace uniqueid=21208 if municipality =="ZACATLAN"
replace uniqueid=21209 if municipality =="ZAPOTITLAN"
replace uniqueid=21210 if municipality =="ZAPOTITLAN DE MENDEZ"
replace uniqueid=21211 if municipality =="ZARAGOZA"
replace uniqueid=21212 if municipality =="ZAUTLA"
replace uniqueid=21213 if municipality =="ZIHUATEUTLA"
replace uniqueid=21214 if municipality =="ZINACATEPEC"
replace uniqueid=21215 if municipality =="ZONGOZOTLA"
replace uniqueid=21216 if municipality =="ZOQUIAPAN"
replace uniqueid=21217 if municipality =="ZOQUITLAN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC)

foreach var in PAN PRI PRD PT PVEM PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r==1 
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
drop *_r

gen year = 2004
gen month ="November"

sort section

save Puebla_Section_2004.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "ResultadosDeLaElecci贸nVotaci贸nXCasilla Aytos extra. 2005.xlsx", sheet("ayto") cellrange(A7:N15) firstrow clear

rename Secci贸n section
drop if section==.

g municipality = "SANTA INES AHUATEMPAN EXTRAORDINARIO"
g uniqueid = 21147

rename PARTIDOACCI脫NNACIONAL PAN
rename PARTIDODELTRABAJO PT
rename CONVERGENCIA PC
rename Votaci贸nTotal total

collapse (sum) PAN-PC total, by (municipality uniqueid section)


preserve
use "..\..\all_months_years.dta", clear
keep if ed==21 & month==4 & year==2005
rename lista listanominal
rename seccion section
save "merge.dta", replace
restore

merge 1:1 section using "merge.dta", keepusing(listanominal)
drop if _merge==2
drop _merge
erase merge.dta

g turnout = total/listanominal

egen valid = rowtotal(PAN PRI PRD PT PVEM PC)

foreach var in PAN PRI PRD PT PVEM PC total listanominal valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen inv_mun_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r==1 
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
drop *_r

gen year = 2005
gen month ="May"

save Puebla_Section_2005.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel Ayu_Seccion_2007.xlsx, clear case(lower) firstrow

rename municipio  municipality
capture rename secci贸n seccin
rename seccin section
drop total

drop if municipality=="" & section==.
egen total =  rowtotal(pan cupg cpbp pt pna asppn pec noregistrados nulos )
drop if total==. | total==0 

destring pan -  listanominal , replace

collapse (sum)  pan -  pec listanominal total, by (municipality section)

rename pan  PAN
rename cupg  PRI_PVEM
rename cpbp  PRD_PC
rename pt   PT
rename pna PANAL
rename asppn  PAS
rename pec  PEC

gen turnout =  total/listanominal

replace municipality = subinstr(municipality, "谩", "a", .)
replace municipality = subinstr(municipality, "茅", "e", .)
replace municipality = subinstr(municipality, "铆", "i", .)
replace municipality = subinstr(municipality, "贸", "o", .)
replace municipality = subinstr(municipality, "帽", "n", .)
replace municipality = subinstr(municipality, "煤", "u", .)
replace municipality = subinstr(municipality, "脕", "A", .)

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
replace uniqueid=21025 if municipality =="Ayotoxco de Guerrero"
replace uniqueid=21026 if municipality =="Calpan"
replace uniqueid=21027 if municipality =="Caltepec"
replace uniqueid=21028 if municipality =="Camocuautla"
replace uniqueid=21099 if municipality =="Canada Morelos"
replace uniqueid=21029 if municipality =="Caxhuacan"
replace uniqueid=21045 if municipality =="Chalchicomula de Sesma"
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
replace uniqueid=21056 if municipality =="Chila de la Sal"
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
replace uniqueid=21038 if municipality =="Cuapiaxtla de Madero"
replace uniqueid=21039 if municipality =="Cuautempan"
replace uniqueid=21040 if municipality =="Cuautinchan"
replace uniqueid=21041 if municipality =="Cuautlancingo"
replace uniqueid=21042 if municipality =="Cuayuca de Andrade"
replace uniqueid=21043 if municipality =="Cuetzalan del Progreso"
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
replace uniqueid=21073 if municipality =="Huehuetlan el Chico"
replace uniqueid=21150 if municipality =="Huehuetlan El Grande"
replace uniqueid=21074 if municipality =="Huejotzingo"
replace uniqueid=21075 if municipality =="Hueyapan"
replace uniqueid=21076 if municipality =="Hueytamalco"
replace uniqueid=21077 if municipality =="Hueytlalpan"
replace uniqueid=21078 if municipality =="Huitzilan de Serdan"
replace uniqueid=21079 if municipality =="Huitziltepec"
replace uniqueid=21081 if municipality =="Ixcamilpa de Guerrero"
replace uniqueid=21082 if municipality =="Ixcaquixtla"
replace uniqueid=21083 if municipality =="Ixtacamaxtitlan"
replace uniqueid=21084 if municipality =="Ixtepec"
replace uniqueid=21085 if municipality =="Izucar de Matamoros"
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
replace uniqueid=21118 if municipality =="Los Reyes de Juarez"
replace uniqueid=21096 if municipality =="Mazapiltepec de Juarez"
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
replace uniqueid=21110 if municipality =="Palmar de Bravo"
replace uniqueid=21111 if municipality =="Pantepec"
replace uniqueid=21112 if municipality =="Petlalcingo"
replace uniqueid=21113 if municipality =="Piaxtla"
replace uniqueid=21114 if municipality =="Puebla" | municipality=="Heroica Ciudad de Puebla de Zaragoza"
replace uniqueid=21115 if municipality =="Quecholac"
replace uniqueid=21116 if municipality =="Quimixtlan"
replace uniqueid=21117 if municipality =="Rafael Lara Grajales"
replace uniqueid=21119 if municipality =="San Andres Cholula"
replace uniqueid=21120 if municipality =="San Antonio Canada"
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
replace uniqueid=21138 if municipality =="San Nicolas de los Ranchos"
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
replace uniqueid=21153 if municipality =="Tecali de Herrera"
replace uniqueid=21154 if municipality =="Tecamachalco"
replace uniqueid=21155 if municipality =="Tecomatlan"
replace uniqueid=21156 if municipality =="Tehuacan"
replace uniqueid=21157 if municipality =="Tehuitzingo"
replace uniqueid=21158 if municipality =="Tenampulco"
replace uniqueid=21159 if municipality =="Teopantlan"
replace uniqueid=21160 if municipality =="Teotlalco"
replace uniqueid=21161 if municipality =="Tepanco de Lopez"
replace uniqueid=21162 if municipality =="Tepango de Rodriguez"
replace uniqueid=21163 if municipality =="Tepatlaxco de Hidalgo"
replace uniqueid=21164 if municipality =="Tepeaca"
replace uniqueid=21165 if municipality =="Tepemaxalco"
replace uniqueid=21166 if municipality =="Tepeojuma"
replace uniqueid=21167 if municipality =="Tepetzintla"
replace uniqueid=21168 if municipality =="Tepexco"
replace uniqueid=21169 if municipality =="Tepexi de Rodriguez"
replace uniqueid=21170 if municipality =="Tepeyahualco"
replace uniqueid=21171 if municipality =="Tepeyahualco de Cuauhtemoc"
replace uniqueid=21172 if municipality =="Tetela de Ocampo"
replace uniqueid=21173 if municipality =="Teteles de Avila Castillo"
replace uniqueid=21174 if municipality =="Teziutlan"
replace uniqueid=21175 if municipality =="Tianguismanalco"
replace uniqueid=21176 if municipality =="Tilapa"
replace uniqueid=21179 if municipality =="Tlachichuca"
replace uniqueid=21177 if municipality =="Tlacotepec de Benito Juarez"
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
replace uniqueid=21190 if municipality =="Totoltepec de Guerrero"
replace uniqueid=21191 if municipality =="Tulcingo"
replace uniqueid=21192 if municipality =="Tuzamapan de Galeana"
replace uniqueid=21193 if municipality =="Tzicatlacoyan"
replace uniqueid=21194 if municipality =="Venustiano Carranza"
replace uniqueid=21195 if municipality =="Vicente Guerrero"
replace uniqueid=21196 if municipality =="Xayacatlan de Bravo"
replace uniqueid=21197 if municipality =="Xicotepec"
replace uniqueid=21198 if municipality =="Xicotlan"
replace uniqueid=21199 if municipality =="Xiutetelco"
replace uniqueid=21200 if municipality =="Xochiapulco"
replace uniqueid=21201 if municipality =="Xochiltepec"
replace uniqueid=21202 if municipality =="Xochitlan de Vicente Suarez"
replace uniqueid=21203 if municipality =="Xochitlan Todos Santos"
replace uniqueid=21204 if municipality =="Yaonahuac"
replace uniqueid=21205 if municipality =="Yehualtepec"
replace uniqueid=21206 if municipality =="Zacapala"
replace uniqueid=21207 if municipality =="Zacapoaxtla"
replace uniqueid=21208 if municipality =="Zacatlan"
replace uniqueid=21209 if municipality =="Zapotitlan"
replace uniqueid=21210 if municipality =="Zapotitlan de Mendez"
replace uniqueid=21211 if municipality =="Zaragoza"
replace uniqueid=21212 if municipality =="Zautla"
replace uniqueid=21213 if municipality =="Zihuateutla"
replace uniqueid=21214 if municipality =="Zinacatepec"
replace uniqueid=21215 if municipality =="Zongozotla"
replace uniqueid=21216 if municipality =="Zoquiapan"
replace uniqueid=21217 if municipality =="Zoquitlan"

egen valid = rowtotal(PAN PRI_PVEM PRD_PC PT PANAL PAS PEC)

foreach var in PAN PRI_PVEM PRD_PC PT PANAL PAS PEC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD_PC inv_mun_PT inv_mun_PANAL inv_mun_PAS inv_mun_PEC, gen(PAN_r PRI_PVEM_r PRD_PC_r PT_r PANAL_r PAS_r PEC_r)
drop inv_mun_*
 
gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRD_PC" if PRD_PC_r==1 
replace winner = "PT" if PT_r==1
replace winner = "PANAL" if PANAL_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PEC" if PEC_r==1
drop *_r

gen year = 2007
gen month ="November"

sort section

save Puebla_Section_2007.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "ResultadosDeLaElecci贸nVotaci贸nXCasilla Aytos extra. 2008.xlsx", sheet("ayto") cellrange(A7:R25) firstrow clear

destring _all, replace

rename Secci贸n section
drop if section==.

g municipality = "GENERAL FELIPE ANGELES EXTRAORDINARIO"
g uniqueid = 21065

rename PARTIDOACCI脫NNACIONAL PAN
rename PARTIDODELTRABAJO PT
rename CONVERGENCIA PC
rename PARTIDONUEVAALIANZA PANAL
rename ALTERNATIVASOCIALDEM脫CRATAPAR PAS
rename Votaci贸nTotal total
rename ListaNominal listanominal

collapse (sum) PAN-PAS total listanominal, by (municipality uniqueid section)

g turnout = total/listanominal

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAS)

foreach var in PAN PRI PRD PT PVEM PC PANAL PAS total listanominal valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen inv_mun_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r==1
replace winner = "PANAL" if PANAL_r==1
replace winner = "PT" if PT_r ==1 
replace winner = "PAS" if PAS_r ==1 
drop *_r

gen year = 2008
gen month ="June"

save Puebla_Section_2008.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel Ayu_Seccion_2010.xlsx, clear firstrow case(lower)

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan_prd_pc_panal - pt total listanominal  , replace

collapse (sum)  pan_prd_pc_panal - pt total listanominal, by (municipality section)

rename pan_prd_pc_panal  PAN_PRD_PC_PANAL
rename pri_pvem  PRI_PVEM
rename pt   PT

gen turnout =  total/listanominal

replace municipality = subinstr(municipality, "脕", "A", .)
replace municipality = subinstr(municipality, "脡", "E", .)
replace municipality = subinstr(municipality, "脥", "I", .)
replace municipality = subinstr(municipality, "脫", "O", .)
replace municipality = subinstr(municipality, "脷", "U", .)
replace municipality = subinstr(municipality, "脩", "N", .)
replace municipality = proper(municipality)
replace municipality = trim(municipality)

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
replace uniqueid=21099 if municipality =="Canada Morelos"
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
replace uniqueid=21120 if municipality =="San Antonio Canada"
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

egen valid = rowtotal(PAN_PRD_PC_PANAL PRI_PVEM PT)

foreach var in PAN_PRD_PC_PANAL PRI_PVEM PT total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD_PC_PANAL inv_mun_PRI_PVEM inv_mun_PT, gen(PAN_PRD_PC_PANAL_r PRI_PVEM_r PT_r)
drop inv_mun_*

gen winner = "PAN_PRD_PC_PANAL" if PAN_PRD_PC_PANAL_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PT" if PT_r==1
drop *_r

gen year = 2010
gen month ="July"

sort section

save Puebla_Section_2010.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "Resul_ por casillas e individuales elec extraordinaria 2011.xlsx", sheet("2011") cellrange(A6:K75) firstrow clear

rename SECCION section
drop if section==.

rename MUNICIPIO municipality
replace municipality = municipality + " EXTRAORDINARIO"
gen   uniqueid= 0
replace uniqueid=21081 if municipality=="IXCAMILPA DE GUERRERO EXTRAORDINARIO"
replace uniqueid=21126 if municipality=="SAN JERONIMO TECUANIPAN EXTRAORDINARIO"
replace uniqueid=21183 if municipality=="TLAOLA EXTRAORDINARIO"

rename E PAN
rename F PRI_PVEM
rename G PC
rename H PANAL
rename VOTACIONTOTAL total

collapse (sum) PAN-PANAL total, by (municipality uniqueid section)

preserve
use "..\..\all_months_years.dta", clear
keep if ed==21 & month==6 & year==2011
rename lista listanominal
rename seccion section
save "merge.dta", replace
restore

merge 1:1 section using "merge.dta", keepusing(listanominal)
drop if _merge==2
drop _merge
erase merge.dta

g turnout = total/listanominal

egen valid = rowtotal(PAN PRI_PVEM PC PANAL)

foreach var in PAN PRI_PVEM PC PANAL total listanominal valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen inv_mun_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PC inv_mun_PANAL, gen(PAN_r PRI_PVEM_r PC_r PANAL_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PC" if PC_r==1
replace winner = "PANAL" if PANAL_r==1
drop *_r

gen year = 2011
gen month ="July"

save Puebla_Section_2011.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

* I AM NOT SURE I GOT THE COALITIONS WRONG
* http://es.wikipedia.org/wiki/Elecciones_estatales_de_Puebla_de_2013
* I put a request to ask for them 

import excel "RESULTADOS_POR_CASILLA_AYUNTAMIENTOS Y DIPUTADOS_2012_2013.xlsx", sheet("AYUNTAMIENTOS FINAL") cellrange(A7:O6903) firstrow clear

rename MUNICIPIO  municipality
capture rename SECCI脫N SECCIN
rename SECCIN section
drop if municipality=="" & section==""
destring section, replace
rename NoDECIUDADANOSENLISTANOMI listanominal
capture rename VOTACI脫NTOTAL VOTACINTOTAL
rename VOTACINTOTAL total
drop if total==. | total==0 
drop N O
compress municipality

destring PAN_PRD_PANAL_CPP -  listanominal  , replace

collapse (sum)  PAN_PRD_PANAL_CPP -  listanominal, by (municipality section)

replace municipality = subinstr(municipality, "谩", "a", .)
replace municipality = subinstr(municipality, "茅", "e", .)
replace municipality = subinstr(municipality, "铆", "i", .)
replace municipality = subinstr(municipality, "贸", "o", .)
replace municipality = subinstr(municipality, "帽", "n", .)
replace municipality = subinstr(municipality, "脕", "A", .)
replace municipality = subinstr(municipality, "煤", "u", .)
replace municipality = proper(municipality)

merge m:1 municipality using Coaliciones_2013.dta
drop _merge

* CPP : Compromiso por Puebla
rename coalicion coalition
gen PAN_PRD_PANAL_CPP_PC =  PAN_PRD_PANAL_CPP +PC if coalition == "PAN_PRD_PANAL_CPP, PC"
replace PAN_PRD_PANAL_CPP = 0 if coalition == "PAN_PRD_PANAL_CPP, PC"
replace PC = 0 if coalition == "PAN_PRD_PANAL_CPP, PC"

gen PAN_PRD_PANAL_CPP_PC_PSI = PAN_PRD_PANAL_CPP + PC + PSI if coalition ==  "PAN_PRD_PANAL_CPP, PC, PSI"
replace PAN_PRD_PANAL_CPP = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PC, PSI"
replace PC = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PC, PSI"
replace PSI = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PC, PSI"

gen PAN_PRD_PANAL_CPP_PSI = PAN_PRD_PANAL_CPP + PSI if coalition ==  "PAN_PRD_PANAL_CPP, PSI"
replace PAN_PRD_PANAL_CPP = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PSI"
replace PSI = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PSI"

gen PAN_PRD_PANAL_CPP_PT = PAN_PRD_PANAL_CPP + PT if coalition ==  "PAN_PRD_PANAL_CPP, PT"
replace PAN_PRD_PANAL_CPP = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PT"
replace PT = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PT"

gen PAN_PRD_PANAL_CPP_PT_PSI  = PAN_PRD_PANAL_CPP + PT + PSI if coalition ==  "PAN_PRD_PANAL_CPP, PT, PSI"
replace PAN_PRD_PANAL_CPP = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PT, PSI"
replace PT = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PT, PSI"
replace  PSI = 0 if coalition ==  "PAN_PRD_PANAL_CPP, PT, PSI"

drop VOTOSNULOS CANDIDATOSNOREGISTRADOS

gen turnout =  total/listanominal

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
replace uniqueid=21099 if municipality =="Canada Morelos"
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
replace uniqueid=21120 if municipality =="San Antonio Canada"
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

egen valid = rowtotal(PAN_PRD_PANAL_CPP PRI_PVEM PT PC PSI PAN_PRD_PANAL_CPP_PC PAN_PRD_PANAL_CPP_PC_PSI PAN_PRD_PANAL_CPP_PSI PAN_PRD_PANAL_CPP_PT PAN_PRD_PANAL_CPP_PT_PSI)

foreach var in PAN_PRD_PANAL_CPP PRI_PVEM PT PC PSI PAN_PRD_PANAL_CPP_PC PAN_PRD_PANAL_CPP_PC_PSI PAN_PRD_PANAL_CPP_PSI PAN_PRD_PANAL_CPP_PT PAN_PRD_PANAL_CPP_PT_PSI total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD_PANAL_CPP inv_mun_PRI_PVEM inv_mun_PT inv_mun_PC inv_mun_PSI inv_mun_PAN_PRD_PANAL_CPP_PC inv_mun_PAN_PRD_PANAL_CPP_PC_PSI inv_mun_PAN_PRD_PANAL_CPP_PSI inv_mun_PAN_PRD_PANAL_CPP_PT inv_mun_PAN_PRD_PANAL_CPP_PT_PSI, gen(PAN_PRD_PANAL_CPP_r PRI_PVEM_r PT_r PC_r PSI_r PAN_PRD_PANAL_CPP_PC_r PAN_PRD_PANAL_CPP_PC_PSI_r PAN_PRD_PANAL_CPP_PSI_r PAN_PRD_PANAL_CPP_PT_r PAN_PRD_PANAL_CPP_PT_PSI_r)
drop inv_mun_*

gen winner = "PAN_PRD_PANAL_CPP" if PAN_PRD_PANAL_CPP_r==1  
replace winner = "PAN_PRD_PANAL_CPP_PC" if PAN_PRD_PANAL_CPP_PC_r ==1
replace winner = "PAN_PRD_PANAL_CPP_PC_PSI" if PAN_PRD_PANAL_CPP_PC_PSI_r ==1
replace winner = "PAN_PRD_PANAL_CPP_PSI" if PAN_PRD_PANAL_CPP_PSI_r ==1
replace winner = "PAN_PRD_PANAL_CPP_PT" if PAN_PRD_PANAL_CPP_PT_r ==1
replace winner = "PAN_PRD_PANAL_CPP_PT_PSI" if PAN_PRD_PANAL_CPP_PT_PSI_r ==1
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PT" if PT_r==1
replace winner = "PC" if PC_r==1
replace winner = "PSI" if PSI_r==1
drop *_r

gen year = 2013
gen month ="July"

save Puebla_Section_2013.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

use Puebla_Section_1998.dta, clear
append using Puebla_Section_2001.dta
append using Puebla_Section_2002.dta
append using Puebla_Section_2004.dta
append using Puebla_Section_2005.dta
append using Puebla_Section_2007.dta
append using Puebla_Section_2008.dta
append using Puebla_Section_2010.dta
append using Puebla_Section_2011.dta
append using Puebla_Section_2013.dta

erase Puebla_Section_1998.dta
erase Puebla_Section_2001.dta
erase Puebla_Section_2002.dta
erase Puebla_Section_2004.dta
erase Puebla_Section_2005.dta
erase Puebla_Section_2007.dta
erase Puebla_Section_2008.dta
erase Puebla_Section_2010.dta
erase Puebla_Section_2011.dta
erase Puebla_Section_2013.dta

tab winner
br if winner==""

br if municipality=="Albino Zertuche" & year==2010
br if municipality=="San Jeronimo Tecuanipan" & year==2010

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP PEC CPP PSI {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

saveold "..\Puebla_ALL.dta", replace version(12)

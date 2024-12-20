capture cd "D:\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Mexico - 1996, 2000, 2003, 2006, 2009, 2012"
capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Mexico - 1996, 2000, 2003, 2006, 2009, 2012"

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1996_No_LN.csv, clear

rename nombre  municipality
rename seccion section
rename otros noregistrados

drop if municipality=="" & section==.
drop if total==. | total==0

destring pan -  total, replace

collapse (sum) pan -  total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pc     PartCardenista
rename  pt     PT
rename  pvem   PVEM
rename  pps    PPS
* Partido Democrata Mexicano
rename  pdm    PDM
* Partido del Pueblo Mexiquense
rename  ppm    PPM

* gen turnout =  total/listanominal

drop  noreg  nulos  

gen   uniqueid= 0
replace uniqueid=15001 if municipality =="ACAMBAY"
replace uniqueid=15002 if municipality =="ACOLMAN"
replace uniqueid=15003 if municipality =="ACULCO"
replace uniqueid=15004 if municipality =="ALMOLOYA DE ALQUISIRAS"
replace uniqueid=15005 if municipality =="ALMOLOYA DE JUAREZ"
replace uniqueid=15006 if municipality =="ALMOLOYA DEL RIO"
replace uniqueid=15007 if municipality =="AMANALCO"
replace uniqueid=15008 if municipality =="AMATEPEC"
replace uniqueid=15009 if municipality =="AMECAMECA"
replace uniqueid=15010 if municipality =="APAXCO"
replace uniqueid=15011 if municipality =="ATENCO"
replace uniqueid=15012 if municipality =="ATIZAPAN"
replace uniqueid=15013 if municipality =="ATIZAPAN DE ZARAGOZA"
replace uniqueid=15014 if municipality =="ATLACOMULCO"
replace uniqueid=15015 if municipality =="ATLAUTLA"
replace uniqueid=15016 if municipality =="AXAPUSCO"
replace uniqueid=15017 if municipality =="AYAPANGO"
replace uniqueid=15018 if municipality =="CALIMAYA"
replace uniqueid=15019 if municipality =="CAPULHUAC"
replace uniqueid=15025 if municipality =="CHALCO"
replace uniqueid=15026 if municipality =="CHAPA DE MOTA"
replace uniqueid=15027 if municipality =="CHAPULTEPEC"
replace uniqueid=15028 if municipality =="CHIAUTLA"
replace uniqueid=15029 if municipality =="CHICOLOAPAN"
replace uniqueid=15030 if municipality =="CHICONCUAC"
replace uniqueid=15031 if municipality =="CHIMALHUACAN"
replace uniqueid=15020 if municipality =="COACALCO"
replace uniqueid=15021 if municipality =="COATEPEC HARINAS"
replace uniqueid=15022 if municipality =="COCOTITLAN"
replace uniqueid=15023 if municipality =="COYOTEPEC"
replace uniqueid=15024 if municipality =="CUAUTITLAN"
replace uniqueid=15121 if municipality =="CUAUTITLAN IZCALLI"
replace uniqueid=15032 if municipality =="DONATO GUERRA"
replace uniqueid=15033 if municipality =="ECATEPEC"
replace uniqueid=15034 if municipality =="ECATZINGO"
replace uniqueid=15064 if municipality =="EL ORO"
replace uniqueid=15035 if municipality =="HUEHUETOCA"
replace uniqueid=15036 if municipality =="HUEYPOXTLA"
replace uniqueid=15037 if municipality =="HUIXQUILUCAN"
replace uniqueid=15038 if municipality =="ISIDRO FABELA"
replace uniqueid=15039 if municipality =="IXTAPALUCA"
replace uniqueid=15040 if municipality =="IXTAPAN DE LA SAL"
replace uniqueid=15041 if municipality =="IXTAPAN DEL ORO"
replace uniqueid=15042 if municipality =="IXTLAHUACA"
replace uniqueid=15044 if municipality =="JALTENCO"
replace uniqueid=15045 if municipality =="JILOTEPEC"
replace uniqueid=15046 if municipality =="JILOTZINGO"
replace uniqueid=15047 if municipality =="JIQUIPILCO"
replace uniqueid=15048 if municipality =="JOCOTITLAN"
replace uniqueid=15049 if municipality =="JOQUICINGO"
replace uniqueid=15050 if municipality =="JUCHITEPEC"
replace uniqueid=15070 if municipality =="LA PAZ"
replace uniqueid=15051 if municipality =="LERMA"
replace uniqueid=15052 if municipality =="MALINALCO"
replace uniqueid=15053 if municipality =="MELCHOR OCAMPO"
replace uniqueid=15054 if municipality =="METEPEC"
replace uniqueid=15055 if municipality =="MEXICALTZINGO"
replace uniqueid=15056 if municipality =="MORELOS"
replace uniqueid=15057 if municipality =="NAUCALPAN"
replace uniqueid=15059 if municipality =="NEXTLALPAN"
replace uniqueid=15058 if municipality =="NEZAHUALCOYOTL"
replace uniqueid=15060 if municipality =="NICOLAS ROMERO"
replace uniqueid=15061 if municipality =="NOPALTEPEC"
replace uniqueid=15062 if municipality =="OCOYOACAC"
replace uniqueid=15063 if municipality =="OCUILAN"
replace uniqueid=15065 if municipality =="OTUMBA"
replace uniqueid=15066 if municipality =="OTZOLOAPAN"
replace uniqueid=15067 if municipality =="OTZOLOTEPEC"
replace uniqueid=15068 if municipality =="OZUMBA"
replace uniqueid=15069 if municipality =="PAPALOTLA"
replace uniqueid=15071 if municipality =="POLOTITLAN"
replace uniqueid=15072 if municipality =="RAYON"
replace uniqueid=15073 if municipality =="SAN ANTONIO LA ISLA"
replace uniqueid=15074 if municipality =="SAN FELIPE DEL PROGRESO"
replace uniqueid=15075 if municipality =="SAN MARTIN DE LAS PIRAMIDES"
replace uniqueid=15076 if municipality =="SAN MATEO ATENCO"
replace uniqueid=15077 if municipality =="SAN SIMON DE GUERRERO"
replace uniqueid=15078 if municipality =="SANTO TOMAS"
replace uniqueid=15079 if municipality =="SOYANIQUILPAN DE JUAREZ"
replace uniqueid=15080 if municipality =="SULTEPEC"
replace uniqueid=15081 if municipality =="TECAMAC"
replace uniqueid=15082 if municipality =="TEJUPILCO"
replace uniqueid=15083 if municipality =="TEMAMATLA"
replace uniqueid=15084 if municipality =="TEMASCALAPA"
replace uniqueid=15085 if municipality =="TEMASCALCINGO"
replace uniqueid=15086 if municipality =="TEMASCALTEPEC"
replace uniqueid=15087 if municipality =="TEMOAYA"
replace uniqueid=15088 if municipality =="TENANCINGO"
replace uniqueid=15089 if municipality =="TENANGO DEL AIRE"
replace uniqueid=15090 if municipality =="TENANGO DEL VALLE"
replace uniqueid=15091 if municipality =="TEOLOYUCAN"
replace uniqueid=15092 if municipality =="TEOTIHUACAN"
replace uniqueid=15093 if municipality =="TEPETLAOXTOC"
replace uniqueid=15094 if municipality =="TEPETLIXPA"
replace uniqueid=15095 if municipality =="TEPOTZOTLAN"
replace uniqueid=15096 if municipality =="TEQUIXQUIAC"
replace uniqueid=15097 if municipality =="TEXCALTITLAN"
replace uniqueid=15098 if municipality =="TEXCALYACAC"
replace uniqueid=15099 if municipality =="TEXCOCO"
replace uniqueid=15100 if municipality =="TEZOYUCA"
replace uniqueid=15101 if municipality =="TIANGUISTENCO"
replace uniqueid=15102 if municipality =="TIMILPAN"
replace uniqueid=15103 if municipality =="TLALMANALCO"
replace uniqueid=15104 if municipality =="TLALNEPANTLA"
replace uniqueid=15105 if municipality =="TLATLAYA"
replace uniqueid=15106 if municipality =="TOLUCA"
replace uniqueid=15107 if municipality =="TONATICO"
replace uniqueid=15108 if municipality =="TULTEPEC"
replace uniqueid=15109 if municipality =="TULTITLAN"
replace uniqueid=15110 if municipality =="VALLE DE BRAVO"
replace uniqueid=15122 if municipality =="V. DE CHALCO SOLIDARIDAD"
replace uniqueid=15111 if municipality =="VILLA DE ALLENDE"
replace uniqueid=15112 if municipality =="VILLA DEL CARBON"
replace uniqueid=15113 if municipality =="VILLA GUERRERO"
replace uniqueid=15114 if municipality =="VILLA VICTORIA"
replace uniqueid=15043 if municipality =="JALATLACO"
replace uniqueid=15115 if municipality =="XONACATLAN"
replace uniqueid=15116 if municipality =="ZACAZONAPAN"
replace uniqueid=15117 if municipality =="ZACUALPAN"
replace uniqueid=15118 if municipality =="ZINACANTEPEC"
replace uniqueid=15119 if municipality =="ZUMPAHUACAN"
replace uniqueid=15120 if municipality =="ZUMPANGO"

egen valid = rowtotal(PAN PRI PRD PartCardenista PVEM PT PPS PDM PPM)

foreach var in PAN PRI PRD PartCardenista PVEM PT PPS PDM PPM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PartCardenista inv_mun_PVEM inv_mun_PT inv_mun_PPS inv_mun_PDM inv_mun_PPM, gen(PAN_r PRI_r PRD_r PartCardenista_r PVEM_r PT_r PPS_r PDM_r PPM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PartCardenista" if PartCardenista_r==1
replace winner = "PPS" if PPS_r==1
replace winner = "PDM" if PDM_r==1
replace winner = "PPM" if PPM_r==1
drop *_r

gen year =1996
gen month ="November"

save Mexico_Section_1996.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2000_No_LN.csv, clear

rename nombre  municipality
rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0

destring pan -  total, replace

collapse (sum) pan -  total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  cd     PC
rename  pt     PT
rename  pvem   PVEM
rename  pas    PAS
* Partido de Centro Democratico
rename  pcd    PCD
rename  psn    PSN
rename  parm   PARM
* Partido Democracia Social
rename  ds     PDS

drop  noreg  nulos  

gen   uniqueid= 0
replace uniqueid=15001 if municipality =="ACAMBAY"
replace uniqueid=15002 if municipality =="ACOLMAN"
replace uniqueid=15003 if municipality =="ACULCO"
replace uniqueid=15004 if municipality =="ALMOLOYA DE ALQUISIRAS"
replace uniqueid=15005 if municipality =="ALMOLOYA DE JUAREZ"
replace uniqueid=15006 if municipality =="ALMOLOYA DEL RIO"
replace uniqueid=15007 if municipality =="AMANALCO"
replace uniqueid=15008 if municipality =="AMATEPEC"
replace uniqueid=15009 if municipality =="AMECAMECA"
replace uniqueid=15010 if municipality =="APAXCO"
replace uniqueid=15011 if municipality =="ATENCO"
replace uniqueid=15012 if municipality =="ATIZAPAN"
replace uniqueid=15013 if municipality =="ATIZAPAN DE ZARAGOZA"
replace uniqueid=15014 if municipality =="ATLACOMULCO"
replace uniqueid=15015 if municipality =="ATLAUTLA"
replace uniqueid=15016 if municipality =="AXAPUSCO"
replace uniqueid=15017 if municipality =="AYAPANGO"
replace uniqueid=15018 if municipality =="CALIMAYA"
replace uniqueid=15019 if municipality =="CAPULHUAC"
replace uniqueid=15025 if municipality =="CHALCO"
replace uniqueid=15026 if municipality =="CHAPA DE MOTA"
replace uniqueid=15027 if municipality =="CHAPULTEPEC"
replace uniqueid=15028 if municipality =="CHIAUTLA"
replace uniqueid=15029 if municipality =="CHICOLOAPAN"
replace uniqueid=15030 if municipality =="CHICONCUAC"
replace uniqueid=15031 if municipality =="CHIMALHUACAN"
replace uniqueid=15020 if municipality =="COACALCO"
replace uniqueid=15021 if municipality =="COATEPEC HARINAS"
replace uniqueid=15022 if municipality =="COCOTITLAN"
replace uniqueid=15023 if municipality =="COYOTEPEC"
replace uniqueid=15024 if municipality =="CUAUTITLAN"
replace uniqueid=15121 if municipality =="CUAUTITLAN IZCALLI"
replace uniqueid=15032 if municipality =="DONATO GUERRA"
replace uniqueid=15033 if municipality =="ECATEPEC"
replace uniqueid=15034 if municipality =="ECATZINGO"
replace uniqueid=15064 if municipality =="EL ORO"
replace uniqueid=15035 if municipality =="HUEHUETOCA"
replace uniqueid=15036 if municipality =="HUEYPOXTLA"
replace uniqueid=15037 if municipality =="HUIXQUILUCAN"
replace uniqueid=15038 if municipality =="ISIDRO FABELA"
replace uniqueid=15039 if municipality =="IXTAPALUCA"
replace uniqueid=15040 if municipality =="IXTAPAN DE LA SAL"
replace uniqueid=15041 if municipality =="IXTAPAN DEL ORO"
replace uniqueid=15042 if municipality =="IXTLAHUACA"
replace uniqueid=15044 if municipality =="JALTENCO"
replace uniqueid=15045 if municipality =="JILOTEPEC"
replace uniqueid=15046 if municipality =="JILOTZINGO"
replace uniqueid=15047 if municipality =="JIQUIPILCO"
replace uniqueid=15048 if municipality =="JOCOTITLAN"
replace uniqueid=15049 if municipality =="JOQUICINGO"
replace uniqueid=15050 if municipality =="JUCHITEPEC"
replace uniqueid=15070 if municipality =="LA PAZ"
replace uniqueid=15051 if municipality =="LERMA"
replace uniqueid=15052 if municipality =="MALINALCO"
replace uniqueid=15053 if municipality =="MELCHOR OCAMPO"
replace uniqueid=15054 if municipality =="METEPEC"
replace uniqueid=15055 if municipality =="MEXICALTZINGO"
replace uniqueid=15056 if municipality =="MORELOS"
replace uniqueid=15057 if municipality =="NAUCALPAN"
replace uniqueid=15059 if municipality =="NEXTLALPAN"
replace uniqueid=15058 if municipality =="NEZAHUALCOYOTL"
replace uniqueid=15060 if municipality =="NICOLAS ROMERO"
replace uniqueid=15061 if municipality =="NOPALTEPEC"
replace uniqueid=15062 if municipality =="OCOYOACAC"
replace uniqueid=15063 if municipality =="OCUILAN"
replace uniqueid=15065 if municipality =="OTUMBA"
replace uniqueid=15066 if municipality =="OTZOLOAPAN"
replace uniqueid=15067 if municipality =="OTZOLOTEPEC"
replace uniqueid=15068 if municipality =="OZUMBA"
replace uniqueid=15069 if municipality =="PAPALOTLA"
replace uniqueid=15071 if municipality =="POLOTITLAN"
replace uniqueid=15072 if municipality =="RAYON"
replace uniqueid=15073 if municipality =="SAN ANTONIO LA ISLA"
replace uniqueid=15074 if municipality =="SAN FELIPE DEL PROGRESO"
replace uniqueid=15075 if municipality =="SAN MARTIN DE LAS PIRAMIDES"
replace uniqueid=15076 if municipality =="SAN MATEO ATENCO"
replace uniqueid=15077 if municipality =="SAN SIMON DE GUERRERO"
replace uniqueid=15078 if municipality =="SANTO TOMAS"
replace uniqueid=15079 if municipality =="SOYANIQUILPAN DE JUAREZ"
replace uniqueid=15080 if municipality =="SULTEPEC"
replace uniqueid=15081 if municipality =="TECAMAC"
replace uniqueid=15082 if municipality =="TEJUPILCO"
replace uniqueid=15083 if municipality =="TEMAMATLA"
replace uniqueid=15084 if municipality =="TEMASCALAPA"
replace uniqueid=15085 if municipality =="TEMASCALCINGO"
replace uniqueid=15086 if municipality =="TEMASCALTEPEC"
replace uniqueid=15087 if municipality =="TEMOAYA"
replace uniqueid=15088 if municipality =="TENANCINGO"
replace uniqueid=15089 if municipality =="TENANGO DEL AIRE"
replace uniqueid=15090 if municipality =="TENANGO DEL VALLE"
replace uniqueid=15091 if municipality =="TEOLOYUCAN"
replace uniqueid=15092 if municipality =="TEOTIHUACAN"
replace uniqueid=15093 if municipality =="TEPETLAOXTOC"
replace uniqueid=15094 if municipality =="TEPETLIXPA"
replace uniqueid=15095 if municipality =="TEPOTZOTLAN"
replace uniqueid=15096 if municipality =="TEQUIXQUIAC"
replace uniqueid=15097 if municipality =="TEXCALTITLAN"
replace uniqueid=15098 if municipality =="TEXCALYACAC"
replace uniqueid=15099 if municipality =="TEXCOCO"
replace uniqueid=15100 if municipality =="TEZOYUCA"
replace uniqueid=15101 if municipality =="TIANGUISTENCO"
replace uniqueid=15102 if municipality =="TIMILPAN"
replace uniqueid=15103 if municipality =="TLALMANALCO"
replace uniqueid=15104 if municipality =="TLALNEPANTLA"
replace uniqueid=15105 if municipality =="TLATLAYA"
replace uniqueid=15106 if municipality =="TOLUCA"
replace uniqueid=15107 if municipality =="TONATICO"
replace uniqueid=15108 if municipality =="TULTEPEC"
replace uniqueid=15109 if municipality =="TULTITLAN"
replace uniqueid=15110 if municipality =="VALLE DE BRAVO"
replace uniqueid=15122 if municipality =="V. DE CHALCO SOLIDARIDAD"
replace uniqueid=15111 if municipality =="VILLA DE ALLENDE"
replace uniqueid=15112 if municipality =="VILLA DEL CARBON"
replace uniqueid=15113 if municipality =="VILLA GUERRERO"
replace uniqueid=15114 if municipality =="VILLA VICTORIA"
replace uniqueid=15043 if municipality =="JALATLACO"
replace uniqueid=15115 if municipality =="XONACATLAN"
replace uniqueid=15116 if municipality =="ZACAZONAPAN"
replace uniqueid=15117 if municipality =="ZACUALPAN"
replace uniqueid=15118 if municipality =="ZINACANTEPEC"
replace uniqueid=15119 if municipality =="ZUMPAHUACAN"
replace uniqueid=15120 if municipality =="ZUMPANGO"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS)

foreach var in PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 15
g seccion = section
capture merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
keep if month==7 & year==2000
drop if _merge==2
drop _merge ed seccion year month
rename lista listanominal

gen turnout =  total/listanominal

foreach var in listanominal{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PCD inv_mun_PSN inv_mun_PARM inv_mun_PAS inv_mun_PDS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PCD_r PSN_r PARM_r PAS_r PDS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PCD" if PCD_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PARM" if PARM_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PDS" if PDS_r==1

drop *_r

gen year =2000
gen month ="July"

save Mexico_Section_2000.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2003_No_LN.csv, clear

rename seccion section
sort section 
merge section using Mapping_Section_2003.dta 
replace municipality = "ECATEPEC" if _merge==1 & municipality =="" & municipio==34
replace municipality = "OCOYOACAC" if _merge==1 & municipality =="" & municipio==63
drop _merge municipio
* rename lista_nominal nominal

drop if municipality=="". & section==.
drop if total==. | total==0

destring pan -  total, replace

collapse (sum) pan -  pacem total, by (municipality section)

rename  pan    PAN
rename  apt    PRI_PVEM
rename  prd    PRD
rename  conv   PC
rename  pt     PT
rename  psn    PSN
rename  pas    PAS
rename  pacem  PCEM

gen   uniqueid= 0
replace uniqueid=15001 if municipality =="ACAMBAY"
replace uniqueid=15002 if municipality =="ACOLMAN"
replace uniqueid=15003 if municipality =="ACULCO"
replace uniqueid=15004 if municipality =="ALMOLOYA DE ALQUISIRAS"
replace uniqueid=15005 if municipality =="ALMOLOYA DE JUAREZ"
replace uniqueid=15006 if municipality =="ALMOLOYA DEL RIO"
replace uniqueid=15007 if municipality =="AMANALCO"
replace uniqueid=15008 if municipality =="AMATEPEC"
replace uniqueid=15009 if municipality =="AMECAMECA"
replace uniqueid=15010 if municipality =="APAXCO"
replace uniqueid=15011 if municipality =="ATENCO"
replace uniqueid=15012 if municipality =="ATIZAPAN"
replace uniqueid=15013 if municipality =="ATIZAPAN DE ZARAGOZA"
replace uniqueid=15014 if municipality =="ATLACOMULCO"
replace uniqueid=15015 if municipality =="ATLAUTLA"
replace uniqueid=15016 if municipality =="AXAPUSCO"
replace uniqueid=15017 if municipality =="AYAPANGO"
replace uniqueid=15018 if municipality =="CALIMAYA"
replace uniqueid=15019 if municipality =="CAPULHUAC"
replace uniqueid=15025 if municipality =="CHALCO"
replace uniqueid=15026 if municipality =="CHAPA DE MOTA"
replace uniqueid=15027 if municipality =="CHAPULTEPEC"
replace uniqueid=15028 if municipality =="CHIAUTLA"
replace uniqueid=15029 if municipality =="CHICOLOAPAN"
replace uniqueid=15030 if municipality =="CHICONCUAC"
replace uniqueid=15031 if municipality =="CHIMALHUACAN"
replace uniqueid=15020 if municipality =="COACALCO"
replace uniqueid=15021 if municipality =="COATEPEC HARINAS"
replace uniqueid=15022 if municipality =="COCOTITLAN"
replace uniqueid=15023 if municipality =="COYOTEPEC"
replace uniqueid=15024 if municipality =="CUAUTITLAN"
replace uniqueid=15121 if municipality =="CUAUTITLAN IZCALLI"
replace uniqueid=15032 if municipality =="DONATO GUERRA"
replace uniqueid=15033 if municipality =="ECATEPEC"
replace uniqueid=15034 if municipality =="ECATZINGO"
replace uniqueid=15064 if municipality =="EL ORO"
replace uniqueid=15035 if municipality =="HUEHUETOCA"
replace uniqueid=15036 if municipality =="HUEYPOXTLA"
replace uniqueid=15037 if municipality =="HUIXQUILUCAN"
replace uniqueid=15038 if municipality =="ISIDRO FABELA"
replace uniqueid=15039 if municipality =="IXTAPALUCA"
replace uniqueid=15040 if municipality =="IXTAPAN DE LA SAL"
replace uniqueid=15041 if municipality =="IXTAPAN DEL ORO"
replace uniqueid=15042 if municipality =="IXTLAHUACA"
replace uniqueid=15044 if municipality =="JALTENCO"
replace uniqueid=15045 if municipality =="JILOTEPEC"
replace uniqueid=15046 if municipality =="JILOTZINGO"
replace uniqueid=15047 if municipality =="JIQUIPILCO"
replace uniqueid=15048 if municipality =="JOCOTITLAN"
replace uniqueid=15049 if municipality =="JOQUICINGO"
replace uniqueid=15050 if municipality =="JUCHITEPEC"
replace uniqueid=15070 if municipality =="LA PAZ"
replace uniqueid=15051 if municipality =="LERMA"
replace uniqueid=15123 if municipality =="LUVIANOS"
replace uniqueid=15052 if municipality =="MALINALCO"
replace uniqueid=15053 if municipality =="MELCHOR OCAMPO"
replace uniqueid=15054 if municipality =="METEPEC"
replace uniqueid=15055 if municipality =="MEXICALTZINGO"
replace uniqueid=15056 if municipality =="MORELOS"
replace uniqueid=15057 if municipality =="NAUCALPAN"
replace uniqueid=15059 if municipality =="NEXTLALPAN"
replace uniqueid=15058 if municipality =="NEZAHUALCOYOTL"
replace uniqueid=15060 if municipality =="NICOLAS ROMERO"
replace uniqueid=15061 if municipality =="NOPALTEPEC"
replace uniqueid=15062 if municipality =="OCOYOACAC"
replace uniqueid=15063 if municipality =="OCUILAN"
replace uniqueid=15065 if municipality =="OTUMBA"
replace uniqueid=15066 if municipality =="OTZOLOAPAN"
replace uniqueid=15067 if municipality =="OTZOLOTEPEC"
replace uniqueid=15068 if municipality =="OZUMBA"
replace uniqueid=15069 if municipality =="PAPALOTLA"
replace uniqueid=15071 if municipality =="POLOTITLAN"
replace uniqueid=15072 if municipality =="RAYON"
replace uniqueid=15073 if municipality =="SAN ANTONIO LA ISLA"
replace uniqueid=15074 if municipality =="SAN FELIPE DEL PROGRESO"
replace uniqueid=15124 if municipality =="SAN JOSE DEL RINCON"
replace uniqueid=15075 if municipality =="SAN MARTIN DE LAS PIRAMIDE"
replace uniqueid=15076 if municipality =="SAN MATEO ATENCO"
replace uniqueid=15077 if municipality =="SAN SIMON DE GUERRERO"
replace uniqueid=15078 if municipality =="SANTO TOMAS"
replace uniqueid=15079 if municipality =="SOYANIQUILPAN DE JUAREZ"
replace uniqueid=15080 if municipality =="SULTEPEC"
replace uniqueid=15081 if municipality =="TECAMAC"
replace uniqueid=15082 if municipality =="TEJUPILCO"
replace uniqueid=15083 if municipality =="TEMAMATLA"
replace uniqueid=15084 if municipality =="TEMASCALAPA"
replace uniqueid=15085 if municipality =="TEMASCALCINGO"
replace uniqueid=15086 if municipality =="TEMASCALTEPEC"
replace uniqueid=15087 if municipality =="TEMOAYA"
replace uniqueid=15088 if municipality =="TENANCINGO"
replace uniqueid=15089 if municipality =="TENANGO DEL AIRE"
replace uniqueid=15090 if municipality =="TENANGO DEL VALLE"
replace uniqueid=15091 if municipality =="TEOLOYUCAN"
replace uniqueid=15092 if municipality =="TEOTIHUACAN"
replace uniqueid=15093 if municipality =="TEPETLAOXTOC"
replace uniqueid=15094 if municipality =="TEPETLIXPA"
replace uniqueid=15095 if municipality =="TEPOTZOTLAN"
replace uniqueid=15096 if municipality =="TEQUIXQUIAC"
replace uniqueid=15097 if municipality =="TEXCALTITLAN"
replace uniqueid=15098 if municipality =="TEXCALYACAC"
replace uniqueid=15099 if municipality =="TEXCOCO"
replace uniqueid=15100 if municipality =="TEZOYUCA"
replace uniqueid=15101 if municipality =="TIANGUISTENCO"
replace uniqueid=15102 if municipality =="TIMILPAN"
replace uniqueid=15103 if municipality =="TLALMANALCO"
replace uniqueid=15104 if municipality =="TLALNEPANTLA"
replace uniqueid=15105 if municipality =="TLATLAYA"
replace uniqueid=15106 if municipality =="TOLUCA"
replace uniqueid=15125 if municipality =="TONANITLA"
replace uniqueid=15107 if municipality =="TONATICO"
replace uniqueid=15108 if municipality =="TULTEPEC"
replace uniqueid=15109 if municipality =="TULTITLAN"
replace uniqueid=15110 if municipality =="VALLE DE BRAVO"
replace uniqueid=15122 if municipality =="VALLE DE CHALCO SOLIDARIDA"
replace uniqueid=15111 if municipality =="VILLA DE ALLENDE"
replace uniqueid=15112 if municipality =="VILLA DEL CARBON"
replace uniqueid=15113 if municipality =="VILLA GUERRERO"
replace uniqueid=15114 if municipality =="VILLA VICTORIA"
replace uniqueid=15043 if municipality =="XALATLACO"
replace uniqueid=15115 if municipality =="XONACATLAN"
replace uniqueid=15116 if municipality =="ZACAZONAPAN"
replace uniqueid=15117 if municipality =="ZACUALPAN"
replace uniqueid=15118 if municipality =="ZINACANTEPEC"
replace uniqueid=15119 if municipality =="ZUMPAHUACAN"
replace uniqueid=15120 if municipality =="ZUMPANGO"

egen valid = rowtotal(PAN PRI_PVEM PRD PT PC PSN PAS PCEM)

foreach var in PAN PRI_PVEM PRD PT PC PSN PAS PCEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 15
g seccion = section
capture merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
keep if month==7 & year==2000
drop if _merge==2
drop _merge ed seccion year month
rename lista listanominal

gen turnout =  total/listanominal

foreach var in listanominal{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_PSN inv_mun_PAS inv_mun_PCEM, gen(PAN_r PRI_PVEM_r PRD_r PT_r PC_r PSN_r PAS_r PCEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PC" if PC_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PCEM" if PCEM_r==1
drop *_r

gen year =2003
gen month ="March"

save Mexico_Section_2003.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

* special elections in Atenco, Chalco, Tepotzotlan - but results only available for Atenco

import excel "Extraordinarios 2003.xlsx", sheet("Sheet1") firstrow clear

rename Sección section

drop if section==.
drop if total==. | total==0

collapse (sum) listanominal PAN - PAS total, by (municipality section)

gen   uniqueid= 15011

egen valid = rowtotal(PAN PRI_PVEM PRD PT PAS)

foreach var in PAN PRI_PVEM PRD PT PAS total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen inv_mun_`var'= 1/mun_`var'
}

gen turnout = total/listanominal
gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PAS, gen(PAN_r PRI_PVEM_r PRD_r PT_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PAS" if PAS_r==1
drop *_r

gen year =2003
gen month ="October"

save Mexico_Section_2003_EXTRAORDINARIO.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2006.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="". & section==.
drop if total==. | total==0

destring pan -  total, replace

collapse (sum) listanominal pan - pc total, by (municipality section)

***********************************************************************************************

gen dummy_pan_pc = 0
gen dummy_pan_prd = 0
gen dummy_pan_prd_pt = 0
gen dummy_pan_prd_pt_pc = 0
gen dummy_pan_pt = 0
gen dummy_prd_pc = 0
gen dummy_prd_pt = 0
gen dummy_prd_pt_pc = 0
gen dummy_pt_pc = 0

replace dummy_pan_pc = 1 if municipality =="XONACATLAN"
replace dummy_pan_prd = 1 if municipality =="TEMOAYA"
replace dummy_pan_prd_pt = 1 if municipality =="TEMASCALCINGO"
replace dummy_pan_prd_pt_pc = 1 if municipality =="ACAMBAY"
replace dummy_pan_pt = 1 if municipality =="VILLA DE ALLENDE" | municipality =="VALLE DE CHALCO SOLIDARIDA"
replace dummy_prd_pc = 1 if municipality =="CUAUTITLAN" | municipality =="JALTENCO" | municipality =="MORELOS" | municipality =="SAN FELIPE DEL PROGRESO"
replace dummy_prd_pt = 1 if municipality=="ALMOLOYA DE ALQUISIRAS" | municipality=="ALMOLOYA DE JUAREZ" | municipality=="ATIZAPAN" | municipality=="COATEPEC HARINAS" | ///
	municipality=="COYOTEPEC" | municipality=="CHICOLOAPAN" | municipality=="CHIMALHUACAN" | municipality=="ECATEPEC" | municipality=="HUIXQUILUCAN" | municipality=="IXTAPAN DE LA SAL" | ///
	municipality=="JIQUIPILCO" | municipality=="METEPEC" | municipality=="MEXICALTZINGO" | municipality=="NAUCALPAN" | municipality=="NOPALTEPEC" | municipality=="OCUILAN" | municipality=="OTZOLOAPAN" | ///
	municipality=="SAN SIMON DE GUERRERO" | municipality=="TEPETLAOXTOC" | municipality=="TEPOTZOTLAN" | municipality=="TEZOYUCA" | municipality=="TLALNEPANTLA" | municipality=="TONATICO" | ///
	municipality=="TULTEPEC" | municipality=="TULTITLAN" | municipality=="VILLA GUERRERO" | municipality=="ZACUALPAN"
replace dummy_prd_pt_pc =1 if municipality=="AMANALCO" | municipality=="ATIZAPAN DE ZARAGOZA" | municipality=="IXTLAHUACA" | municipality=="JOQUICINGO" | municipality=="VILLA DEL CARBON"
replace dummy_pt_pc = 1 if municipality=="ACULCO" | municipality=="APAXCO" | municipality=="ATLACOMULCO" | municipality=="CHIAUTLA" | municipality=="ECATZINGO" | municipality=="IXTAPALUCA" | ///
	municipality=="XALATLACO" | municipality=="JILOTEPEC" | municipality=="LERMA" | municipality=="MALINALCO" | municipality=="OTUMBA" | municipality=="LA PAZ" | municipality=="POLOTITLAN" | ///
	municipality=="SAN MARTIN DE LAS PIRAMIDE" | municipality=="SOYANIQUILPAN DE JUAREZ" | municipality=="SULTEPEC" | municipality=="TEQUIXQUIAC" | municipality=="TEXCALYACAC" | municipality=="TIANGUISTENCO" | ///
	municipality=="TIMILPAN" | municipality=="TOLUCA" | municipality=="ZUMPAHUACAN" | municipality=="TONANITLA"

gen pan_pc = 0
gen pan_prd = 0
gen pan_prd_pt = 0
gen pan_prd_pt_pc = 0
gen pan_pt = 0
gen prd_pc = 0
gen prd_pt = 0
gen prd_pt_pc = 0
gen pt_pc = 0

replace pan_pc = pan + pc if dummy_pan_pc==1
replace pan  = 0 if dummy_pan_pc==1
replace pc = 0 if dummy_pan_pc==1

replace pan_prd = pan + prd if dummy_pan_prd ==1
replace pan = 0  if dummy_pan_prd ==1
replace prd = 0  if dummy_pan_prd ==1

replace pan_prd_pt = pan + prd + pt if dummy_pan_prd_pt ==1
replace pan = 0 if dummy_pan_prd_pt ==1
replace prd = 0 if dummy_pan_prd_pt ==1
replace pt = 0 if dummy_pan_prd_pt ==1

replace pan_prd_pt_pc = pan + prd + pt + pc if dummy_pan_prd_pt_pc ==1
replace pan = 0 if dummy_pan_prd_pt_pc ==1 
replace prd = 0 if dummy_pan_prd_pt_pc ==1
replace pt = 0 if dummy_pan_prd_pt_pc ==1
replace pc = 0 if dummy_pan_prd_pt_pc ==1

replace pan_pt = pan + pt if dummy_pan_pt ==1
replace pan = 0  if dummy_pan_pt ==1 
replace pt = 0  if dummy_pan_pt ==1

replace prd_pc = prd + pc if dummy_prd_pc ==1
replace prd = 0  if dummy_prd_pc ==1
replace pc = 0  if dummy_prd_pc ==1

replace prd_pt = prd + pt if dummy_prd_pt ==1
replace prd = 0 if dummy_prd_pt ==1
replace pt = 0 if dummy_prd_pt ==1

replace prd_pt_pc = prd + pt + pc if dummy_prd_pt_pc ==1
replace prd = 0 if dummy_prd_pt_pc ==1
replace pt = 0 if dummy_prd_pt_pc ==1
replace pc = 0 if dummy_prd_pt_pc ==1

replace pt_pc = pt + pc if dummy_pt_pc ==1
replace pt = 0 if dummy_pt_pc ==1
replace pc = 0 if dummy_pt_pc ==1

drop dummy_pan_pc dummy_pan_prd dummy_pan_prd_pt dummy_pan_prd_pt_pc dummy_pan_pt dummy_prd_pc dummy_prd_pt dummy_prd_pt_pc dummy_pt_pc

***********************************************************************************************

rename  pan     PAN
rename  alianza PRI_PVEM
rename  prd     PRD
rename  pc      PC
rename  pt      PT
rename  pan_pc     PAN_PC
rename  pan_prd    PAN_PRD
rename  pan_prd_pt PAN_PRD_PT
rename  pan_prd_pt_pc  PAN_PRD_PT_PC
rename  pan_pt  PAN_PT
rename  prd_pc  PRD_PC
rename  prd_pt  PRD_PT
rename  prd_pt_pc  PRD_PT_PC
rename  pt_pc  PT_PC

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=15001 if municipality =="ACAMBAY"
replace uniqueid=15002 if municipality =="ACOLMAN"
replace uniqueid=15003 if municipality =="ACULCO"
replace uniqueid=15004 if municipality =="ALMOLOYA DE ALQUISIRAS"
replace uniqueid=15005 if municipality =="ALMOLOYA DE JUAREZ"
replace uniqueid=15006 if municipality =="ALMOLOYA DEL RIO"
replace uniqueid=15007 if municipality =="AMANALCO"
replace uniqueid=15008 if municipality =="AMATEPEC"
replace uniqueid=15009 if municipality =="AMECAMECA"
replace uniqueid=15010 if municipality =="APAXCO"
replace uniqueid=15011 if municipality =="ATENCO"
replace uniqueid=15012 if municipality =="ATIZAPAN"
replace uniqueid=15013 if municipality =="ATIZAPAN DE ZARAGOZA"
replace uniqueid=15014 if municipality =="ATLACOMULCO"
replace uniqueid=15015 if municipality =="ATLAUTLA"
replace uniqueid=15016 if municipality =="AXAPUSCO"
replace uniqueid=15017 if municipality =="AYAPANGO"
replace uniqueid=15018 if municipality =="CALIMAYA"
replace uniqueid=15019 if municipality =="CAPULHUAC"
replace uniqueid=15025 if municipality =="CHALCO"
replace uniqueid=15026 if municipality =="CHAPA DE MOTA"
replace uniqueid=15027 if municipality =="CHAPULTEPEC"
replace uniqueid=15028 if municipality =="CHIAUTLA"
replace uniqueid=15029 if municipality =="CHICOLOAPAN"
replace uniqueid=15030 if municipality =="CHICONCUAC"
replace uniqueid=15031 if municipality =="CHIMALHUACAN"
replace uniqueid=15020 if municipality =="COACALCO"
replace uniqueid=15021 if municipality =="COATEPEC HARINAS"
replace uniqueid=15022 if municipality =="COCOTITLAN"
replace uniqueid=15023 if municipality =="COYOTEPEC"
replace uniqueid=15024 if municipality =="CUAUTITLAN"
replace uniqueid=15121 if municipality =="CUAUTITLAN IZCALLI"
replace uniqueid=15032 if municipality =="DONATO GUERRA"
replace uniqueid=15033 if municipality =="ECATEPEC"
replace uniqueid=15034 if municipality =="ECATZINGO"
replace uniqueid=15064 if municipality =="EL ORO"
replace uniqueid=15035 if municipality =="HUEHUETOCA"
replace uniqueid=15036 if municipality =="HUEYPOXTLA"
replace uniqueid=15037 if municipality =="HUIXQUILUCAN"
replace uniqueid=15038 if municipality =="ISIDRO FABELA"
replace uniqueid=15039 if municipality =="IXTAPALUCA"
replace uniqueid=15040 if municipality =="IXTAPAN DE LA SAL"
replace uniqueid=15041 if municipality =="IXTAPAN DEL ORO"
replace uniqueid=15042 if municipality =="IXTLAHUACA"
replace uniqueid=15044 if municipality =="JALTENCO"
replace uniqueid=15045 if municipality =="JILOTEPEC"
replace uniqueid=15046 if municipality =="JILOTZINGO"
replace uniqueid=15047 if municipality =="JIQUIPILCO"
replace uniqueid=15048 if municipality =="JOCOTITLAN"
replace uniqueid=15049 if municipality =="JOQUICINGO"
replace uniqueid=15050 if municipality =="JUCHITEPEC"
replace uniqueid=15070 if municipality =="LA PAZ"
replace uniqueid=15051 if municipality =="LERMA"
replace uniqueid=15123 if municipality =="LUVIANOS"
replace uniqueid=15052 if municipality =="MALINALCO"
replace uniqueid=15053 if municipality =="MELCHOR OCAMPO"
replace uniqueid=15054 if municipality =="METEPEC"
replace uniqueid=15055 if municipality =="MEXICALTZINGO"
replace uniqueid=15056 if municipality =="MORELOS"
replace uniqueid=15057 if municipality =="NAUCALPAN"
replace uniqueid=15059 if municipality =="NEXTLALPAN"
replace uniqueid=15058 if municipality =="NEZAHUALCOYOTL"
replace uniqueid=15060 if municipality =="NICOLAS ROMERO"
replace uniqueid=15061 if municipality =="NOPALTEPEC"
replace uniqueid=15062 if municipality =="OCOYOACAC"
replace uniqueid=15063 if municipality =="OCUILAN"
replace uniqueid=15065 if municipality =="OTUMBA"
replace uniqueid=15066 if municipality =="OTZOLOAPAN"
replace uniqueid=15067 if municipality =="OTZOLOTEPEC"
replace uniqueid=15068 if municipality =="OZUMBA"
replace uniqueid=15069 if municipality =="PAPALOTLA"
replace uniqueid=15071 if municipality =="POLOTITLAN"
replace uniqueid=15072 if municipality =="RAYON"
replace uniqueid=15073 if municipality =="SAN ANTONIO LA ISLA"
replace uniqueid=15074 if municipality =="SAN FELIPE DEL PROGRESO"
replace uniqueid=15124 if municipality =="SAN JOSE DEL RINCON"
replace uniqueid=15075 if municipality =="SAN MARTIN DE LAS PIRAMIDE"
replace uniqueid=15076 if municipality =="SAN MATEO ATENCO"
replace uniqueid=15077 if municipality =="SAN SIMON DE GUERRERO"
replace uniqueid=15078 if municipality =="SANTO TOMAS"
replace uniqueid=15079 if municipality =="SOYANIQUILPAN DE JUAREZ"
replace uniqueid=15080 if municipality =="SULTEPEC"
replace uniqueid=15081 if municipality =="TECAMAC"
replace uniqueid=15082 if municipality =="TEJUPILCO"
replace uniqueid=15083 if municipality =="TEMAMATLA"
replace uniqueid=15084 if municipality =="TEMASCALAPA"
replace uniqueid=15085 if municipality =="TEMASCALCINGO"
replace uniqueid=15086 if municipality =="TEMASCALTEPEC"
replace uniqueid=15087 if municipality =="TEMOAYA"
replace uniqueid=15088 if municipality =="TENANCINGO"
replace uniqueid=15089 if municipality =="TENANGO DEL AIRE"
replace uniqueid=15090 if municipality =="TENANGO DEL VALLE"
replace uniqueid=15091 if municipality =="TEOLOYUCAN"
replace uniqueid=15092 if municipality =="TEOTIHUACAN"
replace uniqueid=15093 if municipality =="TEPETLAOXTOC"
replace uniqueid=15094 if municipality =="TEPETLIXPA"
replace uniqueid=15095 if municipality =="TEPOTZOTLAN"
replace uniqueid=15096 if municipality =="TEQUIXQUIAC"
replace uniqueid=15097 if municipality =="TEXCALTITLAN"
replace uniqueid=15098 if municipality =="TEXCALYACAC"
replace uniqueid=15099 if municipality =="TEXCOCO"
replace uniqueid=15100 if municipality =="TEZOYUCA"
replace uniqueid=15101 if municipality =="TIANGUISTENCO"
replace uniqueid=15102 if municipality =="TIMILPAN"
replace uniqueid=15103 if municipality =="TLALMANALCO"
replace uniqueid=15104 if municipality =="TLALNEPANTLA"
replace uniqueid=15105 if municipality =="TLATLAYA"
replace uniqueid=15106 if municipality =="TOLUCA"
replace uniqueid=15125 if municipality =="TONANITLA"
replace uniqueid=15107 if municipality =="TONATICO"
replace uniqueid=15108 if municipality =="TULTEPEC"
replace uniqueid=15109 if municipality =="TULTITLAN"
replace uniqueid=15110 if municipality =="VALLE DE BRAVO"
replace uniqueid=15122 if municipality =="VALLE DE CHALCO SOLIDARIDA"
replace uniqueid=15111 if municipality =="VILLA DE ALLENDE"
replace uniqueid=15112 if municipality =="VILLA DEL CARBON"
replace uniqueid=15113 if municipality =="VILLA GUERRERO"
replace uniqueid=15114 if municipality =="VILLA VICTORIA"
replace uniqueid=15043 if municipality =="XALATLACO"
replace uniqueid=15115 if municipality =="XONACATLAN"
replace uniqueid=15116 if municipality =="ZACAZONAPAN"
replace uniqueid=15117 if municipality =="ZACUALPAN"
replace uniqueid=15118 if municipality =="ZINACANTEPEC"
replace uniqueid=15119 if municipality =="ZUMPAHUACAN"
replace uniqueid=15120 if municipality =="ZUMPANGO"

egen valid = rowtotal(PAN PRI_PVEM PRD PT PC PAN_PC PAN_PRD PAN_PRD_PT PAN_PRD_PT_PC PAN_PT PRD_PC PRD_PT PRD_PT_PC PT_PC)

foreach var in PAN PRI_PVEM PRD PT PC PAN_PC PAN_PRD PAN_PRD_PT PAN_PRD_PT_PC PAN_PT PRD_PC PRD_PT PRD_PT_PC PT_PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_PAN_PC inv_mun_PAN_PRD inv_mun_PAN_PRD_PT inv_mun_PAN_PRD_PT_PC inv_mun_PAN_PT inv_mun_PRD_PC inv_mun_PRD_PT inv_mun_PRD_PT_PC inv_mun_PT_PC, gen(PAN_r PRI_PVEM_r PRD_r PT_r PC_r PAN_PC_r PAN_PRD_r PAN_PRD_PT_r PAN_PRD_PT_PC_r PAN_PT_r PRD_PC_r PRD_PT_r PRD_PT_PC_r PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PC" if PC_r==1
replace winner = "PAN_PC" if PAN_PC_r==1  
replace winner = "PAN_PRD" if PAN_PRD_r==1  
replace winner = "PAN_PRD_PT" if PAN_PRD_PT_r==1  
replace winner = "PAN_PRD_PT_PC" if PAN_PRD_PT_PC_r==1  
replace winner = "PAN_PT" if PAN_PT_r==1  
replace winner = "PRD_PC" if PRD_PC_r==1
replace winner = "PRD_PT" if PRD_PT_r==1
replace winner = "PRD_PT_PC" if PRD_PT_PC_r==1
replace winner = "PT_PC" if PT_PC_r==1
drop *_r

gen year =2006
gen month ="March"
sort section

save Mexico_Section_2006.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2009.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="". & section==.
drop if total==. | total==0

destring pan -  total, replace

collapse (sum) listanominal pan -  ptc total, by (municipality section)


*********************************************************************************************

gen sec_dummy_pan_pc = 0
gen sec_dummy_pri_pvem_na_psd_pfd = 0
gen sec_dummy_prd_pt = 0 
gen sec_dummy_pt_pc = 0

replace sec_dummy_pan_pc = 1 if panc > 0 
replace sec_dummy_pri_pvem_na_psd_pfd = 1 if pripvemnapsdpfd > 0 
replace sec_dummy_prd_pt = 1 if prdpt > 0
replace sec_dummy_pt_pc = 1 if ptc  > 0

by municipality: egen  dummy_pan_pc = max(sec_dummy_pan_pc)
by municipality: egen  dummy_pri_pvem_na_psd_pfd = max(sec_dummy_pri_pvem_na_psd_pfd)
by municipality: egen  dummy_prd_pt = max(sec_dummy_prd_pt)
by municipality: egen  dummy_pt_pc = max(sec_dummy_pt_pc)
 
drop  sec_dummy_pan_pc sec_dummy_pri_pvem_na_psd_pfd sec_dummy_prd_pt sec_dummy_pt_pc

gen pan_pc = panc + pan + pc if dummy_pan_pc==1
replace pan = 0 if dummy_pan_pc==1
replace pc  = 0 if dummy_pan_pc==1
drop panc 

gen pri_pvem_na_psd_pfd = pripvemnapsdpfd + pri + pvem + panal + psd + pfd  if dummy_pri_pvem_na_psd_pfd==1
replace pri  = 0  if dummy_pri_pvem_na_psd_pfd==1
replace pvem  = 0  if dummy_pri_pvem_na_psd_pfd==1 
replace panal   = 0  if dummy_pri_pvem_na_psd_pfd==1
replace psd   = 0  if dummy_pri_pvem_na_psd_pfd==1
replace pfd  = 0  if dummy_pri_pvem_na_psd_pfd==1
drop pripvemnapsdpfd

gen  prd_pt = prdpt + prd + pt if dummy_prd_pt==1
replace prd = 0 if dummy_prd_pt==1
replace pt = 0 if dummy_prd_pt==1
drop prdpt

gen pt_pc = ptc + pt+ pc if dummy_pt_pc==1
replace pt = 0 if dummy_pt_pc==1
replace pc = 0 if dummy_pt_pc==1
drop ptc

drop  dummy_pan_pc  dummy_pri_pvem_na_psd_pfd dummy_prd_pt dummy_prd_pt dummy_pt_pc
drop pri pvem  panal psd pfd

*********************************************************************************************
rename  pan     PAN
rename  pri_pvem_na_psd_pfd PRI_PVEM_PANAL_PSD_PFD
rename  prd     PRD
rename  pc      PC
rename  pt      PT
rename  pan_pc     PAN_PC
rename  prd_pt  PRD_PT
rename  pt_pc  PT_PC

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=15001 if municipality =="ACAMBAY"
replace uniqueid=15002 if municipality =="ACOLMAN"
replace uniqueid=15003 if municipality =="ACULCO"
replace uniqueid=15004 if municipality =="ALMOLOYA DE ALQUISIRAS"
replace uniqueid=15005 if municipality =="ALMOLOYA DE JUAREZ"
replace uniqueid=15006 if municipality =="ALMOLOYA DEL RIO"
replace uniqueid=15007 if municipality =="AMANALCO"
replace uniqueid=15008 if municipality =="AMATEPEC"
replace uniqueid=15009 if municipality =="AMECAMECA"
replace uniqueid=15010 if municipality =="APAXCO"
replace uniqueid=15011 if municipality =="ATENCO"
replace uniqueid=15012 if municipality =="ATIZAPAN"
replace uniqueid=15013 if municipality =="ATIZAPAN DE ZARAGOZA"
replace uniqueid=15014 if municipality =="ATLACOMULCO"
replace uniqueid=15015 if municipality =="ATLAUTLA"
replace uniqueid=15016 if municipality =="AXAPUSCO"
replace uniqueid=15017 if municipality =="AYAPANGO"
replace uniqueid=15018 if municipality =="CALIMAYA"
replace uniqueid=15019 if municipality =="CAPULHUAC"
replace uniqueid=15025 if municipality =="CHALCO"
replace uniqueid=15026 if municipality =="CHAPA DE MOTA"
replace uniqueid=15027 if municipality =="CHAPULTEPEC"
replace uniqueid=15028 if municipality =="CHIAUTLA"
replace uniqueid=15029 if municipality =="CHICOLOAPAN"
replace uniqueid=15030 if municipality =="CHICONCUAC"
replace uniqueid=15031 if municipality =="CHIMALHUACAN"
replace uniqueid=15020 if municipality =="COACALCO"
replace uniqueid=15021 if municipality =="COATEPEC HARINAS"
replace uniqueid=15022 if municipality =="COCOTITLAN"
replace uniqueid=15023 if municipality =="COYOTEPEC"
replace uniqueid=15024 if municipality =="CUAUTITLAN"
replace uniqueid=15121 if municipality =="CUAUTITLAN IZCALLI"
replace uniqueid=15032 if municipality =="DONATO GUERRA"
replace uniqueid=15033 if municipality =="ECATEPEC"
replace uniqueid=15034 if municipality =="ECATZINGO"
replace uniqueid=15064 if municipality =="EL ORO"
replace uniqueid=15035 if municipality =="HUEHUETOCA"
replace uniqueid=15036 if municipality =="HUEYPOXTLA"
replace uniqueid=15037 if municipality =="HUIXQUILUCAN"
replace uniqueid=15038 if municipality =="ISIDRO FABELA"
replace uniqueid=15039 if municipality =="IXTAPALUCA"
replace uniqueid=15040 if municipality =="IXTAPAN DE LA SAL"
replace uniqueid=15041 if municipality =="IXTAPAN DEL ORO"
replace uniqueid=15042 if municipality =="IXTLAHUACA"
replace uniqueid=15044 if municipality =="JALTENCO"
replace uniqueid=15045 if municipality =="JILOTEPEC"
replace uniqueid=15046 if municipality =="JILOTZINGO"
replace uniqueid=15047 if municipality =="JIQUIPILCO"
replace uniqueid=15048 if municipality =="JOCOTITLAN"
replace uniqueid=15049 if municipality =="JOQUICINGO"
replace uniqueid=15050 if municipality =="JUCHITEPEC"
replace uniqueid=15070 if municipality =="LA PAZ"
replace uniqueid=15051 if municipality =="LERMA"
replace uniqueid=15123 if municipality =="LUVIANOS"
replace uniqueid=15052 if municipality =="MALINALCO"
replace uniqueid=15053 if municipality =="MELCHOR OCAMPO"
replace uniqueid=15054 if municipality =="METEPEC"
replace uniqueid=15055 if municipality =="MEXICALTZINGO"
replace uniqueid=15056 if municipality =="MORELOS"
replace uniqueid=15057 if municipality =="NAUCALPAN"
replace uniqueid=15059 if municipality =="NEXTLALPAN"
replace uniqueid=15058 if municipality =="NEZAHUALCOYOTL"
replace uniqueid=15060 if municipality =="NICOLAS ROMERO"
replace uniqueid=15061 if municipality =="NOPALTEPEC"
replace uniqueid=15062 if municipality =="OCOYOACAC"
replace uniqueid=15063 if municipality =="OCUILAN"
replace uniqueid=15065 if municipality =="OTUMBA"
replace uniqueid=15066 if municipality =="OTZOLOAPAN"
replace uniqueid=15067 if municipality =="OTZOLOTEPEC"
replace uniqueid=15068 if municipality =="OZUMBA"
replace uniqueid=15069 if municipality =="PAPALOTLA"
replace uniqueid=15071 if municipality =="POLOTITLAN"
replace uniqueid=15072 if municipality =="RAYON"
replace uniqueid=15073 if municipality =="SAN ANTONIO LA ISLA"
replace uniqueid=15074 if municipality =="SAN FELIPE DEL PROGRESO"
replace uniqueid=15124 if municipality =="SAN JOSE DEL RINCON"
replace uniqueid=15075 if municipality =="SAN MARTIN DE LAS PIRAMIDES"
replace uniqueid=15076 if municipality =="SAN MATEO ATENCO"
replace uniqueid=15077 if municipality =="SAN SIMON DE GUERRERO"
replace uniqueid=15078 if municipality =="SANTO TOMAS"
replace uniqueid=15079 if municipality =="SOYANIQUILPAN DE JUAREZ"
replace uniqueid=15080 if municipality =="SULTEPEC"
replace uniqueid=15081 if municipality =="TECAMAC"
replace uniqueid=15082 if municipality =="TEJUPILCO"
replace uniqueid=15083 if municipality =="TEMAMATLA"
replace uniqueid=15084 if municipality =="TEMASCALAPA"
replace uniqueid=15085 if municipality =="TEMASCALCINGO"
replace uniqueid=15086 if municipality =="TEMASCALTEPEC"
replace uniqueid=15087 if municipality =="TEMOAYA"
replace uniqueid=15088 if municipality =="TENANCINGO"
replace uniqueid=15089 if municipality =="TENANGO DEL AIRE"
replace uniqueid=15090 if municipality =="TENANGO DEL VALLE"
replace uniqueid=15091 if municipality =="TEOLOYUCAN"
replace uniqueid=15092 if municipality =="TEOTIHUACAN"
replace uniqueid=15093 if municipality =="TEPETLAOXTOC"
replace uniqueid=15094 if municipality =="TEPETLIXPA"
replace uniqueid=15095 if municipality =="TEPOTZOTLAN"
replace uniqueid=15096 if municipality =="TEQUIXQUIAC"
replace uniqueid=15097 if municipality =="TEXCALTITLAN"
replace uniqueid=15098 if municipality =="TEXCALYACAC"
replace uniqueid=15099 if municipality =="TEXCOCO"
replace uniqueid=15100 if municipality =="TEZOYUCA"
replace uniqueid=15101 if municipality =="TIANGUISTENCO"
replace uniqueid=15102 if municipality =="TIMILPAN"
replace uniqueid=15103 if municipality =="TLALMANALCO"
replace uniqueid=15104 if municipality =="TLALNEPANTLA"
replace uniqueid=15105 if municipality =="TLATLAYA"
replace uniqueid=15106 if municipality =="TOLUCA"
replace uniqueid=15125 if municipality =="TONANITLA"
replace uniqueid=15107 if municipality =="TONATICO"
replace uniqueid=15108 if municipality =="TULTEPEC"
replace uniqueid=15109 if municipality =="TULTITLAN"
replace uniqueid=15110 if municipality =="VALLE DE BRAVO"
replace uniqueid=15122 if municipality =="VALLE DE CHALCO SOLIDARIDAD"
replace uniqueid=15111 if municipality =="VILLA DE ALLENDE"
replace uniqueid=15112 if municipality =="VILLA DEL CARBON"
replace uniqueid=15113 if municipality =="VILLA GUERRERO"
replace uniqueid=15114 if municipality =="VILLA VICTORIA"
replace uniqueid=15043 if municipality =="XALATLACO"
replace uniqueid=15115 if municipality =="XONACATLAN"
replace uniqueid=15116 if municipality =="ZACAZONAPAN"
replace uniqueid=15117 if municipality =="ZACUALPAN"
replace uniqueid=15118 if municipality =="ZINACANTEPEC"
replace uniqueid=15119 if municipality =="ZUMPAHUACAN"
replace uniqueid=15120 if municipality =="ZUMPANGO"

egen valid = rowtotal(PAN PRD PT PC PAN_PC PRI_PVEM_PANAL_PSD_PFD PRD_PT PT_PC)

foreach var in PAN PRD PT PC PAN_PC PRI_PVEM_PANAL_PSD_PFD PRD_PT PT_PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_PAN_PC inv_mun_PRI_PVEM_PANAL_PSD_PFD inv_mun_PRD_PT inv_mun_PT_PC, gen(PAN_r PRD_r PT_r PC_r PAN_PC_r PRI_PVEM_PANAL_PSD_PFD_r PRD_PT_r PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PAN_PC" if PAN_PC_r==1  
replace winner = "PRI_PVEM_PANAL_PSD_PFD" if PRI_PVEM_PANAL_PSD_PFD_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PRD_PT" if PRD_PT_r==1
replace winner = "PT" if PT_r==1
replace winner = "PC" if PC_r==1
replace winner = "PT_PC" if PT_PC_r==1
drop *_r

gen year =2009
gen month ="July"

sort section

save Mexico_Section_2009.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "Ayu_Seccion_2012.xlsx", sheet("CASILLAS_AYUNTAMIENTOS_2012") firstrow clear

rename nombre_municipio municipality
rename seccion section
rename lista_nominal listanominal

rename PRIPVEMPANAL PRI_PVEM_PANAL
rename PRDPTMC PRD_PT_PC 
rename PRDPT PRD_PT
rename PRDMC PRD_PC
rename PRMC PT_PC
rename MC PC

egen total = rowtotal(PAN PRI_PVEM_PANAL PRD PT PC PRD_PT_PC PRD_PT PRD_PC PT_PC NoRegistrados Nulos)

drop if total ==. | total==0

collapse (sum) listanominal PAN - PT_PC total, by(municipality section)

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=15001 if municipality =="ACAMBAY"
replace uniqueid=15002 if municipality =="ACOLMAN"
replace uniqueid=15003 if municipality =="ACULCO"
replace uniqueid=15004 if municipality =="ALMOLOYA DE ALQUISIRAS"
replace uniqueid=15005 if municipality =="ALMOLOYA DE JUAREZ"
replace uniqueid=15006 if municipality =="ALMOLOYA DEL RIO"
replace uniqueid=15007 if municipality =="AMANALCO"
replace uniqueid=15008 if municipality =="AMATEPEC"
replace uniqueid=15009 if municipality =="AMECAMECA"
replace uniqueid=15010 if municipality =="APAXCO"
replace uniqueid=15011 if municipality =="ATENCO"
replace uniqueid=15012 if municipality =="ATIZAPAN"
replace uniqueid=15013 if municipality =="ATIZAPAN DE ZARAGOZA"
replace uniqueid=15014 if municipality =="ATLACOMULCO"
replace uniqueid=15015 if municipality =="ATLAUTLA"
replace uniqueid=15016 if municipality =="AXAPUSCO"
replace uniqueid=15017 if municipality =="AYAPANGO"
replace uniqueid=15018 if municipality =="CALIMAYA"
replace uniqueid=15019 if municipality =="CAPULHUAC"
replace uniqueid=15025 if municipality =="CHALCO"
replace uniqueid=15026 if municipality =="CHAPA DE MOTA"
replace uniqueid=15027 if municipality =="CHAPULTEPEC"
replace uniqueid=15028 if municipality =="CHIAUTLA"
replace uniqueid=15029 if municipality =="CHICOLOAPAN"
replace uniqueid=15030 if municipality =="CHICONCUAC"
replace uniqueid=15031 if municipality =="CHIMALHUACAN"
replace uniqueid=15020 if municipality =="COACALCO DE BERRIOZABAL"
replace uniqueid=15021 if municipality =="COATEPEC HARINAS"
replace uniqueid=15022 if municipality =="COCOTITLAN"
replace uniqueid=15023 if municipality =="COYOTEPEC"
replace uniqueid=15024 if municipality =="CUAUTITLAN"
replace uniqueid=15121 if municipality =="CUAUTITLAN IZCALLI"
replace uniqueid=15032 if municipality =="DONATO GUERRA"
replace uniqueid=15033 if municipality =="ECATEPEC DE MORELOS"
replace uniqueid=15034 if municipality =="ECATZINGO"
replace uniqueid=15064 if municipality =="EL ORO"
replace uniqueid=15035 if municipality =="HUEHUETOCA"
replace uniqueid=15036 if municipality =="HUEYPOXTLA"
replace uniqueid=15037 if municipality =="HUIXQUILUCAN"
replace uniqueid=15038 if municipality =="ISIDRO FABELA"
replace uniqueid=15039 if municipality =="IXTAPALUCA"
replace uniqueid=15040 if municipality =="IXTAPAN DE LA SAL"
replace uniqueid=15041 if municipality =="IXTAPAN DEL ORO"
replace uniqueid=15042 if municipality =="IXTLAHUACA"
replace uniqueid=15044 if municipality =="JALTENCO"
replace uniqueid=15045 if municipality =="JILOTEPEC"
replace uniqueid=15046 if municipality =="JILOTZINGO"
replace uniqueid=15047 if municipality =="JIQUIPILCO"
replace uniqueid=15048 if municipality =="JOCOTITLAN"
replace uniqueid=15049 if municipality =="JOQUICINGO"
replace uniqueid=15050 if municipality =="JUCHITEPEC"
replace uniqueid=15070 if municipality =="LA PAZ"
replace uniqueid=15051 if municipality =="LERMA"
replace uniqueid=15123 if municipality =="LUVIANOS"
replace uniqueid=15052 if municipality =="MALINALCO"
replace uniqueid=15053 if municipality =="MELCHOR OCAMPO"
replace uniqueid=15054 if municipality =="METEPEC"
replace uniqueid=15055 if municipality =="MEXICALTZINGO"
replace uniqueid=15056 if municipality =="MORELOS"
replace uniqueid=15057 if municipality =="NAUCALPAN DE JUAREZ"
replace uniqueid=15059 if municipality =="NEXTLALPAN"
replace uniqueid=15058 if municipality =="NEZAHUALCOYOTL"
replace uniqueid=15060 if municipality =="NICOLAS ROMERO"
replace uniqueid=15061 if municipality =="NOPALTEPEC"
replace uniqueid=15062 if municipality =="OCOYOACAC"
replace uniqueid=15063 if municipality =="OCUILAN"
replace uniqueid=15065 if municipality =="OTUMBA"
replace uniqueid=15066 if municipality =="OTZOLOAPAN"
replace uniqueid=15067 if municipality =="OTZOLOTEPEC"
replace uniqueid=15068 if municipality =="OZUMBA"
replace uniqueid=15069 if municipality =="PAPALOTLA"
replace uniqueid=15071 if municipality =="POLOTITLAN"
replace uniqueid=15072 if municipality =="RAYON"
replace uniqueid=15073 if municipality =="SAN ANTONIO LA ISLA"
replace uniqueid=15074 if municipality =="SAN FELIPE DEL PROGRESO"
replace uniqueid=15124 if municipality =="SAN JOSE DEL RINCON"
replace uniqueid=15075 if municipality =="SAN MARTIN DE LAS PIRAMIDES"
replace uniqueid=15076 if municipality =="SAN MATEO ATENCO"
replace uniqueid=15077 if municipality =="SAN SIMON DE GUERRERO"
replace uniqueid=15078 if municipality =="SANTO TOMAS"
replace uniqueid=15079 if municipality =="SOYANIQUILPAN DE JUAREZ"
replace uniqueid=15080 if municipality =="SULTEPEC"
replace uniqueid=15081 if municipality =="TECAMAC"
replace uniqueid=15082 if municipality =="TEJUPILCO"
replace uniqueid=15083 if municipality =="TEMAMATLA"
replace uniqueid=15084 if municipality =="TEMASCALAPA"
replace uniqueid=15085 if municipality =="TEMASCALCINGO"
replace uniqueid=15086 if municipality =="TEMASCALTEPEC"
replace uniqueid=15087 if municipality =="TEMOAYA"
replace uniqueid=15088 if municipality =="TENANCINGO"
replace uniqueid=15089 if municipality =="TENANGO DEL AIRE"
replace uniqueid=15090 if municipality =="TENANGO DEL VALLE"
replace uniqueid=15091 if municipality =="TEOLOYUCAN"
replace uniqueid=15092 if municipality =="TEOTIHUACAN"
replace uniqueid=15093 if municipality =="TEPETLAOXTOC"
replace uniqueid=15094 if municipality =="TEPETLIXPA"
replace uniqueid=15095 if municipality =="TEPOTZOTLAN"
replace uniqueid=15096 if municipality =="TEQUIXQUIAC"
replace uniqueid=15097 if municipality =="TEXCALTITLAN"
replace uniqueid=15098 if municipality =="TEXCALYACAC"
replace uniqueid=15099 if municipality =="TEXCOCO"
replace uniqueid=15100 if municipality =="TEZOYUCA"
replace uniqueid=15101 if municipality =="TIANGUISTENCO"
replace uniqueid=15102 if municipality =="TIMILPAN"
replace uniqueid=15103 if municipality =="TLALMANALCO"
replace uniqueid=15104 if municipality =="TLALNEPANTLA DE BAZ"
replace uniqueid=15105 if municipality =="TLATLAYA"
replace uniqueid=15106 if municipality =="TOLUCA"
replace uniqueid=15125 if municipality =="TONANITLA"
replace uniqueid=15107 if municipality =="TONATICO"
replace uniqueid=15108 if municipality =="TULTEPEC"
replace uniqueid=15109 if municipality =="TULTITLAN"
replace uniqueid=15110 if municipality =="VALLE DE BRAVO"
replace uniqueid=15122 if municipality =="VALLE DE CHALCO SOLIDARIDAD"
replace uniqueid=15111 if municipality =="VILLA DE ALLENDE"
replace uniqueid=15112 if municipality =="VILLA DEL CARBON"
replace uniqueid=15113 if municipality =="VILLA GUERRERO"
replace uniqueid=15114 if municipality =="VILLA VICTORIA"
replace uniqueid=15043 if municipality =="XALATLACO"
replace uniqueid=15115 if municipality =="XONACATLAN"
replace uniqueid=15116 if municipality =="ZACAZONAPAN"
replace uniqueid=15117 if municipality =="ZACUALPAN"
replace uniqueid=15118 if municipality =="ZINACANTEPEC"
replace uniqueid=15119 if municipality =="ZUMPAHUACAN"
replace uniqueid=15120 if municipality =="ZUMPANGO"

egen valid = rowtotal(PAN PRI_PVEM_PANAL PRD PT PC PRD_PT_PC PRD_PT PRD_PC PT_PC)

foreach var in PAN PRI_PVEM_PANAL PRD PT PC PRD_PT_PC PRD_PT PRD_PC PT_PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM_PANAL inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_PRD_PT_PC inv_mun_PRD_PT inv_mun_PRD_PC inv_mun_PT_PC, gen(PAN_r PRI_PVEM_PANAL_r PRD_r PT_r PC_r PRD_PT_PC_r PRD_PT_r PRD_PC_r PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM_PANAL" if PRI_PVEM_PANAL_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PC" if PC_r==1
replace winner = "PRD_PT_PC" if PRD_PT_PC_r==1
replace winner = "PRD_PT" if PRD_PT_r==1
replace winner = "PRD_PC" if PRD_PC_r==1
replace winner = "PT_PC" if PT_PC_r==1
drop *_r

gen year =2012
gen month ="July"

sort section

save Mexico_Section_2012.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Mexico_Section_1996.dta
append using Mexico_Section_2000.dta
append using Mexico_Section_2003.dta
append using Mexico_Section_2003_EXTRAORDINARIO.dta
append using Mexico_Section_2006.dta
append using Mexico_Section_2009.dta
append using Mexico_Section_2012.dta

erase Mexico_Section_1996.dta
erase Mexico_Section_2000.dta
erase Mexico_Section_2003.dta
erase Mexico_Section_2003_EXTRAORDINARIO.dta
erase Mexico_Section_2006.dta
erase Mexico_Section_2009.dta
erase Mexico_Section_2012.dta

tab winner, missing

br if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS PSN PFD {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

saveold Mexico_ALL.dta, replace version(12)

saveold "..\Mexico_ALL.dta", replace version(12)

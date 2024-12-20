clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Veracruz 2000, 2004, 2007, 2010, 2013"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Veracruz 2000, 2004, 2007, 2010, 2013"

**************************************************************************
**************************************************************************
**************************************************************************

do DoFile_Section_Veracruz2000.do

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Lisado Nominal 2004.xlsx", sheet("Sheet1") firstrow clear

keep section listanominal
drop if section==. & listanominal==.

count if section!=. & listanominal==.
count if section==. & listanominal!=.

sort section 

save Listanominal2004.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using  Ayu_Seccion_2004_No_LN.csv, clear

rename municipio  municipality
rename secc section
* rename listanominal nominal

drop if municipality=="" & section==.

destring pan - nulos , replace

collapse (sum)  pan - nulos , by (municipality section)

egen total = rowtotal(pan fide prd prv no_reg nulos)
drop if total==. | total==0 

rename pan  PAN
rename fide  PRI_PVEM
rename prd  PRD_PT_PC
rename prv   PRV

sort section
merge section using Listanominal2004.dta
drop if _merge==2
drop _merge 

gen turnout =  total/listanominal

drop  nulos   no_reg

gen   uniqueid= 0
replace uniqueid=30001 if municipality =="ACAJETE"
replace uniqueid=30002 if municipality =="ACATLAN"
replace uniqueid=30003 if municipality =="ACAYUCAN"
replace uniqueid=30004 if municipality =="ACTOPAN"
replace uniqueid=30005 if municipality =="ACULA"
replace uniqueid=30006 if municipality =="ACULTZINGO"
replace uniqueid=30204 if municipality =="AGUA DULCE"
replace uniqueid=30160 if municipality =="TEMAPACHE"
replace uniqueid=30008 if municipality =="ALPATLAHUAC"
replace uniqueid=30009 if municipality =="ALTO LUCERO"
replace uniqueid=30010 if municipality =="ALTOTONGA"
replace uniqueid=30011 if municipality =="ALVARADO"
replace uniqueid=30012 if municipality =="AMATITLAN"
replace uniqueid=30014 if municipality =="AMATLAN DE LOS REYES"
replace uniqueid=30015 if municipality =="ANGEL R. CABADA"
replace uniqueid=30017 if municipality =="APAZAPAN"
replace uniqueid=30018 if municipality =="AQUILA"
replace uniqueid=30019 if municipality =="ASTACINGA"
replace uniqueid=30020 if municipality =="ATLAHUILCO"
replace uniqueid=30021 if municipality =="ATOYAC"
replace uniqueid=30022 if municipality =="ATZACAN"
replace uniqueid=30023 if municipality =="ATZALAN"
replace uniqueid=30025 if municipality =="AYAHUALULCO"
replace uniqueid=30026 if municipality =="BANDERILLA"
replace uniqueid=30027 if municipality =="BENITO JUAREZ"
replace uniqueid=30028 if municipality =="BOCA DEL RIO"
replace uniqueid=30029 if municipality =="CALCAHUALCO"
replace uniqueid=30007 if municipality =="CAMARON DE TEJEDA"
replace uniqueid=30030 if municipality =="CAMERINO Z. MENDOZA"
replace uniqueid=30208 if municipality =="CARLOS A. CARRILLO"
replace uniqueid=30031 if municipality =="CARRILLO PUERTO"
replace uniqueid=30157 if municipality =="CASTILLO DE TEAYO"
replace uniqueid=30032 if municipality =="CATEMACO"
replace uniqueid=30033 if municipality =="CAZONES DE HERRERA"
replace uniqueid=30034 if municipality =="CERRO AZUL"
replace uniqueid=30054 if municipality =="CHACALTIANGUIS"
replace uniqueid=30055 if municipality =="CHALMA"
replace uniqueid=30056 if municipality =="CHICONAMEL"
replace uniqueid=30057 if municipality =="CHICONQUIACO"
replace uniqueid=30058 if municipality =="CHICONTEPEC"
replace uniqueid=30059 if municipality =="CHINAMECA"
replace uniqueid=30060 if municipality =="CHINAMPA DE GOROSTIZA"
replace uniqueid=30062 if municipality =="CHOCAMAN"
replace uniqueid=30063 if municipality =="CHONTLA"
replace uniqueid=30064 if municipality =="CHUMATLAN"
replace uniqueid=30035 if municipality =="CITLALTEPETL"
replace uniqueid=30036 if municipality =="COACOATZINTLA"
replace uniqueid=30037 if municipality =="COAHUITLAN"
replace uniqueid=30038 if municipality =="COATEPEC"
replace uniqueid=30039 if municipality =="COATZACOALCOS"
replace uniqueid=30040 if municipality =="COATZINTLA"
replace uniqueid=30041 if municipality =="COETZALA"
replace uniqueid=30042 if municipality =="COLIPA"
replace uniqueid=30043 if municipality =="COMAPA"
replace uniqueid=30044 if municipality =="CORDOBA"
replace uniqueid=30045 if municipality =="COSAMALOAPAN"
replace uniqueid=30046 if municipality =="COSAUTLAN DE CARVAJAL"
replace uniqueid=30047 if municipality =="COSCOMATEPEC"
replace uniqueid=30048 if municipality =="COSOLEACAQUE"
replace uniqueid=30049 if municipality =="COTAXTLA"
replace uniqueid=30050 if municipality =="COXQUIHUI"
replace uniqueid=30051 if municipality =="COYUTLA"
replace uniqueid=30052 if municipality =="CUICHAPA"
replace uniqueid=30053 if municipality =="CUITLAHUAC"
replace uniqueid=30205 if municipality =="EL HIGO"
replace uniqueid=30065 if municipality =="EMILIANO ZAPATA"
replace uniqueid=30066 if municipality =="ESPINAL"
replace uniqueid=30067 if municipality =="FILOMENO MATA"
replace uniqueid=30068 if municipality =="FORTIN"
replace uniqueid=30069 if municipality =="GUTIERREZ ZAMORA"
replace uniqueid=30070 if municipality =="HIDALGOTITLAN"
replace uniqueid=30071 if municipality =="HUATUSCO"
replace uniqueid=30072 if municipality =="HUAYACOCOTLA"
replace uniqueid=30073 if municipality =="HUEYAPAN DE OCAMPO"
replace uniqueid=30074 if municipality =="HUILOAPAN DE CUAUHTEMOC"
replace uniqueid=30075 if municipality =="IGNACIO DE LA LLAVE"
replace uniqueid=30076 if municipality =="ILAMATLAN"
replace uniqueid=30077 if municipality =="ISLA"
replace uniqueid=30078 if municipality =="IXCATEPEC"
replace uniqueid=30079 if municipality =="IXHUACAN DE LOS REYES"
replace uniqueid=30083 if municipality =="IXHUATLAN DE MADERO"
replace uniqueid=30080 if municipality =="IXHUATLAN DEL CAFE"
replace uniqueid=30082 if municipality =="IXHUATLAN DEL SURESTE"
replace uniqueid=30081 if municipality =="IXHUATLANCILLO"
replace uniqueid=30084 if municipality =="IXMATLAHUACAN"
replace uniqueid=30085 if municipality =="IXTACZOQUITLAN"
replace uniqueid=30086 if municipality =="JALACINGO"
replace uniqueid=30088 if municipality =="JALCOMULCO"
replace uniqueid=30089 if municipality =="JALTIPAN"
replace uniqueid=30090 if municipality =="JAMAPA"
replace uniqueid=30091 if municipality =="JESUS CARRANZA"
replace uniqueid=30093 if municipality =="JILOTEPEC"
replace uniqueid=30169 if municipality =="JOSE AZUETA"
replace uniqueid=30094 if municipality =="JUAN RODRIGUEZ CLARA"
replace uniqueid=30095 if municipality =="JUCHIQUE DE FERRER"
replace uniqueid=30016 if municipality =="LA ANTIGUA"
replace uniqueid=30127 if municipality =="LA PERLA"
replace uniqueid=30096 if municipality =="LANDERO Y COSS"
replace uniqueid=30061 if municipality =="LAS CHOAPAS"
replace uniqueid=30107 if municipality =="LAS MINAS"
replace uniqueid=30132 if municipality =="LAS VIGAS DE RAMIREZ"
replace uniqueid=30097 if municipality =="LERDO DE TEJADA"
replace uniqueid=30137 if municipality =="LOS REYES"
replace uniqueid=30098 if municipality =="MAGDALENA"
replace uniqueid=30099 if municipality =="MALTRATA"
replace uniqueid=30100 if municipality =="MANLIO FABIO ALTAMIRANO"
replace uniqueid=30101 if municipality =="MARIANO ESCOBEDO"
replace uniqueid=30102 if municipality =="MARTINEZ DE LA TORRE"
replace uniqueid=30103 if municipality =="MECATLAN"
replace uniqueid=30104 if municipality =="MECAYAPAN"
replace uniqueid=30105 if municipality =="MEDELLIN"
replace uniqueid=30106 if municipality =="MIAHUATLAN"
replace uniqueid=30108 if municipality =="MINATITLAN"
replace uniqueid=30109 if municipality =="MISANTLA"
replace uniqueid=30110 if municipality =="MIXTLA DE ALTAMIRANO"
replace uniqueid=30111 if municipality =="MOLOACAN"
replace uniqueid=30206 if municipality =="NANCHITAL DE L.C. DEL RIO"
replace uniqueid=30112 if municipality =="NAOLINCO"
replace uniqueid=30113 if municipality =="NARANJAL"
replace uniqueid=30013 if municipality =="NARANJOS AMATLAN"
replace uniqueid=30114 if municipality =="NAUTLA"
replace uniqueid=30115 if municipality =="NOGALES"
replace uniqueid=30116 if municipality =="OLUTA"
replace uniqueid=30117 if municipality =="OMEALCA"
replace uniqueid=30118 if municipality =="ORIZABA"
replace uniqueid=30119 if municipality =="OTATITLAN"
replace uniqueid=30120 if municipality =="OTEAPAN"
replace uniqueid=30121 if municipality =="OZULUAMA"
replace uniqueid=30122 if municipality =="PAJAPAN"
replace uniqueid=30123 if municipality =="PANUCO"
replace uniqueid=30124 if municipality =="PAPANTLA"
replace uniqueid=30126 if municipality =="PASO DE OVEJAS"
replace uniqueid=30125 if municipality =="PASO DEL MACHO"
replace uniqueid=30128 if municipality =="PEROTE"
replace uniqueid=30129 if municipality =="PLATON SANCHEZ"
replace uniqueid=30130 if municipality =="PLAYA VICENTE"
replace uniqueid=30131 if municipality =="POZA RICA"
replace uniqueid=30133 if municipality =="PUEBLO VIEJO"
replace uniqueid=30134 if municipality =="PUENTE NACIONAL"
replace uniqueid=30135 if municipality =="RAFAEL DELGADO"
replace uniqueid=30136 if municipality =="RAFAEL LUCIO"
replace uniqueid=30138 if municipality =="RIO BLANCO"
replace uniqueid=30139 if municipality =="SALTABARRANCA"
replace uniqueid=30140 if municipality =="SAN ANDRES TENEJAPAN"
replace uniqueid=30141 if municipality =="SAN ANDRES TUXTLA"
replace uniqueid=30142 if municipality =="SAN JUAN EVANGELISTA"
replace uniqueid=30211 if municipality =="SAN RAFAEL"
replace uniqueid=30212 if municipality =="SANTIAGO SOCHIAPA"
replace uniqueid=30143 if municipality =="SANTIAGO TUXTLA"
replace uniqueid=30144 if municipality =="SAYULA DE ALEMAN"
replace uniqueid=30146 if municipality =="SOCHIAPA"
replace uniqueid=30145 if municipality =="SOCONUSCO"
replace uniqueid=30147 if municipality =="SOLEDAD ATZOMPA"
replace uniqueid=30148 if municipality =="SOLEDAD DE DOBLADO"
replace uniqueid=30149 if municipality =="SOTEAPAN"
replace uniqueid=30150 if municipality =="TAMALIN"
replace uniqueid=30151 if municipality =="TAMIAHUA"
replace uniqueid=30152 if municipality =="TAMPICO ALTO"
replace uniqueid=30153 if municipality =="TANCOCO"
replace uniqueid=30154 if municipality =="TANTIMA"
replace uniqueid=30155 if municipality =="TANTOYUCA"
replace uniqueid=30209 if municipality =="TATAHUICAPAN"
replace uniqueid=30156 if municipality =="TATATILA"
replace uniqueid=30158 if municipality =="TECOLUTLA"
replace uniqueid=30159 if municipality =="TEHUIPANGO"
replace uniqueid=30161 if municipality =="TEMPOAL"
replace uniqueid=30162 if municipality =="TENAMPA"
replace uniqueid=30163 if municipality =="TENOCHTITLAN"
replace uniqueid=30164 if municipality =="TEOCELO"
replace uniqueid=30165 if municipality =="TEPATLAXCO"
replace uniqueid=30166 if municipality =="TEPETLAN"
replace uniqueid=30167 if municipality =="TEPETZINTLA"
replace uniqueid=30168 if municipality =="TEQUILA"
replace uniqueid=30170 if municipality =="TEXCATEPEC"
replace uniqueid=30171 if municipality =="TEXHUACAN"
replace uniqueid=30172 if municipality =="TEXISTEPEC"
replace uniqueid=30173 if municipality =="TEZONAPA"
replace uniqueid=30174 if municipality =="TIERRA BLANCA"
replace uniqueid=30175 if municipality =="TIHUATLAN"
replace uniqueid=30180 if municipality =="TLACHICHILCO"
replace uniqueid=30176 if municipality =="TLACOJALPAN"
replace uniqueid=30177 if municipality =="TLACOLULAN"
replace uniqueid=30178 if municipality =="TLACOTALPAN"
replace uniqueid=30179 if municipality =="TLACOTEPEC DE MEJIA"
replace uniqueid=30181 if municipality =="TLALIXCOYAN"
replace uniqueid=30182 if municipality =="TLALNELHUAYOCAN"
replace uniqueid=30024 if municipality =="TLALTETELA"
replace uniqueid=30183 if municipality =="TLAPACOYAN"
replace uniqueid=30184 if municipality =="TLAQUILPAN"
replace uniqueid=30185 if municipality =="TLILAPAN"
replace uniqueid=30186 if municipality =="TOMATLAN"
replace uniqueid=30187 if municipality =="TONAYAN"
replace uniqueid=30188 if municipality =="TOTUTLA"
replace uniqueid=30207 if municipality =="TRES VALLES"
replace uniqueid=30189 if municipality =="TUXPAN"
replace uniqueid=30190 if municipality =="TUXTILLA"
replace uniqueid=30191 if municipality =="URSULO GALVAN"
replace uniqueid=30210 if municipality =="UXPANAPA"
replace uniqueid=30192 if municipality =="VEGA DE ALATORRE"
replace uniqueid=30193 if municipality =="VERACRUZ"
replace uniqueid=30194 if municipality =="VILLA ALDAMA"
replace uniqueid=30087 if municipality =="XALAPA"
replace uniqueid=30092 if municipality =="XICO"
replace uniqueid=30195 if municipality =="XOXOCOTLA"
replace uniqueid=30196 if municipality =="YANGA"
replace uniqueid=30197 if municipality =="YECUATLA"
replace uniqueid=30198 if municipality =="ZACUALPAN"
replace uniqueid=30199 if municipality =="ZARAGOZA"
replace uniqueid=30200 if municipality =="ZENTLA"
replace uniqueid=30201 if municipality =="ZONGOLICA"
replace uniqueid=30202 if municipality =="ZONTECOMATLAN"
replace uniqueid=30203 if municipality =="ZOZOCOLCO DE HIDALGO"


egen valid = rowtotal(PAN PRI_PVEM PRD_PT_PC PRV)

foreach var in PAN PRI_PVEM PRD_PT_PC PRV listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD_PT_PC inv_mun_PRV, gen(PAN_r PRI_PVEM_r PRD_PT_PC_r PRV_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM PRD_PT_PC PRV {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2004
gen month ="September"

sort section

save Veracruz_Section_2004.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Lisado Nominal 2007.xlsx", sheet("Sheet1") firstrow clear

rename SECCIN section
rename LISTA listanominal
keep section listanominal
drop if section=="total Distrital" | section=="" | listanominal==.
destring *, replace

sort section 

save Listanominal2007.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2007_No_LN.csv, clear

rename municipio  municipality
rename secc section
* rename listanominal nominal

drop if municipality=="" & section==.

destring pan - total , replace

collapse (sum)  pan - total , by (municipality section)

drop if total==0 

rename pan PAN
rename pripvem PRI_PVEM
rename pripvempanal  PRI_PVEM_PANAL
rename prd PRD
rename prdptpc PRD_PT_PC
rename ptpc PT_PC
rename pt PT
rename pc PC
rename prv PRV
rename pas PAS
rename panal PANAL
rename mc MC

replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PANAL if municipality=="HUEYAPAN DE OCAMPO"
replace PANAL =0 if municipality=="HUEYAPAN DE OCAMPO"

sort section 
merge section using Listanominal2007.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

drop  nulos  noregistrados 

gen   uniqueid= 0
replace uniqueid=30001 if municipality =="ACAJETE"
replace uniqueid=30002 if municipality =="ACATLAN"
replace uniqueid=30003 if municipality =="ACAYUCAN"
replace uniqueid=30004 if municipality =="ACTOPAN"
replace uniqueid=30005 if municipality =="ACULA"
replace uniqueid=30006 if municipality =="ACULTZINGO"
replace uniqueid=30204 if municipality =="AGUA DULCE"
replace uniqueid=30160 if municipality =="TEMAPACHE"
replace uniqueid=30008 if municipality =="ALPATLAHUAC"
replace uniqueid=30009 if municipality =="ALTO LUCERO DE GUTIERREZ BARRIOS"
replace uniqueid=30010 if municipality =="ALTOTONGA"
replace uniqueid=30011 if municipality =="ALVARADO"
replace uniqueid=30012 if municipality =="AMATITLAN"
replace uniqueid=30014 if municipality =="AMATLAN DE LOS REYES"
replace uniqueid=30015 if municipality =="ANGEL R. CABADA"
replace uniqueid=30017 if municipality =="APAZAPAN"
replace uniqueid=30018 if municipality =="AQUILA"
replace uniqueid=30019 if municipality =="ASTACINGA"
replace uniqueid=30020 if municipality =="ATLAHUICO"
replace uniqueid=30021 if municipality =="ATOYAC"
replace uniqueid=30022 if municipality =="ATZACAN"
replace uniqueid=30023 if municipality =="ATZALAN"
replace uniqueid=30025 if municipality =="AYAHUALULCO"
replace uniqueid=30026 if municipality =="BANDERILLA"
replace uniqueid=30027 if municipality =="BENITO JUAREZ"
replace uniqueid=30028 if municipality =="BOCA DEL RIO"
replace uniqueid=30029 if municipality =="CALCAHUALCO"
replace uniqueid=30007 if municipality =="CAMARON DE TEJEDA"
replace uniqueid=30030 if municipality =="CAMERINO Z. MENDOZA"
replace uniqueid=30208 if municipality =="CARLOS A. CARRILLO"
replace uniqueid=30031 if municipality =="CARRILLO PUERTO"
replace uniqueid=30157 if municipality =="CASTILLO DE TEAYO"
replace uniqueid=30032 if municipality =="CATEMACO"
replace uniqueid=30033 if municipality =="CAZONES"
replace uniqueid=30034 if municipality =="CERRO AZUL"
replace uniqueid=30054 if municipality =="CHACALTIANGUIS"
replace uniqueid=30055 if municipality =="CHALMA"
replace uniqueid=30056 if municipality =="CHICONAMEL"
replace uniqueid=30057 if municipality =="CHICONQUIACO"
replace uniqueid=30058 if municipality =="CHICONTEPEC"
replace uniqueid=30059 if municipality =="CHINAMECA"
replace uniqueid=30060 if municipality =="CHINAMPA DE GOROSTIZA"
replace uniqueid=30062 if municipality =="CHOCAMAN"
replace uniqueid=30063 if municipality =="CHONTLA"
replace uniqueid=30064 if municipality =="CHUMATLAN"
replace uniqueid=30035 if municipality =="CITLALTÉPETL"
replace uniqueid=30036 if municipality =="COACOATZINTLA"
replace uniqueid=30037 if municipality =="COAHUITLAN"
replace uniqueid=30038 if municipality =="COATEPEC"
replace uniqueid=30039 if municipality =="COATZACOALCOS"
replace uniqueid=30040 if municipality =="COATZINTLA"
replace uniqueid=30041 if municipality =="COETZALA"
replace uniqueid=30042 if municipality =="COLIPA"
replace uniqueid=30043 if municipality =="COMAPA"
replace uniqueid=30044 if municipality =="CORDOBA"
replace uniqueid=30045 if municipality =="COSAMALOAPAN"
replace uniqueid=30046 if municipality =="COSAUTLAN DE CARVAJAL"
replace uniqueid=30047 if municipality =="COSCOMATEPEC"
replace uniqueid=30048 if municipality =="COSOLEACAQUE"
replace uniqueid=30049 if municipality =="COTAXTLA"
replace uniqueid=30050 if municipality =="COXQUIHUI"
replace uniqueid=30051 if municipality =="COYUTLA"
replace uniqueid=30052 if municipality =="CUICHAPA"
replace uniqueid=30053 if municipality =="CUITLAHUAC"
replace uniqueid=30205 if municipality =="EL HIGO"
replace uniqueid=30065 if municipality =="EMILIANO ZAPATA"
replace uniqueid=30066 if municipality =="ESPINAL"
replace uniqueid=30067 if municipality =="FILOMENO MATA"
replace uniqueid=30068 if municipality =="FORTIN"
replace uniqueid=30069 if municipality =="GUTIERREZ ZAMORA"
replace uniqueid=30070 if municipality =="HIDALGOTITLAN"
replace uniqueid=30071 if municipality =="HUATUSCO"
replace uniqueid=30072 if municipality =="HUAYACOCOTLA"
replace uniqueid=30073 if municipality =="HUEYAPAN DE OCAMPO"
replace uniqueid=30074 if municipality =="HUILOAPAN DE CUAUHTEMOC"
replace uniqueid=30075 if municipality =="IGNACIO DE LA LLAVE"
replace uniqueid=30076 if municipality =="ILAMATLAN"
replace uniqueid=30077 if municipality =="ISLA"
replace uniqueid=30078 if municipality =="IXCATEPEC"
replace uniqueid=30079 if municipality =="IXHUACÁN DE LOS REYES"
replace uniqueid=30083 if municipality =="IXHUATLAN DE MADERO"
replace uniqueid=30080 if municipality =="IXHUATLAN DEL CAFÉ"
replace uniqueid=30082 if municipality =="IXHUATLAN DEL SURESTE"
replace uniqueid=30081 if municipality =="IXHUATLANCILLO"
replace uniqueid=30084 if municipality =="IXMATLAHUACAN"
replace uniqueid=30085 if municipality =="IXTACZOQUITLAN"
replace uniqueid=30086 if municipality =="JALACINGO"
replace uniqueid=30088 if municipality =="JALCOMULCO"
replace uniqueid=30089 if municipality =="JALTIPAN"
replace uniqueid=30090 if municipality =="JAMAPA"
replace uniqueid=30091 if municipality =="JESUS CARRANZA"
replace uniqueid=30093 if municipality =="JILOTEPEC"
replace uniqueid=30169 if municipality =="JOSE AZUETA"
replace uniqueid=30094 if municipality =="JUAN RODRIGUEZ CLARA"
replace uniqueid=30095 if municipality =="JUCHIQUE DE FERRER"
replace uniqueid=30016 if municipality =="LA ANTIGUA"
replace uniqueid=30127 if municipality =="LA PERLA"
replace uniqueid=30096 if municipality =="LANDERO Y COSS"
replace uniqueid=30061 if municipality =="LAS CHOAPAS"
replace uniqueid=30107 if municipality =="LAS MINAS"
replace uniqueid=30132 if municipality =="LAS VIGAS DE RAMIREZ"
replace uniqueid=30097 if municipality =="LERDO DE TEJADA"
replace uniqueid=30137 if municipality =="LOS REYES"
replace uniqueid=30098 if municipality =="MAGDALENA"
replace uniqueid=30099 if municipality =="MALTRATA"
replace uniqueid=30100 if municipality =="MANLIO FABIO ALTAMIRANO"
replace uniqueid=30101 if municipality =="MARIANO ESCOBEDO"
replace uniqueid=30102 if municipality =="MARTINEZ DE LA TORRE"
replace uniqueid=30103 if municipality =="MECATLAN"
replace uniqueid=30104 if municipality =="MECAYAPAN"
replace uniqueid=30105 if municipality =="MEDELLIN"
replace uniqueid=30106 if municipality =="MIAHUATLAN"
replace uniqueid=30108 if municipality =="MINATITLAN"
replace uniqueid=30109 if municipality =="MISANTLA"
replace uniqueid=30110 if municipality =="MIXTLA DE ALTAMIRANO"
replace uniqueid=30111 if municipality =="MOLOACAN"
replace uniqueid=30206 if municipality =="NANCHITAL"
replace uniqueid=30112 if municipality =="NAOLINCO"
replace uniqueid=30113 if municipality =="NARANJAL"
replace uniqueid=30013 if municipality =="NARANJOS AMATLAN"
replace uniqueid=30114 if municipality =="NAUTLA"
replace uniqueid=30115 if municipality =="NOGALES"
replace uniqueid=30116 if municipality =="OLUTA"
replace uniqueid=30117 if municipality =="OMEALCA"
replace uniqueid=30118 if municipality =="ORIZABA"
replace uniqueid=30119 if municipality =="OTATITLAN"
replace uniqueid=30120 if municipality =="OTEAPAN"
replace uniqueid=30121 if municipality =="OZULUAMA"
replace uniqueid=30122 if municipality =="PAJAPAN"
replace uniqueid=30123 if municipality =="PANUCO"
replace uniqueid=30124 if municipality =="PAPANTLA"
replace uniqueid=30126 if municipality =="PASO DE OVEJAS"
replace uniqueid=30125 if municipality =="PASO DEL MACHO"
replace uniqueid=30128 if municipality =="PEROTE"
replace uniqueid=30129 if municipality =="PLATÓN SÁNCHEZ"
replace uniqueid=30130 if municipality =="PLAYA VICENTE"
replace uniqueid=30131 if municipality =="POZARICA"
replace uniqueid=30133 if municipality =="PUEBLO VIEJO"
replace uniqueid=30134 if municipality =="PUENTE NACIONAL"
replace uniqueid=30135 if municipality =="RAFAEL DELGADO"
replace uniqueid=30136 if municipality =="RAFAEL LUCIO"
replace uniqueid=30138 if municipality =="RIO BLANCO"
replace uniqueid=30139 if municipality =="SALTABARRANCA"
replace uniqueid=30140 if municipality =="SAN ANDRES TENEJAPAN"
replace uniqueid=30141 if municipality =="SAN ANDRES TUXTLA"
replace uniqueid=30142 if municipality =="SAN JUAN EVANGELISTA"
replace uniqueid=30211 if municipality =="SAN RAFAEL"
replace uniqueid=30212 if municipality =="SANTIAGO SOCHIAPAN"
replace uniqueid=30143 if municipality =="SANTIAGO TUXTLA"
replace uniqueid=30144 if municipality =="SAYULA DE ALEMAN"
replace uniqueid=30146 if municipality =="SOCHIAPA"
replace uniqueid=30145 if municipality =="SOCONUSCO"
replace uniqueid=30147 if municipality =="SOLEDAD ATZOMPA"
replace uniqueid=30148 if municipality =="SOLEDAD DE DOBLADO"
replace uniqueid=30149 if municipality =="SOTEAPAN"
replace uniqueid=30150 if municipality =="TAMALIN"
replace uniqueid=30151 if municipality =="TAMIAHUA"
replace uniqueid=30152 if municipality =="TAMPICO ALTO"
replace uniqueid=30153 if municipality =="TANCOCO"
replace uniqueid=30154 if municipality =="TANTIMA"
replace uniqueid=30155 if municipality =="TANTOYUCA"
replace uniqueid=30209 if municipality =="TATAHUICAPAN"
replace uniqueid=30156 if municipality =="TATATILA"
replace uniqueid=30158 if municipality =="TECOLUTLA"
replace uniqueid=30159 if municipality =="TEHUIPANGO"
replace uniqueid=30161 if municipality =="TEMPOAL"
replace uniqueid=30162 if municipality =="TENAMPA"
replace uniqueid=30163 if municipality =="TENOCHTITLAN"
replace uniqueid=30164 if municipality =="TEOCELO"
replace uniqueid=30165 if municipality =="TEPATLAXCO"
replace uniqueid=30166 if municipality =="TEPETLAN"
replace uniqueid=30167 if municipality =="TEPETZINTLA"
replace uniqueid=30168 if municipality =="TEQUILA"
replace uniqueid=30170 if municipality =="TEXCATEPEC"
replace uniqueid=30171 if municipality =="TEXHUACAN"
replace uniqueid=30172 if municipality =="TEXISTEPEC"
replace uniqueid=30173 if municipality =="TEZONAPA"
replace uniqueid=30174 if municipality =="TIERRA BLANCA"
replace uniqueid=30175 if municipality =="TIHUATLAN"
replace uniqueid=30180 if municipality =="TLACHICHILCO"
replace uniqueid=30176 if municipality =="TLACOJALPAN"
replace uniqueid=30177 if municipality =="TLACOLULAN"
replace uniqueid=30178 if municipality =="TLACOTALPAN"
replace uniqueid=30179 if municipality =="TLACOTEPEC DE MEJIA"
replace uniqueid=30181 if municipality =="TLALIXCOYAN"
replace uniqueid=30182 if municipality =="TLALNELHUAYOCAN"
replace uniqueid=30024 if municipality =="TLALTETELA"
replace uniqueid=30183 if municipality =="TLAPACOYAN"
replace uniqueid=30184 if municipality =="TLAQUILPA"
replace uniqueid=30185 if municipality =="TLILAPAN"
replace uniqueid=30186 if municipality =="TOMATLAN"
replace uniqueid=30187 if municipality =="TONAYAN"
replace uniqueid=30188 if municipality =="TOTUTLA"
replace uniqueid=30207 if municipality =="TRES VALLES"
replace uniqueid=30189 if municipality =="TUXPAN"
replace uniqueid=30190 if municipality =="TUXTILLA"
replace uniqueid=30191 if municipality =="URSULO GALVAN"
replace uniqueid=30210 if municipality =="UXPANAPA"
replace uniqueid=30192 if municipality =="VEGA DE ALATORRE"
replace uniqueid=30193 if municipality =="VERACRUZ"
replace uniqueid=30194 if municipality =="VILLA ALDAMA"
replace uniqueid=30087 if municipality =="XALAPA"
replace uniqueid=30092 if municipality =="XICO"
replace uniqueid=30195 if municipality =="XOXOCOTLA"
replace uniqueid=30196 if municipality =="YANGA"
replace uniqueid=30197 if municipality =="YECUATLA"
replace uniqueid=30198 if municipality =="ZACUALPAN"
replace uniqueid=30199 if municipality =="ZARAGOZA"
replace uniqueid=30200 if municipality =="ZENTLA"
replace uniqueid=30201 if municipality =="ZONGOLICA"
replace uniqueid=30202 if municipality =="ZONTECOMATLAN"
replace uniqueid=30203 if municipality =="ZOZOCOLCO"

egen valid = rowtotal(PAN PRI_PVEM_PANAL PRI_PVEM PRD PRD_PT_PC PT_PC PT PC PRV PAS PANAL MC)

foreach var in PAN PRI_PVEM_PANAL PRI_PVEM PRD PRD_PT_PC PT_PC PT PC PRV PAS PANAL MC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM_PANAL inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PRD_PT_PC inv_mun_PT_PC inv_mun_PT inv_mun_PC inv_mun_PRV inv_mun_PAS inv_mun_PANAL inv_mun_MC, gen(PAN_r PRI_PVEM_PANAL_r PRI_PVEM_r PRD_r PRD_PT_PC_r PT_PC_r PT_r PC_r PRV_r PAS_r PANAL_r MC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM_PANAL PRI_PVEM PRD PRD_PT_PC PT_PC PT PC PRV PAS PANAL MC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2007
gen month ="September"

sort section

save Veracruz_Section_2007.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Lisado Nominal 2010.xlsx", sheet("Sheet1") firstrow clear

keep if DISTRITOLOCAL!="" & MUNICIPIO!="" & section!="" & listanominal!="" 
destring *, replace

keep section listanominal

sort section 

save Listanominal2010.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2010_No_LN.csv, clear

rename municipio  municipality
rename secc section
* rename listanominal nominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan - total , replace

collapse (sum)  pan - total , by (municipality section)

gen sec_dummy_pan_panal = 0
gen sec_dummy_pri_pvem_prv = 0
gen sec_dummy_prd_pt_pc = 0

replace sec_dummy_pan_panal = 1 if panpanal>0
replace sec_dummy_pri_pvem_prv = 1 if pripvemprv>0
replace sec_dummy_prd_pt_pc = 1 if prdptpc>0

bys municipality: egen dummy_pan_panal=max(sec_dummy_pan_panal)
bys municipality: egen dummy_pri_pvem_prv=max(sec_dummy_pri_pvem_prv)
bys municipality: egen dummy_prd_pt_pc=max(sec_dummy_prd_pt_pc)
replace dummy_pri_pvem_prv = 1  if municipality=="JALCOMULCO"

drop sec_dummy_pan_panal sec_dummy_pri_pvem_prv sec_dummy_prd_pt_pc

gen pan_panal = pan + panal + panpanal if dummy_pan_panal==1
replace pan = 0 if dummy_pan_panal==1
replace panal = 0  if dummy_pan_panal==1
drop panpanal

gen pri_pvem_prv = pripvemprv + pri + pvem + prv if dummy_pri_pvem_prv==1
replace pri = 0 if dummy_pri_pvem_prv==1
replace pvem = 0 if dummy_pri_pvem_prv==1
replace prv = 0 if dummy_pri_pvem_prv==1
drop pripvemprv

gen prd_pt_pc = prdptpc + prd + pt + pc if dummy_prd_pt_pc == 1
replace prd = 0 if dummy_prd_pt_pc == 1
replace pt = 0 if dummy_prd_pt_pc == 1
replace pc = 0 if dummy_prd_pt_pc == 1
drop prdptpc

sum  pan -  pc  pan_panal -  prd_pt_pc
drop dummy_* pri pvem prv
 
rename pan PAN
rename pan_panal PAN_PANAL
rename pri_pvem_prv PRI_PVEM_PRV
rename prd PRD
rename prd_pt_pc PRD_PT_PC
rename panal PANAL
rename pt PT
rename pc PC

sort section 
merge section using Listanominal2010.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

drop  nulos  noregistrados 

gen   uniqueid= 0
replace uniqueid=30001 if municipality =="ACAJETE"
replace uniqueid=30002 if municipality =="ACATLAN"
replace uniqueid=30003 if municipality =="ACAYUCAN"
replace uniqueid=30004 if municipality =="ACTOPAN"
replace uniqueid=30005 if municipality =="ACULA"
replace uniqueid=30006 if municipality =="ACULTZINGO"
replace uniqueid=30204 if municipality =="AGUA DULCE"
replace uniqueid=30160 if municipality =="ALAMO"
replace uniqueid=30008 if municipality =="ALPATLAHUAC"
replace uniqueid=30009 if municipality =="ALTO LUCERO"
replace uniqueid=30010 if municipality =="ALTOTONGA"
replace uniqueid=30011 if municipality =="ALVARADO"
replace uniqueid=30012 if municipality =="AMATITLAN"
replace uniqueid=30014 if municipality =="AMATLAN DE LOS REYES"
replace uniqueid=30015 if municipality =="ANGEL R. CABADA"
replace uniqueid=30017 if municipality =="APAZAPAN"
replace uniqueid=30018 if municipality =="AQUILA"
replace uniqueid=30019 if municipality =="ASTACINGA"
replace uniqueid=30020 if municipality =="ATLAHUILCO"
replace uniqueid=30021 if municipality =="ATOYAC"
replace uniqueid=30022 if municipality =="ATZACAN"
replace uniqueid=30023 if municipality =="ATZALAN"
replace uniqueid=30025 if municipality =="AYAHUALULCO"
replace uniqueid=30026 if municipality =="BANDERILLA"
replace uniqueid=30027 if municipality =="BENITO JUAREZ"
replace uniqueid=30028 if municipality =="BOCA DEL RIO"
replace uniqueid=30029 if municipality =="CALCAHUALCO"
replace uniqueid=30007 if municipality =="CAMARON DE TEJEDA"
replace uniqueid=30030 if municipality =="CAMERINO Z. MENDOZA"
replace uniqueid=30208 if municipality =="CARLOS A. CARRILLO"
replace uniqueid=30031 if municipality =="CARRILLO PUERTO"
replace uniqueid=30157 if municipality =="CASTILLO DE TEAYO"
replace uniqueid=30032 if municipality =="CATEMACO"
replace uniqueid=30033 if municipality =="CAZONES DE HERRERA"
replace uniqueid=30034 if municipality =="CERRO AZUL"
replace uniqueid=30054 if municipality =="CHACALTIANGUIS"
replace uniqueid=30055 if municipality =="CHALMA"
replace uniqueid=30056 if municipality =="CHICONAMEL"
replace uniqueid=30057 if municipality =="CHICONQUIACO"
replace uniqueid=30058 if municipality =="CHICONTEPEC"
replace uniqueid=30059 if municipality =="CHINAMECA"
replace uniqueid=30060 if municipality =="CHINAMPA DE GOROSTIZA"
replace uniqueid=30062 if municipality =="CHOCAMAN"
replace uniqueid=30063 if municipality =="CHONTLA"
replace uniqueid=30064 if municipality =="CHUMATLAN"
replace uniqueid=30035 if municipality =="CITLALTEPETL"
replace uniqueid=30036 if municipality =="COACOATZINTLA"
replace uniqueid=30037 if municipality =="COAHUITLAN"
replace uniqueid=30038 if municipality =="COATEPEC"
replace uniqueid=30039 if municipality =="COATZACOALCOS"
replace uniqueid=30040 if municipality =="COATZINTLA"
replace uniqueid=30041 if municipality =="COETZALA"
replace uniqueid=30042 if municipality =="COLIPA"
replace uniqueid=30043 if municipality =="COMAPA"
replace uniqueid=30044 if municipality =="CORDOBA"
replace uniqueid=30045 if municipality =="COSAMALOAPAN"
replace uniqueid=30046 if municipality =="COSAUTLAN DE CARVAJAL"
replace uniqueid=30047 if municipality =="COSCOMATEPEC"
replace uniqueid=30048 if municipality =="COSOLEACAQUE"
replace uniqueid=30049 if municipality =="COTAXTLA"
replace uniqueid=30050 if municipality =="COXQUIHUI"
replace uniqueid=30051 if municipality =="COYUTLA"
replace uniqueid=30052 if municipality =="CUICHAPA"
replace uniqueid=30053 if municipality =="CUITLAHUAC"
replace uniqueid=30205 if municipality =="EL HIGO"
replace uniqueid=30065 if municipality =="EMILIANO ZAPATA"
replace uniqueid=30066 if municipality =="ESPINAL"
replace uniqueid=30067 if municipality =="FILOMENO MATA"
replace uniqueid=30068 if municipality =="FORTIN"
replace uniqueid=30069 if municipality =="GUTIERREZ ZAMORA"
replace uniqueid=30070 if municipality =="HIDALGOTITLAN"
replace uniqueid=30071 if municipality =="HUATUSCO"
replace uniqueid=30072 if municipality =="HUAYACOCOTLA"
replace uniqueid=30073 if municipality =="HUEYAPAN DE OCAMPO"
replace uniqueid=30074 if municipality =="HUILOAPAN DE CUAUHTEMOC"
replace uniqueid=30075 if municipality =="IGNACIO DE LA LLAVE"
replace uniqueid=30076 if municipality =="ILAMATLAN"
replace uniqueid=30077 if municipality =="ISLA"
replace uniqueid=30078 if municipality =="IXCATEPEC"
replace uniqueid=30079 if municipality =="IXHUACAN DE LOS REYES"
replace uniqueid=30083 if municipality =="IXHUATLAN DE MADERO"
replace uniqueid=30080 if municipality =="IXHUATLAN DEL CAFE"
replace uniqueid=30082 if municipality =="IXHUATLAN DEL SURESTE"
replace uniqueid=30081 if municipality =="IXHUATLANCILLO"
replace uniqueid=30084 if municipality =="IXMATLAHUACAN"
replace uniqueid=30085 if municipality =="IXTACZOQUITLAN"
replace uniqueid=30086 if municipality =="JALACINGO"
replace uniqueid=30088 if municipality =="JALCOMULCO"
replace uniqueid=30089 if municipality =="JALTIPAN"
replace uniqueid=30090 if municipality =="JAMAPA"
replace uniqueid=30091 if municipality =="JESUS CARRANZA"
replace uniqueid=30093 if municipality =="JILOTEPEC"
replace uniqueid=30169 if municipality =="JOSE AZUETA"
replace uniqueid=30094 if municipality =="JUAN RODRIGUEZ CLARA"
replace uniqueid=30095 if municipality =="JUCHIQUE DE FERRER"
replace uniqueid=30016 if municipality =="LA ANTIGUA"
replace uniqueid=30127 if municipality =="LA PERLA"
replace uniqueid=30096 if municipality =="LANDERO Y COSS"
replace uniqueid=30061 if municipality =="LAS CHOAPAS"
replace uniqueid=30107 if municipality =="LAS MINAS"
replace uniqueid=30132 if municipality =="LAS VIGAS DE RAMIREZ"
replace uniqueid=30097 if municipality =="LERDO DE TEJADA"
replace uniqueid=30137 if municipality =="LOS REYES"
replace uniqueid=30098 if municipality =="MAGDALENA"
replace uniqueid=30099 if municipality =="MALTRATA"
replace uniqueid=30100 if municipality =="MANLIO FABIO ALTAMIRANO"
replace uniqueid=30101 if municipality =="MARIANO ESCOBEDO"
replace uniqueid=30102 if municipality =="MARTINEZ DE LA TORRE"
replace uniqueid=30103 if municipality =="MECATLAN"
replace uniqueid=30104 if municipality =="MECAYAPAN"
replace uniqueid=30105 if municipality =="MEDELLIN"
replace uniqueid=30106 if municipality =="MIAHUATLAN"
replace uniqueid=30108 if municipality =="MINATITLAN"
replace uniqueid=30109 if municipality =="MISANTLA"
replace uniqueid=30110 if municipality =="MIXTLA DE ALTAMIRANO"
replace uniqueid=30111 if municipality =="MOLOACAN"
replace uniqueid=30206 if municipality =="NANCHITAL DE L.C. DEL RIO"
replace uniqueid=30112 if municipality =="NAOLINCO"
replace uniqueid=30113 if municipality =="NARANJAL"
replace uniqueid=30013 if municipality =="NARANJOS AMATLAN"
replace uniqueid=30114 if municipality =="NAUTLA"
replace uniqueid=30115 if municipality =="NOGALES"
replace uniqueid=30116 if municipality =="OLUTA"
replace uniqueid=30117 if municipality =="OMEALCA"
replace uniqueid=30118 if municipality =="ORIZABA"
replace uniqueid=30119 if municipality =="OTATITLAN"
replace uniqueid=30120 if municipality =="OTEAPAN"
replace uniqueid=30121 if municipality =="OZULUAMA"
replace uniqueid=30122 if municipality =="PAJAPAN"
replace uniqueid=30123 if municipality =="PANUCO"
replace uniqueid=30124 if municipality =="PAPANTLA"
replace uniqueid=30126 if municipality =="PASO DE OVEJAS"
replace uniqueid=30125 if municipality =="PASO DEL MACHO"
replace uniqueid=30128 if municipality =="PEROTE"
replace uniqueid=30129 if municipality =="PLATON SANCHEZ"
replace uniqueid=30130 if municipality =="PLAYA VICENTE"
replace uniqueid=30131 if municipality =="POZA RICA"
replace uniqueid=30133 if municipality =="PUEBLO VIEJO"
replace uniqueid=30134 if municipality =="PUENTE NACIONAL"
replace uniqueid=30135 if municipality =="RAFAEL DELGADO"
replace uniqueid=30136 if municipality =="RAFAEL LUCIO"
replace uniqueid=30138 if municipality =="RIO BLANCO"
replace uniqueid=30139 if municipality =="SALTABARRANCA"
replace uniqueid=30140 if municipality =="SAN ANDRES TENEJAPAN"
replace uniqueid=30141 if municipality =="SAN ANDRES TUXTLA"
replace uniqueid=30142 if municipality =="SAN JUAN EVANGELISTA"
replace uniqueid=30211 if municipality =="SAN RAFAEL"
replace uniqueid=30212 if municipality =="SANTIAGO SOCHIAPA"
replace uniqueid=30143 if municipality =="SANTIAGO TUXTLA"
replace uniqueid=30144 if municipality =="SAYULA DE ALEMAN"
replace uniqueid=30146 if municipality =="SOCHIAPA"
replace uniqueid=30145 if municipality =="SOCONUSCO"
replace uniqueid=30147 if municipality =="SOLEDAD ATZOMPA"
replace uniqueid=30148 if municipality =="SOLEDAD DE DOBLADO"
replace uniqueid=30149 if municipality =="SOTEAPAN"
replace uniqueid=30150 if municipality =="TAMALIN"
replace uniqueid=30151 if municipality =="TAMIAHUA"
replace uniqueid=30152 if municipality =="TAMPICO ALTO"
replace uniqueid=30153 if municipality =="TANCOCO"
replace uniqueid=30154 if municipality =="TANTIMA"
replace uniqueid=30155 if municipality =="TANTOYUCA"
replace uniqueid=30209 if municipality =="TATAHUICAPAN"
replace uniqueid=30156 if municipality =="TATATILA"
replace uniqueid=30158 if municipality =="TECOLUTLA"
replace uniqueid=30159 if municipality =="TEHUIPANGO"
replace uniqueid=30161 if municipality =="TEMPOAL"
replace uniqueid=30162 if municipality =="TENAMPA"
replace uniqueid=30163 if municipality =="TENOCHTITLAN"
replace uniqueid=30164 if municipality =="TEOCELO"
replace uniqueid=30165 if municipality =="TEPATLAXCO"
replace uniqueid=30166 if municipality =="TEPETLAN"
replace uniqueid=30167 if municipality =="TEPETZINTLA"
replace uniqueid=30168 if municipality =="TEQUILA"
replace uniqueid=30170 if municipality =="TEXCATEPEC"
replace uniqueid=30171 if municipality =="TEXHUACAN"
replace uniqueid=30172 if municipality =="TEXISTEPEC"
replace uniqueid=30173 if municipality =="TEZONAPA"
replace uniqueid=30174 if municipality =="TIERRA BLANCA"
replace uniqueid=30175 if municipality =="TIHUATLAN"
replace uniqueid=30180 if municipality =="TLACHICHILCO"
replace uniqueid=30176 if municipality =="TLACOJALPAN"
replace uniqueid=30177 if municipality =="TLACOLULAN"
replace uniqueid=30178 if municipality =="TLACOTALPAN"
replace uniqueid=30179 if municipality =="TLACOTEPEC DE MEJIA"
replace uniqueid=30181 if municipality =="TLALIXCOYAN"
replace uniqueid=30182 if municipality =="TLALNELHUAYOCAN"
replace uniqueid=30024 if municipality =="TLALTETELA"
replace uniqueid=30183 if municipality =="TLAPACOYAN"
replace uniqueid=30184 if municipality =="TLAQUILPAN"
replace uniqueid=30185 if municipality =="TLILAPAN"
replace uniqueid=30186 if municipality =="TOMATLAN"
replace uniqueid=30187 if municipality =="TONAYAN"
replace uniqueid=30188 if municipality =="TOTUTLA"
replace uniqueid=30207 if municipality =="TRES VALLES"
replace uniqueid=30189 if municipality =="TUXPAN"
replace uniqueid=30190 if municipality =="TUXTILLA"
replace uniqueid=30191 if municipality =="URSULO GALVAN"
replace uniqueid=30210 if municipality =="UXPANAPA"
replace uniqueid=30192 if municipality =="VEGA DE ALATORRE"
replace uniqueid=30193 if municipality =="VERACRUZ"
replace uniqueid=30194 if municipality =="VILLA ALDAMA"
replace uniqueid=30087 if municipality =="XALAPA"
replace uniqueid=30092 if municipality =="XICO"
replace uniqueid=30195 if municipality =="XOXOCOTLA"
replace uniqueid=30196 if municipality =="YANGA"
replace uniqueid=30197 if municipality =="YECUATLA"
replace uniqueid=30198 if municipality =="ZACUALPAN"
replace uniqueid=30199 if municipality =="ZARAGOZA"
replace uniqueid=30200 if municipality =="ZENTLA"
replace uniqueid=30201 if municipality =="ZONGOLICA"
replace uniqueid=30202 if municipality =="ZONTECOMATLAN"
replace uniqueid=30203 if municipality =="ZOZOCOLCO DE HIDALGO"


egen valid = rowtotal(PAN PANAL PRD PT PC PAN_PANAL PRI_PVEM_PRV PRD_PT_PC)

foreach var in PAN PANAL PRD PT PC PAN_PANAL PRI_PVEM_PRV PRD_PT_PC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PANAL inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_PAN_PANAL inv_mun_PRI_PVEM_PRV inv_mun_PRD_PT_PC, gen(PAN_r PANAL_r PRD_r PT_r PC_r PAN_PANAL_r PRI_PVEM_PRV_r PRD_PT_PC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PANAL PRD PT PC PAN_PANAL PRI_PVEM_PRV PRD_PT_PC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2010
gen month ="July"

sort section

save Veracruz_Section_2010.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

use Ayu_Seccion_2013_No_LN.dta, clear

rename MC PC

* gen turnout =  total/listanominal

drop  NoRegistrados Nulos   

gen   uniqueid= 0
replace uniqueid=30001 if municipality =="ACAJETE"
replace uniqueid=30002 if municipality =="ACATLAN"
replace uniqueid=30003 if municipality =="ACAYUCAN"
replace uniqueid=30004 if municipality =="ACTOPAN"
replace uniqueid=30005 if municipality =="ACULA"
replace uniqueid=30006 if municipality =="ACULTZINGO"
replace uniqueid=30204 if municipality =="AGUA DULCE"
replace uniqueid=30160 if municipality =="ALAMO TEMAPACHE"
replace uniqueid=30008 if municipality =="ALPATLAHUAC"
replace uniqueid=30009 if municipality =="ALTO LUCERO"
replace uniqueid=30010 if municipality =="ALTOTONGA"
replace uniqueid=30011 if municipality =="ALVARADO"
replace uniqueid=30012 if municipality =="AMATITLAN"
replace uniqueid=30014 if municipality =="AMATLAN DE LOS REYES"
replace uniqueid=30015 if municipality =="ANGEL R. CABADA"
replace uniqueid=30017 if municipality =="APAZAPAN"
replace uniqueid=30018 if municipality =="AQUILA"
replace uniqueid=30019 if municipality =="ASTACINGA"
replace uniqueid=30020 if municipality =="ATLAHUILCO"
replace uniqueid=30021 if municipality =="ATOYAC"
replace uniqueid=30022 if municipality =="ATZACAN"
replace uniqueid=30023 if municipality =="ATZALAN"
replace uniqueid=30025 if municipality =="AYAHUALULCO"
replace uniqueid=30026 if municipality =="BANDERILLA"
replace uniqueid=30027 if municipality =="BENITO JUAREZ"
replace uniqueid=30028 if municipality =="BOCA DEL RIO"
replace uniqueid=30029 if municipality =="CALCAHUALCO"
replace uniqueid=30007 if municipality =="CAMARON DE TEJEDA"
replace uniqueid=30030 if municipality =="CAMERINO Z. MENDOZA"
replace uniqueid=30208 if municipality =="CARLOS A. CARRILLO"
replace uniqueid=30031 if municipality =="CARRILLO PUERTO"
replace uniqueid=30157 if municipality =="CASTILLO DE TEAYO"
replace uniqueid=30032 if municipality =="CATEMACO"
replace uniqueid=30033 if municipality =="CAZONES DE HERRERA"
replace uniqueid=30034 if municipality =="CERRO AZUL"
replace uniqueid=30054 if municipality =="CHACALTIANGUIS"
replace uniqueid=30055 if municipality =="CHALMA"
replace uniqueid=30056 if municipality =="CHICONAMEL"
replace uniqueid=30057 if municipality =="CHICONQUIACO"
replace uniqueid=30058 if municipality =="CHICONTEPEC"
replace uniqueid=30059 if municipality =="CHINAMECA"
replace uniqueid=30060 if municipality =="CHINAMPA DE GOROSTIZA"
replace uniqueid=30062 if municipality =="CHOCAMAN"
replace uniqueid=30063 if municipality =="CHONTLA"
replace uniqueid=30064 if municipality =="CHUMATLAN"
replace uniqueid=30035 if municipality =="CITLALTEPEC"
replace uniqueid=30036 if municipality =="COACOATZINTLA"
replace uniqueid=30037 if municipality =="COAHUITLAN"
replace uniqueid=30038 if municipality =="COATEPEC"
replace uniqueid=30039 if municipality =="COATZACOALCOS"
replace uniqueid=30040 if municipality =="COATZINTLA"
replace uniqueid=30041 if municipality =="COETZALA"
replace uniqueid=30042 if municipality =="COLIPA"
replace uniqueid=30043 if municipality =="COMAPA"
replace uniqueid=30044 if municipality =="CORDOBA"
replace uniqueid=30045 if municipality =="COSAMALOAPAN"
replace uniqueid=30046 if municipality =="COSAUTLAN DE CARBAJAL"
replace uniqueid=30047 if municipality =="COSCOMATEPEC"
replace uniqueid=30048 if municipality =="COSOLEACAQUE"
replace uniqueid=30049 if municipality =="COTAXTLA"
replace uniqueid=30050 if municipality =="COXQUIHUI"
replace uniqueid=30051 if municipality =="COYUTLA"
replace uniqueid=30052 if municipality =="CUICHAPA"
replace uniqueid=30053 if municipality =="CUITLAHUAC"
replace uniqueid=30205 if municipality =="EL HIGO"
replace uniqueid=30065 if municipality =="EMILIANO ZAPATA"
replace uniqueid=30066 if municipality =="ESPINAL"
replace uniqueid=30067 if municipality =="FILOMENO MATA"
replace uniqueid=30068 if municipality =="FORTIN"
replace uniqueid=30069 if municipality =="GUTIERREZ ZAMORA"
replace uniqueid=30070 if municipality =="HIDALGOTITLAN"
replace uniqueid=30071 if municipality =="HUATUSCO"
replace uniqueid=30072 if municipality =="HUAYACOCOTLA"
replace uniqueid=30073 if municipality =="HUEYAPAN DE OCAMPO"
replace uniqueid=30074 if municipality =="HUILOAPAN"
replace uniqueid=30075 if municipality =="IGNACIO DE LA LLAVE"
replace uniqueid=30076 if municipality =="ILAMATLAN"
replace uniqueid=30077 if municipality =="ISLA"
replace uniqueid=30078 if municipality =="IXCATEPEC"
replace uniqueid=30079 if municipality =="IXHUACAN DE LOS REYES"
replace uniqueid=30083 if municipality =="IXHUATLAN DE MADERO"
replace uniqueid=30080 if municipality =="IXHUATLAN DEL CAFÉ"
replace uniqueid=30082 if municipality =="IXHUATLAN DEL SURESTE"
replace uniqueid=30081 if municipality =="IXHUATLANCILLO"
replace uniqueid=30084 if municipality =="IXMATLAHUACAN"
replace uniqueid=30085 if municipality =="IXTACZOQUITLAN"
replace uniqueid=30086 if municipality =="JALACINGO"
replace uniqueid=30088 if municipality =="JALCOMULCO"
replace uniqueid=30089 if municipality =="JALTIPAN"
replace uniqueid=30090 if municipality =="JAMAPA"
replace uniqueid=30091 if municipality =="JESUS CARRANZA"
replace uniqueid=30093 if municipality =="JILOTEPEC"
replace uniqueid=30169 if municipality =="JOSE AZUETA"
replace uniqueid=30094 if municipality =="JUAN RODRIGUEZ CLARA"
replace uniqueid=30095 if municipality =="JUCHIQUE DE FERRER"
replace uniqueid=30016 if municipality =="LA ANTIGUA"
replace uniqueid=30127 if municipality =="LA PERLA"
replace uniqueid=30096 if municipality =="LANDERO Y COSS"
replace uniqueid=30061 if municipality =="LAS CHOAPAS"
replace uniqueid=30107 if municipality =="LAS MINAS"
replace uniqueid=30132 if municipality =="LAS VIGAS"
replace uniqueid=30097 if municipality =="LERDO DE TEJADA"
replace uniqueid=30137 if municipality =="LOS REYES"
replace uniqueid=30098 if municipality =="MAGDALENA"
replace uniqueid=30099 if municipality =="MALTRATA"
replace uniqueid=30100 if municipality =="MANLIO FABIO ALTAMIRANO"
replace uniqueid=30101 if municipality =="MARIANO ESCOBEDO"
replace uniqueid=30102 if municipality =="MARTINEZ DE LA TORRE"
replace uniqueid=30103 if municipality =="MECATLAN"
replace uniqueid=30104 if municipality =="MECAYAPAN"
replace uniqueid=30105 if municipality =="MEDELLIN"
replace uniqueid=30106 if municipality =="MIAHUATLAN"
replace uniqueid=30108 if municipality =="MINATITLAN"
replace uniqueid=30109 if municipality =="MISANTLA"
replace uniqueid=30110 if municipality =="MIXTLA DE ALTAMIRANO"
replace uniqueid=30111 if municipality =="MOLOACAN"
replace uniqueid=30206 if municipality =="NANCHITAL DE L. C. DEL R."
replace uniqueid=30112 if municipality =="NAOLINCO"
replace uniqueid=30113 if municipality =="NARANJAL"
replace uniqueid=30013 if municipality =="NARANJOS AMATLAN"
replace uniqueid=30114 if municipality =="NAUTLA"
replace uniqueid=30115 if municipality =="NOGALES"
replace uniqueid=30116 if municipality =="OLUTA"
replace uniqueid=30117 if municipality =="OMEALCA"
replace uniqueid=30118 if municipality =="ORIZABA"
replace uniqueid=30119 if municipality =="OTATITLAN"
replace uniqueid=30120 if municipality =="OTEAPAN"
replace uniqueid=30121 if municipality =="OZULUAMA"
replace uniqueid=30122 if municipality =="PAJAPAN"
replace uniqueid=30123 if municipality =="PANUCO"
replace uniqueid=30124 if municipality =="PAPANTLA"
replace uniqueid=30126 if municipality =="PASO DE OVEJAS"
replace uniqueid=30125 if municipality =="PASO DEL MACHO"
replace uniqueid=30128 if municipality =="PEROTE"
replace uniqueid=30129 if municipality =="PLATON SANCHEZ"
replace uniqueid=30130 if municipality =="PLAYA VICENTE"
replace uniqueid=30131 if municipality =="POZA RICA"
replace uniqueid=30133 if municipality =="PUEBLO VIEJO"
replace uniqueid=30134 if municipality =="PUENTE NACIONAL"
replace uniqueid=30135 if municipality =="RAFAEL DELGADO"
replace uniqueid=30136 if municipality =="RAFAEL LUCIO"
replace uniqueid=30138 if municipality =="RIO BLANCO"
replace uniqueid=30139 if municipality =="SALTABARRANCA"
replace uniqueid=30140 if municipality =="SAN ANDRES TENEJAPAN"
replace uniqueid=30141 if municipality =="SAN ANDRES TUXTLA"
replace uniqueid=30142 if municipality =="SAN JUAN EVANGELISTA"
replace uniqueid=30211 if municipality =="SAN RAFAEL"
replace uniqueid=30212 if municipality =="SANTIAGO SOCHIAPAN"
replace uniqueid=30143 if municipality =="SANTIAGO TUXTLA"
replace uniqueid=30144 if municipality =="SAYULA DE ALEMAN"
replace uniqueid=30146 if municipality =="SOCHIAPA"
replace uniqueid=30145 if municipality =="SOCONUSCO"
replace uniqueid=30147 if municipality =="SOLEDAD ATZOMPA"
replace uniqueid=30148 if municipality =="SOLEDAD DE DOBLADO"
replace uniqueid=30149 if municipality =="SOTEAPAN"
replace uniqueid=30150 if municipality =="TAMALIN"
replace uniqueid=30151 if municipality =="TAMIAHUA"
replace uniqueid=30152 if municipality =="TAMPICO ALTO"
replace uniqueid=30153 if municipality =="TANCOCO"
replace uniqueid=30154 if municipality =="TANTIMA"
replace uniqueid=30155 if municipality =="TANTOYUCA"
replace uniqueid=30209 if municipality =="TATAHUICAPAN"
replace uniqueid=30156 if municipality =="TATATILA"
replace uniqueid=30158 if municipality =="TECOLUTLA"
replace uniqueid=30159 if municipality =="TEHUIPANGO"
replace uniqueid=30161 if municipality =="TEMPOAL"
replace uniqueid=30162 if municipality =="TENAMPA"
replace uniqueid=30163 if municipality =="TENOCHTITLAN"
replace uniqueid=30164 if municipality =="TEOCELO"
replace uniqueid=30165 if municipality =="TEPATLAXCO"
replace uniqueid=30166 if municipality =="TEPETLAN"
replace uniqueid=30167 if municipality =="TEPETZINTLA"
replace uniqueid=30168 if municipality =="TEQUILA"
replace uniqueid=30170 if municipality =="TEXCATEPEC"
replace uniqueid=30171 if municipality =="TEXHUACAN"
replace uniqueid=30172 if municipality =="TEXISTEPEC"
replace uniqueid=30173 if municipality =="TEZONAPA"
replace uniqueid=30174 if municipality =="TIERRA BLANCA"
replace uniqueid=30175 if municipality =="TIHUATLAN"
replace uniqueid=30180 if municipality =="TLACHICHILCO"
replace uniqueid=30176 if municipality =="TLACOJALPAN"
replace uniqueid=30177 if municipality =="TLACOLULAN"
replace uniqueid=30178 if municipality =="TLACOTALPAN"
replace uniqueid=30179 if municipality =="TLACOTEPEC DE MEJIA"
replace uniqueid=30181 if municipality =="TLALIXCOYAN"
replace uniqueid=30182 if municipality =="TLALNELHUAYOCAN"
replace uniqueid=30024 if municipality =="TLALTETELA"
replace uniqueid=30183 if municipality =="TLAPACOYAN"
replace uniqueid=30184 if municipality =="TLAQUILPA"
replace uniqueid=30185 if municipality =="TLILAPAN"
replace uniqueid=30186 if municipality =="TOMATLAN"
replace uniqueid=30187 if municipality =="TONAYAN"
replace uniqueid=30188 if municipality =="TOTUTLA"
replace uniqueid=30207 if municipality =="TRES VALLES"
replace uniqueid=30189 if municipality =="TUXPAN"
replace uniqueid=30190 if municipality =="TUXTILLA"
replace uniqueid=30191 if municipality =="URSULO GALVAN"
replace uniqueid=30210 if municipality =="UXPANAPA"
replace uniqueid=30192 if municipality =="VEGA DE ALATORRE"
replace uniqueid=30193 if municipality =="VERACRUZ"
replace uniqueid=30194 if municipality =="VILLA ALDAMA"
replace uniqueid=30087 if municipality =="XALAPA"
replace uniqueid=30092 if municipality =="XICO"
replace uniqueid=30195 if municipality =="XOXOCOTLA"
replace uniqueid=30196 if municipality =="YANGA"
replace uniqueid=30197 if municipality =="YECUATLA"
replace uniqueid=30198 if municipality =="ZACUALPAN"
replace uniqueid=30199 if municipality =="ZARAGOZA"
replace uniqueid=30200 if municipality =="ZENTLA"
replace uniqueid=30201 if municipality =="ZONGOLICA"
replace uniqueid=30202 if municipality =="ZONTECOMATLAN"
replace uniqueid=30203 if municipality =="ZOZOCOLCO"

foreach var in PAN PRI_PVEM_PANAL PRD PT PC AVE PFCRN total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM_PANAL inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_AVE inv_mun_PFCRN, gen(PAN_r PRI_PVEM_PANAL_r PRD_r PT_r PC_r AVE_r PFCRN_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM_PANAL PRD PT PC AVE PFCRN {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2013
gen month ="July"

sort section

save Veracruz_Section_2013.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Veracruz_Section_2000.dta
append using  Veracruz_Section_2004.dta
append using  Veracruz_Section_2007.dta
append using  Veracruz_Section_2010.dta
append using  Veracruz_Section_2013.dta

erase Veracruz_Section_2000.dta
erase Veracruz_Section_2004.dta
erase Veracruz_Section_2007.dta
erase Veracruz_Section_2010.dta
erase Veracruz_Section_2013.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

gen PC_winner =0
replace PC_winner =1 if strpos(winner, "PC")>0 &  (strpos(winner, "PC")!=strpos(winner, "PCP"))
replace winner_counter = winner_counter + PC_winner

foreach var in PRI PRD PT PCP PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP PEC PFC PAC PJS PST PRV AVE  PFCRN PARM {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Veracruz_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Veracruz_ALL.dta, replace

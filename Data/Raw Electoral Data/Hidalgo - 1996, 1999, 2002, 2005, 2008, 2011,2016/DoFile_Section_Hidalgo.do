
clear all 
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011"

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1996.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal
drop if municipality=="" & section==.

destring listanominal pan -  total, replace

drop if total==. | total==0

collapse (sum) listanominal pan -  total, by (municipality section)
 
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pc     Partido_Cardenista
rename  pt     PT
rename  pvem   PVEM

gen turnout =  total/listanominal

drop  nulos  

gen   uniqueid= 0
replace uniqueid=13001 if municipality =="ACATLAN"
replace uniqueid=13002 if municipality =="ACAXOCHITLAN"
replace uniqueid=13003 if municipality =="ACTOPAN"
replace uniqueid=13004 if municipality =="AGUA BLANCA DE ITURBIDE"
replace uniqueid=13005 if municipality =="AJACUBA"
replace uniqueid=13006 if municipality =="ALFAJAYUCAN"
replace uniqueid=13007 if municipality =="ALMOLOYA"
replace uniqueid=13008 if municipality =="APAN"
replace uniqueid=13010 if municipality =="ATITALAQUIA"
replace uniqueid=13011 if municipality =="ATLAPEXCO"
replace uniqueid=13013 if municipality =="ATOTONILCO DE TULA"
replace uniqueid=13012 if municipality =="ATOTONILCO EL GRANDE"
replace uniqueid=13014 if municipality =="CALNALI"
replace uniqueid=13015 if municipality =="CARDONAL"
replace uniqueid=13017 if municipality =="CHAPANTONGO"
replace uniqueid=13018 if municipality =="CHAPULHUACAN"
replace uniqueid=13019 if municipality =="CHILCUAUTLA"
replace uniqueid=13016 if municipality =="CUAUTEPEC DE HINOJOSA"
replace uniqueid=13009 if municipality =="EL ARENAL"
replace uniqueid=13020 if municipality =="ELOXOCHITLAN"
replace uniqueid=13021 if municipality =="EMILIANO ZAPATA"
replace uniqueid=13022 if municipality =="EPAZOYUCAN"
replace uniqueid=13023 if municipality =="FRANCISCO I. MADERO"
replace uniqueid=13024 if municipality =="HUASCA DE OCAMPO"
replace uniqueid=13025 if municipality =="HUAUTLA"
replace uniqueid=13026 if municipality =="HUAZALINGO"
replace uniqueid=13027 if municipality =="HUEHUETLA"
replace uniqueid=13028 if municipality =="HUEJUTLA DE REYES"
replace uniqueid=13029 if municipality =="HUICHAPAN"
replace uniqueid=13030 if municipality =="IXMIQUILPAN"
replace uniqueid=13031 if municipality =="JACALA DE LEDEZMA"
replace uniqueid=13032 if municipality =="JALTOCAN"
replace uniqueid=13033 if municipality =="JUAREZ HIDALGO"
replace uniqueid=13040 if municipality =="LA MISION"
replace uniqueid=13034 if municipality =="LOLOTLA"
replace uniqueid=13035 if municipality =="METEPEC"
replace uniqueid=13037 if municipality =="METZTITLAN"
replace uniqueid=13051 if municipality =="MINERAL DE LA REFORMA"
replace uniqueid=13038 if municipality =="MINERAL DEL CHICO"
replace uniqueid=13039 if municipality =="MINERAL DEL MONTE"
replace uniqueid=13041 if municipality =="MIXQUIAHUALA DE JUAREZ"
replace uniqueid=13042 if municipality =="MOLANGO DE ESCAMILLA"
replace uniqueid=13043 if municipality =="NICOLAS FLORES"
replace uniqueid=13044 if municipality =="NOPALA DE VILLAGRAN"
replace uniqueid=13045 if municipality =="OMITLAN DE JUAREZ"
replace uniqueid=13048 if municipality =="PACHUCA DE SOTO"
replace uniqueid=13047 if municipality =="PACULA"
replace uniqueid=13049 if municipality =="PISAFLORES"
replace uniqueid=13050 if municipality =="PROGRESO DE OBREGON"
replace uniqueid=13036 if municipality =="SAN AGUSTIN METZQUITITLAN"
replace uniqueid=13052 if municipality =="SAN AGUSTIN TLAXIACA"
replace uniqueid=13053 if municipality =="SAN BARTOLO TUTOTEPEC"
replace uniqueid=13046 if municipality =="SAN FELIPE ORIZATLAN"
replace uniqueid=13054 if municipality =="SAN SALVADOR"
replace uniqueid=13055 if municipality =="SANTIAGO DE ANAYA"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC DE LUGO GU"
replace uniqueid=13057 if municipality =="SINGUILUCAN"
replace uniqueid=13058 if municipality =="TASQUILLO"
replace uniqueid=13059 if municipality =="TECOZAUTLA"
replace uniqueid=13060 if municipality =="TENANGO DE DORIA"
replace uniqueid=13061 if municipality =="TEPEAPULCO"
replace uniqueid=13062 if municipality =="TEPEHUACAN DE GUERRERO"
replace uniqueid=13063 if municipality =="TEPEJI DEL RIO DE OCAMPO"
replace uniqueid=13064 if municipality =="TEPETITLAN"
replace uniqueid=13065 if municipality =="TETEPANGO"
replace uniqueid=13067 if municipality =="TEZONTEPEC DE ALDAMA"
replace uniqueid=13068 if municipality =="TIANGUISTENGO"
replace uniqueid=13069 if municipality =="TIZAYUCA"
replace uniqueid=13070 if municipality =="TLAHUELILPAN"
replace uniqueid=13071 if municipality =="TLAHUILTEPA"
replace uniqueid=13072 if municipality =="TLANALAPA"
replace uniqueid=13073 if municipality =="TLANCHINOL"
replace uniqueid=13074 if municipality =="TLAXCOAPAN"
replace uniqueid=13075 if municipality =="TOLCAYUCA"
replace uniqueid=13076 if municipality =="TULA DE ALLENDE"
replace uniqueid=13077 if municipality =="TULANCINGO DE BRAVO"
replace uniqueid=13066 if municipality =="VILLA DE TEZONTEPEC"
replace uniqueid=13078 if municipality =="XOCHIATIPAN"
replace uniqueid=13079 if municipality =="XOCHICOATLAN"
replace uniqueid=13080 if municipality =="YAHUALICA"
replace uniqueid=13081 if municipality =="ZACUALTIPAN DE ANGELES"
replace uniqueid=13082 if municipality =="ZAPOTLAN DE JUAREZ"
replace uniqueid=13083 if municipality =="ZEMPOALA"
replace uniqueid=13084 if municipality =="ZIMAPAN"

egen valid = rowtotal(PAN PRI Partido_Cardenista PRD PT PVEM)

foreach var in PAN PRI Partido_Cardenista PRD PT PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_Partido_Cardenista inv_mun_PRD inv_mun_PT inv_mun_PVEM, gen(PAN_r PRI_r Partido_Cardenista_r PRD_r PT_r PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "Partido_Cardenista" if Partido_Cardenista_r==1
drop *_r

gen year =1996
gen month ="November"

save Hidalgo_Section_1996.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1999.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal
drop if municipality=="" & section==.

destring listanominal pan -  total, replace

drop if total==. | total==0

collapse (sum) listanominal pan -  total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pt     PT
rename  pvem   PVEM

gen turnout =  total/listanominal

drop  nulos  

gen   uniqueid= 0
replace uniqueid=13001 if municipality =="ACATLAN"
replace uniqueid=13002 if municipality =="ACAXOCHITLAN"
replace uniqueid=13003 if municipality =="ACTOPAN"
replace uniqueid=13004 if municipality =="AGUA BLANCA DE ITURBIDE"
replace uniqueid=13005 if municipality =="AJACUBA"
replace uniqueid=13006 if municipality =="ALFAJAYUCAN"
replace uniqueid=13007 if municipality =="ALMOLOYA"
replace uniqueid=13008 if municipality =="APAN"
replace uniqueid=13010 if municipality =="ATITALAQUIA"
replace uniqueid=13011 if municipality =="ATLAPEXCO"
replace uniqueid=13013 if municipality =="ATOTONILCO DE TULA"
replace uniqueid=13012 if municipality =="ATOTONILCO EL GRANDE"
replace uniqueid=13014 if municipality =="CALNALI"
replace uniqueid=13015 if municipality =="CARDONAL"
replace uniqueid=13017 if municipality =="CHAPANTONGO"
replace uniqueid=13018 if municipality =="CHAPULHUACAN"
replace uniqueid=13019 if municipality =="CHILCUAUTLA"
replace uniqueid=13016 if municipality =="CUAUTEPEC DE HINOJOSA"
replace uniqueid=13009 if municipality =="EL ARENAL"
replace uniqueid=13020 if municipality =="ELOXOCHITLAN"
replace uniqueid=13021 if municipality =="EMILIANO ZAPATA"
replace uniqueid=13022 if municipality =="EPAZOYUCAN"
replace uniqueid=13023 if municipality =="FRANCISCO I. MADERO"
replace uniqueid=13024 if municipality =="HUASCA DE OCAMPO"
replace uniqueid=13025 if municipality =="HUAUTLA"
replace uniqueid=13026 if municipality =="HUAZALINGO"
replace uniqueid=13027 if municipality =="HUEHUETLA"
replace uniqueid=13028 if municipality =="HUEJUTLA DE REYES"
replace uniqueid=13029 if municipality =="HUICHAPAN"
replace uniqueid=13030 if municipality =="IXMIQUILPAN"
replace uniqueid=13031 if municipality =="JACALA DE LEDEZMA"
replace uniqueid=13032 if municipality =="JALTOCAN"
replace uniqueid=13033 if municipality =="JUAREZ HIDALGO"
replace uniqueid=13040 if municipality =="LA MISION"
replace uniqueid=13034 if municipality =="LOLOTLA"
replace uniqueid=13035 if municipality =="METEPEC"
replace uniqueid=13037 if municipality =="METZTITLAN"
replace uniqueid=13051 if municipality =="MINERAL DE LA REFORMA"
replace uniqueid=13038 if municipality =="MINERAL DEL CHICO"
replace uniqueid=13039 if municipality =="MINERAL DEL MONTE"
replace uniqueid=13041 if municipality =="MIXQUIAHUALA DE JUAREZ"
replace uniqueid=13042 if municipality =="MOLANGO DE ESCAMILLA"
replace uniqueid=13043 if municipality =="NICOLAS FLORES"
replace uniqueid=13044 if municipality =="NOPALA DE VILLAGRAN"
replace uniqueid=13045 if municipality =="OMITLAN DE JUAREZ"
replace uniqueid=13048 if municipality =="PACHUCA DE SOTO"
replace uniqueid=13047 if municipality =="PACULA"
replace uniqueid=13049 if municipality =="PISAFLORES"
replace uniqueid=13050 if municipality =="PROGRESO DE OBREGON"
replace uniqueid=13036 if municipality =="SAN AGUSTIN METZQUITITLAN"
replace uniqueid=13052 if municipality =="SAN AGUSTIN TLAXIACA"
replace uniqueid=13053 if municipality =="SAN BARTOLO TUTOTEPEC"
replace uniqueid=13046 if municipality =="SAN FELIPE ORIZATLAN"
replace uniqueid=13054 if municipality =="SAN SALVADOR"
replace uniqueid=13055 if municipality =="SANTIAGO DE ANAYA"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC DE LUGO GU"
replace uniqueid=13057 if municipality =="SINGUILUCAN"
replace uniqueid=13058 if municipality =="TASQUILLO"
replace uniqueid=13059 if municipality =="TECOZAUTLA"
replace uniqueid=13060 if municipality =="TENANGO DE DORIA"
replace uniqueid=13061 if municipality =="TEPEAPULCO"
replace uniqueid=13062 if municipality =="TEPEHUACAN DE GUERRERO"
replace uniqueid=13063 if municipality =="TEPEJI DEL RIO DE OCAMPO"
replace uniqueid=13064 if municipality =="TEPETITLAN"
replace uniqueid=13065 if municipality =="TETEPANGO"
replace uniqueid=13067 if municipality =="TEZONTEPEC DE ALDAMA"
replace uniqueid=13068 if municipality =="TIANGUISTENGO"
replace uniqueid=13069 if municipality =="TIZAYUCA"
replace uniqueid=13070 if municipality =="TLAHUELILPAN"
replace uniqueid=13071 if municipality =="TLAHUILTEPA"
replace uniqueid=13072 if municipality =="TLANALAPA"
replace uniqueid=13073 if municipality =="TLANCHINOL"
replace uniqueid=13074 if municipality =="TLAXCOAPAN"
replace uniqueid=13075 if municipality =="TOLCAYUCA"
replace uniqueid=13076 if municipality =="TULA DE ALLENDE"
replace uniqueid=13077 if municipality =="TULANCINGO DE BRAVO"
replace uniqueid=13066 if municipality =="VILLA DE TEZONTEPEC"
replace uniqueid=13078 if municipality =="XOCHIATIPAN"
replace uniqueid=13079 if municipality =="XOCHICOATLAN"
replace uniqueid=13080 if municipality =="YAHUALICA"
replace uniqueid=13081 if municipality =="ZACUALTIPAN DE ANGELES"
replace uniqueid=13082 if municipality =="ZAPOTLAN DE JUAREZ"
replace uniqueid=13083 if municipality =="ZEMPOALA"
replace uniqueid=13084 if municipality =="ZIMAPAN"

egen valid = rowtotal(PAN PRI PRD PT PVEM)

foreach var in PAN PRI PRD PT PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM, gen(PAN_r PRI_r PRD_r PT_r PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
drop *_r

gen year =1999
gen month ="November"

save Hidalgo_Section_1999.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2002.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal
drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal pan -  pripvem total, replace

collapse (sum) listanominal pan -  pripvem total, by (municipality section)
 
rename  pan     PAN
rename  pri     PRI
rename  prd     PRD
rename  pt      PT
rename  pvem    PVEM
rename  psn     PSN
rename  pc      PC
rename  pas     PAS
rename  pripvem PRI_PVEM

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=13001 if municipality =="ACATLAN"
replace uniqueid=13002 if municipality =="ACAXOCHITLAN"
replace uniqueid=13003 if municipality =="ACTOPAN"
replace uniqueid=13004 if municipality =="AGUA BLANCA DE ITURBIDE"
replace uniqueid=13005 if municipality =="AJACUBA"
replace uniqueid=13006 if municipality =="ALFAJAYUCAN"
replace uniqueid=13007 if municipality =="ALMOLOYA"
replace uniqueid=13008 if municipality =="APAN"
replace uniqueid=13010 if municipality =="ATITALAQUIA"
replace uniqueid=13011 if municipality =="ATLAPEXCO"
replace uniqueid=13013 if municipality =="ATOTONILCO DE TULA"
replace uniqueid=13012 if municipality =="ATOTONILCO EL GRANDE"
replace uniqueid=13014 if municipality =="CALNALI"
replace uniqueid=13015 if municipality =="CARDONAL"
replace uniqueid=13017 if municipality =="CHAPANTONGO"
replace uniqueid=13018 if municipality =="CHAPULHUACAN"
replace uniqueid=13019 if municipality =="CHILCUAUTLA"
replace uniqueid=13016 if municipality =="CUAUTEPEC DE HINOJOSA"
replace uniqueid=13009 if municipality =="EL ARENAL"
replace uniqueid=13020 if municipality =="ELOXOCHITLAN"
replace uniqueid=13021 if municipality =="EMILIANO ZAPATA"
replace uniqueid=13022 if municipality =="EPAZOYUCAN"
replace uniqueid=13023 if municipality =="FRANCISCO I. MADERO"
replace uniqueid=13024 if municipality =="HUASCA DE OCAMPO"
replace uniqueid=13025 if municipality =="HUAUTLA"
replace uniqueid=13026 if municipality =="HUAZALINGO"
replace uniqueid=13027 if municipality =="HUEHUETLA"
replace uniqueid=13028 if municipality =="HUEJUTLA DE REYES"
replace uniqueid=13029 if municipality =="HUICHAPAN"
replace uniqueid=13030 if municipality =="IXMIQUILPAN"
replace uniqueid=13031 if municipality =="JACALA DE LEDEZMA"
replace uniqueid=13032 if municipality =="JALTOCAN"
replace uniqueid=13033 if municipality =="JUAREZ HIDALGO"
replace uniqueid=13040 if municipality =="LA MISION"
replace uniqueid=13034 if municipality =="LOLOTLA"
replace uniqueid=13035 if municipality =="METEPEC"
replace uniqueid=13037 if municipality =="METZTITLAN"
replace uniqueid=13051 if municipality =="MINERAL DE LA REFORMA"
replace uniqueid=13038 if municipality =="MINERAL DEL CHICO"
replace uniqueid=13039 if municipality =="MINERAL DEL MONTE"
replace uniqueid=13041 if municipality =="MIXQUIAHUALA DE JUAREZ"
replace uniqueid=13042 if municipality =="MOLANGO DE ESCAMILLA"
replace uniqueid=13043 if municipality =="NICOLAS FLORES"
replace uniqueid=13044 if municipality =="NOPALA DE VILLAGRAN"
replace uniqueid=13045 if municipality =="OMITLAN DE JUAREZ"
replace uniqueid=13048 if municipality =="PACHUCA DE SOTO"
replace uniqueid=13047 if municipality =="PACULA"
replace uniqueid=13049 if municipality =="PISAFLORES"
replace uniqueid=13050 if municipality =="PROGRESO DE OBREGON"
replace uniqueid=13036 if municipality =="SAN AGUSTIN METZQUITITLAN"
replace uniqueid=13052 if municipality =="SAN AGUSTIN TLAXIACA"
replace uniqueid=13053 if municipality =="SAN BARTOLO TUTOTEPEC"
replace uniqueid=13046 if municipality =="SAN FELIPE ORIZATLAN"
replace uniqueid=13054 if municipality =="SAN SALVADOR"
replace uniqueid=13055 if municipality =="SANTIAGO DE ANAYA"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC DE LUGO GU"
replace uniqueid=13057 if municipality =="SINGUILUCAN"
replace uniqueid=13058 if municipality =="TASQUILLO"
replace uniqueid=13059 if municipality =="TECOZAUTLA"
replace uniqueid=13060 if municipality =="TENANGO DE DORIA"
replace uniqueid=13061 if municipality =="TEPEAPULCO"
replace uniqueid=13062 if municipality =="TEPEHUACAN DE GUERRERO"
replace uniqueid=13063 if municipality =="TEPEJI DEL RIO DE OCAMPO"
replace uniqueid=13064 if municipality =="TEPETITLAN"
replace uniqueid=13065 if municipality =="TETEPANGO"
replace uniqueid=13067 if municipality =="TEZONTEPEC DE ALDAMA"
replace uniqueid=13068 if municipality =="TIANGUISTENGO"
replace uniqueid=13069 if municipality =="TIZAYUCA"
replace uniqueid=13070 if municipality =="TLAHUELILPAN"
replace uniqueid=13071 if municipality =="TLAHUILTEPA"
replace uniqueid=13072 if municipality =="TLANALAPA"
replace uniqueid=13073 if municipality =="TLANCHINOL"
replace uniqueid=13074 if municipality =="TLAXCOAPAN"
replace uniqueid=13075 if municipality =="TOLCAYUCA"
replace uniqueid=13076 if municipality =="TULA DE ALLENDE"
replace uniqueid=13077 if municipality =="TULANCINGO DE BRAVO"
replace uniqueid=13066 if municipality =="VILLA DE TEZONTEPEC"
replace uniqueid=13078 if municipality =="XOCHIATIPAN"
replace uniqueid=13079 if municipality =="XOCHICOATLAN"
replace uniqueid=13080 if municipality =="YAHUALICA"
replace uniqueid=13081 if municipality =="ZACUALTIPAN DE ANGELES"
replace uniqueid=13082 if municipality =="ZAPOTLAN DE JUAREZ"
replace uniqueid=13083 if municipality =="ZEMPOALA"
replace uniqueid=13084 if municipality =="ZIMAPAN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PSN PC PAS PRI_PVEM)

foreach var in PAN PRI PRD PT PVEM PSN PC PAS PRI_PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PSN inv_mun_PC inv_mun_PAS inv_mun_PRI_PVEM, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PSN_r PC_r PAS_r PRI_PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PC" if PC_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PRI_PVEM" if PRI_PVEM_r==1
drop *_r

gen year =2002
gen month ="November"

sort section

save Hidalgo_Section_2002.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2005.csv, clear

rename nombre  municipality
rename casilla section
rename listadonominal listanominal
rename totales total
drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal pan -  total, replace

collapse (sum) listanominal pan -  alternativa total, by (municipality section)
 
rename pan PAN
rename pri PRI
rename prd PRD
rename pt PT
rename pvem PVEM
rename pc PC
rename alternativa PAS

gen turnout =  total/listanominal

gen   uniqueid= .
replace uniqueid=13001 if municipality =="ACATLÁN"
replace uniqueid=13002 if municipality =="ACAXOCHITLÁN"
replace uniqueid=13003 if municipality =="ACTOPAN"
replace uniqueid=13004 if municipality =="AGUA BLANCA DE ITURBIDE"
replace uniqueid=13005 if municipality =="AJACUBA"
replace uniqueid=13006 if municipality =="ALFAJAYUCAN"
replace uniqueid=13007 if municipality =="ALMOLOYA"
replace uniqueid=13008 if municipality =="APAN"
replace uniqueid=13010 if municipality =="ATITALAQUIA"
replace uniqueid=13011 if municipality =="ATLAPEXCO"
replace uniqueid=13013 if municipality =="ATOTONILCO DE TULA"
replace uniqueid=13012 if municipality =="ATOTONILCO EL GRANDE"
replace uniqueid=13014 if municipality =="CALNALI"
replace uniqueid=13015 if municipality =="CARDONAL"
replace uniqueid=13017 if municipality =="CHAPANTONGO"
replace uniqueid=13018 if municipality =="CHAPULHUACÁN"
replace uniqueid=13019 if municipality =="CHILCUAUTLA"
replace uniqueid=13016 if municipality =="CUAUTEPEC DE HINOJOSA"
replace uniqueid=13009 if municipality =="EL ARENAL"
replace uniqueid=13020 if municipality =="ELOXOCHITLÁN"
replace uniqueid=13021 if municipality =="EMILIANO ZAPATA"
replace uniqueid=13022 if municipality =="EPAZOYUCAN"
replace uniqueid=13023 if municipality =="FRANCISCO I. MADERO"
replace uniqueid=13024 if municipality =="HUASCA DE OCAMPO"
replace uniqueid=13025 if municipality =="HUAUTLA"
replace uniqueid=13026 if municipality =="HUAZALINGO"
replace uniqueid=13027 if municipality =="HUEHUETLA"
replace uniqueid=13028 if municipality =="HUEJUTLA DE REYES"
replace uniqueid=13029 if municipality =="HUICHAPAN"
replace uniqueid=13030 if municipality =="IXMIQUILPAN"
replace uniqueid=13031 if municipality =="JACALA DE LEDEZMA"
replace uniqueid=13032 if municipality =="JALTOCAN"
replace uniqueid=13033 if municipality =="JUÁREZ HIDALGO"
replace uniqueid=13040 if municipality =="LA MISIÓN"
replace uniqueid=13034 if municipality =="LOLOTLA"
replace uniqueid=13035 if municipality =="METEPEC"
replace uniqueid=13037 if municipality =="METZTITLÁN"
replace uniqueid=13051 if municipality =="MINERAL DE LA REFORMA"
replace uniqueid=13038 if municipality =="MINERAL DEL CHICO"
replace uniqueid=13039 if municipality =="MINERAL DEL MONTE"
replace uniqueid=13041 if municipality =="MIXQUIAHUALA DE JUÁREZ"
replace uniqueid=13042 if municipality =="MOLANGO DE ESCAMILLA"
replace uniqueid=13043 if municipality =="NICOLÁS FLORES"
replace uniqueid=13044 if municipality =="NOPALA DE VILLAGRÁN"
replace uniqueid=13045 if municipality =="OMITLÁN DE JUÁREZ"
replace uniqueid=13048 if municipality =="PACHUCA DE SOTO"
replace uniqueid=13047 if municipality =="PACULA"
replace uniqueid=13049 if municipality =="PISAFLORES"
replace uniqueid=13050 if municipality =="PROGRESO DE OBREGÓN"
replace uniqueid=13036 if municipality =="SAN AGUSTÍN METZQUITITLÁN"
replace uniqueid=13052 if municipality =="SAN AGUSTÍN TLAXIACA"
replace uniqueid=13053 if municipality =="SAN BARTOLO TUTOTEPEC"
replace uniqueid=13046 if municipality =="SAN FELIPE ORIZATLÁN"
replace uniqueid=13054 if municipality =="SAN SALVADOR"
replace uniqueid=13055 if municipality =="SANTIAGO DE ANAYA"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC DE LUGO GU"
replace uniqueid=13057 if municipality =="SINGUILUCAN"
replace uniqueid=13058 if municipality =="TASQUILLO"
replace uniqueid=13059 if municipality =="TECOZAUTLA"
replace uniqueid=13060 if municipality =="TENANGO DE DORIA"
replace uniqueid=13061 if municipality =="TEPEAPULCO"
replace uniqueid=13062 if municipality =="TEPEHUACÁN DE GUERRERO"
replace uniqueid=13063 if municipality =="TEPEJI DEL RÍO DE OCAMPO"
replace uniqueid=13064 if municipality =="TEPETITLÁN"
replace uniqueid=13065 if municipality =="TETEPANGO"
replace uniqueid=13067 if municipality =="TEZONTEPEC DE ALDAMA"
replace uniqueid=13068 if municipality =="TIANGUISTENGO"
replace uniqueid=13069 if municipality =="TIZAYUCA"
replace uniqueid=13070 if municipality =="TLAHUELILPAN"
replace uniqueid=13071 if municipality =="TLAHUILTEPA"
replace uniqueid=13072 if municipality =="TLANALAPA"
replace uniqueid=13073 if municipality =="TLANCHINOL"
replace uniqueid=13074 if municipality =="TLAXCOAPAN"
replace uniqueid=13075 if municipality =="TOLCAYUCA"
replace uniqueid=13076 if municipality =="TULA DE ALLENDE"
replace uniqueid=13077 if municipality =="TULANCINGO DE BRAVO"
replace uniqueid=13066 if municipality =="VILLA DE TEZONTEPEC"
replace uniqueid=13078 if municipality =="XOCHIATIPAN"
replace uniqueid=13079 if municipality =="XOCHICOATLÁN"
replace uniqueid=13080 if municipality =="YAHUALICA"
replace uniqueid=13081 if municipality =="ZACUALTIPÁN DE ÁNGELES"
replace uniqueid=13082 if municipality =="ZAPOTLÁN DE JUÁREZ"
replace uniqueid=13083 if municipality =="ZEMPOALA"
replace uniqueid=13084 if municipality =="ZIMAPÁN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PAS)

foreach var in PAN PRI PRD PT PVEM PC PAS total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PAS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PAS" if PAS_r==1
drop *_r

gen year =2005
gen month ="November"

sort section

save Hidalgo_Section_2005.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2008.csv, clear

rename nombre  municipality
rename seccion section
rename listadonominal listanominal
drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal pan -  total, replace

collapse (sum) listanominal pan -  prdpt total, by (municipality section)

rename pan PAN
rename pri PRI
rename prd PRD
rename pt PT
rename pv PVEM
rename pc PC
rename psd PSD
rename alianza PANAL
rename prialianza PRI_PANAL
rename prdpt PRD_PT

* EL TRIBUNAL ANULÓ LA ELECCIÓN CONVOCANDO A ELECCION EXTRAORDINARIA EL DIA 5 JULIO 2009
drop if municipality=="HUAZALINGO" | municipality=="ZIMAPAN"
* EL TRIBUNAL DECRETO UN EMPATE CONVOCANDO A ELECCIÓN EXTRAORDINARIA EL DIA 5 JULIO 2009
drop if municipality=="EMILIANO ZAPATA"

gen turnout =  total/listanominal

gen   uniqueid= .
replace uniqueid=13001 if municipality =="ACATLAN"
replace uniqueid=13002 if municipality =="ACAXOCHITLAN"
replace uniqueid=13003 if municipality =="ACTOPAN"
replace uniqueid=13004 if municipality =="AGUA BLANCA DE ITURBIDE"
replace uniqueid=13005 if municipality =="AJACUBA"
replace uniqueid=13006 if municipality =="ALFAJAYUCAN"
replace uniqueid=13007 if municipality =="ALMOLOYA"
replace uniqueid=13008 if municipality =="APAN"
replace uniqueid=13010 if municipality =="ATITALAQUIA"
replace uniqueid=13011 if municipality =="ATLAPEXCO"
replace uniqueid=13013 if municipality =="ATOTONILCO DE TULA"
replace uniqueid=13012 if municipality =="ATOTONILCO EL GRANDE"
replace uniqueid=13014 if municipality =="CALNALI"
replace uniqueid=13015 if municipality =="CARDONAL"
replace uniqueid=13017 if municipality =="CHAPANTONGO"
replace uniqueid=13018 if municipality =="CHAPULHUACAN"
replace uniqueid=13019 if municipality =="CHILCUAUTLA"
replace uniqueid=13016 if municipality =="CUAUTEPEC DE HINOJOSA"
replace uniqueid=13009 if municipality =="EL ARENAL"
replace uniqueid=13020 if municipality =="ELOXOCHITLAN"
replace uniqueid=13021 if municipality =="EMILIANO ZAPATA"
replace uniqueid=13022 if municipality =="EPAZOYUCAN"
replace uniqueid=13023 if municipality =="FRANCISCO I. MADERO"
replace uniqueid=13024 if municipality =="HUASCA DE OCAMPO"
replace uniqueid=13025 if municipality =="HUAUTLA"
replace uniqueid=13026 if municipality =="HUAZALINGO"
replace uniqueid=13027 if municipality =="HUEHUETLA"
replace uniqueid=13028 if municipality =="HUEJUTLA DE REYES"
replace uniqueid=13029 if municipality =="HUICHAPAN"
replace uniqueid=13030 if municipality =="IXMIQUILPAN"
replace uniqueid=13031 if municipality =="JACALA DE LEDEZMA"
replace uniqueid=13032 if municipality =="JALTOCAN"
replace uniqueid=13033 if municipality =="JUAREZ HIDALGO"
replace uniqueid=13040 if municipality =="LA MISION"
replace uniqueid=13034 if municipality =="LOLOTLA"
replace uniqueid=13035 if municipality =="METEPEC"
replace uniqueid=13037 if municipality =="METZTITLAN"
replace uniqueid=13051 if municipality =="MINERAL DE LA REFORMA"
replace uniqueid=13038 if municipality =="MINERAL DEL CHICO"
replace uniqueid=13039 if municipality =="MINERAL DEL MONTE"
replace uniqueid=13041 if municipality =="MIXQUIAHUALA DE JUAREZ"
replace uniqueid=13042 if municipality =="MOLANGO DE ESCAMILLA"
replace uniqueid=13043 if municipality =="NICOLAS DE FLORES"
replace uniqueid=13044 if municipality =="NOPALA DE VILLAGRAN"
replace uniqueid=13045 if municipality =="OMITLAN DE JUAREZ"
replace uniqueid=13048 if municipality =="PACHUCA DE SOTO"
replace uniqueid=13047 if municipality =="PACULA"
replace uniqueid=13049 if municipality =="PISAFLORES"
replace uniqueid=13050 if municipality =="PROGRESO DE OBREGON"
replace uniqueid=13036 if municipality =="SAN AGUSTIN METZQUITITLAN"
replace uniqueid=13052 if municipality =="SAN AGUSTIN TLAXIACA"
replace uniqueid=13053 if municipality =="SAN BARTOLO TUTOTEPEC"
replace uniqueid=13046 if municipality =="SAN FELIPE ORIZATLAN"
replace uniqueid=13054 if municipality =="SAN SALVADOR"
replace uniqueid=13055 if municipality =="SANTIAGO DE ANAYA"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC DE LUGO GU"
replace uniqueid=13057 if municipality =="SINGUILUCAN"
replace uniqueid=13058 if municipality =="TASQUILLO"
replace uniqueid=13059 if municipality =="TECOZAUTLA"
replace uniqueid=13060 if municipality =="TENANGO DE DORIA"
replace uniqueid=13061 if municipality =="TEPEAPULCO"
replace uniqueid=13062 if municipality =="TEPEHUACAN DE GUERRERO"
replace uniqueid=13063 if municipality =="TEPEJI DEL RIO DE OCAMPO"
replace uniqueid=13064 if municipality =="TEPETITLAN"
replace uniqueid=13065 if municipality =="TETEPANGO"
replace uniqueid=13067 if municipality =="TEZONTEPEC DE ALDAMA"
replace uniqueid=13068 if municipality =="TIANGUISTENGO"
replace uniqueid=13069 if municipality =="TIZAYUCA"
replace uniqueid=13070 if municipality =="TLAHUELILPAN"
replace uniqueid=13071 if municipality =="TLAHUILTEPA"
replace uniqueid=13072 if municipality =="TLANALAPA"
replace uniqueid=13073 if municipality =="TLANCHINOL"
replace uniqueid=13074 if municipality =="TLAXCOAPAN"
replace uniqueid=13075 if municipality =="TOLCAYUCA"
replace uniqueid=13076 if municipality =="TULA DE ALLENDE"
replace uniqueid=13077 if municipality =="TULANCINGO DE BRAVO"
replace uniqueid=13066 if municipality =="VILLA DE TEZONTEPEC"
replace uniqueid=13078 if municipality =="XOCHIATIPAN"
replace uniqueid=13079 if municipality =="XOCHICOATLAN"
replace uniqueid=13080 if municipality =="YAHUALICA"
replace uniqueid=13081 if municipality =="ZACUALTIPAN DE ANGELES"
replace uniqueid=13082 if municipality =="ZAPOTLAN DE JUAREZ"
replace uniqueid=13083 if municipality =="ZEMPOALA"
replace uniqueid=13084 if municipality =="ZIMAPAN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSD PANAL PRI_PANAL PRD_PT)

foreach var in PAN PRI PRD PT PVEM PC PSD PANAL PRI_PANAL PRD_PT total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSD inv_mun_PANAL inv_mun_PRI_PANAL inv_mun_PRD_PT, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PSD_r PANAL_r PRI_PANAL_r PRD_PT_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1

replace winner = "PSD" if PSD_r==1
replace winner = "PANAL" if PANAL_r==1
replace winner = "PRI_PANAL" if PRI_PANAL_r==1
replace winner = "PRD_PT" if PRD_PT_r==1
drop *_r

gen year =2008
gen month ="November"

sort section

save Hidalgo_Section_2008.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "Ayu_Seccion_2011.xlsx", sheet("Ayu_Seccion_2011") firstrow clear

rename NOMBRE  municipality

drop if CASILLAS=="CASILLAS"
drop if LISTANOMINAL=="NOMINAL"
drop if IdMun==""
replace CASILLAS= subinstr(CASILLAS, "B", "", .)
forvalues i = 1(1)10 {
replace CASILLAS= subinstr(CASILLAS, "C`i'", "", .)
replace CASILLAS= subinstr(CASILLAS, "X`i'", "", .)
}
destring CASILLAS, replace
rename CASILLAS section

rename LISTANOMINAL listanominal
rename TOTAL total

foreach var in PAN PRI PRD PT PVEM PC PANAL PAN_PRD PT_PC PT_PC PRI_PVEM  {
replace `var'= subinstr(`var', "N/R", "", .)
}

drop if strpos(PC, "CASILLA ANULADA")>0
drop if strpos(PVEM, "CASILLA ANULADA")>0
destring *, replace

drop if municipality=="" & section==.
drop if total==. | total==0

collapse (sum) listanominal - PRI_PVEM total, by (municipality section)

* ANULADA LA ELECCION POR EL TEPJF, CONVOCANDO A ELECCIÓN EXTRAORDINARIA EL DIA 18 MARZO 2012
drop if municipality=="SANTIAGO TULANTEPEC"  | municipality=="XOCHICOATLAN"

gen turnout =  total/listanominal

gen   uniqueid= .
replace uniqueid=13001 if municipality =="ACATLAN"
replace uniqueid=13002 if municipality =="ACAXOCHITLAN"
replace uniqueid=13003 if municipality =="ACTOPAN"
replace uniqueid=13004 if municipality =="AGUA BLANCA DE ITURBIDE"
replace uniqueid=13005 if municipality =="AJACUBA"
replace uniqueid=13006 if municipality =="ALFAJAYUCAN"
replace uniqueid=13007 if municipality =="ALMOLOYA"
replace uniqueid=13008 if municipality =="APAN"
replace uniqueid=13010 if municipality =="ATITALAQUIA"
replace uniqueid=13011 if municipality =="ATLAPEXCO"
replace uniqueid=13013 if municipality =="ATOTONILCO DE TULA"
replace uniqueid=13012 if municipality =="ATOTONILCO EL GRANDE"
replace uniqueid=13014 if municipality =="CALNALI"
replace uniqueid=13015 if municipality =="CARDONAL"
replace uniqueid=13017 if municipality =="CHAPANTONGO"
replace uniqueid=13018 if municipality =="CHAPULHUACAN"
replace uniqueid=13019 if municipality =="CHILCUAUTLA"
replace uniqueid=13016 if municipality =="CUAUTEPEC DE HINOJOS"
replace uniqueid=13009 if municipality =="EL ARENAL"
replace uniqueid=13020 if municipality =="ELOXOCHITLAN"
replace uniqueid=13021 if municipality =="EMILIANO ZAPATA"
replace uniqueid=13022 if municipality =="EPAZOYUCAN"
replace uniqueid=13023 if municipality =="FRANCISCO I. MADERO"
replace uniqueid=13024 if municipality =="HUASCA DE OCAMPO"
replace uniqueid=13025 if municipality =="HUAUTLA"
replace uniqueid=13026 if municipality =="HUAZALINGO"
replace uniqueid=13027 if municipality =="HUEHUETLA"
replace uniqueid=13028 if municipality =="HUEJUTLA DE REYES"
replace uniqueid=13029 if municipality =="HUICHAPAN"
replace uniqueid=13030 if municipality =="IXMIQUILPAN"
replace uniqueid=13031 if municipality =="JACALA DE LEDEZMA"
replace uniqueid=13032 if municipality =="JALTOCAN"
replace uniqueid=13033 if municipality =="JUAREZ HIDALGO"
replace uniqueid=13040 if municipality =="LA MISION"
replace uniqueid=13034 if municipality =="LOLOTLA"
replace uniqueid=13035 if municipality =="METEPEC"
replace uniqueid=13037 if municipality =="METZTITLAN"
replace uniqueid=13051 if municipality =="MINERAL DE LA REFORM"
replace uniqueid=13038 if municipality =="MINERAL DEL CHICO"
replace uniqueid=13039 if municipality =="MINERAL DEL MONTE"
replace uniqueid=13041 if municipality =="MIXQUIAHUALA DE JUAR"
replace uniqueid=13042 if municipality =="MOLANGO DE ESCAMILLA"
replace uniqueid=13043 if municipality =="NICOLAS DE FLORES"
replace uniqueid=13043 if municipality =="NICOLAS  FLORES"
replace uniqueid=13044 if municipality =="NOPALA DE VILLAGRAN"
replace uniqueid=13045 if municipality =="OMITLAN DE JUAREZ"
replace uniqueid=13048 if municipality =="PACHUCA DE SOTO"
replace uniqueid=13047 if municipality =="PACULA"
replace uniqueid=13049 if municipality =="PISAFLORES"
replace uniqueid=13050 if municipality =="PROGRESO DE OBREGON"
replace uniqueid=13036 if municipality =="SAN AGUSTIN METZQUITITLAN"
replace uniqueid=13052 if municipality =="SAN AGUSTIN TLAXIACA"
replace uniqueid=13053 if municipality =="SAN BARTOLO TUTOTEPEC"
replace uniqueid=13046 if municipality =="SAN FELIPE ORIZATLAN"
replace uniqueid=13054 if municipality =="SAN SALVADOR"
replace uniqueid=13055 if municipality =="SANTIAGO DE ANAYA"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC DE LUGO GU"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC"
replace uniqueid=13057 if municipality =="SINGUILUCAN"
replace uniqueid=13058 if municipality =="TASQUILLO"
replace uniqueid=13059 if municipality =="TECOZAUTLA"
replace uniqueid=13060 if municipality =="TENANGO DE DORIA"
replace uniqueid=13061 if municipality =="TEPEAPULCO"
replace uniqueid=13062 if municipality =="TEPEHUACAN DE GUERRERO"
replace uniqueid=13062 if municipality =="TEPEHUACAN DE GUERR"
replace uniqueid=13063 if municipality =="TEPEJI DEL RIO DE OCAMPO"
replace uniqueid=13064 if municipality =="TEPETITLAN"
replace uniqueid=13065 if municipality =="TETEPANGO"
replace uniqueid=13067 if municipality =="TEZONTEPEC DE ALDAMA"
replace uniqueid=13068 if municipality =="TIANGUISTENGO"
replace uniqueid=13069 if municipality =="TIZAYUCA"
replace uniqueid=13070 if municipality =="TLAHUELILPAN"
replace uniqueid=13071 if municipality =="TLAHUILTEPA"
replace uniqueid=13072 if municipality =="TLANALAPA"
replace uniqueid=13073 if municipality =="TLANCHINOL"
replace uniqueid=13074 if municipality =="TLAXCOAPAN"
replace uniqueid=13075 if municipality =="TOLCAYUCA"
replace uniqueid=13076 if municipality =="TULA DE ALLENDE"
replace uniqueid=13077 if municipality =="TULANCINGO DE BRAVO"
replace uniqueid=13066 if municipality =="VILLA DE TEZONTEPEC"
replace uniqueid=13078 if municipality =="XOCHIATIPAN"
replace uniqueid=13079 if municipality =="XOCHICOATLAN"
replace uniqueid=13080 if municipality =="YAHUALICA"
replace uniqueid=13081 if municipality =="ZACUALTIPAN DE ANGEL"
replace uniqueid=13082 if municipality =="ZAPOTLAN DE JUAREZ"
replace uniqueid=13083 if municipality =="ZEMPOALA"
replace uniqueid=13084 if municipality =="ZIMAPAN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAN_PRD PT_PC PRI_PVEM)

foreach var in PAN PRI PRD PT PVEM PC PANAL PAN_PRD PT_PC PRI_PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAN_PRD inv_mun_PT_PC inv_mun_PRI_PVEM, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PAN_PRD_r PT_PC_r PRI_PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PANAL" if PANAL_r==1
replace winner = "PAN_PRD" if PAN_PRD_r==1
replace winner = "PT_PC" if PT_PC_r==1
replace winner = "PRI_PVEM" if PRI_PVEM_r==1
drop *_r

gen year =2011
gen month ="Julio"

sort section

save Hidalgo_Section_2011.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Hidalgo_Section_1996.dta, clear
append using Hidalgo_Section_1999.dta
append using Hidalgo_Section_2002.dta
append using Hidalgo_Section_2005.dta
append using Hidalgo_Section_2008.dta
append using Hidalgo_Section_2011.dta

erase Hidalgo_Section_1996.dta
erase Hidalgo_Section_1999.dta
erase Hidalgo_Section_2002.dta
erase Hidalgo_Section_2005.dta
erase Hidalgo_Section_2008.dta
erase Hidalgo_Section_2011.dta

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS Partido_Cardenista {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0
tab winner winner_counter

save Hidalgo_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"

save Hidalgo_ALL.dta, replace

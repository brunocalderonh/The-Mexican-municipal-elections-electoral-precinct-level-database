

replace section_merging = 352 if section_merging >= 1332 & section_merging <= 1361 &  year <=2003
replace inegi_code= 26029 if  inegi_code==26072 &  year==1994 & section_merging>=1101 & section_merging<=1108
replace inegi_code= 26026 if  inegi_code==26071 &  year==1994 & (section_merging== 1142 | (section_merging>=1146 & section_merging<=1152)  | (section_merging>=1155 & section_merging<=1157))

* 119	BAJA POR INTEGRACION SECCIONAL 2008

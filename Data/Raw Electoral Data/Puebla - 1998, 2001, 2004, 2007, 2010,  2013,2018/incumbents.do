clear 
cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Puebla"

use Puebla_Section_2013.dta

collapse (first) winner, by (municipality uniqueid)
rename winner incumbent
save incumbents2018a.dta, replace 

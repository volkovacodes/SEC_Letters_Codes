use "C:\Users\ev99\Dropbox\ipo_20170529.dta", clear
label var log_n_letters "Log(# Letters)"
label var log_words "Log(#Words)"
label var log_sec_load_letter "Log(SEC Load), #Letters"
label var log_sec_load_words "Log(SEC Load), #Words"
label var log_person_load_letter "Log(Personal Load, #Letters"
label var log_person_load_words "Log(Personal Load), #Words"
label var log_sale "Log(Sales)"
label var n_segments "Number Segments"
label var age "Log(1 + Age)"
label var UW_rank "UW Rank"
label var law_rank "Law Firm Rank"
label var VC "VC Dummy"
label var JOBS "JOBS Act Dummy"
label var log_S1_words "S1 Length"
label var S1_un "S1 Uncertainty"

encode letter_author, generate(personid)
gen tmp = 1
egen personcount = total(tmp), by(personid)
drop if personcount < 10
local control log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un i.FF_48 i.Year i.Office  
local iv_type1 i.personid
local iv_type2 log_sec_load_letter  i.personid
local iv_type3 log_sec_load_words i.personid
local iv_type4 log_person_load_letter i.personid
local iv_type5 log_person_load_words i.personid 
*** TABLE 1
xi: reg log_n_letters log_sec_load_letters i.personid `control', robust cluster(FF_48)
est store a1

xi: reg log_n_letters log_sec_load_words i.personid `control', robust cluster(FF_48)
est store a2

xi: reg log_n_letters log_person_load_letters i.personid `control', robust cluster(FF_48)
est store a3

xi: reg log_n_letters log_person_load_words i.personid `control', robust cluster(FF_48)
est store a4

esttab a1 a2 a3 a4 using "C:\Users\ev99\Dropbox\test.rtf", replace ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 1: Number of SEC Letters and SEC Load") ///
	order(log_sec_load_letters log_sec_load_words log_person_load_letters log_person_load_words log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *personid* *Office*) ///
	mtitle("Log(#Letters)" "Log(#Letters)" "Log(#Letters)" "Log(#Letters)" ) ///
	addnote("Industry and year FE, robust clustered by industry std errs") 

*** Table 2
xi: reg log_words log_sec_load_letters `control', robust cluster(FF_48)
est store b1

xi: reg log_words log_sec_load_words `control', robust cluster(FF_48)
est store b2

xi: reg log_words log_person_load_letters `control', robust cluster(FF_48)
est store b3

xi: reg log_words log_person_load_words `control', robust cluster(FF_48)
est store b4

esttab b1 b2 b3 b4 using "C:\Users\ev99\Dropbox\test.rtf", append ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 2: Number of Words in the First SEC Letter and SEC Load") ///
	order(log_sec_load_letters log_sec_load_words log_person_load_letters log_person_load_words log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *Office*) ///
	mtitle("Log(#Words)" "Log(#Words)" "Log(#Words)" "Log(#Words)" ) ///
	addnote("Industry and year FE, robust clustered by industry std errs") 

*** Table 3
xi: reg registration_length log_sec_load_letters `control', robust cluster(FF_48)
est store c1

xi: reg registration_length log_sec_load_words `control', robust cluster(FF_48)
est store c2

xi: reg registration_length log_person_load_letters `control', robust cluster(FF_48)
est store c3

xi: reg registration_length log_person_load_words `control', robust cluster(FF_48)
est store c4

esttab c1 c2 c3 c4 using "C:\Users\ev99\Dropbox\test.rtf", append ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 3: Registration Length and SEC Load") ///
	order(log_sec_load_letters log_sec_load_words log_person_load_letters log_person_load_words log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *Office*) ///
	mtitle("Reg. Len." "Reg. Len." "Reg. Len." "Reg. Len" ) ///
	addnote("Industry and year FE, robust clustered by industry std errs")
	
*** Table 4
xi: reg registration_length log_n_letters `control', robust cluster(FF_48)
est store d1

ivregress 2sls registration_length `control' (log_n_letters = `iv_type1'), robust cluster(FF_48)
est store d2

ivregress 2sls registration_length `control' (log_n_letters = `iv_type2'), robust cluster(FF_48)
est store d3

ivregress 2sls registration_length `control' (log_n_letters = `iv_type3'), robust cluster(FF_48)
est store d4

ivregress 2sls registration_length `control' (log_n_letters = `iv_type4'), robust cluster(FF_48)
est store d5

ivregress 2sls registration_length `control' (log_n_letters = `iv_type5'), robust cluster(FF_48)
est store d6

esttab d1 d2 d3 d4 d5 d6 using "C:\Users\ev99\Dropbox\test.rtf", append ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 4: Registration Length and SEC Letters. OLS vs 2SLS") ///
	order(log_n_letters log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *Office*) ///
	mtitle("Reg. Len. (OLS)" "Reg. Len.(2SLS)" "Reg. Len. (2SLS)" "Reg. Len. (2SLS)" "Reg. Len.(2SLS)" "Reg. Len.(2SLS)") ///
	addnote("Industry and year FE, robust clustered by industry std errs")
	
*** Table 5
xi: reg price_update log_n_letters `control', robust cluster(FF_48)
est store e1

ivregress 2sls price_update `control' (log_n_letters = `iv_type1'), robust cluster(FF_48)
est store e2

ivregress 2sls price_update `control' (log_n_letters = `iv_type2'), robust cluster(FF_48)
est store e3

ivregress 2sls price_update `control' (log_n_letters = `iv_type3'), robust cluster(FF_48)
est store e4

ivregress 2sls price_update `control' (log_n_letters = `iv_type4'), robust cluster(FF_48)
est store e5

ivregress 2sls price_update `control' (log_n_letters = `iv_type5'), robust cluster(FF_48)
est store e6

esttab e1 e2 e3 e4 e5 e6 using "C:\Users\ev99\Dropbox\test.rtf", append ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 5: Price Update and SEC Letters. OLS vs 2SLS") ///
	order(log_n_letters log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *Office*) ///
	mtitle("Prc. Upd.(OLS)" "Prc. Upd.(2SLS)" "Prc. Upd.(2SLS)" "Prc. Upd.(2SLS)" "Prc. Upd.(2SLS)" "Prc. Upd.(2SLS)") ///
	addnote("Industry and year FE, robust clustered by industry std errs")
	
*** Table 6
xi: reg IR log_n_letters `control', robust cluster(FF_48)
est store f1

ivregress 2sls IR `control' (log_n_letters = `iv_type1'), robust cluster(FF_48)
est store f2

ivregress 2sls IR `control' (log_n_letters = `iv_type2'), robust cluster(FF_48)
est store f3

ivregress 2sls IR `control' (log_n_letters = `iv_type3'), robust cluster(FF_48)
est store f4

ivregress 2sls IR `control' (log_n_letters = `iv_type4'), robust cluster(FF_48)
est store f5

ivregress 2sls IR `control' (log_n_letters = `iv_type5'), robust cluster(FF_48)
est store f6

esttab f1 f2 f3 f4 f5 f6 using "C:\Users\ev99\Dropbox\test.rtf", append ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 6: Initial Returns and SEC Letters. OLS vs 2SLS") ///
	order(log_n_letters log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *Office*) ///
	mtitle("IR(OLS)" "IR(2SLS)" "IR(2SLS)" "IR(2SLS)" "IR (2SLS)" "IR (2SLS)") ///
	addnote("Industry and year FE, robust clustered by industry std errs")

	
*** Table 7
xi: reg vol log_n_letters `control', robust cluster(FF_48)
est store g1

ivregress 2sls vol `control' (log_n_letters = `iv_type1'), robust cluster(FF_48)
est store g2

ivregress 2sls vol `control' (log_n_letters = `iv_type2'), robust cluster(FF_48)
est store g3

ivregress 2sls vol `control' (log_n_letters = `iv_type3'), robust cluster(FF_48)
est store g4

ivregress 2sls vol `control' (log_n_letters = `iv_type4'), robust cluster(FF_48)
est store g5

ivregress 2sls vol `control' (log_n_letters = `iv_type5'), robust cluster(FF_48)
est store g6

esttab g1 g2 g3 g4 g5 g6 using "C:\Users\ev99\Dropbox\test.rtf", append ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 7: Volatility and SEC Letters. OLS vs 2SLS") ///
	order(log_n_letters log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *Office*) ///
	mtitle("Vol.(OLS)" "Vol.(2SLS)" "Vol.(2SLS)" "Vol.(2SLS)" "Vol.(2SLS)" "Vol.(2SLS)" ) ///
	addnote("Industry and year FE, robust clustered by industry std errs")
	
*** Table 8
xi: reg insider_sales log_n_letters `control', robust cluster(FF_48)
est store h1

ivregress 2sls insider_sales `control' (log_n_letters = `iv_type1'), robust cluster(FF_48)
est store h2

ivregress 2sls insider_sales `control' (log_n_letters = `iv_type2'), robust cluster(FF_48)
est store h3

ivregress 2sls insider_sales `control' (log_n_letters = `iv_type3'), robust cluster(FF_48)
est store h4

ivregress 2sls insider_sales `control' (log_n_letters = `iv_type4'), robust cluster(FF_48)
est store h5

ivregress 2sls insider_sales `control' (log_n_letters = `iv_type5'), robust cluster(FF_48)
est store h6

esttab h1 h2 h3 h4 h5 h6 using "C:\Users\ev99\Dropbox\test.rtf", append ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 8: Insider Sales and SEC Letters. OLS vs 2SLS") ///
	order(log_n_letters log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *Office*) ///
	mtitle("Ins. Sales(OLS)" "Ins. Sales(2SLS)" "Ins. Sales(2SLS)" "Ins. Sales(2SLS)" "Ins. Sales(2SLS)" "Ins. Sales(2SLS)" ) ///
	addnote("Industry and year FE, robust clustered by industry std errs")
	
*** Table 9
xi: reg ret_abn_365 log_n_letters `control', robust cluster(FF_48)
est store i1

ivregress 2sls ret_abn_365 `control' (log_n_letters = `iv_type1'), robust cluster(FF_48)
est store i2

ivregress 2sls ret_abn_365 `control' (log_n_letters = `iv_type2'), robust cluster(FF_48)
est store i3

ivregress 2sls ret_abn_365 `control' (log_n_letters = `iv_type3'), robust cluster(FF_48)
est store i4

ivregress 2sls ret_abn_365 `control' (log_n_letters = `iv_type4'), robust cluster(FF_48)
est store i5

ivregress 2sls ret_abn_365 `control' (log_n_letters = `iv_type5'), robust cluster(FF_48)
est store i6

esttab i1 i2 i3 i4 i5 i6 using "C:\Users\ev99\Dropbox\test.rtf", append ///
	label nonumbers page nogaps b(3)  se ar2 star(* 0.10 ** 0.05 *** 0.01)  ///
	title("Table 9: One-Year Abnormal Returns and SEC Letters. OLS vs 2SLS") ///
	order(log_n_letters log_sale n_segments age UW_rank law_rank VC JOBS log_S1_words S1_un) drop(*Year* *FF_48* *Office*) ///
	mtitle("Abn. Ret.(OLS)" "Abn. Ret.(2SLS)" "Abn. Ret.(2SLS)" "Abn. Ret.(2SLS)" "Abn. Ret.(2SLS)" "Abn. Ret.(2SLS)") ///
	addnote("Industry and year FE, robust clustered by industry std errs")


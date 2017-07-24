require(data.table)
require(bit64)

ipo <- read.csv("./Projects/SEC Letter Project/Data After Review/ipo_all_issues.csv")

colnames(ipo) <- c("Filing_date", "Issue_date", "Issuer", "State", "Nation", "Offer_Price",
                   "Type", "Description", "REIT", "Unit", "Depositary", "Deal_number", "CEF", 
                   "CUSIP", "CUSIP9", "Proceeds", "Issue_priced_range", "VC", "Gross_spread", 
                   "Mgr_codes","Tech_ind", "Managers", "Managers_code", "Law_firm",
                   "Lawyers_code", "Lawyers_name", "SEC_form", "SEC_file_number")

ipo <- as.data.table(ipo)

### format dates
ipo[, Filing_date := as.Date(Filing_date, format = "%m/%d/%y")]
ipo[, Issue_date := as.Date(Issue_date, format = "%m/%d/%y")]
ipo[, Offer_Price := gsub(",","", Offer_Price)]
ipo[, Offer_Price := as.numeric(as.character(Offer_Price))]
ipo[, Year := year(Issue_date)]
ipo <- ipo[Year %in% 1996:2016]

### types of securities we will keep in the data (common/ordinary shares)
### 29,045 obs before ---> 26,264 after
### Comment: sometimes Ritter codes "MLP-Common Shs" as units =1
ex_types <- c("Units", "Ltd Prtnr Int", "MLP-Common Shs", "Shs Benficl Int",
              "Ltd Liab Int", "Stock Unit", "Trust Units", "Beneficial Ints")
ipo <- ipo[!Type %in% ex_types,]

### drop REIT, Units, ADR, penny stocks and CEF
### 26,264 obs before ---> 18,286 after
logic <- ipo$REIT == "" & (ipo$Unit == "No"|ipo$Unit == "") & 
  ipo$Depositary == "No" & !is.na(ipo$Offer_Price) & ipo$Offer_Price > 5 & ipo$CEF == "No"
ipo <- ipo[logic,]

### creating 8-digit CUSIP to match with CRSP
ipo[, CUSIP8 := paste0(CUSIP, 10)]
require(stringr)
ipo[str_length(CUSIP9) > 1, CUSIP8 := substr(CUSIP9, 1,8)]

### just in case check the IPOs in USA
ipo <- ipo[Nation == "United States"]

### loading CRSP monthly data to extract PERMNO
crsp_monthly <- fread("/Volumes/SD_card/Yandex.Disk.localized/CRSP/MSF/CRSP_MSF.csv", 
                      select = c("CUSIP", "PERMNO", "NCUSIP"))
match <- match(ipo$CUSIP8, crsp_monthly$CUSIP)
ipo$Permno_cusip <- crsp_monthly$PERMNO[match]
match <- match(ipo$CUSIP8, crsp_monthly$NCUSIP)
ipo$Permno_ncusip <- crsp_monthly$PERMNO[match]

crsp_monthly[, cusip6 := substr(CUSIP,1,6)]
match <- match(ipo$CUSIP, crsp_monthly$cusip6)
ipo$Permno_ncusip6 <- crsp_monthly$PERMNO[match]

### Load CRSP daily data
crsp <- NULL
permnos <- unique(c(ipo$Permno_cusip, ipo$Permno_ncusip, ipo$Permno_ncusip6))
permnos <- permnos[!is.na(permnos)]

for(yr in 1996:2016)
{
  print(yr)
  tmp <- fread(paste0("/Volumes/SD_card/Yandex.Disk.localized/CRSP/DSF/CRSP_DSF_", yr, ".csv"), 
               select = c("PERMNO", "date","SHRCD", "EXCHCD","PRC"))
  tmp <- tmp[EXCHCD %in% 1:3 & SHRCD %in% 10:19 & !is.na(PRC)]
  tmp <- tmp[PERMNO %in% permnos]
  crsp <- rbind(crsp, tmp)
}

require(lubridate)
crsp[, date := ymd(date)]
setkey(crsp, PERMNO, date)
### here I do matching by both CUSIP and NCUSIP
### and keep the match that appears to CRSP within 5 days

match <- match(ipo$Permno_cusip, crsp$PERMNO)
ipo$First_CRSP_date_cusip <- crsp$date[match]

match <- match(ipo$Permno_ncusip, crsp$PERMNO)
ipo$First_CRSP_date_ncusip <- crsp$date[match]

match <- match(ipo$Permno_ncusip6, crsp$PERMNO)
ipo$First_CRSP_date_ncusip6 <- crsp$date[match]

ipo[, Permno := -999]
ipo[, dif := First_CRSP_date_ncusip - Issue_date]
ipo[ (dif > -5 & dif <= 10) & (Permno == - 999), Permno := Permno_ncusip] 

ipo[, dif := as.numeric(as.character(First_CRSP_date_cusip - Issue_date))]
ipo[ dif > -5  & dif <= 10 &  (Permno == - 999), Permno := Permno_cusip] 

ipo[, dif := First_CRSP_date_ncusip6 - Issue_date]
ipo[ (dif > -5 & dif <= 10) & (Permno == - 999), Permno := Permno_ncusip6]

match <- match(ipo$Permno, crsp$PERMNO)
ipo$First_CRSP_date <- ymd(crsp$date[match])
ipo$Close_price1 <- abs(crsp$PRC[match])
ipo$Close_price2 <- abs(crsp$PRC[match + 1])
ipo <- ipo[!is.na(Permno) & Permno != -999]
##################################################### 
############## match to Compustat ################### 
##################################################### 
comp <- fread("/Volumes/SD_card/Yandex.Disk.localized/Compustat/crsp_compustat_merger_annual.csv",
              select = c("LPERMNO","datadate", "fyr", "fyear", "cik", "GVKEY", "at", "sale"))
comp <- comp[!is.na(LPERMNO)]

match <- match(ipo$Permno, comp$LPERMNO)
ipo$Gvkey <- comp$GVKEY[match]
#ipo$Cik_compustat <- comp$cik[match]

### constructing calendar year from fiscal
comp$year <- comp$fyear
comp$year[comp$fyr %in% 1:5] <- comp$fyear + 1

### match sales and assets from IPO year
match <- match(paste(ipo$Gvkey, ipo$Year), paste(comp$GVKEY, comp$year))
ipo$at <- comp$at[match]
ipo$sale <- comp$sale[match]

ipo <- ipo[!is.na(at)]
ipo <- ipo[!duplicated(CUSIP)] ### deleting second-class records

##################################################### 
############## match to SEC EDGAR ################### 
##################################################### 
### Step 1. Fix error in SDC infor
### These error are manually collected
fix.file <- function(ipo)
{
  ipo$SEC_file_number[which(ipo$Issuer== "McLeod Inc")] <- 3112
  ipo$SEC_file_number[which(ipo$Issuer== "Triangle Pharmaceuticals Inc")] <- 11793
  ipo$SEC_file_number[which(ipo$Issuer== "Telegroup Inc")] <- 25065
  ipo$SEC_file_number[which(ipo$Issuer== "SCM Microsystems Inc")] <- 29073
  ipo$SEC_file_number[which(ipo$Issuer== "Stoneridge Inc")] <- 33285
  ipo$SEC_file_number[which(ipo$Issuer== "The Pantry Inc")] <- 74221
  ipo$SEC_file_number[which(ipo$Issuer== "Clarent Corp")] <- 76051
  ipo$SEC_file_number[which(ipo$Issuer== "Focal Communications Corp")] <- 77995
  ipo$SEC_file_number[which(ipo$Issuer== "Red Hat Inc")] <- 80051
  ipo$SEC_file_number[which(ipo$Issuer== "Broadbase Software Inc")] <- 82251
  ipo$SEC_file_number[which(ipo$Issuer== "Silicon Image Inc")] <- 83665
  ipo$SEC_file_number[which(ipo$Issuer== "OpenTV Corp")] <- 89609
  ipo$SEC_file_number[which(ipo$Issuer== "Alamosa PCS Holdings Inc")] <- 89995
  ipo$SEC_file_number[which(ipo$Issuer== "UTStarcom Inc")] <- 93069
  ipo$SEC_file_number[which(ipo$Issuer== "Entropin Inc")] <- 11308
  ipo$SEC_file_number[which(ipo$Issuer== "Mobility Electronics Inc")] <- 30264
  ipo$SEC_file_number[which(ipo$Issuer== "Lexent Inc")] <- 30660
  ipo$SEC_file_number[which(ipo$Issuer== "Bruker Daltonics Inc")] <- 34820
  ipo$SEC_file_number[which(ipo$Issuer== "Docent Inc")] <- 34546
  ipo$SEC_file_number[which(ipo$Issuer== "Simple Technology Inc")] <- 32478
  ipo$SEC_file_number[which(ipo$Issuer== "EndWave Corp")] <- 41302
  ipo$SEC_file_number[which(ipo$Issuer== "Monsanto Co")] <- 36956
  ipo$SEC_file_number[which(ipo$Issuer== "Reliant Resources Inc")] <- 48038
  ipo$SEC_file_number[which(ipo$Issuer== "Domino's Pizza Inc")] <- "114442-01"
  ipo$SEC_file_number[which(ipo$Issuer== "Visa Inc")] <- 147296
  ipo$SEC_file_number[which(ipo$Issuer== "Golden Minerals Co")] <- 162486
  ipo$SEC_file_number[which(ipo$Issuer== "Tetraphase Pharmaceuticals Inc")] <- 186574
  ipo$SEC_file_number[which(ipo$Issuer== "Pattern Energy Group Inc")] <- 190538
  ipo$SEC_file_number[which(ipo$Issuer== "Cellular Biomedicine Group")] <- 210337
  ipo$SEC_file_number[which(ipo$Issuer== "Medpace Inc")] <- 212236
  ipo$SEC_file_number[which(ipo$Issuer== "Apptio Inc")] <- 213334
  return(ipo)
}
fix.date <- function(ipo)
{
  ipo$Filing_date[which(ipo$Issuer== "Pluma Inc")] <- "1996-12-24"
  ipo$Filing_date[which(ipo$Issuer== "Authentic Specialty Foods Inc")] <- "1997-06-25"
  ipo$Filing_date[which(ipo$Issuer== "Cumulus Media Inc")] <- "1998-03-30"
  ipo$Filing_date[which(ipo$Issuer== "Tut Systems Inc")] <- "1998-07-31"
  ipo$Filing_date[which(ipo$Issuer== "PLX Technology Inc")] <- "1999-02-04"
  ipo$Filing_date[which(ipo$Issuer== "NorthPoint Communications Grp")] <- "1999-02-26"
  ipo$Filing_date[which(ipo$Issuer== "The Pantry Inc")] <- "1999-03-11"
  ipo$Filing_date[which(ipo$Issuer== "Skechers USA Inc")] <- "1998-07-29"
  ipo$Filing_date[which(ipo$Issuer== "Focal Communications Corp")] <- "1999-05-07"
  ipo$Filing_date[which(ipo$Issuer== "interWAVE Communications")] <- "1999-12-17"
  ipo$Filing_date[which(ipo$Issuer== "VarsityBooks.com Inc")] <- "1999-10-14"
  ipo$Filing_date[which(ipo$Issuer== "OTG Software Inc")] <- "1999-12-23"
  ipo$Filing_date[which(ipo$Issuer== "ChipPAC Inc")] <- "2000-06-16"
  ipo$Filing_date[which(ipo$Issuer== "Seattle Genetics Inc")] <- "2000-11-20"
  ipo$Filing_date[which(ipo$Issuer== "TheraSense Inc")] <- "2001-07-03"
  ipo$Filing_date[which(ipo$Issuer== "Crescent Finl Corp,Cary,NC")] <- "2002-05-03"
  ipo$Filing_date[which(ipo$Issuer== "CapitalSource Inc")] <- "2003-06-12"
  ipo$Filing_date[which(ipo$Issuer== "Acusphere Inc")] <- "2003-07-01"
  ipo$Filing_date[which(ipo$Issuer== "Ultra Clean Holdings Inc")] <- "2004-01-14"
  ipo$Filing_date[which(ipo$Issuer== "Texas Roadhouse Inc")] <- "2004-05-07"
  ipo$Filing_date[which(ipo$Issuer== "IntraLase Corp")] <- "2004-05-28"
  ipo$Filing_date[which(ipo$Issuer== "Tower Group Inc")] <- "2004-05-07"
  ipo$Filing_date[which(ipo$Issuer== "Calamos Asset Management Inc")] <- "2004-08-02"
  ipo$Filing_date[which(ipo$Issuer== "Foundation Coal Holdings Inc")] <- "2004-08-20"
  ipo$Filing_date[which(ipo$Issuer== "Symmetry Medical Inc")] <- "2004-05-28"
  ipo$Filing_date[which(ipo$Issuer== "Bluelinx Holdings Inc")] <- "2004-09-02"
  ipo$Filing_date[which(ipo$Issuer== "Cascade Microtech Inc")] <- "2004-03-03"
  ipo$Filing_date[which(ipo$Issuer== "Great Wolf Resorts Inc")] <- "2004-08-12"
  ipo$Filing_date[which(ipo$Issuer== "Advance America Cash Advance")] <- "2004-08-13"
  ipo$Filing_date[which(ipo$Issuer== "Arbinet-thexchange Inc")] <- "2004-07-09"
  ipo$Filing_date[which(ipo$Issuer== "Warren Resources Inc")] <- "2004-08-25"
  ipo$Filing_date[which(ipo$Issuer== "Majesco Holdings Inc")] <- "2004-10-29"
  ipo$Filing_date[which(ipo$Issuer== "Nasdaq Stock Market Inc")] <- "2004-12-14"
  ipo$Filing_date[which(ipo$Issuer== "PRB Gas Transportation Inc")] <- "2004-11-01"
  ipo$Filing_date[which(ipo$Issuer== "Xerium Technologies Inc")] <- "2004-04-22"
  ipo$Filing_date[which(ipo$Issuer== "Ready Mix Inc")] <- "2005-02-11"
  ipo$Filing_date[which(ipo$Issuer== "Somaxon Pharmaceuticals Inc")] <- "2005-10-07"
  ipo$Filing_date[which(ipo$Issuer== "Continental Resources Inc")] <- "2006-03-07"
  ipo$Filing_date[which(ipo$Issuer== "Somaxon Pharmaceuticals Inc")] <- "2005-10-07"
  ipo$Filing_date[which(ipo$Issuer== "MEMSIC Inc")] <- "2007-09-28"
  ipo$Filing_date[which(ipo$Issuer== "NetSuite Inc")] <- "2007-07-02"
  ipo$Filing_date[which(ipo$Issuer== "Meridian Interstate Bancorp")] <- "2007-09-28"
  ipo$Filing_date[which(ipo$Issuer== "Guidewire Software Inc")] <- "2011-09-02"
  ipo$Filing_date[which(ipo$Issuer== "Benefitfocus Inc")] <- "2013-08-14"
  ipo$Filing_date[which(ipo$Issuer== "ServisFirst Bancshares Inc")] <- "2014-01-17"
  return(ipo)
}

ipo <- fix.file(ipo)
ipo <- fix.date(ipo)
### Step 2. Use SDC SEC film number
### film starts with either 333- or 033-
lookup_SEC_film <- function(ipo, prefix)
{
  read_url <- function(url, ...) {
    on.exit(close(url))
    readLines(url, ...)
  }
  ipo[, `:=`(Cik_SDC = "", SEC_name = "", S1_forms = "", S1_dates = "", S1_types = "")]
  for(i in 1:length(ipo$Filing_date))
  {
    if(i %% 500 == 0)
    {
      print(i)
      print(Sys.time())
    }
    if(ipo$SEC_file_number[i] == 0 | is.na(ipo$SEC_file_number[i])) next
    
    link <- paste0(prefix, ipo$SEC_file_number[i],"&owner=exclude&count=100")
    file <- read_url(url(link))
    
    if(length(grep("No matching file number", file)) > 0) next
    ### find links to forms
    fff <- grep("/Archives/edgar/data/", file, ignore.case = T)
    reg <- regexpr('(?<=edgar/data/).*?(?=\" id=)', file[fff], perl = T)
    all_links <- regmatches(file[fff], reg)
    
    ### find types of form in the film chain
    reg <- regexpr('(?<=">).*?(?=</td)', file[fff-1], perl = T)
    all_types <- regmatches(file[fff-1], reg)
    
    ### find dates of form in the film chain
    reg <- regexpr('(?<=<td>)\\d{4}-\\d{2}-\\d{2}(?=</td>)', file, perl = T)
    all_dates <- regmatches(file, reg)
    all_dates <- ymd(all_dates)

    if(min(all_dates[!is.na(all_dates)]) > ipo$Issue_date[i] & length(all_types) < 90) next

    ### find CIK in film
    fff <- grep("cik", file, ignore.case = T)
    header <- file[fff[1]]
    reg <- regexpr('(?<=companyName\">).*?(?= <acronym title)', header, perl = T)
    name <- regmatches(header, reg)
    
    ### find company name
    reg <- regexpr('(?<=count=100\">).*?(?= \\(see all)', header, perl = T)
    cik <- regmatches(header, reg)
    
    ipo$Cik_SDC[i] <- paste0(cik, collapse = "|")
    ipo$SEC_name[i] <- paste0(name, collapse = "|")
    ipo$S1_forms[i] <- paste0(all_links, collapse = "|")
    ipo$S1_types[i] <- paste0(all_types, collapse = "|")
    ipo$S1_dates[i] <- paste0(all_dates, collapse = "|")
    try(closeAllConnections()) 
  }
  return(ipo)
}

prefix <- "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&filenum=333-"
tmp1 <- lookup_SEC_film(ipo, prefix)
ipo[, `:=`(Cik_SDC = tmp1$Cik_SDC, SEC_name = tmp1$SEC_name, 
           S1_forms = tmp1$S1_forms, S1_types = tmp1$S1_types, S1_dates = tmp1$S1_dates)]

#rm(tmp1)
### cut sample to those firms that report
ipo$Date1 <-  gsub(".*\\|", "", ipo$S1_dates)
ipo$Form1 <-  gsub(".*\\|", "", ipo$S1_types)

logic1 <- ipo$Year == 1996 & ipo$Filing_date == ymd(ipo$Date1)
logic2 <- ipo$Year == 1996 & ipo$Filing_date >= ymd("19960509")
logic3 <- ipo$Year == 1996 & grepl("A", ipo$Form1)
logic4 <- ipo$Year %in% 1997:2016

ind <- which( (logic1 | logic2 | logic4) & (!logic3) )
ipo <- ipo[ind]

### long films to fix
require(stringr)
ipo[, n_film := str_count(S1_forms, "\\|")]
long_films <- ipo$SEC_file_number[ipo$n_film > 90]
long_films_ind <- which(ipo$n_film > 90)

lookup_SEC_log_film <- function(ipo, prefix)
{
  read_url <- function(url, ...) {
    on.exit(close(url))
    readLines(url, ...)
  }
  ipo[, `:=`(Cik_SDC = "", SEC_name = "", S1_forms = "", S1_dates = "", S1_types = "")]
  for(i in 1:length(ipo$Filing_date))
  {
    
    if(ipo$SEC_file_number[i] == 0 | is.na(ipo$SEC_file_number[i])) next
    
    print(i)
    link <- paste0(prefix, ipo$SEC_file_number[i],"&owner=exclude&count=100")
    file <- read_url(url(link))
    
    ### find CIK in film
    fff <- grep("cik", file, ignore.case = T)
    header <- file[fff[1]]
    reg <- regexpr('(?<=companyName\">).*?(?= <acronym title)', header, perl = T)
    name <- regmatches(header, reg)
    
    ### find company name
    reg <- regexpr('(?<=count=100\">).*?(?= \\(see all)', header, perl = T)
    cik <- regmatches(header, reg)
    
    all_links <- NULL
    all_types <- NULL
    all_dates <- NULL
    
    end_links <- c("&owner=exclude&count=100", "&type=&dateb=&owner=exclude&start=100&count=100",
                   "&type=&dateb=&owner=exclude&start=200&count=100")
    for(test in 1:3)
    {
      link <- paste0(prefix, ipo$SEC_file_number[i],end_links[test])
      file <- read_url(url(link))
      
      if(length(grep("No matching file number", file)) > 0) next
      ### find links to forms
      fff <- grep("/Archives/edgar/data/", file, ignore.case = T)
      reg <- regexpr('(?<=edgar/data/).*?(?=\" id=)', file[fff], perl = T)
      all_links <- c(all_links, regmatches(file[fff], reg))
      
      ### find types of form in the film chain
      reg <- regexpr('(?<=">).*?(?=</td)', file[fff-1], perl = T)
      all_types <- c(all_types, regmatches(file[fff-1], reg))
      
      ### find dates of form in the film chain
      reg <- regexpr('(?<=<td>)\\d{4}-\\d{2}-\\d{2}(?=</td>)', file, perl = T)
      all_dates <- c(all_dates, regmatches(file, reg))
      
    }
    all_dates <- ymd(all_dates)
    ipo$Cik_SDC[i] <- paste0(cik, collapse = "|")
    ipo$SEC_name[i] <- paste0(name, collapse = "|")
    ipo$S1_forms[i] <- paste0(all_links, collapse = "|")
    ipo$S1_types[i] <- paste0(all_types, collapse = "|")
    ipo$S1_dates[i] <- paste0(all_dates, collapse = "|")
    try(closeAllConnections()) 
  }
  return(ipo)
}

tmp <- lookup_SEC_log_film(ipo[long_films_ind], prefix)

ipo[long_films_ind, `:=`(Cik_SDC = tmp$Cik_SDC, SEC_name = tmp$SEC_name, 
           S1_forms = tmp$S1_forms, S1_types = tmp$S1_types, S1_dates = tmp$S1_dates)]
ipo[, n_film := str_count(S1_forms, "\\|")]
str_count(ipo$S1_dates[long_films_ind], "\\|")

ipo$Date1 <-  gsub(".*\\|", "", ipo$S1_dates)
ipo$Form1 <-  gsub(".*\\|", "", ipo$S1_types)

### I keep only IPOs where filing is around SDC filing date
### The highest difference when SDC filing date on Friday
### Exclude N-2 and S-11 which are Units and CEF
checked <- which(abs(ipo$Filing_date - ymd(ipo$Date1)) <= 10 & !(ipo$Form1 %in% c("N-2","S-11")))
ipo <- ipo[checked]

save(list = ls(all=TRUE), file = "./Projects/SEC Letter Project/Data After Review/Rdata/cleaning SDC.RData")
### deleting extra variables
ipo[, `:=`(Nation = NULL, REIT = NULL, Unit = NULL, Depositary = NULL, CEF = NULL)]
ipo[, `:=`(First_CRSP_date_cusip = NULL, First_CRSP_date_ncusip = NULL, First_CRSP_date_ncusip6 = NULL,
           Permno_cusip = NULL, Permno_ncusip = NULL, Permno_ncusip6 = NULL, dif = NULL)]
rm(crsp)
rm(crsp_monthly)
rm(comp)
write.csv(ipo, "./Projects/SEC Letter Project/Data After Review/ipo_20170422.csv", row.names = F)
## ?? two of just one? 
#ipo$SEC_file_number[which(ipo$Issuer== "Horsehead Holding Corp")] <- 142113
## !!! two films 14995 and 111777
#ipo$SEC_file_number[which(ipo$Issuer== "Dominick's Supermarkets Inc")] <- 11177

### !!! these companies probably filed under the JOBS act, 
### thus SDC filing date is earlier that first SEC EDGAR record
#"NMI Holdings Inc"
### this form coded S-3 but should be S-1 
#"ServisFirst Bancshares Inc"

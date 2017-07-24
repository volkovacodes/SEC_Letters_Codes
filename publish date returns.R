require(data.table)
require(lubridate)
nc <- function(x) return(as.numeric(as.character(x)))
db_name <- "/Volumes/SD_card/Yandex.Disk.localized/SEC Master/SEC_master.sqlite"
ipo <- as.data.table(read.csv("./Projects/SEC Letter Project/Data After Review/ipo_20170510.csv"))
upload_ipo <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
### I've checked these files and they are either Corresp or other boilerplate
upload_ipo <- upload_ipo[n_item > 0 & !out_filename %in% c("1095291_0000000000-06-053374.txt","1095291_0000000000-06-053376.txt","1095291_0000000000-06-053378.txt")]
upload_ipo[,`:=` (letter_number = 1:.N, n_letters = .N), by = CIK]
### load large file
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_all.csv")
upload_ipo$letter_author <- upload$letter_author[match(upload_ipo$out_filename, upload$out_filename)]
upload_ipo[, all_authors := paste0(unique(letter_author), collapse = "|"), by = CIK]
ipo <- ipo[Cik_SDC %in% upload_ipo$CIK]

crsp.file <- "./Projects/SEC Letter Project/Data After Review/Additional Data/crsp_after_2004.rds"

if(file.exists(crsp.file) == T) crsp <- readRDS(crsp.file)
if(!file.exists(crsp.file) == T) 
{
  read.crsp.years <- function(years, var_names, file_out = F, office = T)
  {
    require(data.table)
    require(lubridate)
    
    if(office == T) dir <- "C:/Users/ev99/YandexDisk/CRSP/DSF/"
    if(office == F) dir <- "/Volumes/SD_card/Yandex.Disk.localized/CRSP/DSF/"
    
    info <- fread(paste0(dir, "info.csv"))
    
    crsp <- NULL
    for(yr in years)
    {
      print(Sys.time())
      print(yr)
      n <- info$n[info$year == yr]
      
      crsp_tmp <- fread(paste0(dir, "CRSP_DSF_", yr, ".csv"), nrows = n, select = var_names)
      crsp_tmp[, date := ymd(date)]
      setkey(crsp_tmp, PERMNO, date)
      if("RET" %in% var_names)
      {
        crsp_tmp[, RET := as.numeric(as.character(RET))]
        crsp_tmp <- crsp_tmp[!is.na(RET)]
      }
      crsp <- rbind(crsp, crsp_tmp)
    }
    setkey(crsp, PERMNO, date)
    if(file_out != F) fwrite(crsp, file_out, row.names = F)
    return(crsp)
  }
  var_names <- c("date", "PERMNO", "NCUSIP" ,"PRC", "RET", "SHROUT", "ewretd", "vwretd")
  crsp <- read.crsp.years(2004:2016, var_names = var_names, office = F)
  
  crsp <- crsp[PERMNO %in% ipo$Permno]
  saveRDS(crsp, crsp.file)
}

### matching publishing dates to the IPO file
publish.dates <- fread("./Projects/SEC Letter Project/Data After Review/Additional Data/upload_with_publish_dates.csv")
publish.dates <- publish.dates[out_filename %in% upload_ipo$out_filename]
publish.dates[, DATE_Writen := gsub(".*: ", "", DATE_Writen)]
publish.dates[, DATE_Writen := ymd(substr(DATE_Writen, 1, 8))]
upload_ipo$DATE_Publish <- publish.dates$DATE_Writen[match(upload_ipo$out_filename, publish.dates$out_filename)]

tmp <- upload_ipo[letter_number == 1]
ipo$Publish_date <- tmp$DATE_Publish[match(ipo$Cik_SDC, tmp$CIK)]

### calcualting CAR around 
crsp[, `:=` (date = ymd(date), RET = nc(RET), vwretd = nc(vwretd))]
m <- match(crsp$PERMNO, ipo$Permno)
crsp[, `:=`(Issue_date = ipo$Issue_date[m], Publish_date = ipo$Publish_date[m])]
crsp[, dif := nc(date - Publish_date)]

crsp[, ind := 0]
crsp[dif > 0 & dif <= 5, ind := 1]
crsp[, `:=` (abnret = prod(1+RET[ind==1]) - 1, expret = prod(1+vwretd[ind==1]) - 1), by = PERMNO]
crsp[, n := 1:.N, by = PERMNO]
crsp[, `:=`(vol_half1 = sqrt(126)*sd(RET[n %in% 1:126], na.rm = T),
            vol_half2 = sqrt(126)*sd(RET[n %in% 126:252], na.rm = T)), by = PERMNO]
m <- match(ipo$Permno, crsp$PERMNO)
ipo$ret_publish <- crsp$abnret[m] - crsp$expret[m]
ipo[, `:=` (vol_half1 = crsp$vol_half1[m], vol_half2 = crsp$vol_half2[m])]
ipo[nc(ymd(Issue_date) - Publish_date) >= 365, ret_publish := NA]



nc <- function(x) return(as.numeric(as.character(x)))
db_name <- "/Volumes/SD_card/Yandex.Disk.localized/SEC Master/SEC_master.sqlite"
upload_ipo <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
### I've checked these files and they are either Corresp or other boilerplate
upload_ipo <- upload_ipo[n_item > 0 & !out_filename %in% c("1095291_0000000000-06-053374.txt","1095291_0000000000-06-053376.txt","1095291_0000000000-06-053378.txt")]
upload_ipo[,`:=` (letter_number = 1:.N, n_letters = .N), by = CIK]
### load large file
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_all.csv")
upload_ipo$letter_author <- upload$letter_author[match(upload_ipo$out_filename, upload$out_filename)]
upload_ipo[, all_authors := paste0(unique(letter_author), collapse = "|"), by = CIK]
ipo <- ipo[Cik_SDC %in% upload_ipo$CIK]

### find offices
offices <- fread("./Projects/SEC Letter Project/Data After Review/Sorting SEC letters/All_CIKs.csv")
offices$AD_Office[offices$AD_Office == "2 & 3"] <- 3

ipo$AD_Office <- offices$AD_Office[match(ipo$Cik_SDC, nc(offices$CIK))]
upload$AD_Office <- offices$AD_Office[match(upload$CIK, nc(offices$CIK))]
upload <- as.data.frame(upload)
upload <- upload[!is.na(upload$AD_Office),]
upload <- as.data.table(upload)
upload_ipo$AD_Office <- offices$AD_Office[match(upload_ipo$CIK, nc(offices$CIK))]



### find confidential drafts
require(DBI)
require(RSQLite)
con = dbConnect(SQLite(), dbname = db_name)
line <- paste0('SELECT * FROM master where Form_Type == "DRS"')
DRS = as.data.table(dbGetQuery(con, line))
DRS <- DRS[CIK %in% upload_ipo$CIK]
DRS <- DRS[!duplicated(CIK)]
ipo$Conf_date <- DRS$DATE_Filed[match(ipo$Cik_SDC, DRS$CIK)]

### if there is no confidential filing, then use filing
conf <- upload_ipo[conf == 1 & !CIK %in% DRS$CIK]
setkey(conf, CIK, dates)
conf <- conf[!duplicated(CIK)]
match <- match(ipo$Cik_SDC[ipo$Cik_SDC %in% conf$CIK], conf$CIK)
ipo$Conf_date[ipo$Cik_SDC %in% conf$CIK] <- conf$re_date[match]
ipo[is.na(Conf_date), Conf_date := Filing_date]

### transfering info
setkey(upload_ipo, CIK, letter_number)
upload_ipo[, n_letters := .N, by = CIK]
upload_ipo$PRange_date <- ymd(ipo$PRange_date[match(upload_ipo$CIK, ipo$Cik_SDC)])
upload_ipo[, after_prange := 0]
upload_ipo[ymd(dates) >= PRange_date - 2, after_prange := 1]
upload_ipo[, n_letters_after_price := sum(after_prange), by = CIK]

m <- match(ipo$Cik_SDC, upload_ipo$CIK)
ipo[, `:=` (Upload1_date = upload_ipo$dates[m], Upload1_file = upload_ipo$out_filename[m], 
            Upload1_words = upload_ipo$n_words[m], Upload1_words_clean = upload_ipo$n_words_body[m],
            Upload1_items = upload_ipo$n_item[m], n_letters = upload_ipo$n_letters[m], n_letters_after_price = upload_ipo$n_letters_after_price[m], Upload_all_authors = upload_ipo$all_authors[m])]


upload[, `:=` (date = ymd(dates), n_words = nc(n_words))]
m <- match(ipo$Upload1_file, upload$out_filename)
ipo[, `:=` (letter_author = upload$letter_author[m], letter_sender = upload$letter_sender[m])]
ipo[, `:=`(registration_length = nc(ymd(Issue_date) - ymd(Filing_date)),Upload1_words = nc(Upload1_words))]

#ipo <- ipo[!is.infinite(rel_office_load) & !is.na(rel_office_load_words)]
setkey(ipo, Conf_date)
ipo[, ind_year := paste(FF_48, Year)]
ipo[letter_author == "Amanda McManus", letter_author := "Amanda Ravitz"]
ipo[, `:=` (author_count = .N, exper = 1:.N), by = letter_author]
setkey(ipo, Issue_date)

write.csv(ipo, "./Projects/SEC Letter Project/Data After Review/ipo_20170620.csv", row.names = F)



require(data.table)
require(lubridate)
db_name <- "C:/Users/ev99/YandexDisk/SEC Master/SEC_master.sqlite"
ipo <- as.data.table(read.csv("./Projects/SEC Letter Project/Data After Review/ipo_20170510.csv"))
upload_ipo <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
### I've checked these files and they are either Corresp or other boilerplate
upload_ipo <- upload_ipo[n_item > 0 & !out_filename %in% c("1095291_0000000000-06-053374.txt","1095291_0000000000-06-053376.txt","1095291_0000000000-06-053378.txt")]
upload_ipo[,`:=` (letter_number = 1:.N, n_letters = .N), by = CIK]
### load large file
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_all.csv")
upload[, dates := mdy(DATE_Filed)]
upload_ipo$letter_author <- upload$letter_author[match(upload_ipo$out_filename, upload$out_filename)]
upload_ipo[, all_authors := paste0(unique(letter_author), collapse = "|"), by = CIK]
ipo <- ipo[Cik_SDC %in% upload_ipo$CIK]

offices <- fread("./Projects/SEC Letter Project/Data After Review/Sorting SEC letters/All_CIKs.csv")
offices$AD_Office[offices$AD_Office == "2 & 3"] <- 3

ipo$AD_Office <- offices$AD_Office[match(ipo$Cik_SDC, as.numeric(as.character(offices$CIK)))]
upload$AD_Office <- offices$AD_Office[match(upload$CIK, as.numeric(as.character(offices$CIK)))]
upload <- as.data.frame(upload)
upload <- upload[!is.na(upload$AD_Office),]
upload <- as.data.table(upload)
upload_ipo$AD_Office <- offices$AD_Office[match(upload_ipo$CIK, as.numeric(as.character(offices$CIK)))]

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
m <- match(ipo$Cik_SDC, upload_ipo$CIK)
ipo[, `:=` (Upload1_date = upload_ipo$dates[m], Upload1_file = upload_ipo$out_filename[m], 
            Upload1_words = upload_ipo$n_words[m], Upload1_words_clean = upload_ipo$n_words_body[m],
            Upload1_items = upload_ipo$n_item[m], n_letters = upload_ipo$n_letters[m], Upload_all_authors = upload_ipo$all_authors[m])]


upload[, `:=` (date = ymd(dates), n_words = as.numeric(as.character(n_words)))]

m <- match(ipo$Upload1_file, upload$out_filename)
ipo[, `:=` (letter_author = upload$letter_author[m], letter_sender = upload$letter_sender[m])]

ipo[, n_law := .N, by = Law_firm]
ipo[, law_rank := 0]
ipo[n_law == 1, law_rank := 1]
ipo[n_law == 2, law_rank := 2]
ipo[n_law %in% 3:4, law_rank := 3]
ipo[n_law %in% 5:7, law_rank := 4]
ipo[n_law %in% 8:14, law_rank := 5]
ipo[n_law %in% 15:20, law_rank := 6]
ipo[n_law %in% 21:25, law_rank := 7]
ipo[n_law %in% 26:42, law_rank := 8]
ipo[n_law %in% 43:63, law_rank := 9]
ipo[n_law > 63, law_rank := 10]


#ipo <- ipo[AD_Office != 23]
upload[,  Year := year(dates)]
for(i in 1:length(ipo$Conf_date))
{
  if(i %% 100 == 0) print(i)
  fdate <- ymd(ipo$Conf_date[i]) 
  
  ind1 <- which(upload$dates > fdate -30  & upload$dates <= fdate + 30)# & upload$out_filename != ipo$Upload1_file[i])
  ipo$sec_load[i] <- length(ind1)
  ipo$sec_load_words[i] <- sum(upload$n_words[ind1], na.rm = T)
  
  name <- ipo$letter_author[i]
  ind <- which(upload$letter_author %in% name)
  ipo$person_load[i] <- length(intersect(ind1,ind))
  ipo$person_load_words[i] <- sum(upload$n_words[intersect(ind1,ind)], na.rm = T)
  
  office <- ipo$AD_Office[i]
  ind <- which(upload$AD_Office %in% office)
  ipo$office_load[i] <- length(intersect(ind1,ind))
  ipo$office_load_words[i] <- sum(upload$n_words[intersect(ind1,ind)], na.rm = T)
}

require(psych)
wina <- 0.001
lwin <- function(x, a = wina) return(winsor(log(1+x),a))
w <- function(x, a = wina) return(winsor(x,a))
ipo[, `:=`(registration_length = as.numeric(ymd(Issue_date) - ymd(Filing_date)),Upload1_words = as.numeric(as.character(Upload1_words)))]

#ipo <- ipo[!is.infinite(rel_office_load) & !is.na(rel_office_load_words)]
ipo[, ind_year := paste(AD_Office, Year)]
ipo1 <- ipo[ymd(Filing_date) > ymd("20050512")& ymd(Filing_date) <= ymd("20161231")]
ipo1[, `:=` (log_sale = lwin(sale), n_segments = w(n_segments), age = lwin(Year - founding_year), UW_rank = w(UW_rank), price_update = w(price_update), F_score = w(F_score),
            log_S1_words = lwin(S1_words), S1_un = w(S1_uncertanty), log_words = lwin(Upload1_words), log_n_letters = lwin(n_letters), 
            registration_length = w(registration_length), log_registration_length = lwin(registration_length),
            log_sec_load_letters = lwin(sec_load), log_sec_load_words = lwin(sec_load_words),
            log_person_load_letters = lwin(person_load), log_person_load_words = lwin(person_load_words),
            log_office_load_letters = lwin(office_load), log_office_load_words = lwin(office_load_words))]
reg_line <- function(x, end) 
{  
  
  end <- " + log_sale + n_segments + age +  UW_rank + law_rank + VC + JOBS + log_S1_words + 
  S1_un|Year + FF_48|0|FF_48"
  line <- NULL
  line[[1]] <- paste0(x, " ~ log_sec_load_letters", end)
  line[[2]] <- paste0(x, " ~ log_sec_load_words", end)
  line[[3]] <- paste0(x, " ~ log_person_load_letters", end)
  line[[4]] <- paste0(x, " ~ log_person_load_words", end)
  line[[5]] <- paste0(x, " ~ log_office_load_letters", end)
  line[[6]] <- paste0(x, " ~ log_office_load_words", end)
  return(line)
}
require(lfe)

line_letters <- reg_line("log_n_letters", end)
line_words <- reg_line("log_words", end)
line_days <- reg_line("log_registration_length", end)


my_felm <- function(x) felm(as.formula(unlist(x)), data = ipo1)
model_letters <- lapply(line_letters, my_felm)
model_words <- lapply(line_words, my_felm)
model_days <- lapply(line_days, my_felm)
 
FE_line <- list(c("Industry and Year FE", rep("YES", 8)))
varnames <- c("SEC load, letters", "SEC load, words", "Person load, letters", "Person load, words", "Office load, letters", "Office load, words")
other_vars <- c("Sales", "Number of Segments", "Age", "UW Rank", "Law Firm Rank", "VC Dummy", "JOBS Act Dummy", "Prospectus Length", "Prospectus Uncertanty")
omit_vars <- c("log_S1_words","S1_un", "log_sale","n_segments", "age","UW_rank","law_rank","VC","JOBS")
out_type = "text"
require(stargazer)
out1 <- stargazer(model_letters, type = out_type, omit.stat = c("ser", "f"), dep.var.caption = "Number of SEC Letters", dep.var.labels.include = F,  add.lines = FE_line,  omit = omit_vars)

line <- NULL
end <- " ~  log_sale + n_segments + age +  UW_rank + law_rank + VC + JOBS + log_S1_words +  
S1_un|FF_48 + Year|(log_n_letters ~ factor(letter_author))|FF_48"
line[[1]] <-  paste0("log_registration_length", end) 
line[[2]] <-  paste0("price_update", end) 
line[[3]] <-  paste0("IR", end) 
line[[4]] <-  paste0("insider_sales", end) 
line[[5]] <-  paste0("vol", end)
line[[6]] <-  paste0("ret_abn_365", end)

model <- lapply(line, my_felm)

stargazer(model, type = "text",  report = "vcpt*", omit.stat = c("ser", "f"), omit = omit_vars)

reg <- model[[3]]
print(reg$stage1$rob.iv1fstat$log_n_letters[5])
print(reg$stage1$iv1fstat$log_n_letters[5])

require(ggplot2)
qplot(ymd(ipo1$Filing_date), ipo1$log_person_load_letters)

#require(foreign)
#write.dta(ipo, "testing.dta")


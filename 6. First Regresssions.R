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
upload_ipo$letter_author <- upload$letter_author[match(upload_ipo$out_filename, upload$out_filename)]
upload_ipo[, all_authors := paste0(unique(letter_author), collapse = "|"), by = CIK]
ipo <- ipo[Cik_SDC %in% upload_ipo$CIK]

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

for(i in 1:length(ipo$Conf_date))
{
  if(i %% 100 == 0) print(i)
  fdate <- ymd(ipo$Conf_date[i]) 

  ind1 <- which(upload$date >= fdate & upload$date <= fdate + 30 & upload$out_filename != ipo$Upload1_file[i])
  ipo$sec_load[i] <- length(ind1)
  ipo$sec_load_words[i] <- sum(upload$n_words[ind1], na.rm = T)
  name <- ipo$letter_author[i]
  ind <- which(upload$letter_author %in% name)
  ipo$person_load[i] <- length(intersect(ind1,ind))
  ipo$person_load_words[i] <- sum(upload$n_words[intersect(ind1,ind)], na.rm = T)
}

require(psych)
a <- 0.001
ipo[, `:=`(registration_length = as.numeric(ymd(Issue_date) - ymd(Filing_date)),Upload1_words = as.numeric(as.character(Upload1_words)))]
ipo[, `:=` (log_sale = winsor(log(1+sale), a), n_segments = winsor(n_segments, a), 
            age = winsor(log(1 + Year - founding_year), a),
            UW_rank = winsor(UW_rank, a), price_update = winsor(price_update, a),
            log_S1_words = winsor(log(1+S1_words), a), S1_un = winsor(S1_uncertanty, a),
            F_score = winsor(F_score, a), log_words = winsor(log(1+Upload1_words), a),
            log_n_letters = winsor(log(1 + n_letters), a), 
            registration_length = winsor(registration_length, a),
            log_registration_length = winsor(log(registration_length), a),
            log_sec_load_letters = winsor(log(1 + sec_load), a),
            log_person_load_letters = winsor(log(1 + person_load), a),
            log_sec_load_words = winsor(log(1 + sec_load_words), a),
            log_person_load_words = winsor(log(1 + person_load_words), a),
            proxy = letter_sender == letter_author)]

reg_line <- function(x, end) 
{     
  end <- " + log_sale + n_segments + age +  UW_rank + law_rank + VC + JOBS + log_S1_words + 
  S1_un|Year + FF_48|0|FF_48"
  line <- NULL
  line[[1]] <- paste0(x, " ~ log_sec_load_letters", end)
  line[[2]] <- paste0(x, " ~ log_sec_load_words", end)
  line[[3]] <- paste0(x, " ~ log_person_load_letters", end)
  line[[4]] <- paste0(x, " ~ log_person_load_words", end)
  return(line)
}
require(lfe)

line_letters <- reg_line("log_n_letters", end)
line_words <- reg_line("log_words", end)
line_length <- reg_line("log_registration_length", end)
line_update <- reg_line("price_update", end)
line_ir <- reg_line("IR", end)
line_vol <- reg_line("vol", end)

my_felm <- function(x) felm(as.formula(unlist(x)), data = ipo)
model_letters <- lapply(line_letters, my_felm)
model_length <- lapply(line_length, my_felm)
model_words <- lapply(line_words, my_felm)
model_update <- lapply(line_update, my_felm)
model_ir <- lapply(line_ir, my_felm)
model_vol <- lapply(line_vol, my_felm)
FE_line <- list(c("Industry and Year FE", rep("YES", 4)))
varnames <- c("SEC load, letters", "SEC load, words", "Person load, letters", "Person load, words",
              "Sales", "Number of Segments", "Age", "UW Rank", "Law Firm Rank", "VC Dummy",
              "JOBS Act Dummy", "Prospectus Length", "Prospectus Uncertanty")
out_type = "latex"
require(stargazer)
out1 <- stargazer(model_letters, type = out_type, omit.stat = c("ser", "f"), 
                  dep.var.caption = "Number of SEC Letters", dep.var.labels.include = F, 
                  add.lines = FE_line, covariate.labels = varnames)
out2 <- stargazer(model_words, type = out_type, omit.stat = c("ser", "f"), 
                  dep.var.caption = "Number of Words in the 1st SEC Letter", dep.var.labels.include = F,
                  add.lines = FE_line, covariate.labels = varnames)
out3 <- stargazer(model_length, type = out_type, omit.stat = c("ser", "f"),
                  dep.var.caption = "Registration Length", dep.var.labels.include = F,
                  add.lines = FE_line, covariate.labels = varnames)
out4 <- stargazer(model_update, type = out_type, omit.stat = c("ser", "f"),
                  dep.var.caption = "Price Update", dep.var.labels.include = F,
                  add.lines = FE_line, covariate.labels = varnames)
out5 <- stargazer(model_ir, type = out_type, omit.stat = c("ser", "f"),
                  dep.var.caption = "Initial Returns", dep.var.labels.include = F,
                  add.lines = FE_line, covariate.labels = varnames)
out6 <- stargazer(model_vol, type = out_type, omit.stat = c("ser", "f"),
                  dep.var.caption = "Volatility", dep.var.labels.include = F,
                  add.lines = FE_line, covariate.labels = varnames)

start <- readLines("./R codes/table_start.tex")
file <- c(start, out1,out2,out3,out4, out5, out6, "\\end{document}")

#write(file, "file.tex")
#require(tools)
#texi2pdf("file.tex", clean = T)

reg_line2 <- function(x) 
{
  line <- NULL
  a <- " ~ log_sale + n_segments + age +  UW_rank + law_rank + VC + JOBS + log_S1_words + S1_un|Year + FF_48|(log_n_letters ~ "
  b <- ")|FF_48"
  line[[1]] <- paste0(x, " ~ log_sale + n_segments + age +  UW_rank + law_rank + VC + JOBS + log_S1_words + S1_un + log_n_letters|Year + FF_48|0|FF_48")
  line[[2]] <- paste0(x, a ,"log_sec_load_letters", b)
  line[[3]] <- paste0(x, a, "log_sec_load_words", b)
  line[[4]] <- paste0(x, a, "log_person_load_letters", b)
  line[[5]] <- paste0(x, a, "log_person_load_words", b)
  return(line)
}

line_length <- reg_line2("log_registration_length")
line_update <- reg_line2("price_update")
line_ir <- reg_line2("IR")
line_vol <- reg_line2("vol")

model_length <- lapply(line_length, my_felm)
model_update <- lapply(line_update, my_felm)
model_ir <- lapply(line_ir, my_felm)
model_vol <- lapply(line_vol, my_felm)

stargazer(model_length, type = "text")
stargazer(model_update, type = "text")
stargazer(model_ir, type = "text")
stargazer(model_vol, type = "text")

library(foreign)
#write.csv(ipo, "./Projects/SEC Letter Project/Data After Review/ipo_20170525.csv", row.names = F)
write.dta(ipo, "ipo_20170529.dta")

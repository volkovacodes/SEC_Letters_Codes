require(data.table)
require(lubridate)
require(lfe)  
require(stargazer) 
dir_upload <- "./Projects/SEC Letter Project/Data After Review/IPO Uploads/"
ipo <- as.data.table(read.csv("./Projects/SEC Letter Project/Data After Review/ipo_20170510.csv"))
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo_2004_2016.csv")
upload[, date := ymd(DATE_Filed)]
setkey(upload, CIK, date)
upload[, `:=` (n_letter = length(film), all_authors = paste0(unique(letter_author[!is.na(letter_author)]), collapse = "|")), by = CIK]

ipo <- ipo[Cik_SDC %in% upload$CIK]
m <- match(ipo$Cik_SDC, upload$CIK)
ipo[, `:=`(n_letters = upload$n_letter[m], first_letter = upload$out_filename[m], letter_author = upload$letter_author[m], 
           letter_author_all = upload$all_authors[m], letter_sender = upload$letter_sender[m], first_letter_date = upload$date[m],
           registration_length = as.numeric(as.character(ymd(ipo$Issue_date) - ymd(ipo$Filing_date))), person_id =  upload$letter_author[m])]

ipo[, total_letter := .N, by = person_id]
#ipo[total_letter < 20, person_id := "dummy"]
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

upload_all <- fread("./Projects/SEC Letter Project/Data After Review/upload_all_with_signature.csv")
upload_all[, date := ymd(DATE_Filed)]
setkey(upload_all, CIK, date)
upload_all[is.na(letter_author) | letter_author == "NA", letter_author := letter_sender]
for(i in 1:length(ipo$Filing_date))
{
  if(i %% 100 == 0) print(i)
  fdate <- ymd(ipo$Filing_date[i]) - 0
  idate <- ymd(ipo$Filing_date[i]) + 30
  if(grepl("CT ORDER", ipo$S1_types[i]))
  {
    fdate <- fdate - 30
    idate <- idate - 30
  }
  ind1 <- which(upload_all$date >= fdate & upload_all$date <= idate)
  ipo$sec_load[i] <- length(ind1)
  ipo$sec_load_words[i] <- sum(upload_all$n_words[ind1], na.rm = T)
  name <- ipo$letter_author[i]
  ind <- which(upload_all$letter_author %in% name)
  ipo$person_load[i] <- length(intersect(ind1,ind))
  ipo$person_load_words[i] <- sum(upload_all$n_words[intersect(ind1,ind)], na.rm = T)
}
ipo$first_letter_length <- upload_all$n_words[match(ipo$Cik_SDC, upload_all$CIK)]

### winsoring
require(psych)
a <- 0.005
ipo[, `:=` (log_sale = winsor(log(1+sale), a), n_segments = winsor(n_segments, a), 
            age = winsor(log(1+Year - founding_year), a),
            UW_rank = winsor(UW_rank, a), price_update = winsor(price_update, a),
            log_S1_words = winsor(log(1+S1_words), a), S1_un = winsor(S1_uncertanty, a),
            F_score = winsor(F_score, a), log_words = winsor(log(1+first_letter_length), a),
            log_n_letters = winsor(log(1 + n_letters), a), 
            registration_length = winsor(log(registration_length), a),
            log_sec_load_letters = winsor(log(sec_load), a),
            log_person_load_letters = winsor(log(1 + person_load), a),
            log_sec_load_words = winsor(log(1 + sec_load_words), a),
            log_person_load_words = winsor(log(1 + person_load_words), a),
            proxy = letter_sender == letter_author)]
ipo <- ipo[first_letter_length > 10]
#### regression
 

reg_line <- function(x, end) 
{
  line <- NULL
  line[[1]] <- paste0(x, " ~ log_sec_load_letters", end)
  line[[2]] <- paste0(x, " ~ log_sec_load_words", end)
  line[[3]] <- paste0(x, " ~ log_person_load_letters", end)
  line[[4]] <- paste0(x, " ~ log_person_load_words", end)
  return(line)
}

end <- " + log_sale + n_segments + age +  UW_rank + law_rank + VC + JOBS + log_S1_words + 
S1_un|Year + FF_48|0|FF_48"

line_letters <- reg_line("log_n_letters", end)
line_words <- reg_line("log_words", end)
line_length <- reg_line("registration_length", end)
line_update <- reg_line("price_update", end)
line_ir <- reg_line("IR", end)
line_vol <- reg_line("vol", end)

#my_felm <- function(x) felm(as.formula(unlist(x)), 
#                            data = ipo[!grepl("CT ORDER", ipo$S1_types),])

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

write(file, "file.tex")
require(tools)
texi2pdf("file.tex", clean = T)

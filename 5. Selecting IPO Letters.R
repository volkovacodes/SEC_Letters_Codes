require(data.table)
require(lubridate)
require(stringr)
require(DBI)
require(RSQLite)
require(caroline)
sql_dir <- "/Volumes/SD_card/Yandex.Disk.localized/SEC Master/UPLOAD/UPLOAD sqlite/"
upload_file <- "./Projects/SEC Letter Project/Data After Review/upload_all.csv"
ipo <- read.csv("./Projects/SEC Letter Project/Data After Review/ipo_20170510.csv")
CIKs <- unique(ipo$Cik_SDC[ymd(ipo$Issue_date) > ymd("2004-01-01")])

### writing info from DB into .csv
if(file.exists(upload_file)) upload <- fread(upload_file)
if(!file.exists(upload_file))
{
  upload <- NULL
  for(cyear in 2004:2017)
  {
    print(Sys.time())
    print(cyear)
    db_name <- paste0(sql_dir, "upload_", cyear, ".sqlite")
    con <- dbConnect(SQLite(), db_name)  
    
    files <- dbGetQuery(con, 'SELECT CIK, Company_Name, DATE_Filed, Filename, Link, out_filename, start_link, year FROM content')
    dates <- dbGetQuery(con, 'SELECT * FROM dates')
    extra <- dbGetQuery(con, "SELECT * from extra")
    
    match <- match(files$out_filename, dates$out_filename)
    files$dates <- dates$DATE_sign[match]
    match <- match(files$out_filename, extra$out_filename)
    files$re <- extra$re[match]
    files$sign <- extra$sign[match]
    files$n_words <- extra$n_words[match]
    
    upload <- rbind(upload, files)
    dbDisconnect(con)
    rm(con)
  }
  write.csv(upload, upload_file, row.names = F)
}

### which letters are IPO-related
upload_ipo <- upload[CIK %in% ipo$Cik_SDC]
m <- match(upload_ipo$CIK, ipo$Cik_SDC)
upload_ipo[, `:=` (Issue_date = ymd(ipo$Issue_date[m]), Filing_date = ymd(ipo$Filing_date[m]), 
                   SEC_file_number = ipo$SEC_file_number[m], Form1 = ipo$Form1[m], dates = ymd(dates),
                   S1_types = ipo$S1_types[m], S1_dates = ipo$S1_dates[m])]

### Letters after IPO are not about IPO
upload_ipo <- upload_ipo[Issue_date + 2 >= dates]

### Find the date of original document
month_regex <- ".*(January|February|March|April|May|June|July|August|September|October|November|December)"
year_regex <- " (2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017).*"
upload_ipo[, re_date := NULL]
upload_ipo[, re_date := gsub(month_regex, "\\1", re), by = out_filename]
upload_ipo[, re_date := gsub(year_regex, "\\1", re_date), by = out_filename]
upload_ipo[, re_date := mdy(re_date)]
upload_ipo[, filing_to_original := Filing_date - re_date]
upload_ipo[, ipo_to_original := Issue_date - re_date]

### let's find what to keep
upload_ipo[, Keep := "Don't Know"]

### If film is mentioned than I need it
upload_ipo[, got_film := grepl(SEC_file_number,re), by = out_filename]
upload_ipo[got_film == 1, Keep := "Yes"]

### If it is confidential
conf_regex <- "(Confidential\\s+Draft)|(Draft\\s+Registration)"
upload_ipo[, conf := grepl(conf_regex, re, ignore.case = T)]
upload_ipo[conf == 1 & filing_to_original <= 150, Keep := "Yes"]

### Filings not IPO related
not_ipo_regex <- "(8-K)|(10-K)|(S-4)|(S-3)|(Form 10)|(Schedule 14)"
upload_ipo[, not_ipo := grepl(not_ipo_regex, re), by = out_filename]
upload_ipo[not_ipo == T, Keep := "No"]

### Filings IPO related 
upload_ipo[, ipo_form := grepl(Form1, re), by = out_filename]

### Either Confidetial of Form Should be mentioned
### (I've checked them by hand)
upload_ipo[Keep == "Don't Know" & conf == 0 & !ipo_form, Keep := "No"]

### If document between filing and IPO
### (I've checked them by hand)
upload_ipo[filing_to_original <= 5 & conf == 0 & Keep == "Don't Know", Keep := "Yes"]

### read previous hand-checked data
test <- fread("./Projects/SEC Letter Project/Data After Review/Additional Data/upload_ipo_test1.csv")

upload_ipo[Keep == "Don't Know" & out_filename %in% test$out_filename, Keep := "Yes"]
upload_ipo <- upload_ipo[Keep == "Yes"]

upload_ipo[, `:=` (filing_to_original = NULL, ipo_to_original = NULL, Keep = NULL, got_film = NULL,
                   not_ipo = NULL, ipo_form = NULL, S1_types = NULL, S1_dates = NULL)]

setkey(upload_ipo, CIK, dates)
upload_ipo[, letter_number := 1:.N, by = CIK]


### collect letters
letter <- NULL
for(cyear in 2004:2017)
{
  print(Sys.time())
  print(cyear)
  db_name <- paste0(sql_dir, "upload_", cyear, ".sqlite")
  con <- dbConnect(SQLite(), db_name)  
  files <- dbGetQuery(con, 'SELECT out_filename, Letter FROM content')
  files <- files[files$out_filename %in% upload_ipo$out_filename,]
  letter <- rbind(letter, files)
  dbDisconnect(con)
  rm(con)
}

match <- match(upload_ipo$out_filename, letter$out_filename)
upload_ipo$Letter <- letter$Letter[match]


trim <- function (x) gsub("^\\s+|\\s+$", "", x)

get.body <- function(x)
{
  x <- unlist(x)
  x <- unlist(str_split(x, "\n"))
  
  if(length(x) < 10) return(NA)
  #### finding start
  lines <- trimws(x, "left")
  start <- grep("^1\\.\\s", lines)
  if(length(start) > 1) start <- start[1]
  
  if(length(start) == 0)
  {
    start <- grep("Dear", x[1:100], ignore.case = T)
  }
  if(length(start) > 1) start <- start[1]
  if(length(start) == 0) start <- 1
  
  ### finding end
  end <- NULL
  end[1] <- "^Closing((\\sCommen)|$)"
  end[2] <- "As appropriate, please amend your registration statement"
  end[3] <- "As appropriate, please amend your filings in response"
  end[4] <- "As appropriate, please amend your filing "
  end[5] <- "^If you intend to respond to these comments with an amended"
  end[6] <- "We urge all persons who are responsible"
  end[7] <- "Please amend your registration statement in response"
  end[8] <- "We urge all persons who are responsible for the "
  end[9] <- "Provide a letter keying your responses"
  end[10] <- "File an amendment to the registration"
  end[11] <- "We will consider a written"
  end[12] <- "We direct your attention to "
  end <- c(end, "You may contact ", "If you have questions or comments", 
           "Please contact ", "Our preliminary review of your ", "Questions may be directed to ", "Sincerely ")
  
  x <- trim(x)
  for(i in 1:length(end))
  {
    end_index <- grep(end[i], x)
    if(length(end_index) > 0) 
    {
      if(end_index[1] > start) return(paste0(x[start:(end_index[1]-1)]))
    }
  }
  
  end_index <- length(x)
  return(paste0(x[start:end_index]))
}

get.n.items <- function(x)
{
  x <- unlist(x)
  x <- unlist(str_split(x, "\n"))
  
  ind <- grep("(\\d{1,3})\\.\\s", x, perl = T)
  if(length(ind) == 0) return(0)
  lines <- x[ind]
  lines <- trimws(lines)
  ind <- grep("^((\\d{1,3})\\.\\s)", lines, perl = T)
  if(length(ind) == 0) return(0)
  lines <- lines[ind]
  ind <- gsub("^(\\d{1,3}).*", "\\1", lines, perl = T)
  ind <- as.numeric(as.character(ind))
  ind <- ind[!is.na(ind)]
  if(length(ind) == 0) return(0)
  
  ### here I make sure that random xxx. would not be counted
  ### for large number I need at least some ordering
  ind <- sort(ind, decreasing = T)
  if(length(ind) > 2)
  {
    if(ind[1] - ind[2] > 3) ind <- ind[-1]
  }
  
  if(length(ind) > 2)
  {
    if(ind[1] - ind[2] > 3) ind <- ind[-1]
  }
  return(ind[1])
}


Body <- lapply(upload_ipo$Letter, get.body)

together <- function(x)
{
  x <- paste0(x, collapse = "\n")
  return(x)
}
Body <- sapply(Body, together)
upload_ipo$Body <- Body

items <- lapply(upload_ipo$Body, get.n.items)
upload_ipo$n_item <- unlist(items)

### Cleaning body letters
clean_letters <- function(Body)
{
  keep_enlish_words <- function(x)
  {
    require(qdapDictionaries)
    require(stringr)
    x <- unlist(str_extract_all(x, "\\w+"))
    x <- tolower(x)
    x <- x[x %in% GradyAugmented]
    x <- x[str_length(x) > 2]
    x <- paste(x, collapse = " ")
    return(x)
  }
  exclude_rare_words <- function(x)
  {
    require(stringr)
    x <- unlist(str_extract_all(x, "\\w+"))
    if(length(x) == 0) return(" ")
    x <- x[x %in% vocab]
    x <- paste(x, collapse = " ")
    return(x)
  }

  clean_files <- lapply(Body, keep_enlish_words)
  
  require(tm)
  vocab_tdm <- TermDocumentMatrix(Corpus(VectorSource(clean_files)))
  freq_matrix <- data.frame(ST = vocab_tdm$dimnames$Terms,
                            Freq = rowSums(as.matrix(vocab_tdm)))
  vocab <- freq_matrix$ST[freq_matrix$Freq > 5]
  
  clean_files <- sapply(clean_files, exclude_rare_words)
  return(clean_files)
}

upload_ipo$Clean_Body <- clean_letters(Body)

get.n.words <- function(x)
{
  x <- unlist(str_extract_all(x, "\\w+"))
  return(length(x))
}

n_words <- lapply(upload_ipo$Clean_Body, get.n.words)
upload_ipo$n_words_body <- unlist(n_words)

upload_ipo[letter_number %in% 1:6, list(n1 = mean(n_words_body),
           n2 = mean(as.numeric(as.character(n_words)))), by = letter_number]


### putting results into db
upload_ipo[, `:=`(Issue_date = as.character(Issue_date), 
                  Filing_date = as.character(Filing_date),
                  re_date = as.character(re_date))]
db_name <- "./Projects/SEC Letter Project/Data After Review/ipo_letters.sqlite"
con <- dbConnect(SQLite(), db_name)  

dbSendQuery(conn=con,
            "CREATE TABLE ipoletters
            (CIK TEXT, Company_Name TEXT, DATE_Filed TEXT, Filename TEXT,
            Link TEXT, out_filename TEXT, start_link TEXT, year TEXT,
            dates TEXT, re TEXT, sign TEXT, n_words TEXT, 
            Issue_date TEXT, Filing_date TEXT, SEC_file_number TEXT, Form1 TEXT,
            re_date TEXT, conf TEXT, letter_number TEXT, Letter TEXT,
            Body TEXT, n_item TEXT, Clean_Body TEXT, n_words_body TEXT)")

dbWriteTable(con, "ipoletters", upload_ipo, append = T)
dbDisconnect(con)
rm(con)

upload_ipo$Letter <- NULL
upload_ipo$Body <- NULL
write.csv(upload_ipo, "./Projects/SEC Letter Project/Data After Review/upload_ipo.csv", row.names = F)


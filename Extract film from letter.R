require(data.table)
require(lubridate)

ipo <- fread("/Users/orhahog/Dropbox/Projects/SEC Letter Project/Data After Review/ipo_20170422.csv")
dir_main <- "/Users/orhahog/Dropbox/Projects/SEC Letter Project/Data After Review/"
dir_upload <- "/Users/orhahog/Dropbox/Projects/SEC Letter Project/Data After Review/All Uploads/"
dir_upload_out <- "/Users/orhahog/Dropbox/Projects/SEC Letter Project/Data After Review/IPO Uploads/"
file_upload_csv <- "/Users/orhahog/Dropbox/Projects/SEC Letter Project/Data After Review/upload.csv"
### next three files were hand-checked
hand_check1 <- "/Users/orhahog/Dropbox/Projects/SEC Letter Project/Data After Review/Sorting SEC letters/check_between.csv"
hand_check2 <- "/Users/orhahog/Dropbox/Projects/SEC Letter Project/Data After Review/Sorting SEC letters/check_year_after.csv"
hand_names <- "/Users/orhahog/Dropbox/Projects/SEC Letter Project/Data After Review/Sorting SEC letters/SEC_Names.csv"
upload <- read.csv(file_upload_csv)
upload <- as.data.table(upload)
upload <- upload[!duplicated(out_filename)]

match <- match(upload$CIK, as.numeric(as.character(ipo$Cik_SDC)))

upload$ipo_date <- ymd(ipo$Issue_date[match])
upload$filing_date <- ymd(ipo$Filing_date[match])
upload$film_forms <- ipo$S1_types[match]
upload[, long_film := 0]
upload[grepl("(10-K)|(10-Q)|(8-K)", film_forms), long_film := 1]
upload[, dif_ipo := ymd(DATE_Filed) - ipo_date]
upload[, dif_filing := ymd(DATE_Filed) - filing_date]

upload <- upload[dif_filing >= -2*365 & dif_ipo <= 2*365]

file_regex <- "(File\\s+No)|(File\\sN)|(File\\s#)|(File\\s+number)|(Registration\\s+No)|(File No)|(File no)|(Filed No)|(File 0-)|(Fie\\s+No)|(Fi~\\s+No)|(Commission\\s+file\\s+number)|(File\\s+333-)|(Commission\\s+file\\s+#)|(File\\s+000-)|(Fine\\s+(n|N))"
conf_regex <- "(Confidential\\s+Draft)|(Draft\\s+Registration)"
setwd(dir_upload)

find_film <- function(i)
{
  letter <- unlist(letters[[i]])
  if(length(letter) <= 10) 
  {
    return(0)
  }
  letter <- letter[1:100]
  letter <- gsub("November", "XXX", letter, ignore.case = T)
  film_pos <- min(grep(file_regex, letter))
  if(is.infinite(film_pos)) return(NA)
  line <- letter[film_pos]
  line <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", line, perl=TRUE)
  return(line)
}
find_conf <- function(i)
{
  letter <- unlist(letters[[i]])
  if(length(letter) <= 10) 
  {
    return(0)
  }
  letter <- letter[1:100]
  
  film_pos <- min(grep(conf_regex, letter))
  if(is.infinite(film_pos)) return(NA)
  line <- letter[film_pos]
  line <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", line, perl=TRUE)
  return(line)
}
lines_re <- function(i)
{
  letter <- unlist(letters[[i]])
  if(length(letter) <= 10) 
  {
    return(0)
  }
  letter <- letter[1:100]
  pos <- min(grep("(Re:)|(RE:)", letter))
  if(is.infinite(pos)) return(NA)
  test <- letter[pos:min(pos+6, 100)]
  test <- paste0(test, collapse = " ")
  test <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", test, perl=TRUE)
  return(test)
}

step <- 50
N_step <- length(upload$out_filename) %/% step + 1
for(i in 1:N_step)
{
  closeAllConnections()
  print(i)
  start <- (i-1)*step + 1
  end <- min(i*step, length(upload$out_filename))
  ind <- start:end
  
  files_to_read <- as.character(upload$out_filename[ind])
  letters <- lapply(files_to_read, readLines)
  upload$file_pos[ind] <- sapply(1:length(ind), find_film)
  upload$conf_pos[ind] <- sapply(1:length(ind), find_conf)
  upload$re_lines[ind] <- sapply(1:length(ind), lines_re)
  closeAllConnections()
}

upload[, ipo.film := grepl(film, file_pos), by = out_filename]

upload[, Keep := "no"]
upload[ipo.film == T, Keep := "yes"]
upload[!is.na(conf_pos), Keep := "yes"]
hand_check <- read.csv(hand_check1)
upload[out_filename %in% hand_check$out_filename[hand_check$Keep == "yes"], Keep := "yes"]
hand_check <- read.csv(hand_check2)
upload[out_filename %in% hand_check$out_filename[hand_check$Keep == "yes"], Keep := "yes"]
upload <- upload[Keep == "yes"]
upload[, `:=`(Form_Type = NULL, GOT_TXT = NULL, long_film = NULL, dif_ipo = NULL, Filename = NULL,
              dif_filing = NULL, file_pos = NULL, ipo.film = NULL, Keep = NULL)]

if(!dir.exists(dir_upload_out)) dir.create(dir_upload_out)
file.copy(paste0(dir_upload, upload$out_filename),paste0(dir_upload_out, upload$out_filename))
write.csv(upload, paste0(dir_main, "upload_ipo_2004_2016.csv"), row.names = F)

setwd(dir_upload_out)

lines_sincerely <- function(letter)
{
  if(length(letter) <= 10) 
  {
    return(0)
  }
  letter <- gsub("\\t+", " ", letter)
  letter <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", letter, perl=TRUE)
  pos <- max(grep("(Sincerely)|(Regards,)|(Very truly yours)|(Respectflly submitted,)", letter, ignore.case = T))
  if(is.infinite(pos)) return(NA)
  test <- letter[pos:min(pos+6, length(letter))]
  test <- paste0(test, collapse = " ")
  test <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", test, perl=TRUE)
  test <- gsub("cc:.*", "", test, ignore.case = T)
  return(test)
}

for(i in 1:length(upload$CIK))
{
  closeAllConnections()
  print(i)

  file_to_read <- as.character(upload$out_filename[i])
  letter <- readLines(file_to_read)
  upload$signature[i] <- lines_sincerely(letter)
  closeAllConnections()
}

upload[, signature := gsub(" cc .*", "", signature, ignore.case = T)]
upload[, signature := gsub(" cc. .*", "", signature, ignore.case = T)]
upload[, signature := gsub(" via .*", "", signature, ignore.case = T)]

upload[, who_wrote := gsub("\\bfor\\b.*", "", signature)]
upload[, who_authored := "NA"]
upload[grepl("\\bfor\\b", signature), who_authored := gsub(".*\\bfor\\b", "", signature)]
upload[, `:=` (letter_author = "NA", letter_sender = "NA")]

name_codes <- read.csv(hand_names)
for(i in 1:length(name_codes$code))
{
  print(i)
  name_version <- name_codes[i,]
  name_version <- name_version[!is.na(name_version) & name_version != ""]
  name_regex <- paste0(name_version, collapse = ")|(")
  name_regex <- paste0("(", name_regex, ")")
  
  ind <- grep(name_regex, upload$who_authored)
  upload$letter_author[ind] <- as.character(name_codes$code[i])
  
  ind <- grep(name_regex, upload$who_wrote)
  upload$letter_sender[ind] <- as.character(name_codes$code[i])
}
upload[, `:=` (re_lines = NULL, signature = NULL, who_wrote = NULL, who_authored = NULL)]
upload[is.na(letter_author) | letter_author == "NA", letter_author := letter_sender]
write.csv(upload, paste0(dir_main, "upload_ipo_2004_2016.csv"), row.names = F)

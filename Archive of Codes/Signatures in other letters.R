require(data.table)

#db_name <- "C:/Users/ev99/YandexDisk/SEC Master/SEC_master.sqlite"
master_dir_upload <- "C:/Users/ev99/Desktop/"
file_upload_csv <- "C:/Users/ev99/Desktop/upload.csv"
hand_names <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/Sorting SEC letters/SEC_Names.csv"

upload <- fread(file_upload_csv)

get.files <- function(cyear)
{
  require(DBI)
  require(RSQLite)
  db_name <- paste0(master_dir_upload, "/UPLOAD sqlite/upload_", cyear, ".sqlite")
  con <- dbConnect(SQLite(), db_name)  

  files <- dbGetQuery(con, 'SELECT * FROM content')
  dbDisconnect(con)
  rm(con)
  return(files)
}

lines_sincerely <- function(letter)
{
  letter <- unlist(letter)
  letter <- unlist(strsplit(letter, "\n"))
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
  #test <- gsub("cc:.*", "", test, ignore.case = T)
  return(test)
}


n.words <- function(x)
{
  require(qdapDictionaries)
  require(stringr)
  x <- unlist(str_extract_all(x, "\\w+"))
  x <- tolower(x)
  x <- x[x %in% GradyAugmented]
  return(length(x))
}

upload[, signature := "NA"]
for(cyear in 2004:2017)
{
  print(Sys.time())
  print(cyear)
  
  letters <- get.files(cyear)
  N <- length(letters$CIK)
  print(c("Letters obtained:", N))
  
  for(i in 1:N)
  {
    if(i %% 100 == 0) print(i)
    ind <- which(upload$out_filename == letters$out_filename[i])
    upload$signature[ind] <- lines_sincerely(letters$Letter[i])
    upload$n_words[ind] <- n.words(letters$Letter[i])
  }
  write.csv(upload, paste0(master_dir_upload, "upload_with_signature.csv"), row.names = F)
}

upload[, signature := gsub(" cc:.*", "", signature, ignore.case = T)]
upload[, signature := gsub(" cc .*", "", signature, ignore.case = T)]
upload[, signature := gsub(" cc. .*", "", signature, ignore.case = T)]
upload[, signature := gsub(" via .*", "", signature, ignore.case = T)]
write.csv(upload, paste0(master_dir_upload, "upload_with_signature.csv"), row.names = F)

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

upload[letter_author == "NA", letter_author := letter_sender]
write.csv(upload, "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/upload_all_with_signature.csv", row.names = F)
#upload[, `:=` (re_lines = NULL, signature = NULL, who_wrote = NULL, who_authored = NULL)]

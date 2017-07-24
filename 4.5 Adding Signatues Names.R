### load large file
require(data.table)
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_all.csv")
find.names <- function(upload, name_codes)
{
  upload[, who_wrote := gsub("\\bfor\\b.*", "", sign)]
  upload[, who_authored := "NA"]
  upload[grepl("\\bfor\\b", sign), who_authored := gsub(".*\\bfor\\b", "", sign)]
  upload[, `:=` (letter_author = "NA", letter_sender = "NA")]
  
  #name_codes <- read.csv(hand_names)
  
  for(i in 1:length(name_codes$Code))
  {
    print(i)
    name_version <- name_codes[i,2:5]
    name_version <- name_version[!is.na(name_version) & name_version != ""]
    name_regex <- paste0(name_version, collapse = ")|(")
    name_regex <- paste0("(", name_regex, ")")
    
    ind <- grep(name_regex, upload$who_authored)
    upload$letter_author[ind] <- as.character(name_codes$Code[i])
    
    ind <- grep(name_regex, upload$who_wrote)
    upload$letter_sender[ind] <- as.character(name_codes$Code[i])
  }
  
  upload[letter_author == "NA", letter_author := letter_sender]
  return(upload)
}
require(xlsx)
names <- read.csv("./Projects/SEC Letter Project/Data After Review/Sorting SEC letters/Names and Offices.csv")
upload <- find.names(upload, names)

offices <- fread("./Projects/SEC Letter Project/Data After Review/Sorting SEC letters/All_CIKs.csv")
offices$AD_Office[offices$AD_Office == "2 & 3"] <- 23

upload$AD_Office <- offices$AD_Office[match(upload$CIK, as.numeric(as.character(offices$CIK)))]

tmp <- upload[upload$letter_author %in% names$Code[names$See.Comment == "Yes" ]]
tmp <- tmp[!letter_author %in% c("Andrew Mew", "Anne Nguyen Parker", "Cicely LaMothe", 
                                 "Gus Rodriguez", "Hugh West", "Jill Davis", "Joel Parker",
                                 "Karen Garnett", "Kevin Vaughn", "Kyle Moffatt", "Mark Kronforst",
                                 "Mark Shannon")]
require(lubridate)
tmp[, dates := mdy(dates)]
setkey(tmp, letter_author, dates)
tmp[, N := .N, by = letter_author]
tmp[, min_date := min(dates, na.rm = T), by = letter_author]
tmp[, max_date := max(dates, na.rm = T), by = letter_author]

write.csv(tmp, "tmp.csv", row.names = F)


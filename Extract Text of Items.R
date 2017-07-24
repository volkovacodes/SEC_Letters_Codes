require(DBI)
require(RSQLite)
require(data.table)
ipo <- data.table(read.csv("C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/ipo_20170620.csv"))
require(lubridate)
ipo[, Issue_date := ymd(Issue_date)]
ipo[, Issue_date >= ymd("20050512") & Issue_date <= ymd("20161231")]

db_name <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/ipo_letters.sqlite"
con <- dbConnect(SQLite(), db_name)
letters <- dbGetQuery(con, 'SELECT out_filename, CIK, Body, letter_number FROM ipoletters')
letters <- data.table(letters)
letters <- letters[letter_number == 1 & CIK  %in% ipo$Cik_SDC]

item_headers <- function(letter)
{
  letter <- unlist(strsplit(letter, "\n"))
  topic_regex <- "^[A-Z].*, page .*[0-9]$"
  ind <- grep(topic_regex, letter)
  return(letter[ind])
  
}

items <- sapply(letters$Body, item_headers)
items <- unlist(items)
test <- gsub(", page.*", "", items)
test <- as.data.frame(table(test))
test <- test[order(test$Freq, decreasing = T),]

### let's keep only top 25 items
test <- test[1:25,]
item_corpus <- function(letter)
{
  letter <- unlist(strsplit(letter, "\n"))
  topic_regex <- paste0("^", item_search,"(, page .*[0-9]$|$)")
  ind <- grep(topic_regex, letter)
  if(length(ind) == 0) return(NULL)
  start <- ind[1]
  
  topic_regex <- "^[A-Z].*, page .*[0-9]$"
  ind <- grep(topic_regex, letter)
  if(length(ind) == 0) return(NULL)
  ind <- c(ind, length(letter))
  ind <- ind[ind > start + 1]
  
  if(length(ind) == 0) return(NULL)
  end <- ind[1]
  out <- letter[start:(end-1)]
  out <- paste0(out, collapse = "\n")
  return(out)
}

path <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/Additional Data/Item Text/"
for(i in 1:25)
{
  print(i)
  print(Sys.time())
  item_search <- as.character(test$test[i])
  name_out <- paste0(path, i, ". ", item_search, ".rds")
  list <- lapply(letters$Body, item_corpus)
  list <- unlist(list)
  saveRDS(list, name_out)
}


#match <- match(frequencies$test, tmp$test)
#frequencies$n_fist_letters <- tmp$Freq[match]

#frequencies$Freq_all <- frequencies$Freq/3957
#frequencies$Freq_1 <- frequencies$n_fist_letters/1046

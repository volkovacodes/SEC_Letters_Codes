library(wayback)
library(tidyverse)

all_archives <- get_timemap("https://www.sec.gov/divisions/corpfin/cffilingreview.htm")
all_archives <- all_archives[grep("memento", all_archives$rel),]

require(RCurl)
require(XML)

all_tables <- NULL
for(i in 1:68)
{
  print(i)
  link <- all_archives$link[i]
  tables <- try(readHTMLTable(link))
  if(class(tables) == "try-error") next

  table <- tables[[9]]
  
  table$date <- all_archives$datetime[i]
  
  if(i == 69)
  {
    table <- tables[[9]]
    table$V1 <- NA
    table$date <- all_archives$datetime[i]
    colnames(table) <- colnames(all_tables)
  }
  try(all_tables <- rbind(all_tables, table))
}

for(i in 69:95)
{
  print(i)
  link <- all_archives$link[i]
  table <- try(readHTMLTable(link))
  x <- table[[7]]
  x <- x[-1,]
  x$V4 <- NA
  x$date <- all_archives$datetime[i]
  
  colnames(x) <- colnames(all_tables)
  all_tables <- rbind(all_tables, x)
}

write.csv(all_tables, "tables.csv", row.names = F)

out <-  all_tables[all_tables$Office == 1, c(5, 2:4)]
colnames(out)[2:4] <- paste0(1, "_", colnames(out)[2:4])
for(i in 2:11)
{
  x <- all_tables[all_tables$Office == i, 2:4]
  colnames(x) <- paste0(i, "_", colnames(x))
  out <- cbind(out, x)
}

x <- all_tables[all_tables$Office == 12, 2:5]
match <- match(out$date, x$date)
out$`12_Branch Chief` <- x$`Branch Chief`[match]
out$`12_Senior AssistantChief Accountant`<- x$`Senior AssistantChief Accountant`[match]
out$`12_Associate Chief Accountant` <- x$`Associate Chief Accountant`[match]
write.csv(out, "accouting.csv", row.names = F)

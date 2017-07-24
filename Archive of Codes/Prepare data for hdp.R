require(data.table)
ipo <- as.data.table(read.csv("./Projects/SEC Letter Project/Data After Review/ipo_20170525.csv"))
upload_ipo <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
### I've checked these files and they are either Corresp or other boilerplate
upload_ipo <- upload_ipo[n_item > 0 & !out_filename %in% c("1095291_0000000000-06-053374.txt","1095291_0000000000-06-053376.txt","1095291_0000000000-06-053378.txt")]

docs <- Corpus(VectorSource(upload_ipo$Clean_Body))
#remove stopwords
docs <- tm_map(docs, removeWords, c(stopwords("english"), "please", "page", "comment"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Stem document
require(SnowballC)
docs <- tm_map(docs,stemDocument)
#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames
rownames(dtm) <- upload_ipo$out_filename
to_hdp <- dtm2ldaformat(dtm)

out <- NULL
for(i in 1:length(dtm$dimnames$Docs))
{
  a <- to_hdp$documents[[i]]
  test <- paste0(a[1,], ":", a[2,], collapse = " ")
  test <- paste(length(a[1,]), test)
  out <- c(out, test)
}

keep_ind <- NULL
for(i in 1:250)
{
  ind <- sample(1:length(out), 300)
  keep_ind <- rbind(keep_ind, ind)
  write(out[ind], paste0("/Users/orhahog/hdp/corpuses/corpus_",i,".dat"))
}

save(list = ls(all.names = TRUE), file = "/Users/orhahog/hdp/My info/Gen_data.RData")




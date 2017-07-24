#require(RTextTools)
require(topicmodels)
require(tm)
require(data.table)
clean_letters_files <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/upload_ipo.csv"
upload <- fread(clean_letters_files, select = c("out_filename", "Clean_Body", "letter_number"))
upload <- upload[letter_number == 1]
docs <- Corpus(VectorSource(upload$Clean_Body))
docs <- tm_map(docs, removeWords, c(stopwords("english"), "please", "page", "comment"))
docs <- tm_map(docs, stripWhitespace)
require(SnowballC)
docs <- tm_map(docs,stemDocument)
dtm <- DocumentTermMatrix(docs)

set.seed(383)
ind <- sample(1:length(upload$Clean_Body), 200)
dtm <- dtm[ind,]

require(ldatuning)
print(Sys.time())
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 32, to = 60, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4L,
  verbose = TRUE
)
print(Sys.time())
FindTopicsNumber_plot(result)
print(result$topics[which.min(result$Arun2010)])
print(result$topics[which.min(result$CaoJuan2009)])
print(result$topics[which.max(result$Griffiths2004)])
print(result$topics[which.max(result$Deveaud2014)])

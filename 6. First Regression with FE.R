require(data.table)
require(lubridate)
require(psych)
require(data.table)
ipo <- data.table(read.csv("./Projects/SEC Letter Project/Data After Review/ipo_20170620.csv"))
wina <- 0.001
lwin <- function(x, a = wina) return(winsor(log(1+x),a))
w <- function(x, a = wina) return(winsor(x,a))


ipo1 <- ipo[ymd(Filing_date) > ymd("20050512")  & ymd(Issue_date) < ymd("20151231")] 
ipo1[, `:=` (author_count = .N), by = letter_author]
ipo1 <- ipo1[author_count >= 5]
ipo1[, `:=` (log_sale = lwin(sale), n_segments = w(n_segments), age = lwin(Year - founding_year), UW_rank = w(UW_rank), price_update = w(price_update), F_score = w(F_score),law_rank = w(Law.firm.5years),
             log_S1_words = lwin(S1_words), S1_un = w(S1_uncertanty), log_words = lwin(Upload1_words), log_n_letters = lwin(n_letters), log_n_letters_after = lwin(n_letters_after_price),
             registration_length = w(registration_length), log_registration_length = lwin(registration_length), ret_publish = w(ret_publish))]

ipo1[, disc_pos := 0]
ipo1[ ret_publish > quantile(ret_publish, 0.5), disc_pos := 1]
ipo1[, letter_author_disc := paste0(letter_author, disc_pos)]
### OLS results
end <- " ~  log_sale + n_segments + age +  UW_rank + law_rank + VC + JOBS + log_S1_words + 
S1_un + log_n_letters*disc_pos|FF_48 + Year + AD_Office|0|FF_48"
end_2SLS <- " ~  log_sale + n_segments + age +  UW_rank + law_rank + VC + JOBS + 
log_S1_words + S1_un |FF_48 + Year + AD_Office|(log_n_letters + I(log_n_letters*disc_pos) ~ letter_author)|FF_48"
dep_vars <- c("log_registration_length","price_update", "IR", "insider_sales", "vol", "I(ret_abn_365 - ret_abn_175)", "I(ret_abn_365)", "ret_publish")
col_labs <- c("Reg Len", "Prc Upd", "IR", "Ins Sale", "Vol",  "AbRet(0.5)", "AbRet(1)", "CAR, Pub")
var_labs <- c("Sales", "Number of Segments", "Age", "UW Rank", "Law Firm Rank", "VC Dummy",
               "JOBS Act Dummy", "Prospectus Length", "Prospectus Uncertanty", "N SEC Letters, log")
fe_lines <- list(c("Year FE", rep("YES",length(col_labs))), c("Ind (FF-48) FE", rep("YES",length(col_labs))), c("SEC Office FE", rep("YES",length(col_labs))))
require(lfe)
my_felm <- function(x) felm(as.formula(unlist(x)), data = ipo1)
require(stargazer)

line <- lapply(dep_vars, function(x) return(paste0(x, end)))
line_2sls <- lapply(dep_vars, function(x) return(paste0(x, end_2SLS)))

model <- lapply(line, my_felm)
model_2sls <- lapply(line_2sls, my_felm)

Fstat <- round(model_2sls[[1]]$stage1$iv1fstat$log_n_letters[5],2)
model_2sls[[1]]$stage1$cpval

model_2sls[[1]]$stage1$STATS
fe_lines_2sls <- list(c("F-stat", rep(Fstat, length(col_labs))),fe_lines[[1]], fe_lines[[2]], fe_lines[[3]])
out_type <- "latex"
fn_size <- "footnotesize"
table1 <- stargazer(model, type = out_type,  omit.stat = c("ser", "f"), 
          column.labels = col_labs, dep.var.labels.include = F, title = "OLS Results", add.lines = fe_lines, font.size = fn_size)

table2 <- stargazer(model_2sls, type = out_type, omit.stat = c("ser", "f"), 
          column.labels = col_labs, dep.var.labels.include = F, title = "2SLS Results", add.lines = fe_lines_2sls, font.size = fn_size)


start <- readLines("./R codes/table_start.tex")
file <- c(start, table1, table2, "\\end{document}")

write(file, "regressions_20170606.tex")
require(tools)
texi2pdf("regressions_20170606.tex", clean = T)


table1 <- stargazer(model, type = "text",  omit.stat = c("ser", "f"), 
                    column.labels = col_labs, dep.var.labels.include = F, title = "OLS Results", add.lines = fe_lines, font.size = fn_size)

table2 <- stargazer(model_2sls, type = "text", omit.stat = c("ser", "f"), 
                    column.labels = col_labs, dep.var.labels.include = F, title = "2SLS Results", add.lines = fe_lines_2sls, font.size = fn_size)

english = c(90, 80, 60, 70)
math = c(50, 60, 100, 20)
df_midterm = data.frame(english, math)
df_midterm
class = c(1, 1, 2, 2)
df_midterm = data.frame(english, math, class)
mean(df_midterm$english)
mean(df_midterm$math)

fruits = data.frame(product = c('사과', '딸기', '수박'),
                    price = c(1800, 1500, 3000),
                    sale = c(24, 38, 13))
(fruits$sale)%/%2
# ------------------------------------------------------------------
install.packages('ggplot2')
install.packages('readxl')
library(ggplot2)
library(readxl)
df_exam <- read_excel('source/excel_exam.xlsx')
mean(df_exam$math)
mean(df_exam$english)
mean(df_exam$science)

df_exam_novar <- read_excel('source/excel_exam_novar.xlsx')
df_exam_novar
# 컬럼명 False
df_exam_novar <- read_excel('source/excel_exam_novar.xlsx', col_names = F)
df_exam_novar

df_exam_sheet <- read_excel('source/excel_exam_sheet.xlsx', sheet=3)
df_csv_exam <- read.csv('source/csv_exam.csv')
df_csv_exam

df_csv_exam <- read.csv('source/csv_exam.csv', stringsAsFactors = F)
df_csv_exam

df_midterm = data.frame(english = c(90, 80, 60, 70),
                        math = c(50, 60, 100, 20),
                        class = c(1, 1, 2, 2))
df_midterm

write.csv(df_midterm, file = 'df_midterm.csv')
save(df_midterm, file = 'df_midterm.rda')
rm(df_midterm)
rm(x)
rm(var1)
rm(str1, var2)
load('df_midterm.rda')
df_midterm

exam <- read.csv('source/csv_exam.csv')
head(exam)
tail(exam)
View(exam)
dim(exam)
str(exam)
summary(exam)

head(exam, 10)

mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
tail(mpg)
View(mpg)
view(mpg)
View(head(mpg))
View(summary(mpg))

dim(mpg)

str(mpg)
?mpg

df_raw = data.frame(var1 = c(1, 2, 1),
                    var2 = c(2, 3, 2))
install.packages('dplyr')
library(dplyr)

df_new <- df_raw
df_new
df_new <- rename(df_new, v2 = var2)
df_new

df_mpg = as.data.frame(ggplot2::mpg)
df_mpg_copy <- df_mpg
head(df_mpg_copy)
df_mpg_copy <- rename(df_mpg_copy, city = cty, highway = hwy)
head(df_mpg_copy, 3)
head(df_mpg)

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df$var_sum <- df$var1 + df$var2
df
df['var_mean'] <- (df['var1'] + df['var2'])/2
df

mpg
mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg, 3)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total)
boxplot(mpg$total)

mpg$test <- ifelse(mpg$total >= 20, 'pass', 'fail')
head(mpg)

table(mpg$test)

qplot(mpg$test)

mpg$test2 <- ifelse(mpg$total >= 30, 'A', 
                    ifelse(mpg$total >= 20, 'B', 'C'))
head(mpg, 20)


qplot(mpg$test2)

mpg$test3 <- ifelse(mpg$total >= 30, 'A', 
                    ifelse(mpg$total >= 25, 'B', 
                           ifelse(mpg$total >= 20, 'C', 'D')))
head(mpg)


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

mpg
library(ggplot2)
df <- as.data.frame(ggplot2::midwest)
df

summary(df)
boxplot(df$PID)

View(head(df))
hist(df$popasian)


df$per_asian <- df$popasian / df$poptotal
hist(df$per_asian)

mean_per_asian <- mean(df$per_asian)
df$target <- ifelse(df$per_asian > mean_per_asian, 'large', 'small')
qplot(df$target)

# 데이터 전처리

# dplyr 라이브러리
library(dplyr)
exam <- read.csv('source/csv_exam.csv')
head(exam)
getwd()

head(exam)
library(ggplot2)

exam %>% filter(english == 90)

exam %>% filter(class == 2)

exam %>% filter(class != 2)

exam %>% filter(class != 2 | math >= 80)

exam %>% filter(class != 2 & english > 80)

exam %>% filter(class == 1 | class == 2 | class ==3)
exam %>% filter(class %in% c(1, 3, 5))

class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2) 

mean(class1$math)
mean(class2$math)
mpg
# 문제 1
mean((mpg %>% filter(displ <= 4))$hwy)
mean((mpg %>% filter(displ >= 5))$hwy)

# 문제 2
mean((mpg %>% filter(manufacturer == 'audi'))$cty)
mean((mpg %>% filter(manufacturer == 'toyota'))$cty)

# 문제 3
mean((mpg %>% filter(manufacturer %in% c('chevrolet', 'ford', 'honda')))$hwy)


exam %>% select(math)

exam %>% select('class', math, english)

exam %>% select(-'math')

View(exam %>% select(-math, -english))

(exam %>% filter(class == 1))$english

exam %>% 
    filter(class == 1) %>% 
    select(english)

exam %>% 
    select(id, math) %>% 
    tail

mpg_tmp <- mpg %>% select(class, cty)
mean((mpg_tmp %>% filter(class == 'suv'))$cty)
mean((mpg_tmp %>% filter(class == 'compact'))$cty)


exam %>% arrange(desc(math))
exam %>% arrange(desc(math, class))

mpg %>% 
    filter(manufacturer == 'audi') %>% 
    arrange(desc(hwy)) %>% 
    head(5)

exam %>% 
    mutate(total = math + english + science) %>% 
    head
exam

exam %>% 
    mutate(total = math + english + science,
           mean = (total) / 3) %>% 
    head

exam %>% 
    mutate(test = ifelse(science>=60, 'pass', 'fail')) %>% 
    head

exam %>% 
    mutate(total = math + english + science,
           test = ifelse(science>=60, 'pass', 'fail')) %>% 
    arrange(total) %>% 
    head

mpg_copy <- mpg
mpg_copy <- mpg_copy %>% 
    mutate(total = hwy + cty)

mpg_copy <- mpg_copy %>% 
    mutate(mean = total / 2)
mpg_copy %>% 
    arrange(desc(mean)) %>% 
    head(3)

mpg %>% 
    mutate(mean = (hwy+cty)/2) %>% 
    arrange(desc(mean)) %>% 
    head(3)

exam %>% summarise(mean_math = mean(math))

exam %>% 
    group_by(class) %>% 
    summarize(mean_math = mean(math))

exam %>% 
    group_by(class) %>% 
    summarise(mean_math = mean(math),
              sum_math = sum(math),
              median_math = median(math),
              sd_math = sd(math),
              n = n())

mpg %>% 
    group_by(manufacturer, drv) %>% 
    summarise(mean_city = mean(cty)) %>% 
    head(10)

mpg %>% 
    group_by(manufacturer) %>% 
    filter(class == 'suv') %>% 
    mutate(tot = (cty+hwy)/2) %>% 
    summarise(mean_tot = mean(tot)) %>% 
    arrange(desc(mean_tot)) %>% 
    head(5)

mpg %>% 
    group_by(class) %>% 
    summarise(mean_cty = mean(cty)) %>% 
    arrange(desc(mean_cty))

mpg %>% 
    group_by(manufacturer) %>% 
    summarise(mean_hwy = mean(hwy)) %>% 
    arrange(desc(mean_hwy)) %>% 
    head(3)
mpg

mpg %>% 
    filter(class == 'compact') %>% 
    group_by(manufacturer) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n))

test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))

test1
test2
total = left_join(test1, test2, by='id')
total

name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c('kim', 'lee', 'park', 'choi', 'jung'))
name
exam_new <- left_join(exam, name, by='class')
exam_new

group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_all <- bind_rows(group_a, group_b)
group_all

fuel <- data.frame(f1 = c('c', 'd', 'e', 'p', 'r'),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel
View(mpg)

fuel
names(fuel)[1] <- c('fl')
fuel

mpg <- left_join(mpg, fuel, by='fl')
mpg

mpg %>% 
    select('model', 'fl', 'price_fl') %>% 
    head(5)

midwest <- as.data.frame(ggplot2::midwest)
View(midwest)

midwest <-midwest %>% 
    mutate(childratio = (1 - popadults / poptotal) * 100)
View(midwest)

midwest %>% 
    arrange(desc(childratio)) %>% 
    select(county, childratio) %>% 
    head(5)

midwest <- midwest %>% 
    mutate(childeratio_grade = 
               ifelse(childratio >= 40, 'large',
                      ifelse(childratio >= 30, 'middle', 'small')))
midwest %>% 
    group_by(childeratio_grade) %>% 
    summarise(n=n())
table(midwest$childeratio_grade)

midwest %>% 
    mutate(asianratio = popasian / poptotal * 100) %>% 
    arrange(asianratio) %>% 
    select(state, county, asianratio) %>% 
    tail(10)

df <- data.frame(sex = c('M', 'F', NA, 'M', 'F'),
                score = c(5, 4, 3, 4, NA))
df
is.na(df)

table(is.na(df))
table(is.na(df[1]))
table(is.na(df['sex']))
table(is.na(df$sex))

sum(df$score)

df %>% filter(is.na(score))

df %>% filter(!is.na(score))

df_nomiss <- df %>% filter(!is.na(score))
sum(df_nomiss$score)

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss
df_nomiss2 <- na.omit(df)
df_nomiss2

mean(df$score, na.rm = T)
sum(df$score, na.rm = T)

exam <- read.csv('source/csv_exam.csv')
exam[c(3, 8, 15), 'math'] <- NA
exam

exam %>% summarise(mean_math = mean(math))
exam %>% summarise(mean_math = mean(math, na.rm = T))
exam %>% summarise(mean_math = mean(math, na.rm = T),
                   median_math = median(math, na.rm = T),
                   sd_math = sd(math, na.rm = T))

mean(exam$math, na.rm = T)
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))

mean(exam$math)

mpg <- as.data.frame(ggplot2::mpg)

mpg[c(65, 124, 131, 153, 212), 'hwy'] <- NA
table(is.na(mpg$drv))
table(is.na(mpg$hwy))
mpg %>% 
    filter(!is.na(mpg$hwy)) %>% 
    group_by(drv) %>% 
    summarise(mean_hwy = mean(hwy))

outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier
table(outlier$sex)
table(outlier$score)

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

outlier %>% 
    filter(!is.na(sex) & !is.na(score)) %>% 
    group_by(sex) %>% 
    summarise(mean_score = mean(score))
66666666666
boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats


mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
    group_by(drv) %>% 
    summarize(mean_hwy = mean(hwy, na.rm=T))

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10, 14, 58, 93), 'drv'] <- 'k'
mpg[c(29, 43, 129, 203), 'cty'] <- c(3, 4, 39, 42)

table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c('4', 'f', 'r'), mpg$drv, NA)
table(mpg$drv)

boxplot(mpg$cty)
boxplot(mpg$cty)$stats

mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
table(is.na(mpg$cty))

mpg %>% 
    filter(!is.na(cty) & !is.na(drv)) %>% 
    group_by(drv) %>% 
    summarise(mean_cty = mean(cty))

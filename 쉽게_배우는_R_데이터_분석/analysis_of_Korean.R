# analysis of Korean
install.packages('foreign')

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

# Load data(spss file extension)
raw_welfare <- read.spss(file = 'source/Koweps_hpc10_2015_beta1.sav',
                         to.data.frame = T)
# Copy
welfare <- raw_welfare

# exploration
head(welfare)
View(tail(welfare))
dim(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
View(welfare)

# - - - - - - - - - - - - - -
# compare income by sex

class(welfare$sex)
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 1, 'male', 'female')
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
boxplot(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)

summary(welfare$income)

welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

summary(welfare$income)

sex_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(sex) %>% 
    summarise(mean_income = mean(income))
sex_income
ggplot(data=sex_income, aes(x=sex, y=mean_income)) + geom_col()

# - - - - - - - - - - - - - -
# compare income by age

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)

age_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(age) %>% 
    summarise(mean_income = mean(income))
ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_line()


# - - - - - - - - - - - - - -
# compare income by age group
welfare <- welfare %>% 
    mutate(ageg = ifelse(welfare$age < 30, 'young',
                         ifelse(welfare$age < 60, 'middle', 'old')))
table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(ageg) %>% 
    summarise(mean_income = mean(income))
ageg_income

ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + geom_col()

ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + 
    geom_col() +
    scale_x_discrete(limits = c('young', 'middle', 'old'))

# - - - - - - - - - - - - - -
# compare income by age group and sex

sex_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(ageg, sex) %>% 
    summarise(mean_income = mean(income))
View(sex_income)

ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex)) + 
    geom_col() +
    scale_x_discrete(limits = c('young', 'middle', 'old'))

ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex)) + 
    geom_col(position = 'dodge') +
    scale_x_discrete(limits = c('young', 'middle', 'old'))

# - - - - - - - - - - - - - -
# compare income by age and sex

sex_age <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(age, sex) %>% 
    summarise(mean_income = mean(income))
ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex)) +
    geom_line()
    
# - - - - - - - - - - - - - -
# compare income by age and sex

class(welfare$code_job)
table(welfare$code_job)

list_job <- read_excel('source/Koweps_Codebook.xlsx', col_names=T, sheet=2)
head(list_job)

dim(list_job)

welfare <- left_join(welfare, list_job, id='code_job')
welfare %>% 
    filter(!is.na(code_job)) %>% 
    select(code_job, job) %>% 
    head(10)

job_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(job) %>% 
    summarise(mean_income = mean(income))
head(job_income)

top10 <- job_income %>% 
    arrange(desc(mean_income)) %>% 
    head(10)
top10

ggplot(data=top10, aes(x=reorder(job, mean_income), y=mean_income)) + 
    geom_col() +
    coord_flip()

bottom10 <- job_income %>% 
    arrange(mean_income) %>% 
    head(10)
bottom10
ggplot(data=bottom10, aes(x=reorder(job, -mean_income),
                          y=mean_income)) +
    geom_col() +
    coord_flip() +
    ylim(0, 850)

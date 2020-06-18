# Statistical hypothesis test

# t.test
mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)
mpg_diff <- mpg %>% 
    select(class, cty) %>% 
    filter(class %in% c('compact', 'suv'))

head(mpg_diff)
table(mpg_diff$class)

t.test(data=mpg_diff, cty ~ class, var.equal=T)

mpg_diff <- mpg %>% 
    filter(fl %in% c('r', 'p')) %>% 
    select(fl, cty)
t.test(data=mpg_diff, cty ~ fl, var.equal=T)

# correlation analysis
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

# heatmap
head(mtcars)

car_cor <- cor(mtcars)
round(car_cor, 2)

library(corrplot)

corrplot(car_cor)

corrplot(car_cor, method='number')

col <- colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD', '#4477AA'))

corrplot(car_cor,
         method='color',
         col=col(200),
         type='lower',
         order='hclust',
         addCoef.col='black',
         tl.col='black',
         tl.srt=45,
         diag=F)
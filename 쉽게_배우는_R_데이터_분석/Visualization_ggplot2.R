# Data Visualization

## Make graphs with ggplot2
library(ggplot2)
# Set displ as x axis,  hwy as y axis
ggplot(data = mpg, aes(x=displ, y=hwy))
# Add scatterplot with 'geom_point' func
ggplot(data = mpg, aes(x=displ, y=hwy)) + geom_point()
# Set range of axis with xlim, ylim func
ggplot(data = mpg, aes(x=displ, y=hwy)) + geom_point() + xlim(3, 6)
# Imporve Code readability
ggplot(data=mpg, aes(x=displ, y=hwy)) +
    geom_point() +
    xlim(3, 6) +
    ylim(10, 30)
# - - - - - - - - -
# Q1
ggplot(data=mpg, aes(x=cty, y=hwy)) +
    geom_point()

# Q2
ggplot(data=ggplot2::midwest, aes(x=poptotal, y=popasian)) +
    geom_point() +
    xlim(0, 500000) +
    ylim(0, 10000)
# - - - - - - - - -
library(dplyr)

df_mpg <- mpg %>% 
    group_by(drv) %>% 
    summarise(mean_hwy = mean(hwy))
df_mpg

# Make barplot with goem_col()
ggplot(data=df_mpg, aes(x=drv, y=mean_hwy)) +
    geom_col()

# Arrange var with reorder
ggplot(data=df_mpg, aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) +
    geom_col()

# geom_bar()
ggplot(data=mpg, aes(x=drv)) + geom_bar()

ggplot(data=mpg, aes(x=hwy)) + geom_bar()

# - - - - - - - - -
# Q1
mpg
df_mpg <- mpg %>% 
    filter(class=='suv') %>% 
    group_by(manufacturer) %>% 
    summarise(mean_cty = mean(cty)) %>% 
    arrange(desc(mean_cty)) %>% 
    head(5)

ggplot(data=df_mpg, aes(x=reorder(manufacturer, -mean_cty), 
                        y=mean_cty)) + 
    geom_col()

# Q2
ggplot(data=mpg, aes(x=class)) + geom_bar()
# - - - - - - - - -

# Make line graph
ggplot(data=economics, aes(x=date, y=unemploy)) + geom_line()

ggplot(data=economics, aes(x=date, y=psavert)) + geom_line()

# Make box plot
ggplot(data=mpg, aes(x=drv, y=hwy)) + geom_boxplot()

df_mpg <- mpg %>% 
    filter(class %in% c('compact', 'subcompact', 'suv'))
ggplot(data=df_mpg, aes(x=class, y=cty)) + geom_boxplot()


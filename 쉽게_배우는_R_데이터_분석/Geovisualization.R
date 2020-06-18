# geovisualization
## crime in America
library(ggiraphExtra)
str(USArrests)
head(USArrests)

library(tibble)
crime <- rownames_to_column(USArrests, var='state')
crime$state <- tolower(crime$state)
str(crime)

library(ggplot2)
library(maps)
states_map <- map_data('state')

states_map

library(mapproj)
ggChoropleth(data=crime, 
             aes(fill=Murder,
                 map_id=state),
             map=states_map)

ggChoropleth(data=crime,
             aes(fill=Murder,
                 map_id=state),
             map=states_map,
             interactive = T)
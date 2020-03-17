



library(tidyverse)


######### CHALLENGE ########
##Question1 ##
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?


bom_data <- read_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Data-school-week2/BOM_data.csv")


bom_stations <- read_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Data-school-week2/BOM_stations.csv")


#need to separate temperature column


head(bom_date)

bom_data_sep <- 
            bom_data %>% 
              separate(Temp_min_max, into = c('tmp_min', 'tmp_max'), sep = '/')    #splitting the combined max&min tmp
              filter(tmp_min != '-', tmp_max != '-', Rainfall != '-', Solar_exposure !='-')  #excluding all rows with missing values


answerQ1 <- 
            bom_data_sep %>% 
              group_by(Station_number) %>%  #NOOOOO '' when grouping
              summarise('Day' = n())

  

view(answerQ1)

##Question2 ##
#Which month saw the lowest average daily temperature difference?



  
  
  
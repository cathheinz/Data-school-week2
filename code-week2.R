
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
              separate(Temp_min_max, into = c('tmp_min', 'tmp_max'), sep = '/') %>%    #splitting the combined max&min tmp
              filter(tmp_min != '-', tmp_max != '-', Rainfall != '-', Solar_exposure !='-')  #excluding all rows with missing values

  #filter (is.na(tmp_min, tmp_max)) can only be used when a dataframe/tibble has NA in it - one can replace eg - with NA
              
            bom_data_sep %>% 
              group_by(Station_number) %>%  #NOOOOO '' when grouping
              summarise('Day' = n()) %>% 
              write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ1.csv")


##Question2 ##
#Which month saw the lowest average daily temperature difference?

bom_data_difftmp <- 
   bom_data_sep %>%   
        mutate_at(vars(tmp_min, tmp_max, Rainfall, Solar_exposure), as.numeric) %>%  # change values/measurements to numbers
        mutate(diff_daily_tmp = tmp_max - tmp_min) %>%   # create tmp_diff column
        group_by(Month) %>% 
        summarize(mean_month_tmp = mean(diff_daily_tmp)) %>% 
        write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ2.csv")


## Question 3 ##
# Which state saw the lowest average daily temperature difference?



  
  
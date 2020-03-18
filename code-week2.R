
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
              filter(tmp_min != '-', tmp_max != '-', Rainfall >= 0)  #excluding  missing values

  #filter (is.na(tmp_min, tmp_max)) can only be used when a dataframe/tibble has NA in it - one can replace eg - with NA
              
            bom_data_sep %>% 
              group_by(Station_number) %>%  #NOOOOO '' when grouping
              summarise('Day' = n()) %>% 
              write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ1.csv")


##Question2 ##
#Which month saw the lowest average daily temperature difference?

bom_data_avrdaytmp <- 
   bom_data_sep %>%   
        mutate_at(vars(tmp_min, tmp_max, Rainfall), as.numeric) %>%  # change values/measurements to numbers
        mutate(diff_daily_tmp = tmp_max - tmp_min) %>%   # create tmp_diff column
        group_by(Month) %>% 
        summarize(mean_month_tmp = mean(diff_daily_tmp)) %>% 
        write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ2.csv")

view(bom_data_avrdaytmp)




## Question 3 ##
# Which state saw the lowest average daily temperature difference?
  # get the 'bom-stations' and BOM_data files on forms so they can be joined
  # join somehow... with 'bom_data' file


#bom_station file ready to be joined
bom_stations_join <- 
      bom_stations  %>% 
        gather( key = Station_number, value = measure, -info) %>% # the -info seems to let R know to exclude the info column
        spread( key = info, value = measure )

#getting the bom_data file ready to be joined
bom_data_join <- 
      bom_data_sep %>% 
        mutate_at(vars(tmp_min, tmp_max, Rainfall), as.numeric) %>%
        group_by(Station_number) %>% 
        summarise( mean_tmp_min = mean(tmp_min), mean_tmp_max = mean(tmp_max), mean_Rainfall =  mean(Rainfall) ) %>% 
        mutate_at(vars(Station_number), as.character)

#joining the two new tibbles, both 20x4/8
bom_joined <- 
      inner_join(bom_stations_join, bom_data_join, by = 'Station_number')

# seems like all join-functions work in this instance
  # left_join(bom_stations_join, bom_data_join, by = 'Station_number')
  # full_join(bom_stations_join, bom_data_join, by = 'Station_number')  


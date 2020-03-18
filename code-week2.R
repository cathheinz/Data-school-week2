
library(tidyverse)


######### CHALLENGE ########
##Question1 ##
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?


bom_data <- read_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Data-school-week2/BOM_data.csv")


bom_stations <- read_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Data-school-week2/BOM_stations.csv")


#need to separate temperature column


head(bom_data)
tail(bom_data)

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
      
  bom_joined %>% 
      group_by(state) %>% 
      summarise(mean_state_tmp_min = mean(mean_tmp_min), mean_state_tmp_max = mean(mean_tmp_max)) %>% 
      mutate("mean_tmp_diff_state" = mean_state_tmp_max - mean_state_tmp_min ) %>% 
      arrange(mean_tmp_diff_state) %>% 
      write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ3.csv")


# seems like all join-functions work in this instance
  # left_join(bom_stations_join, bom_data_join, by = 'Station_number')
  # full_join(bom_stations_join, bom_data_join, by = 'Station_number')  



## Question 4 ##
# Does the westmost (lowest longitude) or eastmost (highest longitude) weather station
  # in our dataset have a higher average solar exposure?

# create new bom_data with solar exposure
  
bom_data_solar <- 
  bom_data %>% 
    filter(Solar_exposure != '-')  %>%   #only including solar exposure, doesn't matter if no tmp or rainfall
    group_by(Station_number) %>% 
    mutate_at(vars(Solar_exposure), as.numeric) %>%
    summarise(mean_solar = mean(Solar_exposure))  

#join new solar data (only 19 stations) with the file that has longitude (and 20 stations)

bom_stat_solar <- 
  bom_data_solar %>% 
  mutate_at(vars(Station_number), as.character) %>%
  full_join(bom_stations_join, bom_data_solar, by = 'Station_number')

#left_join(bom_stations_join, bom_data_solar, by = 'Station_number') ## works too but 20 rows must be on left!

bom_date_solar_lon <- 
  bom_stat_solar %>%
    arrange(desc(lon)) %>% 
    select(Station_number, lon, mean_solar, state) %>% 
    write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ4.csv")
    
    
    


    

















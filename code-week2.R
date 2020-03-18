
# read in packages to be used
library(tidyverse)

#read in data the two files to be used for the challenge
bom_data <- read_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Data-school-week2/BOM_data.csv")

bom_stations <- read_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Data-school-week2/BOM_stations.csv")

# looking at the data
head(bom_data)
tail(bom_data)

## Challenge1 #####################################################################################################
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?
  #need to separate temperature column containing both min and max temperature
      # tidy data according to min tmp, max tmp and rainfall
      # count rows with data

#make tidy tibble that answers the question
bom_data_sepCH1 <- 
      bom_data %>% 
        separate(Temp_min_max, into = c('tmp_min', 'tmp_max'), sep = '/') %>%         #splitting the combined max&min tmp
        filter(tmp_min != '-', tmp_max != '-', Rainfall >= 0) %>%                     #excluding  missing values
        group_by(Station_number) %>%  
        summarise('Day' = n()) %>% 
        write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ1.csv")

# view table
view(bom_data_sep)

#####NOTES
    # filter (is.na(tmp_min, tmp_max)) can only be used when a dataframe/tibble has NA in it - one can replace eg - with NA
    # checked in excel that '-' was indicating all missing values
    # NOOOOO quatiation marks '' when grouping. No error message is given


## Challenge2 ############################################################################################
#Which month saw the lowest average daily temperature difference?
  # from the tidy tibble masssage the new tibble 

# get the tidy tibble
bom_data_sep <- 
  bom_data %>% 
  separate(Temp_min_max, into = c('tmp_min', 'tmp_max'), sep = '/') %>%    #splitting the combined max&min tmp
  filter(tmp_min != '-', tmp_max != '-', Rainfall >= 0)

# organise tibble that answers the question
bom_data_avrdaytmp <- 
   bom_data_sep %>%   
        mutate_at(vars(tmp_min, tmp_max, Rainfall), as.numeric) %>%   # change values/measurements to numbers so can calculate
        mutate(diff_daily_tmp = tmp_max - tmp_min) %>%                # create tmp_diff column
        group_by(Month) %>%   
        summarize(mean_month_tmp = mean(diff_daily_tmp)) %>%   
        arrange(mean_month_tmp) %>% 
        write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ2.csv")

view(bom_data_avrdaytmp)

#####NOTES
      # consider when to safe tidy tibble, here had to make again because piped away in Challenge1
      # be on top of character, numeric. Check in the right top window under Environment


## Challenge 3 ########################################################################################
# Which state saw the lowest average daily temperature difference?
  # get the 'bom-stations' and bom_data files on forms so they can be joined
  # join somehow... with 'bom_data' file

# bom_station file ready in a 'joinable' form
bom_stations_join <- 
      bom_stations  %>% 
        gather( key = Station_number, value = measure, -info) %>%   # the -info seems to let R know to include the info column
        spread( key = info, value = measure )                       # to get the first original column (info) as headings, and sone station one row

# REUSE - tidy seperated bom_data tibble
bom_data_sep <- 
  bom_data %>% 
  separate(Temp_min_max, into = c('tmp_min', 'tmp_max'), sep = '/') %>%    #splitting the combined max&min tmp
  filter(tmp_min != '-', tmp_max != '-', Rainfall >= 0)


# getting the bom_data file to fit the one station one row
bom_data_join <- 
      bom_data_sep %>% 
        mutate_at(vars(tmp_min, tmp_max, Rainfall), as.numeric) %>%   # change the character to numeric to calculate means
        group_by(Station_number) %>% 
        summarise( mean_tmp_min = mean(tmp_min), mean_tmp_max = mean(tmp_max), mean_Rainfall =  mean(Rainfall) ) %>% 
        mutate_at(vars(Station_number), as.character)                # must be character to join (next step)

# joining the two new tibbles, both 20x4/8
bom_joined <- 
      inner_join(bom_stations_join, bom_data_join, by = 'Station_number') 

# organise tibble that answers the question
state_low_avr_tmp <- 
      bom_joined %>% 
        group_by(state) %>% 
        summarise(mean_state_tmp_min = mean(mean_tmp_min), mean_state_tmp_max = mean(mean_tmp_max)) %>% 
        mutate("mean_tmp_diff_state" = mean_state_tmp_max - mean_state_tmp_min ) %>% 
        arrange(mean_tmp_diff_state) %>% 
        write_csv("C:/Users/ing06d/Documents/Data school FOCUS/Data-week2/Outputs/answerQ3.csv")

  #### NOTES
  # joining is tricky.Reckons it often requires more than one step
  # seems like all join-functions work in this instance, tibble both at 20 identical stations
  # it matters in which order one muta, group and summarise 


## Challenge 4 ##################################################################################################
# Does the westmost (lowest longitude) or eastmost (highest longitude) weather station
# in our dataset have a higher average solar exposure?

      
# REUSE - bom_station file ready in a 'joinable' form
bom_stations_join <- 
  bom_stations  %>% 
  gather( key = Station_number, value = measure, -info) %>%   # the -info seems to let R know to include the info column
  spread( key = info, value = measure )                       # to get the first original column (info) as headings, and sone station one row


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


#### NOTES
  # I can't figure out how to extract only row 1 and 20 in the tibble
  # grouping by column with non-identical factors make no difference
    






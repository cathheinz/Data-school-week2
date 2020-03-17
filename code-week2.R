
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
  # gather the 'bom-stations' file
  # join somehow... with 'bom_data' file


bom_stations_int <- 
      bom_stations  %>% 
        gather( key = station_number, value = measure)  %>% 
        gather( key = Station_number, value = measure, -info)

gather(bom_stations, )
  
  > cows_long <- gather(cows, key = weight_type, value = weight, -id)

  
# gather(cows, key = "weight_type", value = "weight",  weight1, weight2)

  view(bom_stations_int)

example(gather)



######
 ############################# functions gather, spread, separate, write_csv - after lunch starred by Stephen ################
> 
> 
> cows <- tibble(id = c(1,2,3), weight1 =c(203, 227, 193), weight2 = c(365, 344, 329))
> 

> cows
# A tibble: 3 x 3
     id weight1 weight2
  <dbl>   <dbl>   <dbl>
1     1     203     365
2     2     227     344
3     3     193     329


> gather(cows, key = "weight_type", value = "weight",  weight1, weight2)
# A tibble: 6 x 3
     id weight_type weight
  <dbl> <chr>        <dbl>
1     1 weight1        203
2     2 weight1        227
3     3 weight1        193
4     1 weight2        365
5     2 weight2        344
6     3 weight2        329
> 
> gather(cows, key = "weight_type", value = "weight")
# A tibble: 9 x 2
  weight_type weight
  <chr>        <dbl>
1 id               1
2 id               2
3 id               3
4 weight1        203
5 weight1        227
6 weight1        193
7 weight2        365
8 weight2        344
9 weight2        329
> 
> cows_long <- gather(cows, key = weight_type, value = weight, -id)





  
  
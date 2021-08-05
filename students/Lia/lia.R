# 2021 
# 489 for Lia on Generational Anxiety
# libraries 

# libraries 
library("tidyverse")
library("here")
library("ggplot")
library("naniar")

# read NZAVS data
df <- readRDS(here::here("data_raw","df"))

# look at the dataframe
dplyr::glimpse(df)

# select variables that  
# Relid, 
# Hours.Religious, 
# religion.church, 
# BigDoms,
# HoursCharity,
# CharityDonate
#df$Religion.Church
# create 


ldf <- df %>% 
  select(Id, 
         YearMeasured, 
         years,  
         Wave, 
         Relid, 
         Hours.Religious, 
         Religion.Church, 
         BigDoms,
         HoursCharity,
         CharityDonate,
         Household.INC
         )

glimpse(ldf)

saveRDS(ldf, here::here("data_raw", "ldf"))


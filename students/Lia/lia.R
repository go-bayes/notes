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

# create 
pdf <- df %>% 
  select(Id, 
         YearMeasured, 
         years,  
         Wave, 
         BELONG, 
         SUPPORT, 
         Hours.Friends,
         Hours.Family,
         SWB.SoC01, # neighbourhood community,
         # SELF.ESTEEM jb to get
         # PERFECTIONISM
         # Emotional regulation
         PWI, 
         LIFESAT,
         KESSLER6sum,
         Hours.CompGames,
         Hours.SocialMedia,
         Hours.Internet)



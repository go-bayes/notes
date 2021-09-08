# 2021 
# 489 for Paris Holden on Generational Anxiety
# libraries 

# libraries 
library("tidyverse")

library("here")
library("ggplot")
library("naniar")
# 
# # read NZAVS data
# df <- readRDS(here::here("data","paris_dat.rds"))
# 
# # look at the dataframe
# dplyr::glimpse(df)
# 
# # select variables that  
# 
# # create 
# pdf <- df %>% 
#   select(Id, 
#          YearMeasured, 
#          years,  
#          Wave, 
#          BELONG, 
#          SUPPORT, 
#          Hours.Friends,
#          Hours.Family,
#          SWB.SoC01, # neighbourhood community,
#          SELF.ESTEEM, #jb to get
#          PWI, 
#          LIFESAT,
#          KESSLER6sum,
#          Hours.CompGames,
#          Hours.SocialMedia,
#          Hours.Internet,
#          Age,
#          Male,
#          Relid,
#          Edu,
#          Partner,
#          EthnicCats
#          )
# saveRDS(pdf, here::here("data_raw", "pdf"))
# 
# here()
# test <- readRDS(here::here("data_raw", "pdf"))
# head(test)

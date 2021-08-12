# Amy Du's data script 4 Aug 2021


# libraries 
library("tidyverse")
library("here")
library("ggplot")
library("naniar")

# read NZAVS data
df <- readRDS(here::here("data_raw","df"))

# look at the dataframe
dplyr::glimpse(df)

ID, 
Wave,
YearMeasured,
Male,
SELF.ESTEEM,
PERFECTIONISM,




# create 
amydf <- df %>% 
  filter(Wave == 2018 | Wave == 2019) %>%
  filter(YearMeasured == 1) %>%
  select(Id, 
         YearMeasured, 
         years,  
         Wave,
         Household.INC,
         LIFEMEANING,
         SELF.CONTROL,
         SELF.ESTEEM,
         Emp.JobSat,
         Emp.JobValued,
         HomeOwner,
         LIFESAT,
         GRATITUDE,
         Edu,
         Relid,
         Believe.God,
         Believe.Spirit,
         Immigrant.TimeNZYears,
         SexualSatisfaction,
         BELONG,  
         SUPPORT, 
         Hours.Friends,
         Hours.Family,
         SWB.SoC01, # neighbourhood community,
         SELF.ESTEEM, #jb to get
         PWI, 
         LIFESAT,
         KESSLER6sum,
        # Hours.CompGames,
         Hours.SocialMedia,
         Hours.Internet,
         Age,
         Male,
         Relid,
         Edu,
         Partner,
         EthnicCats,
         Urban, 
         Partner,
         ChildrenNum
  )
saveRDS(amydf, here::here("data_raw", "amydf"))

 here()
test <- readRDS(here::here("data_raw", "amydf"))
head(test)
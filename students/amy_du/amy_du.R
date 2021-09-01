# Amy Du's data script 4 Aug 2021


# libraries 
library("tidyverse")
library("here")
library("ggplot")
library("naniar")

# read NZAVS data
df <- readRDS(here::here("data","ldf.5"))


# look at the dataframe
dplyr::glimpse(df)



str(df$Employed

# create 
amydf <- df %>% 
  filter(Wave == 2018 | Wave == 2019) %>%
  filter(YearMeasured == 1) %>%
  select(Id, 
         YearMeasured, 
         years,  
         Wave,
         Employed,
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
saveRDS(amydf, here::here("data-raw", "amydf"))

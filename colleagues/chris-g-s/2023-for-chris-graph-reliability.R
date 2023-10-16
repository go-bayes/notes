# graph for chris

# request
# "The first file has two variable, each with upper and lower CI. Could you plot these two variables and their Cis on the same graph? Idea is to show where they overlap."
# "The second file has just one variable (and its Cis). This should plotted on its own graph"

# libraries
library("tidyverse")
library("patchwork")
library("here")
library("readr")
library("janitor")
library("tidyr")
library("readr")
# note import files as tab delimited in to R as data frames

# read omega data
omega <- read_delim(here::here("colleagues","chris-g-s", "Omega.txt"), delim = "\t", locale = locale(encoding = "UTF-8"))

# check
head(omega)

# if we need to make good
#omega$Scale <- iconv(omega$Scale, to = "ASCII//TRANSLIT")


# read short reminder data
s_reminder  <- read_delim(here::here("colleagues","chris-g-s", "short-remainder.txt"), delim = "\t", locale = locale(encoding = "UTF-8"))

# if we need to make good
# s_reminder$Scale <- iconv(s_reminder$Scale, to = "ASCII//TRANSLIT")

# check
head( s_reminder )


# clean header names using the janitor package
omega_cleaned <- omega %>% clean_names()

# so same for reliability
s_reminder_cleaned <- s_reminder %>% clean_names()

# view data
head(omega_cleaned)
head(s_reminder_cleaned)


# this is in the reliability data but not the omega data
s_reminder_cleaned|> 
  filter(scale == "Individual Permeability")

omega_cleaned|> 
  filter(scale == "Individual Permeability")



# data wrangling -- data is a wee bit  messy
library(tidyr)
library(dplyr)
library(stringr)

# step 1: separate the full scale and short-form scale data
omega_full_scale <- omega_cleaned %>%
  select(scale, omega_full_scale, lower_99_percent_ci_3, upper_99_percent_ci_4)

# check
str(s_reminder_cleaned$upper_99_percent_ci)


# step2: get the short form omega
omega_short_form_scale <- omega_cleaned %>%
  select(scale, omega_short_form_scale, lower_99_percent_ci_6, upper_99_percent_ci_7)

# check
str( omega_short_form_scale )

# need to make good on encodings
omega_full_scale$omega_full_scale <- iconv(omega_full_scale$omega_full_scale, to = "ASCII//TRANSLIT")
omega_short_form_scale$omega_short_form_scale <- iconv(omega_short_form_scale$omega_short_form_scale, to = "ASCII//TRANSLIT")

# step 3: rename columns omega full
omega_full_scale_transformed <- omega_full_scale |> 
  rename( omega_scale = omega_full_scale,
          lower_ci = lower_99_percent_ci_3,
          upper_ci = upper_99_percent_ci_4) |> 
  mutate(type = rep("Full-Scale", nrow(omega_full_scale))) |> 
  mutate(id = as.factor( 1:nrow(omega_full_scale))) 

# check
head(omega_full_scale_transformed)


# step 4: do same for short_form scale
omega_short_form_scale_transformed <- omega_short_form_scale |> 
  rename( omega_scale = omega_short_form_scale,
          lower_ci = lower_99_percent_ci_6,
          upper_ci = upper_99_percent_ci_7) |> 
  mutate(type = rep("Short-Scale or Single-Item Indicator", nrow(omega_full_scale))) |> 
  mutate(id = as.factor( 1:nrow(omega_full_scale))) 

# check
head(omega_short_form_scale_transformed)


# step 5: merge data frames
dat <- rbind(omega_full_scale_transformed, omega_short_form_scale_transformed)

# check
head(dat)

# spit and shine
dat$omega_scale <- iconv(dat$omega_scale, to = "ASCII//TRANSLIT")

# if needed to debug uncomment
# Find rows with non-numeric omega_scale
# invalid_rows <- which(!grepl("^\\d*\\.?\\d+$", dat$omega_scale))
# 
# # print invalid rows
# dat[invalid_rows,]
# 
# # remove invalid rows
# dat <- dat[-invalid_rows,]
#



# step  6 convert omega_scale to numeric
dat$omega_scale <- as.numeric(dat$omega_scale)

# data wrangle arrange by difference in omega scale
# note # chris doesn't want to order, but uncomment to do so
# dat <- dat %>%
  # group_by(id) %>%
  # mutate(diff_within_group = c(diff(omega_scale), NA)) %>%
  # ungroup() %>%
  # mutate(abs_diff = abs(diff_within_group)) %>%
  # arrange(desc(abs_diff), id) %>%
  # select(-diff_within_group, -abs_diff) 


# arrange data  by levels of scale
dat$scale <- factor(dat$scale, levels = unique(dat$scale))

# build graph 
library(ggplot2)

# convert omega_scale to a numeric variable
dat$omega_scale <- as.numeric(dat$omega_scale)
dat$lower_ci <- as.numeric(dat$lower_ci)
dat$upper_ci <- as.numeric(dat$upper_ci)
dat$omega_scale <- as.numeric(dat$omega_scale)

# plotting
omega_graph <- ggplot(data = dat, aes(x = omega_scale, y = scale, colour = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +
  labs(
    title = "Validation: Long and Short Sale",
    x = "Omega Reliability",
    y = "Scale",
    colour = "Type"
  ) +
  scale_x_continuous(limits = c(0,1)) +
  scale_colour_manual(values = c("Full-Scale" = "blue", "Short-Scale or Single-Item Indicator" = "red")) +
  theme_classic() +
  theme(legend.position = "bottom") 

# view
omega_graph

# save graph
ggsave(
  omega_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "omega_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


## graph for reliability


dat2 <- s_reminder_cleaned 
# chris doesn't want order, uncomment if you'd like to.
# %>%
#   group_by(id) |> 
#   arrange(-desc(omega_scale))

# arrange by levels 
dat2$scale <- factor(dat2$scale, levels = unique(dat2$scale))


# Convert omega_scale to a numeric variable
dat2$short_remainder_reliability <- as.numeric(dat2$short_remainder_reliability)
dat2$lower_99_percent_ci <- as.numeric(dat2$lower_99_percent_ci)
dat2$upper_99_percent_ci <- as.numeric(dat2$upper_99_percent_ci)

# plotting
library(ggplot2)

# graph
reliability_graph_short <- ggplot(data = dat2, aes(x = short_remainder_reliability, y = scale)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower_99_percent_ci, xmax = upper_99_percent_ci), height = 0.2) +
  labs(
    title = "Short Sale Reliability",
    x = "Chronbach's Alpha",
    y = "Scale",
    colour = "Type"
  )+  theme_classic() + scale_x_continuous(limits = c(0,1))
#  scale_colour_manual(values = c("full_scale" = "blue", "short_scale" = "red")) 

# viw
reliability_graph_short

# save graph
ggsave(
  reliability_graph_short,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "reliability_graph_short.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# create combo graph using forest plot
all_graph <- omega_graph + reliability_graph_short

# save combo
ggsave(
  all_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 20,
  height = 15,
  units = "in",
  filename = "all_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



# find the NAs in the data frames
dat |> 
  filter(is.na(upper_ci)) |> 
  arrange(id)

dat2 |> 
  filter(is.na(upper_99_percent_ci)) 

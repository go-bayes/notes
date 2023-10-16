# graph for chris

# request
# "The first file has two variable, each with upper and lower CI. Could you plot these two variables and their Cis on the same graph? Idea is to show where they overlap."
# "The second file has just one variable (and its Cis). This should plotted on its own graph"

# libraries
library("tidyverse")
# data wrangling -- 
library("tidyr")
library("dplyr")
library("stringr")
library("patchwork")
library("here")
library("readr")
library("janitor")
library("tidyr")
library("readr")
library("haven") # read SPSS files
# note import files as tab delimited in to R as data frames

# read omega data (old)
# omega <- read_delim(here::here("colleagues","chris-g-s", "Omega.txt"), delim = "\t", locale = locale(encoding = "UTF-8"))
# omega <- read_delim(here::here("colleagues","chris-g-s", "Omega.txt"), delim = "\t", locale = locale(encoding = "UTF-8"))
# head(omega)
# nrow(omega)
# read spss data 
omega_2 <- haven::read_sav(here::here("colleagues","chris-g-s", "Figure_Bifactor_Model.sav"))
omega_2 <- haven::zap_formats(omega_2)
omega_2 <- haven::zap_label(omega_2)
omega_2 <- haven::zap_widths(omega_2)

omega_2<- as.data.frame(omega_2)
str(omega_2)
head(omega_2)
nrow(omega_2)

# clean header names using the janitor package
omega_cleaned <- omega_2 %>% clean_names() %>% 
   mutate(scale = as.factor(scale)) %>% 
   arrange(desc(scale))
omega_cleaned$scale <- factor(omega_cleaned$scale, levels = unique(omega_cleaned$scale))


str(omega_cleaned)
head(omega_cleaned)

# load necessary packages
library(tidyverse)
library(ggplot2)

# create a new column to indicate missing values
omega_cleaned <- omega_cleaned %>% 
  mutate(is_missing = ifelse(is.na(ratio_reliable_variance) | is.na(low_ci) | is.na(up_ci), 1, 0))

colour_vector <- setNames(omega_cleaned$is_missing, omega_cleaned$scale)
colour_vector[colour_vector == 1] <- "red"
colour_vector[colour_vector == 0] <- "black"

head(omega_cleaned)



# generate ggplot
graph_scale <- ggplot(data = omega_cleaned, aes(x = ratio_reliable_variance, y = scale)) +
  geom_point(size = 0.5) +
  geom_errorbarh(aes(xmin = low_ci, xmax = up_ci)) +
  theme_classic() +
  labs(title = "NZAVS Scale Ratio Reliable Variance",
       x = "Ratio Reliable Variance",
       y = "Scale") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(colour = colour_vector))

# show plot
graph_scale

# show plot
graph_scale


# generate ggplot
graph_scale_2 <- ggplot(data = omega_cleaned, aes(x = ratio_reliable_variance, y = scale)) +
  geom_point(size = 0.5) +
  geom_errorbarh(aes(xmin = low_ci, xmax = up_ci)) +
  theme_classic() +
  labs(title = "NZAVS Scale Ratio Reliable Variance",
       x = "Ratio Reliable Variance",
       y = "Scale") +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.position = "bottom")

# show plot
graph_scale_2

# show plot
graph_scale

# save graph
ggsave(
  graph_scale,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "nzavs_scale_ratio_reliabilty_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)
p

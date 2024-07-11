
# 2024 July 11 Reconstruct B Murphy's study

#RCode for Analyses on Perceptual Speed and Visual Comparison, using datafile "FinalCleanedData.xlsl"

# Load necessary libraries
#library(readxl)
library(ggplot2)
library(here)
library(ggeffects) # graphs
library(tidyverse) # wrangling 
library(parameters) # nice tables
library(marginaleffects)
library(here)
library(janitor) # better names

# Step 1: Load and read the dataset
data <- read.csv(here::here("students", "briana_murphy", "FinalCleanedData.csv"))



# jb's work --------------------------------------------------------------
# inspect data
head(data)

df <- data |> 
  janitor::clean_names()

# inspect
head(df)




# model of relationship between fingerprint_raw and artifical_raw, adjusting for age and gender
fit_0 <- lm(vc_general  ~ p_stotal_score +  age + female, data = df) 

# table
parameters::model_parameters(fit_0)

# explore
marginaleffects::avg_slopes(fit_0, variables = "p_stotal_score")


# graph
predicted_fit_0 <- predict_response(fit_0, terms = "p_stotal_score")


# plots response
p <- plot( 
  predicted_fit_0, 
  dot_alpha = 0.35,
  show_data = TRUE,
  jitter = .05
  )  +  scale_y_continuous(limits = c(2,2)) # set as desired

# p + title(main = "Predicted values of VC General",
#         xlab = "Perceptual Speed Total Score",
#         ylab = "Visual Comparison General Score") 


# bm's analysis -----------------------------------------------------------


# Step 2: Compute means and standard deviations for specified variables
variables <- c("ArtificialAcc", "FingerprintAcc", "PStotal_score", "IMI")
means <- sapply(data[variables], mean, na.rm = TRUE)
sds <- sapply(data[variables], sd, na.rm = TRUE)

# Print means and standard deviations
print(data.frame(Variable = variables, Mean = means, SD = sds))

# Step 3: One-tailed t-tests for "ArtificialAcc" and "FingerprintAcc"

# JB. why assess whether mean accuracy is greater than .5? 

t_test_artificial <- t.test(data$ArtificialAcc, mu = 0.5, alternative = "greater")
t_test_fingerprint <- t.test(data$FingerprintAcc, mu = 0.5, alternative = "greater")

# Print t-test results
print(t_test_artificial)
print(t_test_fingerprint)

# Load necessary library
library(ggplot2)

# Step 4: Create the histogram plot with enhanced axis label styling
p <- ggplot(data, aes(x = PStotal_score)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  xlab("Scores") +
  ylab("Frequency") +
  theme(text = element_text(size = 12),  # Adjusts text size for all text elements
        axis.title = element_text(size = 16, face = "bold"),  # Bolder and larger axis titles
        axis.text = element_text(size = 14))  # Larger text for axis numbers

# Display the plot
print(p)

# Save the plot to an image file
#ggsave("Perceptual_Speed_Scores.png", plot = p, width = 10, height = 8, dpi = 300)

# Step 5: Correlation between FingerprintAcc and ArtificialAcc
test_fingerprint_artificial <- cor.test(data$FingerprintAcc, data$ArtificialAcc, use = "complete.obs")
cor_fingerprint_artificial <- test_fingerprint_artificial$estimate
pvalue_fingerprint_artificial <- test_fingerprint_artificial$p.value
print(paste("Correlation between FingerprintAcc and ArtificialAcc: ", cor_fingerprint_artificial, ", p-value: ", pvalue_fingerprint_artificial))

# Load necessary libraries for composite visual comparison (i.e., VCGeneral)
library(readxl)
library(dplyr)

# Step 6: Compute means and std devs for VCGeneral
variables <- c("ArtificialAcc", "FingerprintAcc", "PStotal_score", "IMI")
stats <- data[variables] %>%
  summarise(across(.cols = everything(), .fns = list(mean = ~mean(., na.rm = TRUE), 
                                                     sd = ~sd(., na.rm = TRUE))))

# Extract means and std devs for VCGeneral
mean_artificial <- stats$ArtificialAcc_mean
sd_artificial <- stats$ArtificialAcc_sd
mean_fingerprint <- stats$FingerprintAcc_mean
sd_fingerprint <- stats$FingerprintAcc_sd

# Step 7: Calculate z-scores for "ArtificialAcc" and "FingerprintAcc"
data$ArtificialAcc_z <- (data$ArtificialAcc - mean_artificial) / sd_artificial
data$FingerprintAcc_z <- (data$FingerprintAcc - mean_fingerprint) / sd_fingerprint

# Step 8: Create the composite score "VCGeneral"
data$VCGeneral <- (data$ArtificialAcc_z + data$FingerprintAcc_z) / 2

# Step 9: Correlation between VCGeneral and Perceptual Speed
test_vcgeneral_pstotal <- cor.test(data$VCGeneral, data$PStotal_score, use = "complete.obs")
cor_vcgeneral_pstotal <- test_vcgeneral_pstotal$estimate
pvalue_vcgeneral_pstotal <- test_vcgeneral_pstotal$p.value

# Step 10: Correlation between Intrinsic Motivation and VCGeneral
test_imi_vcgeneral <- cor.test(data$IMI, data$VCGeneral, use = "complete.obs")
cor_imi_vcgeneral <- test_imi_vcgeneral$estimate
pvalue_imi_vcgeneral <- test_imi_vcgeneral$p.value

# Step 11: Correlation between Intrinsic Motivation and Perceptual Speed
test_imi_pstotal <- cor.test(data$IMI, data$PStotal_score, use = "complete.obs")
cor_imi_pstotal <- test_imi_pstotal$estimate
pvalue_imi_pstotal <- test_imi_pstotal$p.value

# Step 12: Correlation between Age and VCGeneral
test_age_vcgeneral <- cor.test(data$Age, data$VCGeneral, use = "complete.obs")
cor_age_vcgeneral <- test_age_vcgeneral$estimate
pvalue_age_vcgeneral <- test_age_vcgeneral$p.value

# Step 13: Correlation between Age and Perceptual Speed
test_age_pstotal <- cor.test(data$Age, data$PStotal_score, use = "complete.obs")
cor_age_pstotal <- test_age_pstotal$estimate
pvalue_age_pstotal <- test_age_pstotal$p.value

#Step 14: Correlation between Age and Intrinsic Motivation
test_age_imi <- cor.test(data$Age, data$IMI, use = "complete.obs")
cor_age_imi <- test_age_imi$estimate
pvalue_age_imi <- test_age_imi$p.value

# Step 15: Print all correlations
print(paste("Correlation between VCGeneral and PStotal_score: ", cor_vcgeneral_pstotal, ", p-value: ", pvalue_vcgeneral_pstotal))
print(paste("Correlation between IMI and VCGeneral: ", cor_imi_vcgeneral, ", p-value: ", pvalue_imi_vcgeneral))
print(paste("Correlation between IMI and PStotal_score: ", cor_imi_pstotal, ", p-value: ", pvalue_imi_pstotal))
print(paste("Correlation between Age and VCGeneral: ", cor_age_vcgeneral, ", p-value: ", pvalue_age_vcgeneral))
print(paste("Correlation between Age and PStotal_score: ", cor_age_pstotal, ", p-value: ", pvalue_age_pstotal))
print(paste("Correlation between Age and IMI: ", cor_age_imi, ", p-value: ", pvalue_age_imi))

# Step 16: Partial correlation for VCGeneral and Perceptual Speed, controlling for Age
install.packages("ppcor")
library(ppcor)

# Perform partial correlation
partial_corr_result <- pcor.test(data$PStotal_score, data$VCGeneral, data$Age, method = "pearson")

# Print result
print(partial_corr_result)

# Load necessary library for scatterplots
library(ggplot2)

# Step 17: Create a customised scatterplot for VCGeneral and Perceptual Speed
scatter_plot <- ggplot(data, aes(x = PStotal_score, y = VCGeneral)) +
  geom_point() +  # Add points for scatter plot
  geom_smooth(method = "lm", color = "darkblue", fill = "#1f77b4AA") +  # Add linear model trendline with customized CI
  xlab("Perceptual Speed") +  # X-axis label
  ylab("Visual Comparison") +  # Y-axis label
  theme(text = element_text(size = 12),  # Adjusts text size for all text elements
        axis.title = element_text(size = 16, face = "bold"),  # Bolder and larger axis titles
        axis.text = element_text(size = 14))  # Larger text for axis numbers

# Display the plot
print(scatter_plot)

# Install and load necessary libraries for patchwork plots
install.packages("tidyr")
install.packages("patchwork")
library(tidyr)
library(patchwork)

# Step 18: Create patchwork plots for Intrinsic Motivation, VCGeneral, and Perceptual Speed
# Reshape the data to long format for plotting
long_data <- data %>%
  select(IMI, VCGeneral, PStotal_score) %>%
  pivot_longer(cols = c("VCGeneral", "PStotal_score"), names_to = "Variable", values_to = "Value")

# Create separate plots for each variable with specified x-axis breaks for VCGeneral
plot_VCGeneral <- ggplot(long_data %>% filter(Variable == "VCGeneral"), aes(x = Value, y = IMI)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkblue", fill = "#1f77b4AA") +
  scale_x_continuous(breaks = c(-1, 0, 1)) +  # Set breaks for the x-axis
  xlab("Visual Comparison") +
  ylab("Intrinsic Motivation") +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14))

# Create Perceptual Speed plot
plot_PStotal_score <- ggplot(long_data %>% filter(Variable == "PStotal_score"), aes(x = Value, y = IMI)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkblue", fill = "#1f77b4AA") +
  xlab("Perceptual Speed") +
  ylab("") +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title.y = element_blank())  # Remove the y-axis title for this plot

# Combine the plots side by side with the patchwork package
combined_plot <- plot_VCGeneral | plot_PStotal_score

# Display the combined plot
print(combined_plot)
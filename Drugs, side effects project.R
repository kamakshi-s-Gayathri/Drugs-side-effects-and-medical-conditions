# Install if not already installed
install.packages(c("tidyverse", "ggplot2", "dplyr", "readr", "janitor", "corrplot"))

# Load libraries
library(tidyverse)
library(janitor)

# Load CSV
df= read_csv("C:\\Users\\intel\\OneDrive\\Desktop\\unified mentor\\Drugs, Side Effects\\drugs_side_effects_drugs_com.csv")


# Clean column names
df=clean_names(df)

# View basic structure
glimpse(df)
summary(df)

# Count missing values
colSums(is.na(df))

# filter out NA Values
ggplot(df %>% filter(!is.na(rating)), aes(x = rating)) +
  geom_histogram(bins = 10, fill = "#69b3a2", color = "black") +
  labs(title = "Distribution of Drug Ratings", x = "Rating", y = "Frequency")

# Distribution of rating-- without filtering NA 
ggplot(df, aes(x = rating)) +
  geom_histogram(bins = 10, fill = "#69b3a2", color = "black") +
  labs(title = "Distribution of Drug Ratings", x = "Rating", y = "Frequency")

#Top drugs by condition
df %>%
  count(medical_condition, drug_name, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(drug_name, n), y = n, fill = medical_condition)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Drugs by Condition", x = "Drug", y = "Count")

#Most common side effects
df %>%
  filter(!is.na(side_effects)) %>%
  count(side_effects, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(side_effects, n), y = n)) +
  geom_col(fill = "#ff9999") +
  coord_flip() +
  labs(title = "Top 10 Side Effects", x = "Side Effect", y = "Count")

#Boxplot-drug rating by class
ggplot(df %>% filter(!is.na(rating)), aes(x = drug_classes, y = rating)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Ratings by Drug Class", x = "Drug Class", y = "Rating")

##Data cleaning and feature engineering
#convert to numeric
df$activity = as.numeric(gsub("%", "", df$activity)) / 100

#Handling Missing values
# Fill NA in 'rating' and 'no_of_reviews' with 0 or placeholder
df$rating[is.na(df$rating)] = 0
df$no_of_reviews[is.na(df$no_of_reviews)] = 0

# Fill 'side_effects' and 'related_drugs' with 'Unknown'
df$side_effects[is.na(df$side_effects)] = "Unknown"
df$related_drugs[is.na(df$related_drugs)] = "Unknown"

# Fill categorical with 'Unknown'
df$generic_name[is.na(df$generic_name)] = "Unknown"
df$drug_classes[is.na(df$drug_classes)] = "Unknown"
df$rx_otc[is.na(df$rx_otc)] <- "Unknown"
df$pregnancy_category[is.na(df$pregnancy_category)] = "Unknown"
df$alcohol[is.na(df$alcohol)] = "0"
df$alcohol[df$alcohol == "X"] = "1"
df$alcohol = as.numeric(df$alcohol)

summary(df)

#Top side effects
# Split side effects text and count frequency
library(tidyverse)

top_side_effects =df %>%
  filter(!is.na(side_effects)) %>%
  mutate(side_effects = strsplit(side_effects, ";")) %>%
  unnest(side_effects) %>%
  mutate(side_effects = str_trim(tolower(side_effects))) %>%
  count(side_effects, sort = TRUE)

# View top 10
head(top_side_effects, 10)

# Plot: Top 10 Side Effects
library(ggplot2)

top_side_effects %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(side_effects, n), y = n)) +
  geom_bar(stat = "identity", fill = "#FF9999", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Reported Side Effects",
       x = "Side Effect",
       y = "Frequency") +
  theme_minimal()


#Top drug classes
top_drug_classes = df %>%
  filter(!is.na(drug_classes)) %>%
  mutate(drug_classes = strsplit(drug_classes, ",")) %>%
  unnest(drug_classes) %>%
  mutate(drug_classes = str_trim(tolower(drug_classes))) %>%
  count(drug_classes, sort = TRUE)

# View top 10
head(top_drug_classes, 10)

# Plot: Top 10 Drug Classes
top_drug_classes %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(drug_classes, n), y = n)) +
  geom_bar(stat = "identity", fill = "#66CCFF", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Drug Classes",
       x = "Drug Class",
       y = "Frequency") +
  theme_minimal()


#Top medical condition
top_medical_conditions <- df %>%
  count(medical_condition = tolower(medical_condition), sort = TRUE)

# View top 10
head(top_medical_conditions, 10)
# Plot: Top 10 Medical Conditions
top_medical_conditions %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(medical_condition, n), y = n)) +
  geom_bar(stat = "identity", fill = "#99CC99", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Medical Conditions",
       x = "Medical Condition",
       y = "Frequency") +
  theme_minimal()
##Feature engineering
# Lowercase for consistent pattern matching
df$side_effects = tolower(df$side_effects)
df$drug_classes = tolower(df$drug_classes)
df$medical_condition = tolower(df$medical_condition)

# Top 3 Side Effects Flags
df$has_hives = grepl("hives", df$side_effects)
df$has_difficult_breathing = grepl("difficult breathing|difficulty breathing", df$side_effects)
df$has_itching = grepl("itching", df$side_effects)

# Top 3 Drug Class Flags
df$is_upper_respiratory = grepl("upper respiratory combinations", df$drug_classes)
df$is_topical_acne = grepl("topical acne agents", df$drug_classes)
df$is_topical_steroid = grepl("topical steroids", df$drug_classes)

# Top 3 Medical Condition Flags
df$has_pain = grepl("pain", df$medical_condition)
df$has_colds_flu = grepl("colds & flu", df$medical_condition)
df$has_acne = grepl("acne", df$medical_condition)

# Create a numeric-only dataframe for correlation
df_numeric = df %>%
  select(rating, no_of_reviews, activity, alcohol,
         has_hives, has_difficult_breathing, has_itching,
         is_upper_respiratory, is_topical_acne, is_topical_steroid,
         has_pain, has_colds_flu, has_acne) %>%
  mutate(across(everything(), as.numeric))  # Convert to numeric

# Install if not installed
install.packages("corrplot")
library(corrplot)

# Compute correlation matrix
cor_matrix = cor(df_numeric, use = "complete.obs")

# Plot heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.8, number.cex = 0.7, addCoef.col = "black")








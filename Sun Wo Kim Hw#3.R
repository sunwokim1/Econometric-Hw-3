---
#Group Members: Sun Wo Kim, Leonardo Alcaide, Arifa Begum, Nene Diallo, Maria Camila
title: 'Homework #3'
output: github_document
---
  ### Due 8am EST Friday Sept 20, 2024 
  ### Econ B2000, MA Econometrics


require(tidyverse)
require(ggplot2)
load("/Users/jaydenkim/Desktop/Econometrics/Household_Pulse_data_ph4c2.RData")

# Load your dataset
# load("/Users/jaydenkim/Desktop/Econometrics/Household_Pulse_data_ph4c2.RData")

# Fill missing values in RECVDVACC (assuming missing = not vaccinated)
Household_Pulse_data$RECVDVACC <- ifelse(is.na(Household_Pulse_data$RECVDVACC), "No", Household_Pulse_data$RECVDVACC)

# Recode income_midpoint as numeric
Household_Pulse_data$income_midpoint <- fct_recode(Household_Pulse_data$INCOME, 
                                                   "12500" = "HH income less than $25k",
                                                   "30000" = "HH income $25k - $34.9k",
                                                   "40000" = "HH income $35k - 49.9",
                                                   "62500" = "HH income $50k - 74.9",
                                                   "82500" = "HH income $75 - 99.9",
                                                   "125000" = "HH income $100k - 149",
                                                   "175000" = "HH income $150 - 199",
                                                   "225000" = "HH income $200k +",
                                                   NULL = "NA")
Household_Pulse_data$income_midpoint <- as.numeric(levels(Household_Pulse_data$income_midpoint))[Household_Pulse_data$income_midpoint]

# Recode education years as numeric
Household_Pulse_data$Educ_years <- fct_recode(Household_Pulse_data$EEDUC,
                                              "8" = "less than hs",
                                              "11" = "some hs",
                                              "12" = "HS diploma",
                                              "13" = "some coll",
                                              "14" = "assoc deg",
                                              "16" = "bach deg",
                                              "18" = "adv deg")
Household_Pulse_data$Educ_years <- as.numeric(levels(Household_Pulse_data$Educ_years))[Household_Pulse_data$Educ_years]

# Calculate age
Household_Pulse_data$Age <- 2024 - Household_Pulse_data$TBIRTH_YEAR

# Create a bar plot to show education levels by employment and vaccination status
p_bar_educ_vax <- ggplot(Household_Pulse_data, aes(x = factor(Educ_years), fill = factor(Educ_years))) +
  geom_bar(position = "dodge") +  # Dodge will separate the bars by education level
  facet_wrap(~ANYWORK) +  # Faceting by employment status
  labs(title = "Bar Plot: Education Levels by Employment and Vaccination Status",
       x = "Years of Education", y = "Vaccination Count", fill = "Education Level") +  # Changed y-axis label
  theme_minimal() +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#ff7f00", "#984ea3", "#e41a1c", "#ffff33", "#a65628"))  # Custom color palette for each bar

# Display the bar plot
print(p_bar_educ_vax)


# The graph shows that individuals with higher education levels, such as bachelor’s and advanced degrees, have significantly higher vaccination rates across all employment statuses. 
# Conversely, those with lower education levels show much lower vaccination rates, particularly among those who have not been employed recently. 
# Employment also appears to be associated with higher vaccination rates, as individuals with recent employment have greater vaccination numbers compared to those without. 
# Overall, the data suggests that both higher education and employment are strong factors in increased vaccination rates.







library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

load("/Users/jaydenkim/Desktop/Econometrics/Household_Pulse_data_ph4c2.RData")


ls()
ggplot(Household_Pulse_data, aes(x = RECVDVACC,fill = factor(EEDUC))) +
  geom_density(alpha=0.5) +
  labs(title = 'Density plot of Vaccination Status Distribution by Education Level',
       x = 'VACCINATION STATUS',
       y = 'DENSITY') +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red", "green", "purple", "orange","gray","pink"))+
  facet_wrap(~ EEDUC, ncol = 7) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

#Density Plot: 
Each facet represents the density distribution of RECVDVACC for a specific education level (EEDUC). 
Each facet will show a smoothed curve representing the distribution of RECVDVACC for the corresponding education level.

#By examining the different facets (plots for each education level), we can see how the distribution of vaccination status varies with education level. 
#We observe that individuals with some college diplomas, associate degrees, and bachelor’s degrees have higher densities for the vaccination status labeled as "Yes, got vaccine."
#There are also more "N/A" entries for individuals with high school diplomas and some college degrees.
#This may be because some individuals did not provide their vaccination status or because some participants chose not to answer the vaccination status question.







load("/Users/jaydenkim/Desktop/Econometrics/Household_Pulse_data_ph4c2.RData")

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
Mental <- Household_Pulse_data %>%
  select( RECVDVACC, WORRY, ANXIOUS, DOWN)
View(Mental)
Mental_2 <- Household_Pulse_data %>%
  filter(!is.na(RECVDVACC), !is.na(WORRY), !is.na(ANXIOUS), !is.na(DOWN)) %>%
  mutate(
    WORRY = case_when(
      WORRY == "no worry over past 2 wks" ~ 0,
      WORRY == "several days worried over past 2 wks" ~ 1,
      WORRY == "more than half the days worried over past 2 wks" ~ 2,
      WORRY == "nearly every day worry" ~ 3
    ),
    ANXIOUS = case_when(
      ANXIOUS == "no anxiety over past 2 wks" ~ 0,
      ANXIOUS == "several days anxiety over past 2 wks" ~ 1,
      ANXIOUS == "more than half the days anxiety over past 2 wks" ~ 2,
      ANXIOUS == "nearly every day anxiety" ~ 3
    ),
    DOWN = case_when(
      DOWN == "no days in past" ~ 0,
      DOWN == "several days over past 2 wks" ~ 1,
      DOWN == "more than half the days over past 2 wks" ~ 2,
      DOWN == "nearly every day over past 2 wks" ~ 3 ) )
Mental_2.5 <- Mental_2 %>%
  pivot_longer(cols = c(WORRY, ANXIOUS, DOWN), names_to = "Mental_Health_Factor",
               values_to = "Level")
ggplot(Mental_2.5, aes(x = as.factor(Level), fill = Mental_Health_Factor)) +
  geom_bar(position = "fill") +
  facet_wrap(~ RECVDVACC) +
  labs(title = "Proportional Distribution of Mental Health Levels by Vaccination Status",
       x = "Level (0: None, 1: Mild, 2: Moderate, 3: Severe)",
       y = "Proportion",
       fill = NULL) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top" )
#Explanation:
#The graph shows that individuals who received the vaccine tend to report better
#mental health outcomes, with lower levels of anxiety, feeling down, and worry compared
#to those who did not get vaccinated. Unvaccinated individuals are more likely to
#experience severe anxiety, feelings of being down, and worry. Overall,
#vaccination appears to be associated with reduced mental health distress during the pandemic.






library(ggplot2)
library(dplyr)  # For data manipulation
# Load the data
load("/Users/jaydenkim/Desktop/Econometrics/Household_Pulse_data_ph4c2.RData")
# Calculate the percentage of vaccinated individuals for each income level
Household_Pulse_data <- Household_Pulse_data %>%
  group_by(INCOME) %>%
  summarize(Vaccinated = sum(RECVDVACC == "yes got vaxx", na.rm = TRUE),
            Total = n()) %>%
  mutate(Percentage = Vaccinated / Total * 100)
# Plot with percentage on the y-axis
ggplot(data = Household_Pulse_data, aes(x = INCOME, y = Percentage, fill = INCOME)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title = "Percentage of Vaccinated Individuals by Income Level", 
       x = "Income", 
       y = "Percentage Vaccinated") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +  # Converts y-axis to percentage format
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = .5))
## Through my lab, I concluded that there is a notable trend correlated between income levels and whether people are vaccinated or not. It seems that across all income levels, at least 75%  of every level is vaccinated. The higher income levels seem to have larger percentages of people being vaccinated. However, I do not think this directly means that income plays a role in whether someone is vaccinated or not. I think the trend is like this because the population for higher incomes is less, therefore, even if there is a little more people vaccinated, it shows up as a bigger portion of the population being vaccinated (almost 90%).






load("/Users/jaydenkim/Desktop/Econometrics/Household_Pulse_data_ph4c2.RData")
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
#successful
ggplot(Household_Pulse_data, aes(x = RECVDVACC, fill =factor(EGENID_BIRTH))) +
  geom_bar() + labs(title = 'Gender and Vaccination Status', subtitle = 'BIRTH GENDER',
                    y = 'Number of People', x = 'Vaccination Status') + theme_economist_white()
# This data only asks the birth gender of the recipents. They were also asked about their gender description however too many people answered NA so it is very difficult to analysis that data set. For the people who did get vaccined, a little over half of them(over 25000), were male and those who did not get vaccined, a little over half (over 5000) were female.As for the NA, it is looks to have slight majority of women. This means that men were overall men were more likely to get the vaccine.

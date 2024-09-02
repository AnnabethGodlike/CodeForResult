# Acknowledgement:  
#I acknowledge the use of ChatGPT (https://chat.openai.com/) to help with fixing this code
# in order to complete some statistical tests



# Install all packages needed
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("stringr")
install.packages("grid")

# Load the packages
library(dplyr)   # For data manipulation
library(tidyr)   # For handling missing data
library(ggplot2)
library(gridExtra)
library(stringr)
library(knitr)   # For table making
library(kableExtra)
library(broom)
library(grid)

# Load the survey data from CSV file
survey_data <- read.csv("Perceptions of Online Harassment on Live Streaming Platforms.csv")

# Display the structure of the data to identify columns
str(survey_data)

# Remove unnecessary metadata columns and filter data for age 18-25 and nationality not "Other"
survey_data_cleaned <- survey_data %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Duration..in.seconds., 
            Finished, RecordedDate, ResponseId, RecipientLastName, 
            RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage)) %>%
  filter(Q1 >= 18 & Q1 <= 25, Q3 != "Other (please specify)")

# Display the cleaned data
str(survey_data_cleaned)

# Verify the changes by checking the structure and first few rows of the cleaned data
str(survey_data_cleaned)
head(survey_data_cleaned)

# Check for missing values in the dataset
missing_values_summary <- sapply(survey_data_cleaned, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_values_summary)

# Remove rows where Age (Q1) is missing
survey_data_cleaned <- survey_data_cleaned %>%
  filter(!is.na(Q1))

# Verify that rows with missing Age are removed
sum(is.na(survey_data_cleaned$Q1))

# Optionally, save the cleaned data to a new CSV file
write.csv(survey_data_cleaned, "cleaned_survey_data.csv", row.names = FALSE)

# If you want to check the summary of the cleaned Age column:
age_summary_cleaned <- summary(survey_data_cleaned$Q1)
print("Age Summary (after cleaning):")
print(age_summary_cleaned)

# Further cleaning - Remove rows with any missing data in the cleaned dataset
final_survey_data <- survey_data_cleaned %>%
  drop_na()

# Save the final cleaned dataset to a new CSV file
write.csv(final_survey_data, "final_cleaned_survey_data.csv", row.names = FALSE)


str(survey_data_cleaned)


remaining_responses <- nrow(survey_data_cleaned)

# To show remaning answers
print(paste("Number of valid responses after filtering:", remaining_responses))

################################################################################
# Descriptive Analysis for age distribution

# Convert the Age column to numeric 
final_survey_data$Q1 <- as.numeric(final_survey_data$Q1)

# Convert Age to numeric, forcing any non-numeric values to NA
survey_data_cleaned$Q1 <- as.numeric(survey_data_cleaned$Q1)

# Remove rows with unrealistic age values (e.g., < 18 or > 65)
survey_data_cleaned <- survey_data_cleaned %>%
  filter(Q1 >= 18 & Q1 <= 65)

# Re-check the summary of the Age column
age_summary_cleaned <- summary(survey_data_cleaned$Q1)
print("Age Summary (after filtering unrealistic values):")
print(age_summary_cleaned)

# Create a histogram for age distribution
ggplot(data = survey_data_cleaned, aes(x = Q1)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

#############################################################################
#Descriptive analysis for gender distribution
# Inspect unique values in the Gender column to check for anomalies
unique_genders <- unique(survey_data_cleaned$Q2)
print("Unique Gender Values:")
print(unique_genders)

# Define all valid gender responses, including "Prefer to self-describe"
valid_genders <- c("Male", "Female", "Non-binary / third gender", 
                   "Prefer not to say", "Prefer to self-describe")

# Filter out rows with invalid gender values
survey_data_cleaned <- survey_data_cleaned %>%
  filter(Q2 %in% valid_genders)

# Check Gender distribution after cleaning
gender_distribution <- table(survey_data_cleaned$Q2)
print("Gender Distribution (after cleaning):")
print(gender_distribution)

# Calculate the percentage distribution for Gender
gender_percentage <- prop.table(gender_distribution) * 100
print("Gender Percentage Distribution (after cleaning):")
print(gender_percentage)

# Gender distribution
gender_distribution <- table(final_survey_data$Q2)
print("Gender Distribution:")
print(gender_distribution)

# Gender distribution as percentages
gender_percentage <- prop.table(gender_distribution) * 100
print("Gender Percentage Distribution:")
print(gender_percentage)

# Create a bar plot for gender distribution
ggplot(data = survey_data_cleaned, aes(x = Q2)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()


# Gender distribution for British respondents
british_data <- survey_data_cleaned %>% filter(Q3 == "British")
british_gender_distribution <- table(british_data$Q2)
print("British Gender Distribution:")
print(british_gender_distribution)


chinese_data <- survey_data_cleaned %>% filter(Q3 == "Chinese")
chinese_gender_distribution <- table(chinese_data$Q2)
print("Chinese Gender Distribution:")
print(chinese_gender_distribution)

# Combine British and Chinese gender data into a single data frame
british_data <- survey_data_cleaned %>% filter(Q3 == "British") %>% mutate(Nationality = "British")
chinese_data <- survey_data_cleaned %>% filter(Q3 == "Chinese") %>% mutate(Nationality = "Chinese")

combined_data <- bind_rows(british_data, chinese_data)

# Create the combined gender distribution plot
combined_gender_plot <- ggplot(combined_data, aes(x = Q2, fill = Nationality)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution by Nationality", x = "Gender", y = "Count") +
  theme_minimal()

# Display the plot
print(combined_gender_plot)

# Statistical Tests to Ensure Equivalence Between British and Chinese Groups

# Q3 is the nationality column (British or Chinese)
# Age comparison
# Check normality for age in both groups
shapiro_british <- shapiro.test(survey_data_cleaned$Q1[survey_data_cleaned$Q3 == "British"])
shapiro_chinese <- shapiro.test(survey_data_cleaned$Q1[survey_data_cleaned$Q3 == "Chinese"])

# Print Shapiro-Wilk test results
print("Shapiro-Wilk Test for Age - British:")
print(shapiro_british)
print("Shapiro-Wilk Test for Age - Chinese:")
print(shapiro_chinese)

# The data from previous test is not norma;.
# So now apply Mann-Whitney U test
if (shapiro_british$p.value > 0.05 & shapiro_chinese$p.value > 0.05) {
  # Variance homogeneity test
  levene_test <- leveneTest(Q1 ~ Q3, data = survey_data_cleaned)
  print(levene_test)
  
  if (levene_test$p.value > 0.05) {
    # Perform t-test
    age_t_test <- t.test(Q1 ~ Q3, data = survey_data_cleaned)
    print("T-test for Age by Nationality:")
    print(age_t_test)
  } else {
    # Use Welch's t-test
    age_welch_test <- t.test(Q1 ~ Q3, data = survey_data_cleaned, var.equal = FALSE)
    print("Welch's T-test for Age by Nationality:")
    print(age_welch_test)
  }
} else {
  # Perform Mann-Whitney U test
  age_mann_whitney <- wilcox.test(Q1 ~ Q3, data = survey_data_cleaned)
  print("Mann-Whitney U Test for Age by Nationality:")
  print(age_mann_whitney)
}

 # Create a boxplot for age distribution by nationality(Appendix)
   age_boxplot <- ggplot(survey_data_cleaned, aes(x = Q3, y = Q1, fill = Q3)) +
     geom_boxplot() +
     labs(
         title = "Age Distribution by Nationality",
         x = "Nationality",
         y = "Age",
         caption = "Boxplot showing age distribution for British and Chinese participants"
       ) 
     theme_minimal()

   # Display the plot
   print(age_boxplot)

# Create a data frame to store the Mann-Whitney U test results
mann_whitney_results <- data.frame(
  Test = "Mann-Whitney U Test for Age by Nationality",
  Statistic = 6011.5,
  P_Value = 0.06407,
  Conclusion = "Not statistically significant at the 0.05 level"
)

#Display the table

mann_whitney_table <- mann_whitney_results %>%
  kable("html", caption = "Mann-Whitney U Test Results for Age by Nationality") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE)

# Show the table
mann_whitney_table

#############################################################################

# Gender Logistic Regression Analysis
# Convert Gender to a binary variable (e.g., Female = 1, Male = 0)
survey_data_cleaned$gender_binary <- ifelse(survey_data_cleaned$Q2 == "Female", 1, 0)

# Perform logistic regression
gender_logit <- glm(gender_binary ~ Q3, family = binomial, data = survey_data_cleaned)
summary(gender_logit)

# Extract and display the key results
gender_logit_summary <- tidy(gender_logit)
print(gender_logit_summary)

# Create a summary table for logistic regression results
gender_logit_table <- gender_logit_summary %>%
  select(term, estimate, std.error, statistic = statistic, p.value) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Standard Error` = std.error,
    `Z Value` = statistic,
    `P Value` = p.value
  )

# Display the logistic regression summary table
gender_logit_table %>%
  kable("html", caption = "Logistic Regression Results for Gender by Nationality") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE)

# Plot the logistic regression results
ggplot(gender_logit_summary, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Logistic Regression of Gender by Nationality",
       x = "Nationality",
       y = "Logit Estimate",
       fill = "Term") +
  theme_minimal()
################################################################################
# Descriptive analysis for Nationality
# Remove rows where Nationality (Q3) is missing or contains unwanted values
final_survey_data <- final_survey_data %>%
  filter(!is.na(Q3) & !Q3 %in% c('{"ImportId":"QID5"}', 'What is your nationality? - Selected Choice', 'Other'))

# Verify that rows with missing or unwanted Nationality are removed
print(table(final_survey_data$Q3))

# Nationality distribution
nationality_distribution <- table(final_survey_data$Q3)
print("Nationality Distribution:")
print(nationality_distribution)

# Nationality distribution as percentages
nationality_percentage <- prop.table(nationality_distribution) * 100
print("Nationality Percentage Distribution:")
print(nationality_percentage)

# Nationality distribution plot
# Create a data frame for plotting
nationality_df <- as.data.frame(nationality_distribution)
colnames(nationality_df) <- c("Nationality", "Count")

# Create the bar chart
nationality_bar <- ggplot(nationality_df, aes(x = Nationality, y = Count, fill = Nationality)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Nationality Distribution", x = "Nationality", y = "Count") +
  geom_text(aes(label = paste0(Count, " (", round((Count / sum(Count)) * 100, 2), "%)")), 
            vjust = -0.5)

# Display the bar chart
print(nationality_bar)
grid::grid.text("Figure 3: Nationality Distribution", x = 0.5, y = -0.1, gp = grid::gpar(fontsize = 14))
##########################################################
# Convert 'Q4' (Years Spent in Country of Nationality) and 'Q1' (Age) to numeric
years_in_country <- as.numeric(final_survey_data$Q4)
age <- as.numeric(final_survey_data$Q1)

# Descriptive statistics for 'Years Spent in Country of Nationality'
years_summary <- summary(years_in_country)
print("Descriptive Statistics for Years Spent in Country of Nationality:")
print(years_summary)

# Identify and display unrealistic outliers in the 'Years Spent in Country' column (e.g., above 100 years)
outliers <- final_survey_data %>% filter(as.numeric(Q4) > 100)
print("Outliers identified in 'Years Spent in Country':")
print(outliers)

# Remove outliers from the dataset (e.g., remove entries where years in country is greater than 100)
final_survey_data_clean <- final_survey_data %>%
  filter(as.numeric(Q4) <= 100) %>%
  drop_na(Q1, Q4)  # Ensure there are no missing values in the relevant columns

# Plot the data without the outliers
plot(as.numeric(final_survey_data_clean$Q1), as.numeric(final_survey_data_clean$Q4),
     xlab = "Age",
     ylab = "Years Spent in Country of Nationality",
     main = "Cross-tabulation of Age and Years Spent in Country (After Cleaning)",
     pch = 16,
     col = "blue")

# Add a trend line to the cleaned scatter plot
abline(lm(as.numeric(Q4) ~ as.numeric(Q1), data = final_survey_data_clean), col = "red")

# Calculate Pearson correlation coefficient
correlation <- cor(as.numeric(final_survey_data_clean$Q1), as.numeric(final_survey_data_clean$Q4))
print(paste("Correlation between Age and Years Spent in Country:", round(correlation, 2)))

# Fit a linear regression model
model <- lm(as.numeric(Q4) ~ as.numeric(Q1), data = final_survey_data_clean)
summary_model <- summary(model)
print("Linear Regression Summary:")
print(summary_model)

# Table for this
# Descriptive statistics for Age
age_summary <- summary(as.numeric(final_survey_data_clean$Q1))
print("Descriptive Statistics for Age:")
print(age_summary)

# Descriptive statistics for Years Spent in Country
years_summary <- summary(as.numeric(final_survey_data_clean$Q4))
print("Descriptive Statistics for Years Spent in Country:")
print(years_summary)

# Calculate descriptive statistics for Years Spent in Country
years_summary <- summary(as.numeric(final_survey_data_clean$Q4))

# Create a data frame to store the summary statistics
years_summary_table <- data.frame(
  Statistic = c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"),
  Value = c(years_summary["Min."], years_summary["1st Qu."], 
            years_summary["Median"], years_summary["Mean"], 
            years_summary["3rd Qu."], years_summary["Max."])
)

# Print the table
print("Descriptive Statistics for Years Spent in Country of Nationality:")
print(years_summary_table)

###############################################################################
# Descriptive analysis for Internet Usage
# Convert Q5 to numeric (if it's not already numeric)
final_survey_data$Q5 <- as.numeric(final_survey_data$Q5)


internet_usage_summary <- summary(final_survey_data$Q5)
print(internet_usage_summary)

# Create a data frame to store the summary statistics
internet_usage_table <- data.frame(
  Statistic = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum"),
  Hours = c(internet_usage_summary["Min."], internet_usage_summary["1st Qu."], 
            internet_usage_summary["Median"], internet_usage_summary["Mean"], 
            internet_usage_summary["3rd Qu."], internet_usage_summary["Max."])
)

# Create the table grob
table_grob <- tableGrob(internet_usage_table)

# Create a plot with the table
grid.arrange(table_grob, top = "Summary of Internet Usage for Leisure Activities")


###########################################################
# Example of separating multiple responses into individual rows
platforms_long <- final_survey_data %>%
  separate_rows(Q6, sep = ",") %>%
  filter(Q6 != "")

# Check the unique platforms after splitting
unique_platforms <- unique(platforms_long$Q6)
print("Unique live streaming platforms used:")
print(unique_platforms)
# Calculate the frequency of each platform
platform_frequency <- platforms_long %>%
  group_by(Q6) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Display the frequency table
print("Frequency of each live streaming platform used:")
print(platform_frequency)
# Bar plot for platform usage
library(ggplot2)

ggplot(platform_frequency, aes(x = reorder(Q6, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Live Streaming Platform", y = "Count", title = "Usage of Live Streaming Platforms in the Last 12 Months") +
  theme_minimal() +
  coord_flip()  # Flip coordinates for better readability if there are many platforms

#############################################################################
#Section2
# Most watched content type
# Filter out unwanted entries 
final_survey_data <- final_survey_data[!grepl("ImportId|What type of content", final_survey_data$Q2.1), ]

# Calculate frequencies for each content type
content_freq <- table(final_survey_data$Q2.1)

# Convert to a data frame for easier inspection
content_df <- as.data.frame(content_freq)
colnames(content_df) <- c("Content_Type", "Count")

# Print out the frequencies
print(content_df)
# Create the data frame
content_df <- data.frame(
  Content_Type = c(
    "Fitness and Health", 
    "Gaming", 
    "Lifestyle", 
    "Other", 
    "Social Interaction", 
    "Technology"
  ),
  Count = c(11, 103, 41, 10, 23, 5)
)

# Print the data frame to verify
print(content_df)
# Create the bar graph
ggplot(data = content_df, aes(x = Content_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Most Watched Content Type on Streaming Platforms", x = "Content Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################################################################
# Convert the Q3.1(Time spend on live streaming) column into an ordered factor
final_survey_data$hours_watching_streams <- factor(final_survey_data$Q3.1,
                                                   levels = c("Never", "Less than 1 hour", "1-2 hours", "2-3 hours", "3-4 hours", "More than 4 hours"),
                                                   ordered = TRUE)
# Create a summary table
summary_hours_watching <- table(final_survey_data$hours_watching_streams)
print(summary_hours_watching)

ggplot(data=as.data.frame(summary_hours_watching), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="lightblue", color="black") +
  labs(title="Hours Spent Watching Live Streams Per Day", x="Hours Watching Streams", y="Count") +
  theme_minimal()

##############################################################################
# Remove rows with NA values in the Q4.1 (Interaction) column
final_survey_data_clean <- final_survey_data[!is.na(final_survey_data$Q4.1) & final_survey_data$Q4.1 != "", ]

# Convert the interaction frequency to an ordered factor using the Q4.1 column
final_survey_data_clean$interaction_frequency <- factor(final_survey_data_clean$Q4.1,
                                                        levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost every time"),
                                                        ordered = TRUE)

# Summary of interaction frequency
interaction_summary <- summary(final_survey_data_clean$interaction_frequency)
print(interaction_summary)

# Bar chart to visualize interaction frequency
ggplot(data=final_survey_data_clean, aes(x=interaction_frequency)) +
  geom_bar(fill="lightblue", color="black") +
  labs(title="Frequency of Interaction During Live Streams", x="Interaction Frequency", y="Count") +
  theme_minimal()

#######################################################################
#To calculate the overall mean for each scene
# Convert the columns to numeric, handling potential issues with non-numeric entries
final_survey_data$Q1_1 <- as.numeric(as.character(final_survey_data$Q1_1))
final_survey_data$Q1_1.1 <- as.numeric(as.character(final_survey_data$Q1_1.1))
final_survey_data$Q1_1.2 <- as.numeric(as.character(final_survey_data$Q1_1.2))
final_survey_data$Q1_1.3 <- as.numeric(as.character(final_survey_data$Q1_1.3))
#calculate
mean_verbal_harm_overall <- mean(final_survey_data$Q1_1, na.rm = TRUE)
mean_stalking_harm_overall <- mean(final_survey_data$Q1_1.1, na.rm = TRUE)
mean_sexual_harm_overall <- mean(final_survey_data$Q1_1.2, na.rm = TRUE)
mean_racial_harm_overall <- mean(final_survey_data$Q1_1.3, na.rm = TRUE)

print(mean_verbal_harm_overall)
print(mean_stalking_harm_overall)
print(mean_sexual_harm_overall)
print(mean_racial_harm_overall)

# To print Table
# Create a data frame to store the results
mean_table <- data.frame(
  Scene = c("Verbal Harassment", "Cyber Stalking", "Sexual Harassment", "Racial Harassment"),
  `Mean Perceived Harm` = c(mean_verbal_harm_overall, mean_stalking_harm_overall, mean_sexual_harm_overall, mean_racial_harm_overall)
)

# Generate a styled table using kable and kableExtra
mean_table %>%
  kable("html", caption = "Overall Mean Perceived Harm in Each Scene") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, background = "lightblue")
################################################################
#By cotent type, calculate mean
# Remove rows with NA in Q2.1
final_survey_data_clean <- final_survey_data[!is.na(final_survey_data$Q2.1), ]
# Mean perceived harm by content type for Verbal Harassment
mean_verbal_harm_by_content <- aggregate(Q1_1 ~ Q2.1, data = final_survey_data, FUN = mean, na.rm = TRUE)

# Mean perceived harm by content type for Stalking/Cyberstalking
mean_stalking_harm_by_content <- aggregate(Q1_1.1 ~ Q2.1, data = final_survey_data, FUN = mean, na.rm = TRUE)

# Mean perceived harm by content type for Sexual Harassment
mean_sexual_harm_by_content <- aggregate(Q1_1.2 ~ Q2.1, data = final_survey_data, FUN = mean, na.rm = TRUE)

# Mean perceived harm by content type for Racial/Ethnic Harassment
mean_racial_harm_by_content <- aggregate(Q1_1.3 ~ Q2.1, data = final_survey_data, FUN = mean, na.rm = TRUE)

# Print the results
print(mean_verbal_harm_by_content)
print(mean_stalking_harm_by_content)
print(mean_sexual_harm_by_content)
print(mean_racial_harm_by_content)

install.packages("tidyverse")
library(tidyverse)

# Create the data frame with simplified Content_Type
content_df <- data.frame(
  Content_Type = c(
    "Fitness and Health",       
    "Gaming", 
    "Lifestyle", 
    "Other", 
    "Social Interaction",         
    "Technology"            
  ),
  Verbal_Harm = mean_verbal_harm_by_content$Q1_1,
  Stalking_Harm = mean_stalking_harm_by_content$Q1_1.1,
  Sexual_Harm = mean_sexual_harm_by_content$Q1_1.2,
  Racial_Harm = mean_racial_harm_by_content$Q1_1.3
)


# Reshape the data using pivot_longer from tidyr
content_df_long <- content_df %>%
  pivot_longer(cols = c(Verbal_Harm, Stalking_Harm, Sexual_Harm, Racial_Harm),
               names_to = "Scene", values_to = "Mean_Harm")

# Set the levels of the Scene factor to the desired order
content_df_long$Scene <- factor(content_df_long$Scene, 
                                levels = c("Verbal_Harm", "Stalking_Harm", "Sexual_Harm", "Racial_Harm"))

# Now plot the grouped bar plot by content type and scene with the specified order
ggplot(content_df_long, aes(x = Content_Type, y = Mean_Harm, fill = Scene)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Perceived Harm by Content Type and Scene",
       x = "Content Type", y = "Mean Perceived Harm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))


#####################################################################
# Figure out content type in Other
# Extract the answers for Q2_6_TEXT
q2_6_text_answers <- survey_data %>% select(Q2_6_TEXT)

# View the extracted answers
print(q2_6_text_answers)

data <- c("音乐", "唱歌", "音乐", "Sports", "no streaming", "None", "Music related", "体育", "no streaming", "Don't use, can't stand them")


category_counts <- table(data)


category_proportions <- prop.table(category_counts)


category_percentages <- category_proportions * 100


print(category_percentages)


#Print Table for this now
content_table <- content_df %>%
  kable("html", caption = "Mean Perceived Harm by Content Type and Scene") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:5, background = "lightblue")

print(content_table)
############################################################
# Chi-square Test on all content type scenarios

# Chi-square Test for Verbal Harassment by Nationality
# Assuming Q1_1 is the categorical score for Verbal Harassment and Q3 is Nationality
chisq_verbal <- chisq.test(table(final_survey_data$Q1_1, final_survey_data$Q3))
print(chisq_verbal)

# Chi-square Test for Stalking/Cyberstalking by Nationality
# Assuming Q1_1.1 is the categorical score for Stalking/Cyberstalking and Q3 is Nationality
chisq_stalking <- chisq.test(table(final_survey_data$Q1_1.1, final_survey_data$Q3))
print(chisq_stalking)

# Chi-square Test for Sexual Harassment by Nationality
# Assuming Q1_1.2 is the categorical score for Sexual Harassment and Q3 is Nationality
chisq_sexual <- chisq.test(table(final_survey_data$Q1_1.2, final_survey_data$Q3))
print(chisq_sexual)

# Chi-square Test for Racial/Ethnic Harassment by Nationality
# Assuming Q1_1.3 is the categorical score for Racial/Ethnic Harassment and Q3 is Nationality
chisq_racial <- chisq.test(table(final_survey_data$Q1_1.3, final_survey_data$Q3))
print(chisq_racial)

# Create a data frame to store the Chi-square results
chisq_results <- data.frame(
  Scene = c("Verbal Harassment", "Stalking/Cyberstalking", "Sexual Harassment", "Racial/Ethnic Harassment"),
  Chi_Squared_Value = c(chisq_verbal$statistic, chisq_stalking$statistic, 
                        chisq_sexual$statistic, chisq_racial$statistic),
  P_Value = c(chisq_verbal$p.value, chisq_stalking$p.value, 
              chisq_sexual$p.value, chisq_racial$p.value)
)

# Print the results in a table with a specific caption for content type analysis
chisq_table <- chisq_results %>%
  kable("html", caption = "Chi-square Test Results for Harassment Type Scenarios by Nationality") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE)

# Display the table
print(chisq_table)

####################################################
# Multinomial test for content and Harassment
# Load necessary package
library(nnet)

# Ensure no missing data by removing rows with NA values
final_survey_data_clean <- na.omit(final_survey_data)

# Perform multinomial logistic regression analysis for each type of harassment based on content type (Q2.1)
# Verbal Harassment Regression
verbal_multinom <- multinom(Q1_1 ~ Q2.1, data = final_survey_data_clean)
# Stalking/Cyberstalking Regression
stalking_multinom <- multinom(Q1_1.1 ~ Q2.1, data = final_survey_data_clean)
# Sexual Harassment Regression
sexual_multinom <- multinom(Q1_1.2 ~ Q2.1, data = final_survey_data_clean)
# Racial/Ethnic Harassment Regression
racial_multinom <- multinom(Q1_1.3 ~ Q2.1, data = final_survey_data_clean)

# Tidy up the results for easier manipulation and plotting
verbal_multinom_results <- tidy(verbal_multinom)
stalking_multinom_results <- tidy(stalking_multinom)
sexual_multinom_results <- tidy(sexual_multinom)
racial_multinom_results <- tidy(racial_multinom)

# Add a 'Scene' column to distinguish between the different harassment types
verbal_multinom_results$Scene <- "Verbal Harassment"
stalking_multinom_results$Scene <- "Stalking/Cyberstalking"
sexual_multinom_results$Scene <- "Sexual Harassment"
racial_multinom_results$Scene <- "Racial/Ethnic Harassment"

# Combine all the results into one data frame
content_multinom_results <- rbind(verbal_multinom_results, stalking_multinom_results,
                                  sexual_multinom_results, racial_multinom_results)

# Exclude the intercept term from the results since it's not informative for this analysis
content_multinom_results <- content_multinom_results[content_multinom_results$term != "(Intercept)",]

# Recode the 'term' variable to provide more descriptive content type labels using the full names
content_multinom_results$term <- recode(content_multinom_results$term, 
                                        "Q2.1Fitness and health （This category features live streams related to physical exercise, wellness routines, healthy eating, and mental health practices. Streamers may provide workout sessions, yoga classes, nutrition advice, or discussions on maintaining a healthy lifestyle.）" = "Fitness & Health", 
                                        "Q2.1Gaming （This category includes live streams focused on video games, including gameplay, tutorials, competitive gaming (eSports), and game-related commentary. ）" = "Gaming",
                                        "Q2.1Lifestyle (This category encompasses a broad range of content related to daily life, hobbies, and personal interests. It may include cooking shows, travel vlogs, fashion and beauty tips, home improvement projects, and other lifestyle topics.)" = "Lifestyle",
                                        "Q2.1Other (please specify)" = "Other",
                                        "Q2.1Social Interaction （Streams in this category focus on personal interactions and community building. This can include talk shows, Q&A sessions, casual chats, and collaborative activities. The emphasis is on engaging with the audience and fostering a sense of community.）" = "Social Interaction",
                                        "Q2.1Technology (Streams in this category focus on technology-related content, including product reviews, tech news, gadget unboxings, software tutorials, and coding sessions. )" = "Tech")

# Plotting the results
ggplot(content_multinom_results, aes(x = term, y = estimate, fill = Scene)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(
    title = "Multinomial Logistic Regression Coefficients
    by Harassment Types and Content Type",
    x = "Content Type", 
    y = "Regression Coefficient",
    caption = "Error bars represent standard errors of the estimates"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10)) +  # Adjust size if needed
  coord_flip()  # Flip the plot to make it horizontal

#########################################################
# Remove rows with NA in Q3
final_survey_data_clean <- final_survey_data[!is.na(final_survey_data$Q3), ]

# Calculate the verbal harm mean by nationality (assuming Q3 is the nationality column)
verbal_mean_by_nationality <- aggregate(Q1_1 ~ Q3, data = final_survey_data_clean, FUN = mean, na.rm = TRUE)
print(verbal_mean_by_nationality)

# Cyber Stalking Mean by Nationality
stalking_mean_by_nationality <- aggregate(Q1_1.1 ~ Q3, data = final_survey_data_clean, FUN = mean, na.rm = TRUE)
print(stalking_mean_by_nationality)

# Sexual Harassment Mean by Nationality
sexual_mean_by_nationality <- aggregate(Q1_1.2 ~ Q3, data = final_survey_data_clean, FUN = mean, na.rm = TRUE)
print(sexual_mean_by_nationality)

# Racial Harassment Mean by Nationality
racial_mean_by_nationality <- aggregate(Q1_1.3 ~ Q3, data = final_survey_data_clean, FUN = mean, na.rm = TRUE)
print(racial_mean_by_nationality)

# Combine the mean data by nationality into one data frame
nationality_df <- data.frame(
  Nationality = verbal_mean_by_nationality$Q3,
  Verbal_Harm = verbal_mean_by_nationality$Q1_1,
  Stalking_Harm = stalking_mean_by_nationality$Q1_1.1,
  Sexual_Harm = sexual_mean_by_nationality$Q1_1.2,
  Racial_Harm = racial_mean_by_nationality$Q1_1.3
)

# Reshape the data using pivot_longer from tidyr
nationality_df_long <- nationality_df %>%
  pivot_longer(cols = c(Verbal_Harm, Stalking_Harm, Sexual_Harm, Racial_Harm),
               names_to = "Scene", values_to = "Mean_Harm")

# Plot the grouped bar plot by nationality and scene
ggplot(nationality_df_long, aes(x = Nationality, y = Mean_Harm, fill = Scene)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Perceived Harm by Nationality and Scene",
       x = "Nationality", y = "Mean Perceived Harm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Set the levels of the Scene factor to the desired order
nationality_df_long$Scene <- factor(nationality_df_long$Scene, 
                                    levels = c("Verbal_Harm", "Stalking_Harm", "Sexual_Harm", "Racial_Harm"))

# Now plot the grouped bar plot by nationality and scene with the specified order
ggplot(nationality_df_long, aes(x = Nationality, y = Mean_Harm, fill = Scene)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Perceived Harm by Nationality and Scene",
       x = "Nationality", y = "Mean Perceived Harm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Print the table 
print(nationality_table)
# Count the number of non-missing responses for each combination of Nationality and Scene
response_counts <- final_survey_data_clean %>%
  summarise(
    Verbal_Harm_Count = sum(!is.na(Q1_1)),
    Stalking_Harm_Count = sum(!is.na(Q1_1.1)),
    Sexual_Harm_Count = sum(!is.na(Q1_1.2)),
    Racial_Harm_Count = sum(!is.na(Q1_1.3))
  )

# Print the response counts for each scene
print(response_counts)


# Chi-square Test for Content Type and Verbal Harassment
# Assuming Q1_1 is the categorical score for Verbal Harassment and Q2 is Content Type
chisq_verbal_content <- chisq.test(table(final_survey_data$Q1_1, final_survey_data$Q2))
print(chisq_verbal_content)

# Chi-square Test for Content Type and Stalking/Cyberstalking
# Assuming Q1_1.1 is the categorical score for Stalking/Cyberstalking and Q2 is Content Type
chisq_stalking_content <- chisq.test(table(final_survey_data$Q1_1.1, final_survey_data$Q2))
print(chisq_stalking_content)

# Chi-square Test for Content Type and Sexual Harassment
# Assuming Q1_1.2 is the categorical score for Sexual Harassment and Q2 is Content Type
chisq_sexual_content <- chisq.test(table(final_survey_data$Q1_1.2, final_survey_data$Q2))
print(chisq_sexual_content)

# Chi-square Test for Content Type and Racial/Ethnic Harassment
# Assuming Q1_1.3 is the categorical score for Racial/Ethnic Harassment and Q2 is Content Type
chisq_racial_content <- chisq.test(table(final_survey_data$Q1_1.3, final_survey_data$Q2))
print(chisq_racial_content)

# Create a data frame to store the Chi-square results
chisq_content_results <- data.frame(
  Scene = c("Verbal Harassment", "Stalking/Cyberstalking", "Sexual Harassment", "Racial/Ethnic Harassment"),
  Chi_Squared_Value = c(chisq_verbal_content$statistic, chisq_stalking_content$statistic, 
                        chisq_sexual_content$statistic, chisq_racial_content$statistic),
  P_Value = c(chisq_verbal_content$p.value, chisq_stalking_content$p.value, 
              chisq_sexual_content$p.value, chisq_racial_content$p.value)
)

# Print the results in a table with a specific caption for content type and harassment type analysis
chisq_content_table <- chisq_content_results %>%
  kable("html", caption = "Chi-square Test Results by Content Type and Harassment Type") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE)

# Display the table
print(chisq_content_table)

#Now regression for nationality
# Remove rows with any NA values across the dataset
final_survey_data_clean <- na.omit(final_survey_data)

# Verbal Harassment Regression
verbal_regression_clean <- lm(Q1_1 ~ Q3, data = final_survey_data_clean)
summary(verbal_regression_clean)

# Cyber Stalking Regression
stalking_regression_clean <- lm(Q1_1.1 ~ Q3, data = final_survey_data_clean)
summary(stalking_regression_clean)

# Sexual Harassment Regression
sexual_regression_clean <- lm(Q1_1.2 ~ Q3, data = final_survey_data_clean)
summary(sexual_regression_clean)

# Racial Harassment Regression
racial_regression_clean <- lm(Q1_1.3 ~ Q3, data = final_survey_data_clean)
summary(racial_regression_clean)

nationality_verbal_results <- tidy(verbal_regression_clean)
nationality_stalking_results <- tidy(stalking_regression_clean)
nationality_sexual_results <- tidy(sexual_regression_clean)
nationality_racial_results <- tidy(racial_regression_clean)

nationality_verbal_results$Scene <- "Verbal Harassment"
nationality_stalking_results$Scene <- "Stalking/Cyberstalking"
nationality_sexual_results$Scene <- "Sexual Harassment"
nationality_racial_results$Scene <- "Racial/Ethnic Harassment"

nationality_all_results <- rbind(nationality_verbal_results, nationality_stalking_results,
                                 nationality_sexual_results, nationality_racial_results)

nationality_all_results <- nationality_all_results[nationality_all_results$term != "(Intercept)",]
nationality_all_results$term <- recode(nationality_all_results$term, 
                                       "Q3Chinese" = "Nationality: Chinese", 
                                       "Q3Other (please specify)" = "Nationality: Other")

ggplot(nationality_all_results, aes(x = term, y = estimate, fill = Scene)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(
    title = "Regression Coefficients by Harassment Types and Nationality",
    x = "Nationality (Reference: British)", 
    y = "Regression Coefficient",
    caption = "Error bars represent standard errors of the estimates"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#######################################################################
#Now move to Q2_1,which represents inclined report data

# Remove NA data from the dataset
final_survey_data_clean <- na.omit(final_survey_data)

# Convert the columns to numeric, handling potential issues with non-numeric entries
final_survey_data_clean$Q2_1 <- as.numeric(as.character(final_survey_data_clean$Q2_1))
final_survey_data_clean$Q2_1.1 <- as.numeric(as.character(final_survey_data_clean$Q2_1.1))
final_survey_data_clean$Q2_1.2 <- as.numeric(as.character(final_survey_data_clean$Q2_1.2))
final_survey_data_clean$Q2_1.3 <- as.numeric(as.character(final_survey_data_clean$Q2_1.3))

# For Verbal Harassment
verbal_residuals <- residuals(verbal_anova_content_type)
shapiro_verbal <- shapiro.test(verbal_residuals)
print(shapiro_verbal)

# For Stalking/Cyberstalking
stalking_residuals <- residuals(stalking_anova_content_type)
shapiro_stalking <- shapiro.test(stalking_residuals)
print(shapiro_stalking)

# For Sexual Harassment
sexual_residuals <- residuals(sexual_anova_content_type)
shapiro_sexual <- shapiro.test(sexual_residuals)
print(shapiro_sexual)

# For Racial/Ethnic Harassment
racial_residuals <- residuals(racial_anova_content_type)
shapiro_racial <- shapiro.test(racial_residuals)
print(shapiro_racial)

# Calculate the mean reporting likelihood for Verbal Harassment
verbal_reporting_mean <- mean(final_survey_data_clean$Q2_1, na.rm = TRUE)
verbal_reporting <- data.frame(Scenario = "Verbal Harassment", MeanReportingLikelihood = verbal_reporting_mean)

# Calculate the mean reporting likelihood for Stalking/Cyberstalking
stalking_reporting_mean <- mean(final_survey_data_clean$Q2_1.1, na.rm = TRUE)
stalking_reporting <- data.frame(Scenario = "Stalking/Cyberstalking", MeanReportingLikelihood = stalking_reporting_mean)

# Calculate the mean reporting likelihood for Sexual Harassment
sexual_reporting_mean <- mean(final_survey_data_clean$Q2_1.2, na.rm = TRUE)
sexual_reporting <- data.frame(Scenario = "Sexual Harassment", MeanReportingLikelihood = sexual_reporting_mean)

# Calculate the mean reporting likelihood for Racial/Ethnic Harassment
racial_reporting_mean <- mean(final_survey_data_clean$Q2_1.3, na.rm = TRUE)
racial_reporting <- data.frame(Scenario = "Racial/Ethnic Harassment", MeanReportingLikelihood = racial_reporting_mean)
# Define the order of scenarios
combined_reporting_mean$Scenario <- factor(combined_reporting_mean$Scenario, 
                                           levels = c("Verbal Harassment", 
                                                      "Stalking/Cyberstalking", 
                                                      "Sexual Harassment", 
                                                      "Racial/Ethnic Harassment"))
# Combine the data into one dataframe
combined_reporting_mean <- rbind(verbal_reporting, stalking_reporting, sexual_reporting, racial_reporting)

# Plot the mean reporting likelihood by scenario
ggplot(combined_reporting_mean, aes(x = Scenario, y = MeanReportingLikelihood, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Mean Reporting Likelihood for different Online Harassment types",
    x = "Scenario",
    y = "Mean Reporting Likelihood (Scale 0-100)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(verbal_reporting_mean)
print(stalking_reporting_mean)
print(sexual_reporting_mean)
print(racial_reporting_mean)

# Convert character data to numeric where needed
final_survey_data_clean$Q2_1 <- as.numeric(final_survey_data_clean$Q2_1)
final_survey_data_clean$Q2_1.1 <- as.numeric(final_survey_data_clean$Q2_1.1)
final_survey_data_clean$Q2_1.2 <- as.numeric(final_survey_data_clean$Q2_1.2)
final_survey_data_clean$Q2_1.3 <- as.numeric(final_survey_data_clean$Q2_1.3)

# Convert categorical variables to factor
final_survey_data_clean$Q3 <- as.factor(final_survey_data_clean$Q3)  # Nationality
final_survey_data_clean$Q2.1 <- as.factor(final_survey_data_clean$Q2.1)  # Content type

# Calculate the mean reporting likelihood for each content type (Q2.1)
mean_report_by_content_type <- final_survey_data_clean %>%
  group_by(Q2.1) %>%  # Group by content type
  summarise(
    mean_verbal_harassment = mean(Q2_1, na.rm = TRUE),
    mean_stalking = mean(Q2_1.1, na.rm = TRUE),
    mean_sexual_harassment = mean(Q2_1.2, na.rm = TRUE),
    mean_racial_harassment = mean(Q2_1.3, na.rm = TRUE)
  )

mean_report_by_content_type$Q2.1 <- recode(mean_report_by_content_type$Q2.1, 
                                           "Fitness and health （This category features live streams related to physical exercise, wellness routines, healthy eating, and mental health practices. Streamers may provide workout sessions, yoga classes, nutrition advice, or discussions on maintaining a healthy lifestyle.）" = "Fitness & Health", 
                                           "Gaming （This category includes live streams focused on video games, including gameplay, tutorials, competitive gaming (eSports), and game-related commentary. ）" = "Gaming",
                                           "Lifestyle (This category encompasses a broad range of content related to daily life, hobbies, and personal interests. It may include cooking shows, travel vlogs, fashion and beauty tips, home improvement projects, and other lifestyle topics.)" = "Lifestyle",
                                           "Other (please specify)" = "Other",
                                           "Social Interaction （Streams in this category focus on personal interactions and community building. This can include talk shows, Q&A sessions, casual chats, and collaborative activities. The emphasis is on engaging with the audience and fostering a sense of community.）" = "Social Interaction",
                                           "Technology (Streams in this category focus on technology-related content, including product reviews, tech news, gadget unboxings, software tutorials, and coding sessions. )" = "Tech")

# Display the table with shortened titles
mean_report_by_content_type %>%
  kable("html", caption = "Mean Reporting Likelihood by Content Type for Different Harassment Scenarios") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE)
##############################################################
# Perform Kruskal-Wallis tests for each harassment type by content type
kruskal_verbal <- kruskal.test(Q2_1 ~ Q2.1, data = final_survey_data_clean)
kruskal_stalking <- kruskal.test(Q2_1.1 ~ Q2.1, data = final_survey_data_clean)
kruskal_sexual <- kruskal.test(Q2_1.2 ~ Q2.1, data = final_survey_data_clean)
kruskal_racial <- kruskal.test(Q2_1.3 ~ Q2.1, data = final_survey_data_clean)

# Create a summary data frame for the Kruskal-Wallis test results
kruskal_summary <- data.frame(
  Scenario = c("Verbal Harassment", "Stalking/Cyberstalking", "Sexual Harassment", "Racial/Ethnic Harassment"),
  Kruskal_Chi_Squared = c(kruskal_verbal$statistic, kruskal_stalking$statistic, kruskal_sexual$statistic, kruskal_racial$statistic),
  Degrees_of_Freedom = c(kruskal_verbal$parameter, kruskal_stalking$parameter, kruskal_sexual$parameter, kruskal_racial$parameter),
  P_Value = c(kruskal_verbal$p.value, kruskal_stalking$p.value, kruskal_sexual$p.value, kruskal_racial$p.value)
)

# Display the summary table
kruskal_summary_table <- kruskal_summary %>%
  kable("html", caption = "Kruskal-Wallis Test Results for Content Type and Harassment Type") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE)

# Show the table
kruskal_summary_table
#######################################

# Nationality Association with Reporting likelihood
# Convert character data to numeric
final_survey_data_clean$Q2_1 <- as.numeric(final_survey_data_clean$Q2_1)
final_survey_data_clean$Q2_1.1 <- as.numeric(final_survey_data_clean$Q2_1.1)
final_survey_data_clean$Q2_1.2 <- as.numeric(final_survey_data_clean$Q2_1.2)
final_survey_data_clean$Q2_1.3 <- as.numeric(final_survey_data_clean$Q2_1.3)
final_survey_data_clean$Q3 <- as.factor(final_survey_data_clean$Q3)  # Assuming nationality is categorical
final_survey_data_clean$Q2.1 <- as.factor(final_survey_data_clean$Q2.1)  # Assuming content type is categorical

# Calculate the mean reporting likelihood for each scenario by Nationality
mean_report_by_nationality <- final_survey_data_clean %>%
  group_by(Q3) %>%  # Group by Nationality
  summarise(
    mean_verbal_harassment = mean(Q2_1, na.rm = TRUE),
    mean_stalking = mean(Q2_1.1, na.rm = TRUE),
    mean_sexual_harassment = mean(Q2_1.2, na.rm = TRUE),
    mean_racial_harassment = mean(Q2_1.3, na.rm = TRUE)
  )


# Extract data for each scenario
verbal_content_data <- extract_anova_data(verbal_anova_content_type)
stalking_content_data <- extract_anova_data(stalking_anova_content_type)
sexual_content_data <- extract_anova_data(sexual_anova_content_type)
racial_content_data <- extract_anova_data(racial_anova_content_type)

# Calculate the mean reporting likelihood for each scenario by Nationality
mean_report_by_nationality <- final_survey_data_clean %>%
  group_by(Q3) %>%  # Group by Nationality
  summarise(
    mean_verbal_harassment = mean(as.numeric(Q2_1), na.rm = TRUE),
    mean_stalking = mean(as.numeric(Q2_1.1), na.rm = TRUE),
    mean_sexual_harassment = mean(as.numeric(Q2_1.2), na.rm = TRUE),
    mean_racial_harassment = mean(as.numeric(Q2_1.3), na.rm = TRUE)
  )

# Create and display the table
mean_report_by_nationality %>%
  kable("html", caption = "Mean Reporting Likelihood by Nationality for Different Harassment Scenarios") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE)  # Make the nationality column bold

###########################################################
# Reordering the scenarios for the plot
yes_no_data <- data.frame(
  Scenario = factor(rep(c("Verbal", "Stalking/Cyberstalking", "Sexual", "Racial/Ethnic"), each = 2), 
                    levels = c("Verbal", "Stalking/Cyberstalking", "Sexual", "Racial/Ethnic")),
  Response = rep(c("Yes", "No"), 4),
  Count = c(yes_no_count$Verbal_Harassment_Yes, yes_no_count$Verbal_Harassment_No,
            yes_no_count$Stalking_Harassment_Yes, yes_no_count$Stalking_Harassment_No,
            yes_no_count$Sexual_Harassment_Yes, yes_no_count$Sexual_Harassment_No,
            yes_no_count$Racial_Harassment_Yes, yes_no_count$Racial_Harassment_No)
)

# Creating the bar plot with reordered scenarios
ggplot(yes_no_data, aes(x = Scenario, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Responses to Whether Perception of Harm Changes by Live Stream Type",
       x = "Scenario", y = "Count") +
  theme_minimal()

# Count the number of non-missing responses for each scenario
num_participants_verbal <- sum(!is.na(final_survey_data$Q4.2))
num_participants_stalking <- sum(!is.na(final_survey_data$Q4.3))
num_participants_sexual <- sum(!is.na(final_survey_data$Q4.4))
num_participants_racial <- sum(!is.na(final_survey_data$Q4.5))

# Combine the counts into a data frame
participants_data <- data.frame(
  Scenario = c("Verbal", "Stalking/Cyberstalking", "Sexual", "Racial/Ethnic"),
  Participants = c(num_participants_verbal, num_participants_stalking, num_participants_sexual, num_participants_racial)
)

# Print the number of participants for each scenario
print(participants_data)
##############################################
# Live stream type for highest perceived harm for four online harassment types
# Filter only "Yes" responses for each scenario
verbal_yes <- final_survey_data %>% filter(Q4.2 == "Yes" & !is.na(Q5.1))
stalking_yes <- final_survey_data %>% filter(Q4.3 == "Yes" & !is.na(Q5.2))
sexual_yes <- final_survey_data %>% filter(Q4.4 == "Yes" & !is.na(Q5.3))
racial_yes <- final_survey_data %>% filter(Q4.5 == "Yes" & !is.na(Q5.4))

# Count the preferred live stream type for each scenario
verbal_type_count <- verbal_yes %>% count(Q5.1)
stalking_type_count <- stalking_yes %>% count(Q5.2)
sexual_type_count <- sexual_yes %>% count(Q5.3)
racial_type_count <- racial_yes %>% count(Q5.4)
# Remove any rows where Q5.1 has NA or unwanted categories
final_survey_data_cleaned <- final_survey_data %>%
  filter(!is.na(Q5.1) & Q5.1 != "")  # Assuming unwanted category might be an empty string or NA

# Re-count the preferred live stream type for the Verbal Harassment scenario
verbal_type_count_cleaned <- final_survey_data_cleaned %>% filter(Q4.2 == "Yes") %>% count(Q5.1)



# Calculate the count and percentage for each category in Q5.1
verbal_type_count <- final_survey_data %>%
  filter(Q4.2 == "Yes" & !is.na(Q5.1)) %>%
  count(Q5.1) %>%
  mutate(Percentage = (n / sum(n)) * 100)



# Remove any rows where Q5.1 has NA or unwanted categories
verbal_yes_cleaned <- verbal_yes %>%
  filter(!is.na(Q5.1) & Q5.1 != "")

# Re-count the preferred live stream type for Verbal Harassment
verbal_type_count_cleaned <- verbal_yes_cleaned %>% count(Q5.1)

# Calculate the percentage for each live stream type
verbal_type_count_cleaned <- verbal_type_count_cleaned %>%
  mutate(Percentage = n / sum(n) * 100)

# Re-plotting the cleaned data with percentage labels
ggplot(verbal_type_count_cleaned, aes(x = Q5.1, y = n, fill = Q5.1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5) +
  labs(title = "Live Stream Type for Highest Perceived Harm 
       (Verbal Harassment)",
       x = "Live Stream Type", y = "Count") +
  theme_minimal() +
  coord_flip()

# Remove any rows where Q5.2 has NA or unwanted categories
verbal_yes_cleaned <- verbal_yes %>%
  filter(!is.na(Q5.1) & Q5.1 != "")
# Re-count the preferred live stream type for Stalking/Cyberstalking
verbal_type_count_cleaned <- verbal_yes_cleaned %>% count(Q5.1)

# Stalking/Cyberstalking
stalking_yes_cleaned <- stalking_yes %>%
  filter(!is.na(Q5.2) & Q5.2 != "")

stalking_type_count_cleaned <- stalking_yes_cleaned %>% 
  count(Q5.2) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(stalking_type_count_cleaned, aes(x = Q5.2, y = n, fill = Q5.2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5) +
  labs(title = "Live Stream Type for Highest Perceived Harm 
       (Stalking/Cyberstalking)",
       x = "Live Stream Type", y = "Count") +
  theme_minimal() +
  coord_flip()

# Remove any rows where Q5.3 has NA or unwanted categories
sexual_yes_cleaned <- sexual_yes %>%
  filter(!is.na(Q5.3) & Q5.3 != "")

# Re-count the preferred live stream type for Sexual Harassment
sexual_type_count_cleaned <- sexual_yes_cleaned %>% count(Q5.3)

# Sexual Harassment
sexual_yes_cleaned <- sexual_yes %>%
  filter(!is.na(Q5.3) & Q5.3 != "")

sexual_type_count_cleaned <- sexual_yes_cleaned %>% 
  count(Q5.3) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(sexual_type_count_cleaned, aes(x = Q5.3, y = n, fill = Q5.3)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5) +
  labs(title = "Live Stream Type for Highest Perceived Harm 
       (Sexual Harassment)",
       x = "Live Stream Type", y = "Count") +
  theme_minimal() +
  coord_flip()

# Remove any rows where Q5.4 has NA or unwanted categories
racial_yes_cleaned <- racial_yes %>%
  filter(!is.na(Q5.4) & Q5.4 != "")

# Re-count the preferred live stream type for Racial/Ethnic Harassment
racial_type_count_cleaned <- racial_yes_cleaned %>% count(Q5.4)

# Racial/Ethnic Harassment
racial_yes_cleaned <- racial_yes %>%
  filter(!is.na(Q5.4) & Q5.4 != "")

racial_type_count_cleaned <- racial_yes_cleaned %>% 
  count(Q5.4) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(racial_type_count_cleaned, aes(x = Q5.4, y = n, fill = Q5.4)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5) +
  labs(title = "Live Stream Type for Highest Perceived Harm 
       (Racial/Ethnic Harassment)",
       x = "Live Stream Type", y = "Count") +
  theme_minimal() +
  coord_flip()

 # Filter only "Yes" responses for each scenario
   verbal_yes <- final_survey_data %>% filter(Q4.2 == "Yes" & !is.na(Q5.1))
 stalking_yes <- final_survey_data %>% filter(Q4.3 == "Yes" & !is.na(Q5.2))
 sexual_yes <- final_survey_data %>% filter(Q4.4 == "Yes" & !is.na(Q5.3))
 racial_yes <- final_survey_data %>% filter(Q4.5 == "Yes" & !is.na(Q5.4))
 
   # Count the preferred live stream type for each scenario
   verbal_type_count <- verbal_yes %>% count(Q5.1)
 stalking_type_count <- stalking_yes %>% count(Q5.2)
 sexual_type_count <- sexual_yes %>% count(Q5.3)
 racial_type_count <- racial_yes %>% count(Q5.4)
 # Remove any rows where Q5.1 has NA or unwanted categories
   final_survey_data_cleaned <- final_survey_data %>%
     filter(!is.na(Q5.1) & Q5.1 != "")  # Assuming unwanted category might be an empty string or NA
 
   # Re-count the preferred live stream type for the Verbal Harassment scenario
   verbal_type_count_cleaned <- final_survey_data_cleaned %>% filter(Q4.2 == "Yes") %>% count(Q5.1)
 
   
   
   # Calculate the count and percentage for each category in Q5.1
   verbal_type_count <- final_survey_data %>%
     filter(Q4.2 == "Yes" & !is.na(Q5.1)) %>%
     count(Q5.1) %>%
     mutate(Percentage = (n / sum(n)) * 100)
 
  
   
   # Remove any rows where Q5.1 has NA or unwanted categories
   verbal_yes_cleaned <- verbal_yes %>%
     filter(!is.na(Q5.1) & Q5.1 != "")
 
   # Re-count the preferred live stream type for Verbal Harassment
   verbal_type_count_cleaned <- verbal_yes_cleaned %>% count(Q5.1)

   # Calculate the percentage for each live stream type
   verbal_type_count_cleaned <- verbal_type_count_cleaned %>%
     mutate(Percentage = n / sum(n) * 100)
 
   # Re-plotting the cleaned data with percentage labels
   ggplot(verbal_type_count_cleaned, aes(x = Q5.1, y = n, fill = Q5.1)) +
    geom_bar(stat = "identity") +
     geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                             position = position_stack(vjust = 0.5), 
                             color = "white", size = 3.5) +
     labs(title = "Live Stream Type for Highest Perceived Harm 
        (Verbal Harassment)",
                   x = "Live Stream Type", y = "Count") +
     theme_minimal() +
     coord_flip()


 # Participants for each question
 # For Verbal Harassment
verbal_n <- sum(verbal_contingency_table)
print(paste("Number of participants for Verbal Harassment:", verbal_n))

# For Stalking/Cyberstalking
stalking_n <- sum(stalking_contingency_table)
print(paste("Number of participants for Stalking/Cyberstalking:", stalking_n))

# For Sexual Harassment
sexual_n <- sum(sexual_contingency_table)
print(paste("Number of participants for Sexual Harassment:", sexual_n))

# For Racial/Ethnic Harassment
racial_n <- sum(racial_contingency_table)
print(paste("Number of participants for Racial/Ethnic Harassment:", racial_n))

# Verbal Harassment (Q5.1)
verbal_Q5_responses <- sum(!is.na(final_survey_data$Q5.1))
print(paste("Number of responses for Q5.1 (Verbal Harassment):", verbal_Q5_responses))

# Stalking/Cyberstalking (Q5.2)
stalking_Q5_responses <- sum(!is.na(final_survey_data$Q5.2))
print(paste("Number of responses for Q5.2 (Stalking/Cyberstalking):", stalking_Q5_responses))

# Sexual Harassment (Q5.3)
sexual_Q5_responses <- sum(!is.na(final_survey_data$Q5.3))
print(paste("Number of responses for Q5.3 (Sexual Harassment):", sexual_Q5_responses))

# Racial/Ethnic Harassment (Q5.4)
racial_Q5_responses <- sum(!is.na(final_survey_data$Q5.4))
print(paste("Number of responses for Q5.4 (Racial/Ethnic Harassment):", racial_Q5_responses))

# Load the dataset
final_cleaned_survey_data <- read.csv("C:/Users/jiang/OneDrive/Desktop/Online Hrassment/final_cleaned_survey_data.csv")

# Remove rows where any of the Q5.X columns are blank
final_cleaned_survey_data_filtered <- final_cleaned_survey_data %>%
  filter(
    !is.na(Q5.1) & Q5.1 != "",
    !is.na(Q5.2) & Q5.2 != "",
    !is.na(Q5.3) & Q5.3 != "",
    !is.na(Q5.4) & Q5.4 != ""
  )


# Count the number of non-blank responses for each Q5.X column
Q5_1_responses <- sum(!is.na(final_cleaned_survey_data$Q5.1) & final_cleaned_survey_data$Q5.1 != "")
Q5_2_responses <- sum(!is.na(final_cleaned_survey_data$Q5.2) & final_cleaned_survey_data$Q5.2 != "")
Q5_3_responses <- sum(!is.na(final_cleaned_survey_data$Q5.3) & final_cleaned_survey_data$Q5.3 != "")
Q5_4_responses <- sum(!is.na(final_cleaned_survey_data$Q5.4) & final_cleaned_survey_data$Q5.4 != "")

# Print the results
print(paste("Number of complete responses for Q5.1:", Q5_1_responses))
print(paste("Number of complete responses for Q5.2:", Q5_2_responses))
print(paste("Number of complete responses for Q5.3:", Q5_3_responses))
print(paste("Number of complete responses for Q5.4:", Q5_4_responses))
#################################################################




#######################################################################
# Frequency of online harassment taking place and adressing adequecy
# Convert Q1_1.4 and Q2_1.4 to numeric, handling any non-numeric values
final_survey_data$Q1_1.4 <- as.numeric(as.character(final_survey_data$Q1_1.4))
final_survey_data$Q2_1.4 <- as.numeric(as.character(final_survey_data$Q2_1.4))

# Handle any conversion warnings or NA values that might result
final_survey_data$Q1_1.4[is.na(final_survey_data$Q1_1.4)] <- NA
final_survey_data$Q2_1.4[is.na(final_survey_data$Q2_1.4)] <- NA

# Perform descriptive statistics analysis for Q1_1.4 (adequately addressing online harassment)
summary_Q1 <- final_survey_data %>%
  summarise(
    mean_Q1 = mean(Q1_1.4, na.rm = TRUE),    # Calculate the mean
    median_Q1 = median(Q1_1.4, na.rm = TRUE), # Calculate the median
    sd_Q1 = sd(Q1_1.4, na.rm = TRUE),         # Calculate the standard deviation
    min_Q1 = min(Q1_1.4, na.rm = TRUE),       # Find the minimum value
    max_Q1 = max(Q1_1.4, na.rm = TRUE)        # Find the maximum value
  )

# Perform descriptive statistics analysis for Q2_1.4 (frequency of online harassment)
summary_Q2 <- final_survey_data %>%
  summarise(
    mean_Q2 = mean(Q2_1.4, na.rm = TRUE),    # Calculate the mean
    median_Q2 = median(Q2_1.4, na.rm = TRUE), # Calculate the median
    sd_Q2 = sd(Q2_1.4, na.rm = TRUE),         # Calculate the standard deviation
    min_Q2 = min(Q2_1.4, na.rm = TRUE),       # Find the minimum value
    max_Q2 = max(Q2_1.4, na.rm = TRUE)        # Find the maximum value
  )

# Print the summary statistics for both questions
print(summary_Q1)
print(summary_Q2)

# Create a histogram to show the distribution of ratings for Q1_1.4
ggplot(final_survey_data, aes(x = Q1_1.4)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution for Ratings of how effective is 
       Online harassment Adressed",
       x = "Rating (0-100)", y = "Frequency") +
  theme_minimal()

# Create a histogram to show the distribution of ratings for Q2_1.4
ggplot(final_survey_data, aes(x = Q2_1.4)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Frequency of Online Harassment",
       x = "Rating (0-100)", y = "Frequency") +
  theme_minimal()


# Load necessary libraries
library(dplyr)
library(knitr)
library(kableExtra)

# Perform descriptive statistics for Q1_1.4 (adequately addressing online harassment)
summary_Q1 <- final_survey_data %>%
  summarise(
    Mean = mean(Q1_1.4, na.rm = TRUE),    # Calculate the mean
    Median = median(Q1_1.4, na.rm = TRUE), # Calculate the median
    SD = sd(Q1_1.4, na.rm = TRUE),         # Calculate the standard deviation
    Min = min(Q1_1.4, na.rm = TRUE),       # Find the minimum value
    Max = max(Q1_1.4, na.rm = TRUE)        # Find the maximum value
  )

# Perform descriptive statistics for Q2_1.4 (frequency of online harassment)
summary_Q2 <- final_survey_data %>%
  summarise(
    Mean = mean(Q2_1.4, na.rm = TRUE),    # Calculate the mean
    Median = median(Q2_1.4, na.rm = TRUE), # Calculate the median
    SD = sd(Q2_1.4, na.rm = TRUE),         # Calculate the standard deviation
    Min = min(Q2_1.4, na.rm = TRUE),       # Find the minimum value
    Max = max(Q2_1.4, na.rm = TRUE)        # Find the maximum value
  )

# Combine the summaries into a single table
summary_table <- rbind(
  summary_Q1,
  summary_Q2
)

# Add descriptive row names
rownames(summary_table) <- c("Adequately Addressed", "Frequency of Harassment ")

# Create a nice-looking table
summary_table %>%
  kable("html", caption = "Summary Statistics for AddressEfficiency
        and Frequency of Online Harassment") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)


# Perform Kruskal-Wallis Test for Q1_1.4
kruskal_Q1 <- kruskal.test(Q1_1.4 ~ Q3, data = final_survey_data)
print("Kruskal-Wallis Test for Q1_1.4:")
print(kruskal_Q1)

# Perform Kruskal-Wallis Test for Q2_1.4
kruskal_Q2 <- kruskal.test(Q2_1.4 ~ Q3, data = final_survey_data)
print("Kruskal-Wallis Test for Q2_1.4:")
print(kruskal_Q2)

# Create a summary table
kruskal_summary <- data.frame(
  Scenario = c("Adequately Addressing", 
               "Frequency of Online Harassment"),
  Chi_Square_Statistic = c(kruskal_Q1$statistic, kruskal_Q2$statistic),
  Degrees_of_Freedom = c(kruskal_Q1$parameter, kruskal_Q2$parameter),
  P_Value = c(kruskal_Q1$p.value, kruskal_Q2$p.value)
)

# Display the table using kable for a nice format
kruskal_summary %>%
  kable("html", caption = "Kruskal-Wallis Test Results for AddressEfficiency
        and Frequency of Online Harassment by Nationality") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
####################################################################
# About content type and Addressing adequecy and Frequency
# Perform Kruskal-Wallis Test for Q1_1.4 (Adequately Addressing Online Harassment) by Content Type
kruskal_Q1_content <- kruskal.test(Q1_1.4 ~ Q2.1, data = final_survey_data)
print("Kruskal-Wallis Test for Q1_1.4 by Content Type:")
print(kruskal_Q1_content)

# Perform Kruskal-Wallis Test for Q2_1.4 (Frequency of Online Harassment) by Content Type
kruskal_Q2_content <- kruskal.test(Q2_1.4 ~ Q2.1, data = final_survey_data)
print("Kruskal-Wallis Test for Q2_1.4 by Content Type:")
print(kruskal_Q2_content)

# Create a summary table for Content Type
kruskal_summary_content <- data.frame(
  Scenario = c("Adequately Addressing", 
               "Frequency of Online Harassment"),
  Chi_Square_Statistic = c(kruskal_Q1_content$statistic, kruskal_Q2_content$statistic),
  Degrees_of_Freedom = c(kruskal_Q1_content$parameter, kruskal_Q2_content$parameter),
  P_Value = c(kruskal_Q1_content$p.value, kruskal_Q2_content$p.value)
)

# Display the table using kable for a nice format
kruskal_summary_content %>%
  kable("html", caption = "Kruskal-Wallis Test Results for AddressEfficiency
        and Frequency of Online Harassment by Content Type") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

####################################################################

# Step 1: Clean the data by removing NA and blank entries for Q2.2 and Q3.7
final_survey_data_clean <- final_survey_data_clean %>%
  filter(!is.na(Q2.2) & Q2.2 != "",
         !is.na(Q3.7) & Q3.7 != "",
         !is.na(Q3) & Q3 != "")  # Ensure Q3 (Nationality) has no NA or blanks

# Step 2: Count the yes/no responses for Q2.2 and Q3.7
other_language_counts <- final_survey_data_clean %>%
  count(Q2.2)

standard_differ_counts <- final_survey_data_clean %>%
  count(Q3.7)

# Step 3: Combine the counts into one table
combined_counts <- rbind(
  data.frame(Category = "Other Language", Response = other_language_counts$Q2.2, Count = other_language_counts$n),
  data.frame(Category = "Standard Differ", Response = standard_differ_counts$Q3.7, Count = standard_differ_counts$n)
)

# Step 4: Generate a table with kable
combined_counts_table <- combined_counts %>%
  kable("html", caption = "Counts of Responses for Other Language and Standard Differ (NA Removed)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

# Step 5: Display the table
combined_counts_table

# Step 1: Create groups based on Q2.2 (Other Language)
group_yes <- final_survey_data_clean %>% filter(Q2.2 == "Yes")
group_no <- final_survey_data_clean %>% filter(Q2.2 == "No")

# Step 2: Analyze the distribution of Q3.7 responses in each group
yes_standard_differ <- group_yes %>% count(Q3.7)
no_standard_differ <- group_no %>% count(Q3.7)

# Step 3: Perform Chi-Square Test
chi_square_standard_differ <- chisq.test(table(final_survey_data_clean$Q2.2, final_survey_data_clean$Q3.7))

# Step 4: Perform Kruskal-Wallis Test (if Q3.7 is ordinal/continuous)
kruskal_standard_differ <- kruskal.test(Q3.7 ~ Q2.2, data = final_survey_data_clean)

# Step 5: Interpret Results
print(chi_square_standard_differ)
print(kruskal_standard_differ)

# Create a summary table for the Chi-Square test results
chi_square_summary <- data.frame(
  Scenario = c("Relationship between Watching Second Language Live Streams and Perception of Differing Harassment Standards"),
  `Chi-Square Statistic` = round(4.0117, 3),
  `Degrees of Freedom` = 1,
  `p-value` = format.pval(0.04519, digits = 3)
)

# Generate the table with kable
chi_square_summary_table <- chi_square_summary %>%
  kable("html", caption = "Chi-Square Test Results for Nationality and Perception of Differing Harassment Standards") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Display the table
chi_square_summary_table

# Step 6: Create a contingency table for Q2.2 and Nationality
language_nationality_table <- table(final_survey_data_clean$Q2.2, final_survey_data_clean$Q3)

# Step 7: Perform the Chi-Square test
chi_square_test_language <- chisq.test(language_nationality_table)

# Step 8: Generate the bar plot for Q2.2 and Nationality
barplot(
  language_nationality_table,
  beside = TRUE,
  legend = TRUE,
  col = c("skyblue", "lightgreen"),
  main = "Relationship between Nationality 
  and Watching Other Language Live Streams",
  xlab = "Nationality",
  ylab = "Count"
)

# Step 9: Create a contingency table for Q3.7 and Nationality
standard_nationality_table <- table(final_survey_data_clean$Q3.7, final_survey_data_clean$Q3)

# Step 10: Perform the Chi-Square test
chi_square_test_standard <- chisq.test(standard_nationality_table)

# Extract the Chi-Square test results
chi_square_results <- data.frame(
  `Test Statistic` = "Chi-Squared",
  Value = round(chi_square_test_standard$statistic, 3),
  `Degrees of Freedom` = chi_square_test_standard$parameter,
  `p-value` = format.pval(chi_square_test_standard$p.value, digits = 3)
)

# Generate the table with kable
chi_square_results_table <- chi_square_results %>%
  kable("html", caption = "Chi-Square Test Results for Nationality and Perception of Differing Harassment Standards") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Display the table
chi_square_results_table

# Step 11: Generate the bar plot for Q3.7 and Nationality
barplot(
  standard_nationality_table,
  beside = TRUE,
  legend = TRUE,
  col = c("skyblue", "lightgreen"),
  main = "Relationship between Nationality and 
  Perception of Differing Harassment Standards",
  xlab = "Nationality",
  ylab = "Count"
)
###########################################################################
# Count the responses for Q6.5
q6_5_counts <- final_survey_data_clean %>%
  filter(!is.na(Q6.5)) %>%  # Remove NA values
  count(Q6.5)

# Display the counts in a table
q6_5_counts_table <- q6_5_counts %>%
  kable("html", caption = "Counts of Responses for Q6.5 (Effect of Cultural Values on Perception of Online Harassment)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Show the table
q6_5_counts_table

# Create a contingency table for Q6.5 and Q3 (Nationality)
q6_5_nationality_table <- final_survey_data_clean %>%
  filter(!is.na(Q6.5), !is.na(Q3)) %>%  # Remove NA values
  count(Q3, Q6.5)

# Display the contingency table
q6_5_nationality_table %>%
  kable("html", caption = "Counts of Responses for Q6.5 by Nationality") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Create a bar plot for Q6.5 responses
ggplot(q6_5_counts, aes(x = Q6.5, y = n, fill = Q6.5)) +
  geom_bar(stat = "identity") +
  labs(title = "Perception of the Effect of Cultural Values on Online Harassment",
       x = "Response",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Create a contingency table for Q6.5 and Nationality (Q3)
cultural_nationality_table <- table(final_survey_data_clean$Q6.5, final_survey_data_clean$Q3)

# Perform the Chi-Square test
chi_square_cultural_nationality <- chisq.test(cultural_nationality_table)

# Display the contingency table and Chi-Square test results
cultural_nationality_table
chi_square_cultural_nationality

# Create a table for the Chi-Square test results
chi_square_results <- data.frame(
  Test.Statistic = "Chi-Squared",
  Value = round(chi_square_cultural_nationality$statistic, 3),
  Degrees.of.Freedom = chi_square_cultural_nationality$parameter,
  p.value = format.pval(chi_square_cultural_nationality$p.value, digits = 3)
)

# Display the Chi-Square test results in a table
chi_square_results_table <- chi_square_results %>%
  kable("html", caption = "Chi-Square Test Results for Nationality and Perception of Cultural Impact on Harassment") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

# Show the table
chi_square_results_table


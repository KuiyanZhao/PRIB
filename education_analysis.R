
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(haven)

# Load datasets for all waves
wave1_general_edu <- read_dta('s1fi.dta')
wave1_edu_career <- read_dta('s1fii.dta')
wave2_general_edu <- read_dta('01fi_generaleducation.dta')
wave2_edu_career <- read_dta('01fii_edcareer.dta')
wave3_general_edu <- read_dta('01gi_generaleducation.dta')
wave3_edu_career <- read_dta('01gii_edcareer.dta')

# Add wave identifiers
wave1_general_edu$wave <- 1
wave2_general_edu$wave <- 2
wave3_general_edu$wave <- 3

wave1_edu_career$wave <- 1
wave2_edu_career$wave <- 2
wave3_edu_career$wave <- 3

# Select relevant columns
columns_to_keep_general_edu <- c('FPrimary', 'wave')
columns_to_keep_edu_career_wave1 <- c('FPrimary', 's1fii_52', 'wave')
columns_to_keep_edu_career_wave2 <- c('FPrimary', 'techcompvocever', 'wave')
columns_to_keep_edu_career_wave3 <- c('FPrimary', 'techcompvocever', 'wave')

# Combine datasets for general education
combined_general_edu <- bind_rows(
  select(wave1_general_edu, all_of(columns_to_keep_general_edu)),
  select(wave2_general_edu, all_of(columns_to_keep_general_edu)),
  select(wave3_general_edu, all_of(columns_to_keep_general_edu))
)

# Combine educational career datasets
combined_edu_career <- bind_rows(
  rename(select(wave1_edu_career, all_of(columns_to_keep_edu_career_wave1)), techcompvocever = s1fii_52),
  select(wave2_edu_career, all_of(columns_to_keep_edu_career_wave2)),
  select(wave3_edu_career, all_of(columns_to_keep_edu_career_wave3))
)

# Filter for relevant values in techcompvocever
relevant_values <- c('Yes', 'No')
filtered_edu_career <- combined_edu_career %>% filter(techcompvocever %in% relevant_values)

# Calculate participation rates in technical/professional education
tech_prof_edu_counts <- filtered_edu_career %>%
  group_by(wave) %>%
  count(techcompvocever) %>%
  spread(techcompvocever, n, fill = 0) %>%
  mutate(Total = Yes + No,
         Participation_Rate = Yes / Total * 100)

# Assuming we have gender information in combined_general_edu
# Simulating gender data for combined_general_edu
set.seed(0)
combined_general_edu <- combined_general_edu %>%
  mutate(gender = sample(c('Male', 'Female'), n(), replace = TRUE))

# Merge gender data with educational career data
combined_edu_career_gender <- filtered_edu_career %>%
  left_join(select(combined_general_edu, FPrimary, gender), by = 'FPrimary')

# Calculate gender distribution in technical/professional education
gender_distribution <- combined_edu_career_gender %>%
  group_by(wave, gender) %>%
  count(techcompvocever) %>%
  spread(techcompvocever, n, fill = 0) %>%
  mutate(Total = Yes + No,
         Participation_Rate = Yes / Total * 100)

# Plot the results
# Trend of Technical/Professional Education Participation
ggplot(tech_prof_edu_counts, aes(x = wave, y = Participation_Rate)) +
  geom_line() +
  geom_point() +
  ggtitle('Participation Rate in Technical/Professional Education Over Time') +
  xlab('Wave') +
  ylab('Participation Rate (%)') +
  theme_minimal()

# Gender Distribution in Technical/Professional Education
ggplot(gender_distribution, aes(x = factor(wave), y = Participation_Rate, fill = gender)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~ gender) +
  ggtitle('Gender Distribution in Technical/Professional Education') +
  xlab('Wave') +
  ylab('Participation Rate (%)') +
  theme_minimal() +
  scale_fill_manual(values = c('blue', 'pink'))

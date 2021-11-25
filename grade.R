library(readr)
library(ggplot2)
library(dplyr)
grade_dataset <- read_csv("grade.csv")
glimpse(grade_dataset)
head(grade_dataset)
summary(grade_dataset)

# convert datatypes

grade_dataset <- grade_dataset %>%
  mutate(school = as.factor(school))
grade_dataset <- grade_dataset %>%
  mutate(sex = as.factor(sex))
grade_dataset <- grade_dataset %>%
  mutate(Mjob = as.factor(Mjob))
grade_dataset <- grade_dataset %>%
  mutate(Fjob = as.factor(Fjob))
grade_dataset <- grade_dataset %>%
  mutate(reason = as.factor(reason))
grade_dataset <- grade_dataset %>%
  mutate(guardian = as.factor(guardian))
grade_dataset <- grade_dataset %>%
  mutate(schoolsup = as.factor(schoolsup))
grade_dataset <- grade_dataset %>%
  mutate(famsup = as.factor(famsup))
grade_dataset <- grade_dataset %>%
  mutate(paid = as.factor(paid))
grade_dataset <- grade_dataset %>%
  mutate(activities = as.factor(activities))
grade_dataset <- grade_dataset %>%
  mutate(nursery = as.factor(nursery))
grade_dataset <- grade_dataset %>%
  mutate(higher = as.factor(higher))
grade_dataset <- grade_dataset %>%
  mutate(internet = as.factor(internet))
grade_dataset <- grade_dataset %>%
  mutate(romantic = as.factor(romantic))

summary(grade_dataset)
glimpse(grade_dataset)

# Calculate some statistics
# 1 SEX_STATS 
sex_stats <- grade_dataset$sex
table(sex_stats)
table(sex_stats)/length(sex_stats)
# 2 AGE_STATS
age_stats <- grade_dataset$age
sd(age_stats)
var(age_stats)
# 3 STUDY_TIME_STATS
study_time_stats <- grade_dataset$studytime
sd(study_time_stats)
var(study_time_stats)
# 4 FAILURES_STATS
failure_stats <- grade_dataset$failures
sd(failure_stats)
var(failure_stats)
# 5 ABSENCES_STATS
absences_stats <- grade_dataset$absences
sd(absences_stats)
var(absences_stats)
# 6 G1
G1_stats <- grade_dataset$G1
sd(G1_stats)
var(G1_stats)
# 7 G2
G2_stats <- grade_dataset$G2
sd(G2_stats, na.rm = TRUE)
var(G2_stats, na.rm = TRUE)
# 8 G3
G3_stats <- grade_dataset$G3
sd(G3_stats, na.rm = TRUE)
var(G3_stats, na.rm = TRUE)

# Graph
# 1. Age
bar_age <- grade_dataset %>%
ggplot( aes(age)) + 
  geom_bar(bins=30)
bar_age
# 2. Failures
bar_failures <- grade_dataset %>%
  ggplot( aes(failures)) + 
  geom_bar(bins=30)
bar_failures
# 3. Study time
bar_studytime <- grade_dataset %>%
  ggplot( aes(studytime)) +
  geom_bar(bins = 30)
bar_studytime
# 4. absences
bar_absences <- grade_dataset %>%
  ggplot( aes(absences)) +
  geom_bar(bins = 30)
bar_absences
#quite similar to geometric distribution
# 3. G3
bar_G3 <- grade_dataset %>%
  ggplot( aes(G3)) + 
  geom_bar(bins=30)
bar_G3
# Optional
grade_dataset <- grade_dataset %>%
  mutate(classified = ifelse(G3 >= 10, "Pass", "Fail"))
grade_dataset <- grade_dataset %>%
  mutate(classified = as.factor(classified))
summary(grade_dataset)
classify_stats <- grade_dataset$classified
table(classify_stats)
table(classify_stats)/length(classify_stats)

# 4. Study time
plot_G3_studytime <- grade_dataset %>%
  ggplot(aes(x=G3, y=studytime)) + 
  geom_line()+ 
  geom_point()
plot_G3_studytime
# filter study time with no absences
not_absences <- grade_dataset %>%
  filter(absences == 0)
# Graph
plot_G3_with_not_absences <- not_absences %>%
  ggplot(aes(x=G3, y=studytime)) +
  geom_line()+
  geom_point()
plot_G3_with_not_absences
# Filter 0 absences, 0 failure in study time
not_failures_in_absences <- not_absences %>%
  filter(failures == 0)
# Draw graph again
plot_G3_without_failures_in_absences <- not_failures_in_absences %>%
  ggplot(aes(x=G3, y=studytime)) +
  geom_line() +
  geom_point()
plot_G3_without_failures_in_absences

# 5. Sex factor to G3
plot_G3_sex <- grade_dataset %>%
  ggplot(aes(x=G3, y=sex)) +
  geom_line() +
  geom_point()
plot_G3_sex
# 6. Age to G3
plot_G3_age <- grade_dataset %>%
  ggplot(aes(x=G3, y=age)) + 
  geom_line() +
  geom_point()
plot_G3_age
# G1, G2 to G3
plot_g1_with_g3 <- grade_dataset %>%
  ggplot(aes(x=G3, y=G1)) +
  geom_line()
plot_g1_with_g3

plot_g2_with_g3 <- grade_dataset %>%
  ggplot(aes(x=G3, y=G2)) +
  geom_line()
plot_g2_with_g3
# So there is a relationship between G1, G2
# Testing hypothesis using the linear regression formula
result1 = lm(data = grade_dataset, G3 ~ sex + age + studytime + failures + higher + absences + G1 + G2)
summary(result1)
result2 = lm(data = grade_dataset, G3 ~ absences + G1 + G2)
summary(result2)

anova(result1, result2)

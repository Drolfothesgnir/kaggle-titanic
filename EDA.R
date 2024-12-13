# EDA 2
library(ggplot2)
library(dplyr)
library(patchwork)

source("utils/load_data.R")

train_df <- load_train_data()
test_df <- load_test_data()

head(train_df)
head(test_df)

dim(train_df) # 891  12
dim(test_df) # 418 11

colSums(is.na(train_df))
# 2 values for Embarked are missing
colSums(is.na(train_df)) / 891
# 77% of Cabin is missing
# 20% of Age is missing

colSums(is.na(test_df))
# test_df has 1 missing Fare value
colSums(is.na(test_df)) / 418
# 78% of Cabin is missing
# 21% of Age is missing

# Drop Cabin out of the analysis
# Impute Age
numeric_vars <- c("Survived", "SibSp", "Parch", "Fare")
# actually Survived is rather categorical, but it's analysis is located here

# Distribution of numerical variables in the data
# Survived
mean(train_df$Survived == 1) 
# 38% survival test

survival_data <- data.frame(
  status = c("Did Not Survive", "Survived"),
  count = c(549, 342)  # 549 did not survive, 342 survived
) %>%
  mutate(
    percentage = count / sum(count),
    label = scales::percent(percentage, accuracy = 0.1)
  )

source("utils/create_pie_chart.R")

# Create the plot
create_pie_chart(
  survival_data,
  "Titanic Passenger Survival Rate",
  "Survival Status",
  colors = c("#E74C3C", "#2ECC71")
)

# Distribution of Parch and SibSp
p1 <- ggplot(train_df, aes(x = Parch)) +
  geom_bar(fill = "lightblue") +
  theme_minimal()

p2 <- ggplot(train_df, aes(x = SibSp)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal()

combined_plot <- p1 + p2 + 
  plot_layout(widths = c(1, 1)) +
  plot_annotation(title = "Family members count analysis")

combined_plot

# Fare distribution
ggplot(train_df, aes(x = Fare)) +
  geom_histogram(fill="violet") +
  theme_minimal() +
  labs(title = "Distribution of ticket fares")

# Fare is strongly skewed to the right

# Age distribution
ggplot(train_df, aes(x = Age)) +
  geom_histogram(fill="skyblue") +
  theme_minimal() +
  labs(title = "Distribution of passengers age")

# Distribution of categorical variables
categorical_vars <- c("Sex", "Pclass", "Embarked", "Cabin", "Ticket", "Name")
# Sex
n_females <- sum(train_df$Sex == "female")
sex_data <- data.frame(status = c("Female", "Male"),
                       count = c(n_females, 891 - n_females)) %>%
  mutate(
    percentage = count / sum(count),
    label = scales::percent(percentage, accuracy = 0.1)
  )

create_pie_chart(sex_data, "Titanic passenger Sex distribution", "Passenger's sex")
# 64.8% were males

# Embarked
train_df %>%
  drop_na(Embarked) %>%
  ggplot(aes(x = Embarked, fill = Embarked)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Distribution of embarkation port",
    x = "Port of embarkation"
  )

# Pclass
train_df %>%
  ggplot(aes(x = Pclass, fill = Pclass)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Distribution of passenger class",
    x = "Class"
  )

# Correlating variables with Survived

# Survived vs SibSp and Parch
# Grouped bar plot
ggplot(train_df, aes(x = factor(SibSp), fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival by Number of Siblings/Spouses",
    x = "Number of Siblings/Spouses",
    y = "Count",
    fill = "Survived"
  ) +
  theme_minimal()

ggplot(train_df, aes(x = factor(Parch), fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival by Number of Parents/Children",
    x = "Number of Parents/Children",
    y = "Count",
    fill = "Survived"
  ) +
  theme_minimal()

table(train_df$Survived, train_df$SibSp)
table(train_df$Survived, train_df$Parch)

train_df$SibSp_grouped <- ifelse(train_df$SibSp >= 3, "3+", as.character(train_df$SibSp))
fisher.test(table(train_df$Survived, train_df$SibSp_grouped))
# p-value = 6.449e-08

train_df$Parch_grouped <- ifelse(train_df$Parch >= 3, "3+", as.character(train_df$Parch))
fisher.test(table(train_df$Survived, train_df$Parch_grouped))
# p-value = 2.57e-05

mean(train_df$SibSp < 1)
# 68% of people didn't have spouse or sibling aboard

mean(train_df$Parch < 1)
# 76% of people didn't have parents or children aboard


# Survived vs Fare

ggplot(train_df, aes(x = Survived, y = Fare, fill = Survived)) +
  geom_boxplot() +
  labs(
    title = "Distribution of ticket fare grouped by survival status"
  ) +
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_minimal()

ggplot(train_df, aes(
  x = log(Fare),
  fill = Survived
)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Log Fares by Survival Status",
       x = "Fare",
       y = "Density",
       fill = "Survived") +
  theme_minimal()

wilcox.test(Fare ~ Survived, data = train_df)
# Reject H0: Fares are the same for survivors and non-survivors

# Survival vs Age
ggplot(train_df, aes(x = Survived, y = Age, fill = Survived)) +
  geom_boxplot() +
  labs(
    title = "Distribution of passengers age grouped by survival status"
  ) +
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_minimal()

ggplot(train_df, aes(
  x = Age,
  fill = Survived
)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of passengers age by Survival Status",
       x = "Age",
       y = "Density",
       fill = "Survived") +
  theme_minimal()

wilcox.test(Age ~ Survived, data = train_df)
# p-value = 0.1605, can't reject H0

# Survival vs Sex
ggplot(train_df, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival rate based on Sex", y = "Percentage")

table(train_df$Survived, train_df$Sex)
#     male female
# No   468     81
# Yes  109    233
chisq.test(table(train_df$Survived, train_df$Sex))
# p-value < 2.2e-16

# Survival vs Embarked
train_df %>%
  drop_na(Embarked) %>%
  ggplot(aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival rate based on embarkation port", y = "Percentage")

table(train_df$Survived, train_df$Embarked)
#       S   C   Q
# No  427  75  47
# Yes 217  93  30
chisq.test(table(train_df$Survived, train_df$Embarked))
# p-value = 1.77e-06

# Survival vs Pclass
ggplot(train_df, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival rate based on passengers class", y = "Percentage")

table(train_df$Survived, train_df$Pclass)
#       1   2   3
# No   80  97 372
# Yes 136  87 119
chisq.test(table(train_df$Survived, train_df$Pclass))
# p-value < 2.2e-16

# Analysis of engineered features
source("feature_engineering.R")
source("utils/create_boxplot.R")

train_df <- engineer_features(train_df)

# Age vs Title
create_boxplot(
  train_df,
  "title_clean",
  "Age",
  "Distribution of passengers age grouped by title",
  "Title",
  "Age"
)
create_boxplot(
  train_df,
  "title_clean_2",
  "Age",
  "Distribution of passengers age grouped by title",
  "Title",
  "Age"
)

# title_clean + Pclass explains more variance in Age, so it is used to impute 
# missing Age data
summary(aov(Age ~ Pclass + title_clean, data = train_df))
#              Df Sum Sq Mean Sq F value Pr(>F)    
# Pclass        2  20930   10465   82.06 <2e-16 ***
# title_clean   6  39622    6604   51.78 <2e-16 ***
# Residuals   705  89905     128                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 177 observations deleted due to missingness

summary(aov(Age ~ Pclass + Sex, data = train_df))
#              Df Sum Sq Mean Sq F value   Pr(>F)    
# Pclass        2  20930   10465   58.90  < 2e-16 ***
# Sex           1   3389    3389   19.08 1.44e-05 ***
# Residuals   710 126138     178                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 177 observations deleted due to missingness

# Survived vs is_alone
ggplot(train_df, aes(x = is_alone, fill = Survived)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival rate vs traveled alone", y = "Percentage", x = "Traveled alone")

table(train_df$Survived, train_df$is_alone)
#       0   1
# No  175 374
# Yes 179 163
chisq.test(table(train_df$Survived, train_df$is_alone))
# p-value = 1.973e-09

# Survived vs family size
ggplot(train_df, aes(x = factor(family_size), fill = Survived)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival rate grouped by family size", y = "Percentage", x = "Family size")

table(train_df$Survived, train_df$family_size)
#       1   2   3   4   5   6   7   8  11
# No  374  72  43   8  12  19   8   6   7
# Yes 163  89  59  21   3   3   4   0   0

ggplot(train_df, aes(x = factor(family_band), fill = Survived)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival rate grouped by family size", y = "Percentage", x = "Family size")

table(train_df$Survived, train_df$family_band)
#     Solo Small Large
# No   374   115    60
# Yes  163   148    31
chisq.test(table(train_df$Survived, train_df$family_band))
# p-value = 8.644e-12

source("age.R")

train_df <- train_df %>%
  impute_age() %>%
  engineer_features_2()

# Survived vs age band

ggplot(train_df, aes(x = age_band, fill = Survived)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival rate by age group", y = "Percentage", x = "Age group")

table(train_df$Survived, train_df$age_band)
chisq.test(table(train_df$Survived, train_df$age_band))

# Survived vs fare band
ggplot(train_df, aes(x = fare_band, fill = Survived)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival rate by fare group", y = "Percentage", x = "Fare group")



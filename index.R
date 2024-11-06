library(boot)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(GGally)

df <- read_rds('./processed_data.rds')

head(df)
sapply(df, function(x)
  sum(is.na(x)))

long_data <- df %>%
  select(Age, SibSp, Parch, Fare) %>%
  gather(key = "Variable", value = "Value")

# Create the plot
ggplot(long_data, aes(x = Value)) +
  geom_histogram(fill = "steelblue",
                 color = "white",
                 bins = 30) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(title = "Distribution of Numeric Variables in Titanic Dataset", x = "Value", y = "Count") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.spacing = unit(1, "lines")
  )

library(reshape2)

# Assuming your data is named 'titanic'
# Select only numeric columns
numeric_data <- df %>%
  select(Age, SibSp, Parch, Fare)

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Convert correlation matrix to long format for ggplot
cor_melted <- melt(cor_matrix)

# Create the heatmap
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  # Add correlation values in the cells
  geom_text(aes(label = sprintf("%.2f", value)), color = "white") +
  # Use a diverging color palette centered at 0
  scale_fill_gradient2(
    low = "#E46262",
    # Red for negative correlations
    mid = "white",
    # White for correlations near 0
    high = "#6BB0E5",
    # Blue for positive correlations
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  # Customize the theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10
    ),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  # Add labels
  labs(title = "Correlation Matrix of Titanic Numeric Variables",
       x = "",
       y = "",
       fill = "Correlation") +
  # Make the plot square
  coord_fixed()

# Calculate average values for each column by Survived status
pivot_table <- df %>%
  group_by(Survived) %>%
  summarize(
    Avg_Age = mean(Age, na.rm = TRUE),
    Avg_SibSp = mean(SibSp, na.rm = TRUE),
    Avg_Parch = mean(Parch, na.rm = TRUE),
    Avg_Fare = mean(Fare, na.rm = TRUE)
  )

pivot_table


"Passenger Class Analysis"
df %>%
  ggplot(aes(x = Survived, fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "red3", "1" = "green4")) +
  labs(title = "Overall survival") +
  theme_minimal()

p1 <- df %>%
  ggplot(aes(x = Pclass, fill = Pclass)) +
  geom_bar() +
  labs(title = "Class Distribution", x = "Passenger Class", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend as it's redundant

# Create the second plot - Survival by Class
p2 <- ggplot(df, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Rate by Class",
       x = "Passenger Class",
       y = "Percentage",
       fill = "Survived") +
  theme_minimal()

# Combine the plots
combined_plot <- p1 + p2 +
  plot_layout(widths = c(1, 1)) +  # Equal widths for both plots
  plot_annotation(title = "Passenger Class Analysis",
                  theme = theme(plot.title = element_text(
                    size = 16, face = "bold", hjust = 0.5
                  )))

# Display the combined plot
combined_plot


"Passenger Sex Analysis"
p1 <- df %>%
  ggplot(aes(x = Sex, fill = Sex)) +
  geom_bar() +
  labs(title = "Sex distribution", x = "Passenger Sex", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(df, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Rate by Sex",
       x = "Passenger Sex",
       y = "Percentage",
       fill = "Survived") +
  theme_minimal()

combined_plot <- p1 + p2 +
  plot_layout(widths = c(1, 1)) +  # Equal widths for both plots
  plot_annotation(title = "Passenger Sex Analysis",
                  theme = theme(plot.title = element_text(
                    size = 16, face = "bold", hjust = 0.5
                  )))

combined_plot

cont_class_vs_embarked <- table(df$Pclass, df$Embarked)
chisq.test(cont_class_vs_embarked)


"Passenger Embarkation Analysis"
p1 <- df %>%
  ggplot(aes(x = Embarked, fill = Embarked)) +
  geom_bar() +
  labs(title = "Embarkation distribution", x = "Passenger Embarkation port", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# p2 <- ggplot(df, aes(x = Embarked, fill = Pclass)) +
#   geom_bar(position = "fill") +
#   scale_y_continuous(labels = scales::percent) +
#   labs(title = "Embarkation Frequency Rate by Passengers Class",
#        x = "Embarkation Port",
#        y = "Percentage",
#        fill = "Pclass") +
#   theme_minimal()

p2 <- ggplot(df, aes(x = Embarked, fill = Pclass)) +
  geom_bar(position = "dodge") +
  labs(title = "Passenger Class Distribution by Embarkation Port",
       x = "Embarkation Port",
       y = "Count",
       fill = "Pclass") +
  theme_minimal()

combined_plot <- p1 + p2 +
  plot_layout(widths = c(1, 1)) +  # Equal widths for both plots
  plot_annotation(title = "Passenger Embarkation Analysis",
                  theme = theme(plot.title = element_text(
                    size = 16, face = "bold", hjust = 0.5
                  )))

combined_plot

ggplot(df, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Rate by Embarkation Port",
       x = "Embarkation Port",
       y = "Percentage",
       fill = "Survive") +
  theme_minimal()



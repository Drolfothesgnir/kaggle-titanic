library(tidyverse)

# Regex for extracting title from Name of the passenger
# 'Moran, Mr. James' -> 'Mr.'
regex_string <- ".*, ([A-Za-z ]+\\.?) "

# Should work fine, since every passenger (in the training set) has a title
extract_title <- function(name) {
  str_match(name, regex_string)[2]
}

# Combining rare titles into more common groups

coerce_titles <- function(title) {
  case_when(
    grepl("Capt|Col|Major", title) ~ "Military",
    grepl("Mme|Ms", title) ~ "Mrs.",
    grepl("Don|Lady|Sir|the Countess|Jonkheer", title) ~ "Noble",
    grepl("Mlle", title) ~ "Miss.",
    grepl("Dr|Rev", title) ~ "Professional",
    .default  = as.character(title)
  )
}

coerce_titles_2 <- function(title) {
  case_when(
    grepl("Mme|Ms", title) ~ "Mrs.",
    grepl(
      "Don|Lady|Sir|the Countess|Jonkheer|Capt|Col|Major|Dr|Rev",
      title
    ) ~ "Rare",
    grepl("Mlle", title) ~ "Miss.",
    .default  = as.character(title)
  )
}

augment_by_title_alt <- function(title_clean, sex) {
  case_when(
    # Group all high-status males together (Military, Noble, Professional)
    title_clean %in% c("Military", "Noble", "Professional") &
      sex == "male" ~
      "Mr.",
    # Group all high-status females together
    title_clean %in% c("Noble", "Professional") & sex == "female" ~
      "Mrs.",
    .default = title_clean
  )
}

augment_by_title <- function(df) {
  # extracting title from a name
  df <- df %>%
    mutate(title = as.factor(sapply(Name, extract_title, USE.NAMES = FALSE)))
  
  # Rare titles are grouped together because with their unique titles they represent
  # only few people
  title_clean <- coerce_titles(as.character(df$title))
  title_clean_2 <- coerce_titles_2(as.character(df$title))
  
  title_clean_alt <- augment_by_title_alt(title_clean, df$Sex)
  
  df <- df %>%
    mutate(
      title_clean = as.factor(title_clean),
      title_clean_2 = as.factor(title_clean_2),
      title_clean_alt = as.factor(title_clean_alt)
    )
  
  return(df)
}

augment_by_is_female <- function(df) {
  df %>%
    mutate(is_female = factor(ifelse(Sex == "female", 1, 0)))
}


# combining number of parents/children and number of siblings
augment_by_family_size <- function(df) {
  df %>%
    mutate(family_size = Parch + SibSp + 1)
}

augment_by_is_alone <- function(df) {
  df %>%
    mutate(is_alone = factor(ifelse(Parch + SibSp == 0, 1, 0)))
}

augment_by_family_band <- function(data, family_size_col = "family_size") {
  # Input validation
  if (!family_size_col %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data frame", family_size_col))
  }
  
  if (any(data[[family_size_col]] < 0, na.rm = TRUE)) {
    stop("Negative family size values found")
  }
  
  # Create bands based on survival patterns
  family_bands <- cut(
    data[[family_size_col]],
    breaks = c(-Inf, 1, 3, Inf),
    labels = c("Solo", "Small", "Large"),
    right = TRUE,
    ordered_result = TRUE
  )
  
  data %>%
    mutate(family_band = family_bands)
}

augment_by_age_band <- function(df) {
  age_band <- cut(
    df$Age,
    breaks = seq(
      min(df$Age, na.rm = TRUE),
      max(df$Age, na.rm = TRUE),
      length.out = 6
    ),
    labels = c("<16", "16-32", "32-48", "48-64", "64+"),
    include.lowest = TRUE,
    ordered_result = TRUE
  )
  
  mutate(df, age_band = age_band)
}

augment_by_fare_band <- function(df) {
  # Handle zero fares separately
  fare_breaks <- c(
    0, # Explicitly include 0 as the lowest break
    quantile(
      df$Fare[df$Fare > 0], # Calculate quantiles only for non-zero fares
      probs = seq(0.25, 1, 0.25),
      na.rm = TRUE
    )
  )
  
  fare_band <- cut(
    df$Fare,
    breaks = fare_breaks,
    labels = c("1", "2", "3", "4"),
    include.lowest = TRUE,
    ordered_result = TRUE
  )
  
  mutate(df, fare_band = fare_band)
}

engineer_features <- function(df) {
  df %>%
    augment_by_family_size() %>%
    augment_by_is_alone() %>%
    augment_by_title() %>%
    augment_by_is_female() %>%
    augment_by_family_band()
}

engineer_features_2 <- function(df) {
  df %>%
    augment_by_age_band() %>%
    augment_by_fare_band()
}

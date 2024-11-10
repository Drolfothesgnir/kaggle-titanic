library(tidyverse)

# Inferring deck from cabin number and number of cabins reserved by a passenger

augment_by_cabin_info <- function(df) {
  return (df %>%
            mutate(
              cabin_multiple = ifelse(is.na(Cabin), 0, str_count(Cabin, " ") + 1),
              cabin_deck = as.factor(ifelse(is.na(Cabin), "NA", substr(Cabin, 1, 1)))
            ))
}

# combining number of parents/children and number of siblings
augment_by_family_size <- function(df) {
  return(df %>%
           mutate(family_size = Parch + SibSp))
}


# Regex for extracting title from Name of the passenger
# 'Moran, Mr. James' -> 'Mr.'
regex_string <- ".*, ([A-Za-z ]+\\.?) "

# Should work fine, since every passenger (in the training set) has a title
extract_title <- function(name) {
  return (str_match(name, regex_string)[2])
}

# Combining rare titles into more common groups

coerce_titles <- function(title) {
  return(
    case_when(
      grepl("Capt|Col|Major", title) ~ "Military",
      grepl("Mme|Ms", title) ~ "Mrs.",
      grepl("Don|Lady|Sir|the Countess|Jonkheer", title) ~ "Noble",
      grepl("Mlle", title) ~ "Miss.",
      grepl("Dr|Rev", title) ~ "Professional",
      .default  = as.character(title)
    )
  )
}

augment_by_title <- function(df) {
  # extracting title from a name
  df <- df %>%
    mutate(title = as.factor(sapply(Name, extract_title, USE.NAMES = FALSE)))
  
  # Rare titles are grouped together because with their unique titles they represent
  # only few people
  title_clean <- coerce_titles(as.character(df$title))
  
  df <- df %>%
    mutate(title_clean = factor(title_clean))
  
  return(df)
}

engineer_features <- function(df) {
  df %>%
    augment_by_cabin_info() %>%
    augment_by_family_size() %>%
    augment_by_title()
}

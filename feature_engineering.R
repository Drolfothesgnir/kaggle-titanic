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

augment_by_family_type <- function(df) {
  family_type <- case_when(
    df$family_size == 0 ~ "Solo",
    df$family_size < 4 ~ "Small family",
    # df$family_size < 6 ~ "Med. sized family",
    df$family_size >= 4 ~ "Large family"
  )
  
  family_type <- factor(family_type,
                        levels = c("Solo", "Small family","Large family"))
  
  return(df %>%
           mutate(family_type = family_type))
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

augment_by_title_alt <- function(title_clean, sex) {
  case_when(
    # Group all high-status males together (Military, Noble, Professional)
    title_clean %in% c("Military", "Noble", "Professional") &
      sex == "male" ~
      "HighStatus_male",
    # Group all high-status females together
    title_clean %in% c("Noble", "Professional") & sex == "female" ~
      "HighStatus_female",
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
  
  title_clean_alt <- augment_by_title_alt(title_clean, df$Sex)
  
  df <- df %>%
    mutate(
      title_clean = factor(title_clean),
      title_clean_alt = factor(title_clean_alt)
    )
  
  return(df)
}

engineer_features <- function(df) {
  df %>%
    augment_by_cabin_info() %>%
    augment_by_family_size() %>%
    augment_by_family_type() %>%
    augment_by_title()
}

# Data Wrangling Exercise 2: Dealing with missing values

# Load data
titanic <- tbl_df(read.csv("titanic_original.csv"))
glimpse(titanic)

# Check for missing values in embarkation column
any(is.na(titanic$embarked))
any(titanic$embarked == "")

# Missing values are known to be "Southampton" (S), so replace
titanic$embarked[titanic$embarked == ""] <-  "S"

# Check for missing values in Age column
any(is.na(titanic$age))

# Replace missing values with the mean of Age
titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE)

# Check for missing values in the boat column
any(is.na(titanic$boat))
any(titanic$boat == "")

# Replace missing values in boat column with 0
titanic$boat[titanic$boat == ""] <- "None"

# Check for missing value in the cabin column
any(is.na(titanic$cabin))
any(titanic$cabin == "")

# Create a new column for whether or not cabin number is missing
titanic <- titanic %>%
  mutate(has_cabin_number = case_when(cabin == "" ~ 1, TRUE ~ 0)
  )

# Save the cleaned data
write.csv(titanic, "titanic_clean.csv")

### This script contains the code to clean and prepare the Titanic dataset
### for analysis, following a specific set of tasks provided in an university
### assignment. The dataset is assumed to be a file named 'titanic.csv'.
### The cleaned dataset will be stored in a new file named 'titanic_clean.csv'.

library(stringr)
titanic <- read.csv("titanic.csv") # Load the dataset as a data frame

# Encode the variables 'Survived', 'Sex', 'Embarked' as factors
titanic$Survived <- factor(titanic$Survived)
titanic$Sex      <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)

# Convert the variable 'Pclass' to an ordered factor.
titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE)

# Extract formal titles from 'Name' into new 'Title' column
titanic$Title <- str_extract(
  titanic$Name, 
  "(Mr\\.|Mrs\\.|Mse\\.|Ms\\.|Miss\\.|Mlle\\.|Master\\.)"
)

# Impute the missing values in 'Age' with the median of the available values
# for the corresponding 'Title' group
titanic$Age <- ave(
  titanic$Age, titanic$Title, 
  FUN = function(x) {ifelse(is.na(x), median(x, na.rm = TRUE), x)}
)

# Assigning port or starboard labels to the 'Side' variable depending on the
# parity of the number in the 'Cabin' variable (using regular expressions)
titanic$Side <- ifelse(
  as.numeric(str_extract(titanic$Cabin, "[0-9]+")) %% 2 == 1,
  "Steuerbord",
  "Backbord"
)

# Assigning deck labels to the 'Deck' variable based on the first letter of the
# 'Cabin' variable
titanic$Deck <- substr(titanic$Cabin, 1, 1)

# Set entries with unknown cabin numbers to NA
titanic$Side[titanic$Cabin == ""] <- NA
titanic$Deck[titanic$Cabin == ""] <- NA

# Remove the desired variables
titanic <- subset(titanic, select = -c(PassengerId, Name, Ticket, Cabin))

# Save the cleaned dataset to a new file
write.csv(titanic, "titanic_clean.csv", row.names = FALSE)
### Data cleaning and preparation for the analysis ###


library(stringr)
titanic <- read.csv("titanic.csv")

# Extract and store titles from 'Name' into new 'Title' column         
titanic$Title <- str_extract(titanic$Name, "(Mr\\.|Mrs\\.|Mse\\.|Ms\\.|Miss\\.|Mlle\\.|Master\\.)")
table(titanic$Title)
# Encode the variables 'Survived', 'Sex', 'Embarked' as factors
titanic$Survived <- factor(titanic$Survived)
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)

# Convert the variable 'Pclass' to an ordered factor.
titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE)

# Assuming we have a data frame named 'titanic' with columns 'Age' and 'Title'

# We use the 'ave' function to impute missing values in 'Age' based on 'Title'
titanic$Age <- ave(
  titanic$Age,                   # The variable to be imputed
  titanic$Title,                 # The grouping variable ('Title' in this case)
  FUN = function(x) {            # The imputation function
    ifelse(is.na(x),             # If the value is missing
           median(x, na.rm = TRUE),  # Impute with the median of the available values
           x)                     # Otherwise, leave the value unchanged
  }
)

# Here, the 'ave' function is used to impute missing values in 'Age' based on 'Title'.
# If the 'Age' value is missing, it uses the median of the available 'Age' values for the corresponding 'Title' group.
# Otherwise, it leaves the value unchanged.


# Port or starboard
titanic$Side <- ifelse(as.numeric(str_extract(titanic$Cabin, "[0-9]+")) %% 2 == 1, "Steuerbord", "Backbord")

# Deck: Initial letter of the cabin number
titanic$Deck <- substr(titanic$Cabin, 1, 1)

# Set entries with unknown cabin numbers to NA
titanic$Side[titanic$Cabin == ""] <- NA
titanic$Deck[titanic$Cabin == ""] <- NA

# Remove the desired variables
titanic <- subset(titanic, select = -c(Name, Ticket, Cabin))

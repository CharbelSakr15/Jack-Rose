library(stringr)
titanic <- read.csv("titanic.csv")
#head(titanic)
View(titanic)
# Extract and store titles from 'Name' into new 'Title' column         
titanic$Title <- str_extract(titanic$Name, "(Mr\\.|Mrs\\.|Mse\\.|Ms\\.|Miss\\.|Mlle\\.|Master\\.)")
table(titanic$Title)
#Encode the variables 'Survived', 'Sex', 'Embarked' as factors
titanic$Survived <- factor(titanic$Survived)
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)

#Convert the variable 'Pclass' to an ordered factor.
titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE)

# Impute missing values in 'Age' based on 'Title'
titanic$Age <- ave(titanic$Age, titanic$Title, FUN = function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Backbord oder Steuerbord
titanic$Side <- ifelse(as.numeric(str_extract(titanic$Cabin, "[0-9]+")) %% 2 == 1, "Steuerbord", "Backbord")

# Deck: Vorangehender Buchstabe der Kabinennummer
titanic$Deck <- substr(titanic$Cabin, 1, 1)

# Setze Einträge mit unbekannter Kabinennummer auf NA
titanic$Side[titanic$Cabin == ""] <- NA
titanic$Deck[titanic$Cabin == ""] <- NA

# Entferne die gewünschten Variablen 
titanic <- subset(titanic, select = -c(Name, Ticket, Cabin))

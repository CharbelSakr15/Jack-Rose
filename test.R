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



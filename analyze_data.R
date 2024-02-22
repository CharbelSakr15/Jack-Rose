### This script brings together all the tasks from the assignment.

source("analysis.R")

# Load the dataset
factors = c("Survived", "Sex", "Embarked")
ordered_factors = c("Pclass")
titanic = load_factorized_data("titanic_clean.csv", factors, ordered_factors)

# Describe the numeric variables (i)
print(describe_numerics(titanic))

# Describe the categorical variables (ii)
print(describe_categoricals(titanic))

# Compute bivariate statistics for two categorical variables (iii)
print(contingency_coefficient(titanic$Survived, titanic$Sex))

# Compute bivariate statistics for a binary and a numeric variable (iv)
print(point_biserial_correlation(titanic$Survived, titanic$Age))

# Visualize categorical variables (v)
factors = c("Pclass", "Sex", "Age")
plot = plot_titanic_alluvial(
    df = factorize_categoricals(titanic, factors), 
    factors = factors, 
    title = c("Passengers on the maiden voyage of the Titanic",
                "Stratified by demographics and survival")
)
print(plot)

# Visualize numeric variables (vi)
plot = box_plot(titanic, "Fare", "Survived")
print(plot)
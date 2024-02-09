### This file contains functions to analyze the Titanic dataset, following a 
### specific set of tasks provided in an university assignment.

source("utils.R")


# Task 2.a.i:
# This function computes descriptive statistics for numerical variables in a
# data frame.
# 
# Parameters:
#   df: data frame
#      The data frame to be analyzed
#
# Returns:
#   A data frame with the following statistics for each numerical variable:
#   Mean, Median, Standard deviation, Minimum, Maximum, NA count
describe_numerics = function(df) {
    # Check if df is a data frame
    if (!is.data.frame(df)) {
        stop("The input must be a data frame")
    }
    # Check if df contains any numerical variables
    if (length(Filter(is.numeric, df)) == 0) {
        stop("The data frame does not contain any numerical variables")
    }

    # Select only the numerical variables
    num_vars = Filter(is.numeric, df)
    
    # Compute the descriptive statistics
    desc_stats = sapply(num_vars, function(x) {
        c(
        mean   = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        sd     = sd(x, na.rm = TRUE),
        min    = min(x, na.rm = TRUE),
        max    = max(x, na.rm = TRUE),
        nac    = sum(is.na(x))
        )
    })
  
    # Return the results as a data frame
    return(as.data.frame(desc_stats))
}

# Task 2.a.ii:
# This function computes descriptive statistics for categorical variables in a
# data frame.
#
# Parameters:
#   df: data frame
#      The data frame to be analyzed
#
# Returns:
#   A named list with the following statistics for each categorical variable:
#   Counts, Frequencies, NA count
describe_categoricals = function(df) {
    # Check if df is a data frame
    if (!is.data.frame(df)) {
        stop("The input must be a data frame")
    }
    # Check if df contains any categorical variables
    if (length(Filter(is.factor, df)) == 0) {
        stop("The data frame does not contain any categorical variables")
    }

    # Select only the categorical variables
    cat_vars = Filter(is.factor, df)
    
    # Compute the descriptive statistics
    desc_stats = lapply(cat_vars, function(x) {
        counts      = matrix(table(x, useNA = "ifany"))
        frequencies = prop.table(counts)
        nac         = sum(is.na(x))

        stats = data.frame(
            counts, frequencies, nac, 
            row.names = levels(x)
        )
        colnames(stats) = c("Count", "Frequency", "NA")

        return(stats)
    })
    
    # Return the results as a data frame
    return(desc_stats)
}

# Task 2.a.iii:
# This function computes the Pearson contingency coefficient for two 
# categorical variables.
#
# Parameters:
#   x: factor
#      The first categorical vector
#   y: factor
#      The second categorical vector
#
# Returns:
#   A list containing the contingency table, the chi-squared statistic, and the
#   contingency coefficient
contingency_coefficient = function(x, y, correct = TRUE) {
    # Sanity checks
    is_factor(x)
    is_factor(y)
    have_same_length(x, y)

    # Compute the contingency table
    cont_table = table(x, y)

    # Compute the total number of observations and the row and column sums
    total    = sum(cont_table)
    row_sums = rowSums(cont_table)
    col_sums = colSums(cont_table)

    # Compute the expected frequencies
    exp_freq = outer(row_sums, col_sums) / total

    # Compute the chi-squared statistic
    chi_sq = sum((cont_table - exp_freq)^2 / exp_freq)

    # Compute the contingency coefficient
    C = sqrt(chi_sq / (chi_sq + total))

    # Correct the coefficient for small sample sizes
    if (correct) {
        k = min(nrow(cont_table), ncol(cont_table))
        C = sqrt(k / (k - 1)) * C
    }

    result = list(
        contingency_table       = cont_table,
        chi_squared             = chi_sq,
        contingency_coefficient = C
    )
    return(result)
}

# Task 2.a.iv:
# This function computes the point-biserial correlation coefficient between a
# binary variable and a continuous variable.
#
# Parameters:
#   x: numeric
#      The continuous variable
#   y: factor
#      The binary variable
#
# Returns:
#   The point-biserial correlation coefficient
point_biserial_correlation = function(x, y, bias = TRUE) {
    # Sanity checks
    is_dichotomous(y)
    is_numeric(x)
    have_same_length(x, y)

    # Drop missing values
    complete = complete.cases(x, y)
    x = x[complete]
    y = y[complete]

    # Mask the binary variable
    mask0 = y == levels(y)[1]
    mask1 = y == levels(y)[2]

    # Compute the number of observations
    n0 = sum(mask0)
    n1 = sum(mask1)
    n  = n0 + n1

    # Compute means and standard deviations
    M0 = mean(x[mask0], na.rm = TRUE)
    M1 = mean(x[mask1], na.rm = TRUE)
    sn = sqrt(1 / (n - bias) * sum((x - mean(x))^2))

    # Compute the point-biserial correlation coefficient
    rpb = (M1 - M0) / sn * sqrt(n0 * n1 / n^2)

    return(rpb)
}
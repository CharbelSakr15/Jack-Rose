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
        colnames(stats) = c("Count", "Freq", "NA")

        return(stats)
    })
    
    # Return the results as a data frame
    return(desc_stats)
}

# Task 2.a.iii:
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
    is_dichotomous(x)
    is_numeric(y)
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
    M0 = mean(y[mask0], na.rm = TRUE)
    M1 = mean(y[mask1], na.rm = TRUE)
    sn = sqrt(1 / (n - bias) * sum((y - mean(x))^2))

    # Compute the point-biserial correlation coefficient
    rpb = (M1 - M0) / sn * sqrt(n0 * n1 / n^2)

    return(rpb)
}

# Task 2.a.v:
# This function creates a suitable visualization of three or four categorical
# variables.

# Parameters:
#   df: data frame
#      The data frame containing the variables to be visualized
#   x: factor
#      The first categorical variable
#   y: factor
#      The second categorical variable
#   z: factor
#      The third categorical variable
#   w: factor
#      The fourth categorical variable

# Returns:
#   A ggplot object containing the visualization
visualize_categoricals = function(df, x, y, z = NULL, w = NULL) {
    # Sanity checks
    is_factor(x)
    is_factor(y)
    if (!is.null(z)) {
        is_factor(z)
    }
    if (!is.null(w)) {
        is_factor(w)
    }

    # Create the visualization
    if (is.null(z) && is.null(w)) {
        plot = ggplot(df, aes(x = x, fill = y)) +
               geom_bar(position = "dodge") +
               labs(title = "Bar plot of " ~ x ~ " and " ~ y,
                    x = x, y = "Count") +
               theme_minimal()
    } else if (!is.null(z) && is.null(w)) {
        plot = ggplot(df, aes(x = x, fill = y)) +
               geom_bar(position = "dodge") +
               facet_wrap(~ z) +
               labs(title = "Bar plot of " ~ x ~ " and " ~ y,
                    x = x, y = "Count") +
               theme_minimal()
    } else if (!is.null(z) && !is.null(w)) {
        plot = ggplot(df, aes(x = x, fill = y)) +
               geom_bar(position = "dodge") +
               facet_grid(z ~ w) +
               labs(title = "Bar plot of " ~ x ~ " and " ~ y,
                    x = x, y = "Count") +
               theme_minimal()
    }

    return(plot)
}


# This function creates a factorized frequency table for the titanic data frame.
# It only retains the variables: 'Pclass', 'Sex', 'Age', and 'Survived'.
# The function returns a data frame with the frequency of each combination of
# these categories.
# 
# Parameters:
#   titanic: data frame
#      The data frame to be analyzed
#
# Returns:
#   A data frame with the factorized variables counts
factorize_categoricals = function(titanic, factors) {
    # Sanity checks
    if (!is.data.frame(titanic)) {
        stop("The input must be a data frame")
    }
    if (!("Survived" %in% colnames(titanic))) {
        stop("The 'Survived' variable must be in the data frame")
    }
    if (!all(factors %in% colnames(titanic))) {
        stop("The data frame does not contain all the specified factors")
    }

    titanic_freq = data.frame(titanic)
    if ("Age" %in% factors) {
        titanic_freq$Age = ifelse(titanic_freq$Age < 14, "Child", "Adult")
    }
    if ("Pclass" %in% factors) {
        titanic_freq$Pclass = ifelse(
            titanic_freq$Pclass == 1, "1st", 
            ifelse(titanic_freq$Pclass == 2, "2nd", "3rd")
        )
    }
    titanic_freq$Survived = ifelse(titanic_freq$Survived == 1, "Yes", "No")

    titanic_freq = table(titanic_freq[, c(factors, "Survived")])
    titanic_freq = as.data.frame(titanic_freq)
    colnames(titanic_freq) = c(factors, "Survived", "Freq")

    return (titanic_freq)
}

# This function creates an alluvial diagram for the titanic data set. The 
# input data frame must contain the factorized categorical variables produces by
# the factorize_categoricals function.
# The diagram visualizes the frequency of each combination of the categorical
# variables, stratified by the 'Survived' variable.
#
# Parameters:
#   df: data frame
#      The data frame containing the variables to be visualized
#   factors: character vector
#      The names of the categorical variables to be visualized
#   title: character vector
#      The title of the plot
#
# Returns:
#   A ggplot object containing the alluvial diagram
plot_titanic_alluvial = function(df, factors, title = NULL) {
    # Sanity checks
    if (!is.data.frame(df)) {
        stop("The input must be a data frame")
    }
    if (length(factors) > 4) {
        stop("The factors parameter must contain less than four variables")
    }
    if (!("Survived" %in% colnames(df))) {
        stop("The 'Survived' variable must be in the data frame")
    }
    if (!("Freq" %in% colnames(df))) {
        stop("The data frame must contain a 'Freq' variable")
    }

    # Create the aesthetic mappings with stupido code R requires
    n = length(factors)
    if (n == 1) {
        aess = aes(
            axis1 = !!rlang::sym(factors[1]), 
            y = Freq
        )
    } else if (n == 2) {
        aess = aes(
            axis1 = !!rlang::sym(factors[1]), 
            axis2 = !!rlang::sym(factors[2]), 
            y = Freq
        )
    } else if (n == 3) {
        aess = aes(
            axis1 = !!rlang::sym(factors[1]), 
            axis2 = !!rlang::sym(factors[2]), 
            axis3 = !!rlang::sym(factors[3]), 
            y = Freq
        )
    } else if (n == 4) {
        aess = aes(
            axis1 = !!rlang::sym(factors[1]), 
            axis2 = !!rlang::sym(factors[2]), 
            axis3 = !!rlang::sym(factors[3]), 
            axis4 = !!rlang::sym(factors[4]), 
            y = Freq
        )
    }

    # create the alluvial plot
    plot = ggplot(data = df, aess) +
        scale_x_discrete(limits = factors, expand = c(.2, .05)) +
        geom_alluvium(aes(fill = Survived)) +
        geom_stratum() +
        geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
        theme_minimal() +
        ggtitle(title[1], title[2])

    return(plot)
}


# Task 2.a.vi:
# further functions suitable for description and visualization
# This function creates a box plot for a numerical variable, grouped by a
# categorical variable
box_plot = function(df, factor1, factor2) {
    # Sanity checks
    if (!is.data.frame(df)) {
        stop("The input must be a data frame")
    }
    if (!is.numeric(df[[factor1]])) {
        stop("The first variable must be numeric")
    }
    if (!is.factor(df[[factor2]])) {
        stop("The second variable must be a factor")
    }
    
    x_data = df[[factor1]]
    y_data = df[[factor2]]

    # Create the box plot
    plot = ggplot(df, aes(x = y_data, y = x_data)) +
           geom_boxplot() +
           labs(title = paste0("Box plot of ", factor2, " by ", factor1),
                x = factor2, y = factor1) +
           theme_minimal()
    return(plot)
}


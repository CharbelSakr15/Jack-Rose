### This file contains miscellaneous utility functions that are used in 
### analysis.R.

# This function loads a csv file into a data frame and encodes the specified
# variables as factors.
#
# Parameters:
#   file: character
#      The name of the file to be loaded
#   factors: character vector
#      The names of the variables to be encoded as factors
#   ordered_factors: character vector
#      The names of the variables to be encoded as ordered factors
#
# Returns:
#   A data frame with the variables of class integer encoded as factors
load_factorized_data = function(file, factors = NULL, ordered_factors = NULL) {
    # Load the data
    data = read.csv(file)
  
    # Encode the variables as factors
    if (!is.null(factors)) {
        for (var in factors) {
            data[[var]] = factor(data[[var]])
        }
    }
    # Encode the ordered variables as factors
    if (!is.null(ordered_factors)) {
        for (var in ordered_factors) {
            data[[var]] = factor(data[[var]], ordered = TRUE)
        }
    }

  return(data)
}


######### Sanity Checks #########
# This function checks if a variable is numeric.
#
# Parameters:
#   x: any
#      The variable to be checked
#
# Throws:
#   An error if the input is not numeric
is_numeric = function(x) {
    if (!is.numeric(x)) {
        stop("The input must be numeric")
    }
}

# This function checks if a variable is a factor.
#
# Parameters:
#   x: any
#      The variable to be checked
#
# Throws:
#   An error if the input is not a factor
is_factor = function(x) {
    if (!is.factor(x)) {
        stop("The input must be a factor")
    }
}

# This function checks if a factor is dichotomous, i.e., if it has exactly two
# levels.
#
# Parameters:
#   x: factor
#      The factor to be checked
#
# Throws:
#   An error if the input is not a factor or if it does not have exactly two
#   levels
is_dichotomous = function(x) {
    is_factor(x)
    if (length(levels(x)) != 2) {
        stop("The factor must have exactly two levels")
    }
}

# This function checks if two variables have the same length.
#
# Parameters:
#   x: any
#      The first variable to be checked
#   y: any
#      The second variable to be checked
#
# Throws:
#   An error if the inputs do not have the same length
have_same_length = function(x, y) {
    if (length(x) != length(y)) {
        stop("The inputs must have the same length")
    }
}
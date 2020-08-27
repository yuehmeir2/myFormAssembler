#' Build the constraint data frame for a linear program from a filter vector.
#'
#' @param x A numeric vector with its length equal to the number of decision variables.
#'          Each entry is either a numeric value or NA.
#'
#' @return A constraint data frame for the linear program
#'
#' @details
#' If x[i] is a numeric value then the i-th decision variable is fixed to be this value;
#' if x[i] is NA then no requirement is placed on the corresponding decision variable.


gen_filter_constraint <- function (x, label = "", consId = NULL) {

    w = which(!is.na(x))

    num.con = length(w)

    if (num.con == 0)
    {
      return (NULL)
    }

    x = x[w]

    constraint = vector(mode = "list", length = num.con)

    for (i in seq(num.con))
    {
      constraint[[i]] = list(
        label = label,
        consId   = consId,
        variable = w[i],
        coef = 1,
        type = "=",
        rhs = x[i])
    }

    return (constraint)
  }




#' Calculate Fisher information for 3PL for a single item and
#' a vector of theta values

fisher.3PL =
  function(theta, a, b, c, D = 1)
  {
    p = c + (1 - c) / (1 + exp(a * D * (b - theta)))

    (D * a * (p - c) / (1 - c))^2 * (1 - p) / p
  }



#' Calculate Fisher information for GPC for a single item and a vector
#' of theta values

#' The Calculation is based on Equation 10 in the Muraki (1993) paper,
#' which says that
#'       I(theta) = (a * D)^2 * var( score | theta )
#'

fisher.GPC =
  function(theta, a, b, d, D = 1)
  {
    d = d[!is.na(d)]

    s = seq_along(d)

    W = cumsum(d) + outer(s, theta - b)

    W = exp(W * a * D)

    W = scale(W, scale = colSums(W), center = FALSE)

    (a * D)^2 * (colSums(W * s^2) - colSums(W * s)^2)
  }



#' Calculate Fisher information for 3PL or GPC for multiple items
#' and a vector of theta values
#'
#' @param theta
#' A vector of ability values
#'
#' @param IRT.par
#' A matrix of IRT parameters. This should have the columns required by the
#' IRT model of choice.
#'
#' @param IRT.model
#' A vector of character strings that specify the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRT.scale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @param getTestInf
#' A boolean indicating if item-level information or test information is wanted.
#'
#' @return
#' If getTestInf = F, an item by theta matrix of Fisher information values is return;
#' otherwise, a vector of test information vector is returned.
#'
#' @export



fisher = function (theta, IRT.par, IRT.model = "3PL", IRT.scale = 1.7, getTestInf = F) {

    if (is.vector(IRT.par))
    {
      IRT.par = matrix(IRT.par, nrow = 1)
    }

    if (is.data.frame(IRT.par))
    {
      #         IRT.par = as.matrix(IRT.par)
      IRT.par = data.matrix(IRT.par)
    }

    num.items = nrow(IRT.par)

    m = which(IRT.model == "2PL" | IRT.model == "1PL")

    if (length(m) > 0)
      IRT.model[m] = "3PL"

    m = which(IRT.model == "RPC")

    if (length(m) > 0)
      IRT.model[m] = "GPC"

    if (length(IRT.model) == 1)
    {
      IRT.model = rep(IRT.model, num.items)
    }

    fisher = matrix(nrow = num.items, ncol = length(theta))

    rownames(fisher) = rownames(IRT.par)
    colnames(fisher) = paste("theta:[", theta, "]", sep="")

    a = IRT.par[, 1] * IRT.scale
    b = IRT.par[, 2]
    c = IRT.par[, 3]
    d = IRT.par[, -(1:2)]

    for (i in which(IRT.model == "3PL"))
    {
      fisher[i, ] = fisher.3PL(theta, a[i], b[i], c[i])
    }

    for (i in which(IRT.model == "GPC"))
    {
      fisher[i, ] = fisher.GPC(theta, a[i], b[i], d[i,])
    }

    if (!getTestInf) return (fisher)

    return(colSums(fisher))
}

#' Calculate Test Information for 3PL or GPC for multiple items
#' and a vector of theta values
#'
#' @param theta
#' A vector of ability values
#'
#' @param IRT.par
#' A matrix of IRT parameters. This should have the columns required by the
#' IRT model of choice.
#'
#' @param IRT.model
#' A vector of character strings that specify the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRT.scale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @return
#' A vector of CSEM values for each of theta points over all items.
#'
#' @export


test_fisher = function (theta, IRT.par, IRT.model = "3PL", IRT.scale = 1.7) {

  return(fisher (theta = theta, IRT.par = IRT.par, IRT.model = IRT.model, IRT.scale = IRT.scale, getTestInf = T))

}

#' Calculate average information for 3PL or GPC for multiple items
#' and a vector of theta values
#'
#' @param theta
#' A vector of ability values
#'
#' @param IRT.par
#' A matrix of IRT parameters. This should have the columns required by the
#' IRT model of choice.
#'
#' @param IRT.model
#' A vector of character strings that specify the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRT.scale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @return
#' A vector of CSEM values for each of theta points over all items.
#'
#' @export

avg_fisher = function (theta, IRT.par, IRT.model = "3PL", IRT.scale = 1.7) {

  if (is.vector(IRT.par))
    n_item = 1 else n_item = nrow(IRT.par)

  return(fisher (theta = theta, IRT.par = IRT.par, IRT.model = IRT.model,
                 IRT.scale = IRT.scale, getTestInf = T) / n_item)

}

#' Calculate CSEM for 3PL or GPC for multiple items
#' and a vector of theta values
#'
#' @param theta
#' A vector of ability values
#'
#' @param IRT.par
#' A matrix of IRT parameters. This should have the columns required by the
#' IRT model of choice.
#'
#' @param IRT.model
#' A vector of character strings that specify the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRT.scale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @return
#' A vector of CSEM values for each of theta points over all items.
#'
#' @export



CSEM = function (theta, IRT.par, IRT.model = "3PL", IRT.scale = 1.7) {

  inf = fisher (theta = theta, IRT.par = IRT.par, IRT.model = IRT.model, IRT.scale = IRT.scale)
  return(1.0 / sqrt(colSums(inf)))

}

#' Calculate expected item scores for 3PL for a single item and
#' a vector of theta values

expected_score_3PL = function(theta, a, b, c, D = 1) {

    c + (1 - c) / (1 + exp(a * D * (b - theta)))
}



#' Calculate expected item scores for GPC for a single item and
#' a vector of theta values

expected_score_GPC = function(theta, a, b, d, D = 1) {

    d = d[!is.na(d)]

    s = seq_along(d)

    W = cumsum(d) + outer(s, theta - b)

    W = exp(W * a * D)

    colSums(W * s) / colSums(W) - 1
}



#' Calculate expected item scores for 3PL or GPC for multiple items
#' and a vector of theta values
#'
#' @param theta
#' A vector of ability values
#'
#' @param IRT.par
#' A matrix of IRT parameters. This should have the columns required by the IRT model of choice.
#'
#' @param IRT.model
#' A vector of character strings that specify the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRT.scale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @param is_percent_scale
#' if TRUE, the expected percent score for the given thetas will be returned, which is the tcc.
#'
#' @return
#' An item by theta matrix of expected item scores
#'
#' @export



expected_score = function (theta, IRT.par, IRT.model = "3PL", IRT.scale = 1.7, is_percent_scale = F) {

    if (is.vector(IRT.par))
    {
      IRT.par = matrix(IRT.par, nrow = 1)
    }

    if (is.data.frame(IRT.par))
    {
      IRT.par = as.matrix(IRT.par)
    }

    num.items = nrow(IRT.par)

    if (length(IRT.model) == 1)
    {
      IRT.model = rep(IRT.model, num.items)
    }

    score = matrix(nrow = num.items, ncol = length(theta))
    raw_score = 0

    rownames(score) = rownames(IRT.par)
    colnames(score) = paste("theta:[", theta, "]", sep="")

    a = IRT.par[, 1] * IRT.scale
    b = IRT.par[, 2]
    c = IRT.par[, 3]
    d = IRT.par[, -(1:2)]

    m = which(IRT.model == "2PL" | IRT.model == "1PL")

    if (length(m) > 0)
      IRT.model[m] = "3PL"

    m = which(IRT.model == "RPC")

    if (length(m) > 0)
      IRT.model[m] = "GPC"

    for (i in which(IRT.model == "3PL"))
    {
      score[i, ] = expected_score_3PL(theta, a[i], b[i], c[i])
      raw_score = raw_score + 1
    }

    for (i in which(IRT.model == "GPC"))
    {
      the_score = sum(!is.na(d[i,])) - 1
      score[i, ] = expected_score_GPC(theta, a[i], b[i], d[i,])
      # raw_score = raw_score + length(d[i,]) - 1
      raw_score = raw_score + the_score
    }

    if (is_percent_scale && raw_score > 0 && nrow(IRT.par) > 1) {
      return (colSums(score)/raw_score)
    }
    return (score)
}

#' Calculate expected percent scores for 3PL or/and GPC items
#'
#' @param theta
#' A vector of ability values
#'
#' @param IRT.par
#' A matrix of IRT parameters. This should have the columns required by the IRT model of choice.
#'
#' @param IRT.model
#' A vector of character strings that specify the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRT.scale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @param is_percent_scale
#' if TRUE, the expected percent score for the given thetas will be returned, which is the tcc.
#'
#' @return
#' An vector of expected percent score for all items
#'
#' @export


test_expected_percent_score = function (theta, IRT.par, IRT.model = "3PL", IRT.scale = 1.7) {

  return(expected_score (theta = theta, IRT.par = IRT.par, IRT.model = IRT.model,
                         IRT.scale = IRT.scale, is_percent_scale = T))

}

#' Calculate expected raw scores for 3PL or/and GPC items
#'
#' @param theta
#' A vector of ability values
#'
#' @param IRT.par
#' A matrix of IRT parameters. This should have the columns required by the IRT model of choice.
#'
#' @param IRT.model
#' A vector of character strings that specify the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRT.scale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @param is_percent_scale
#' if TRUE, the expected percent score for the given thetas will be returned, which is the tcc.
#'
#' @return
#' An vector of expected percent score for all items
#'
#' @export


test_expected_raw_score = function (theta, IRT.par, IRT.model = "3PL", IRT.scale = 1.7) {

  return(colSums(expected_score (theta = theta, IRT.par = IRT.par, IRT.model = IRT.model,
                         IRT.scale = IRT.scale, is_percent_scale = F)))

}



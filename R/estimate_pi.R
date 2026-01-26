#' @importFrom stats sd pbeta runif dbeta rbeta rgamma
#' @importFrom graphics hist
#' @importFrom grDevices rgb
NULL

# Empirical Methods -------------------------------------------------------


# Idea: Generate uniformly distributed points between 0 and 1 and consider them
#       to be vectors pointing to that random coordinate. Next count the vectors
#       that have a vector norm of less than 1, i.e. points within the unit circle.
#       The ratio of the points within to the points beyond the unit circle,
#       are an approximation of pi/4.
#
#       The accurcacy of the estimate dependens on the number of generated uniform points

#' Generate uniformly distributed points
#'
#' @param n The number of points to generate.
#'
#' @return A matrix of uniformly distributed points.
#' @export
#'
#' @examples
#' generate_points(10)
generate_points <- function(n) {
  # put them in a matrix
  XY <- matrix(runif(2 * n), nrow = n, ncol = 2)

  return(XY)
}

#' Get distance
#' Calculates the vector norm and checks whether the point is within the radius (i.e. < 1).
#' @param XY A matrix of points.
#'
#' @return A logical vector indicating whether each point is within the unit circle.
#' @export
#'
#' @examples
#' pts <- generate_points(10)
#' get_distance(pts)
get_distance <- function(XY) {
  # calculate vector norm and test if its bigger than the radius (i.e. 1)
  dist <- sqrt(XY[, 1]^2 + XY[, 2]^2) < 1
  return(dist)
}

#' Approximate pi
#' Approximates pi using the formula 4 * points_within_circle / total_points.
#' @param dist A logical vector indicating whether each point is within the unit circle.
#'
#' @return An approximation of pi.
#' @export
#'
#' @examples
#' pts <- generate_points(1000)
#' dist <- get_distance(pts)
#' approx_pi(dist)
approx_pi <- function(dist) {
  # pi*r^2 / 4 == numb_circle / numb_total (divided by for because we have just one quadrant)
  return(4 * sum(dist) / length(dist))
}


# Main Function Empirical -------------------------------------------------


#' Estimate pi empirically
#' The empirical version just generates data between 0 and 1 and calculates the
#' distance to the radius of the unit circle.
#' More details here : https://en.wikipedia.org/wiki/Approximations_of_%CF%80#Summing_a_circle's_area
#' @param n The number of points to generate.
#'
#' @return An approximation of pi.
#' @export
#'
#' @examples
#' estimate_pi_empirical(1000)
estimate_pi_empirical <- function(n) {
  dist <- get_distance(generate_points(n = n))
  return(approx_pi(dist = dist))
}


# Resampling Methods ------------------------------------------------------

# Idea: The idea is to circumvent the generation of high numbers of points to
#       get an accurate approximate of pi. Instead, we calculate rather innacurate
#       estimates of the ratio of points within to beyond the unit circle and generate
#       a beta or gamma distribution of the ratios. Parameter estimation of
#       the beta distribution is based on the methods of moments to find starting
#       values for alpha and beta in ~ Beta(alpha, beta). Once the distribution
#       is setup, we sample from that said distribution and take the mean of the
#       distribution, which should be a good estimate of pi.
#
#       This method has many parameters, such as, the number of points to calculate
#       one ratio, the number of initial ratios to generate, the number of ratios
#       to draw from the newly made distribution, thus this needs some optimization.

#' Calculate ratio of points within
#'
#' @param n The number of points to generate.
#' @param samplingSize The number of samples to take.
#' @param plot A logical indicating whether to plot a histogram of the ratios.
#'
#' @return A vector of ratios.
#' @export
#'
#' @examples
#' calc_ratio(100, 10, FALSE)
calc_ratio <- function(n, samplingSize, plot) {
  distMatrix <- matrix(get_distance(generate_points(n)), nrow = samplingSize)
  ratioVector <- rowSums(distMatrix) / ncol(distMatrix)

  if (plot) {
    hist(ratioVector, freq = FALSE)
  }

  return(ratioVector)
}

#' Generate gamma distribution
#' Fits a gamma distribution to a ratio data set and samples from it.
#' @param ratioVector A vector of ratios.
#' @param outputLength The number of samples to generate.
#' @param plot A logical indicating whether to plot a histogram of the generated samples.
#'
#' @return A vector of samples from the gamma distribution.
#' @export
#'
#' @examples
#' ratios <- calc_ratio(100, 10, FALSE)
#' generate_gamma(ratios, 100, FALSE)
generate_gamma <- function(ratioVector, outputLength, plot) {
  thetaGamma <- MASS::fitdistr(ratioVector, "gamma")$estimate

  set.seed(sample(1:1e6, 1))

  # histogram
  if (plot) {
    hist(rgamma(outputLength, shape = thetaGamma[1], rate = thetaGamma[2]),
      add = TRUE, col = rgb(0.9, 0.1, 0.1, 0.2), freq = FALSE
    )
  }
  return(rgamma(outputLength, shape = thetaGamma[1], rate = thetaGamma[2]))
}

#' Beta method of moments
#' from: https://stats.stackexchange.com/questions/376634/how-to-pick-starting-parameters-for-massfitdist-with-the-beta-distribution
#' @param x A numeric vector.
#'
#' @return A list with the shape1 and shape2 parameters of the beta distribution.
#' @export
#'
#' @examples
#' ratios <- calc_ratio(100, 10, FALSE)
#' beta_mom(ratios)
beta_mom <- function(x) {
  m_x <- mean(x, na.rm = TRUE)
  s_x <- sd(x, na.rm = TRUE)


  # TODO: untested done by Jules
  if (s_x == 0) {
    # Cannot estimate parameters if sd is 0, return some defaults
    out <- list(shape1 = 1, shape2 = 1)
    return(out)
  }

  alpha <- m_x * ((m_x * (1 - m_x) / s_x^2) - 1)
  beta <- (1 - m_x) * ((m_x * (1 - m_x) / s_x^2) - 1)

  out <- list(shape1 = alpha, shape2 = beta)

  # TODO: untested done by Jules
  # Check for negative or non-finite values and return defaults if so
  if (!is.finite(alpha) || !is.finite(beta) || alpha <= 0 || beta <= 0) {
    out <- list(shape1 = 1, shape2 = 1)
    return(out)
  }

  return(out)
}

#' Generate beta distribution
#' Fits a beta distribution to a ratio data set and samples from this distribution.
#' @param ratioVector A vector of ratios.
#' @param outputLength The number of samples to generate.
#' @param plot A logical indicating whether to plot a histogram of the generated samples.
#'
#' @return A vector of samples from the beta distribution.
#' @export
#'
#' @examples
#' ratios <- calc_ratio(100, 10, FALSE)
#' generate_beta(ratios, 100, FALSE)
generate_beta <- function(ratioVector, outputLength, plot) {
  # remove 1s and 0s (the latter is very unlikely)
  ratioVector <- ratioVector[which(ratioVector < 1)]


  # use methods of moments to get an estimate for alpha (shape1) and beta (shape2)
  # fitdistr needs starting values for beta-distribution
  thetaBetaStartValues <- beta_mom(ratioVector)

  # MLE of beta distribution given ratioVector data

  thetaBeta <- MASS::fitdistr(ratioVector, "beta", start = thetaBetaStartValues)$estimate

  # histogram
  if (plot) {
    hist(rbeta(outputLength, shape1 = thetaBeta[1], shape2 = thetaBeta[2]),
      add = TRUE, col = rgb(0.9, 0.1, 0.1, 0.2), freq = FALSE
    )
  }

  # generate data from the beta-distribution with MLE-estimates
  return(rbeta(outputLength, shape1 = thetaBeta[1], shape2 = thetaBeta[2]))
}


#' Get beta distribution parameters
#' Fits a beta distribution to a ratio data set and returns the parameters.
#' @param ratioVector A vector of ratios.
#'
#' @return The parameters of the beta distribution.
#' @export
#'
#' @examples
#' ratios <- calc_ratio(100, 10, FALSE)
#' get_beta_dist(ratios)
get_beta_dist <- function(ratioVector) {
  # remove 1s and 0s (the latter is very unlikely)
  ratioVector <- ratioVector[which(ratioVector < 1)]


  # use methods of moments to get an estimate for alpha (shape1) and beta (shape2)
  # fitdistr needs starting values for beta-distribution
  thetaBetaStartValues <- beta_mom(ratioVector)

  # MLE of beta distribution given ratioVector data
  out <- MASS::fitdistr(ratioVector, "beta", start = thetaBetaStartValues)$estimate
  return(out)
}

#' Approximate pi using resampling
#'
#' @param randVec A vector of random numbers.
#'
#' @return An approximation of pi.
#' @export
#'
#' @examples
#' ratios <- calc_ratio(100, 10, FALSE)
#' rand_beta <- generate_beta(ratios, 100, FALSE)
#' approx_pi_resample(rand_beta)
approx_pi_resample <- function(randVec) {
  withinCircle <- mean(randVec)
  outsideCircle <- 1 - withinCircle

  return(4 * (withinCircle / (withinCircle + outsideCircle)))
}

# Main Function Resampling ------------------------------------------------

#' Estimate pi using resampling
#' This function uses a probabilistic approach. The ratio of points that are
#' within the radius is sampled from a fitted gamma distribution. Currently, the
#' mean of randomly generated data from this vector is used to estimate pi.
#' @param n The number of points to generate.
#' @param outputLength The number of samples to generate from the distribution.
#' @param samplingSize The number of samples to take for the ratio calculation.
#' @param distr The distribution to use for resampling ("beta" or "gamma").
#' @param plot A logical indicating whether to plot a histogram of the generated samples.
#'
#' @return An approximation of pi.
#' @export
#'
#' @examples
#' estimate_pi_resampled(100, 100, 10)
estimate_pi_resampled <- function(n,
                                  outputLength = 1e6,
                                  samplingSize = 1e5,
                                  distr = "beta",
                                  plot = FALSE) {
  # generate ratios from n
  if (distr == "beta") {
    rand <- generate_beta(
      calc_ratio(n, samplingSize = samplingSize, plot = plot),
      plot = plot,
      outputLength = outputLength
    )
  } else {
    rand <- generate_gamma(
      calc_ratio(n, samplingSize = samplingSize, plot = plot),
      plot = plot,
      outputLength = outputLength
    )
  }

  # use resampled data to approx pi
  return(approx_pi_resample(rand))
}



# MCMC-like Functions -----------------------------------------------------

#' MCMC-like algorithm for pi estimation
#' MCMC-like algorithm, but instead of using a hastings ratio, I used the accuracy measure,
#' which kinda breaks the purpose of the MCMC, which is used when the true value is unknown.
#' But heck, its just for fun, right.
#' @param nInit The number of initial points to generate for the prior distribution.
#' @param samplingSize The number of samples to take for the prior distribution.
#' @param nSD The number of points to generate for the standard deviation calculation.
#' @param nIter The number of iterations for the MCMC chain.
#'
#' @return A vector representing the MCMC chain.
#' @export
#'
#' @examples
#' MCMC_Pi(nIter = 100)
MCMC_Pi <- function(nInit = 1e6, samplingSize = 1e4, nSD = 1000, nIter) {
  # 0.) initialize result vector and get a value for d
  x <- rep(0, nIter)

  d <- sd(calc_ratio(nInit, nSD, FALSE))

  # 1.) create a prior distribution

  # beta parameter from 1e6 ratios >> prior distribution params
  thetaBetaMLE <- get_beta_dist(calc_ratio(nInit, samplingSize, FALSE))

  # mean of beta distribution
  meanBeta <- unname(1 / (1 + (thetaBetaMLE[2] / thetaBetaMLE[1])))

  pbeta(meanBeta, shape1 = thetaBetaMLE[1], shape2 = thetaBetaMLE[2])

  # 2.) Propose first move (start with mean of prior)
  x[1] <- meanBeta

  # 3.) Initiate loop

  for (iter in 2:nIter) {
    # 4.) propose a move with uniform proposal kernel
    x[iter] <- runif(n = 1, min = x[iter - 1] - d / 2, max = x[iter - 1] + d / 2)

    # 3.) Compute accuracy, accept move if accuracy is better else stay
    current <- abs(approx_pi_resample(x[iter]) - pi)
    previous <- abs(approx_pi_resample(x[iter - 1]) - pi)

    if (current < previous) {
      next
    } else {
      x[iter] <- x[iter - 1]
    }
  }
  return(x)
}

#' Metropolis-Hastings-like algorithm for pi estimation
#' Same as MCMC_Pi but with a real Hastings ratio.
#' @param nInit The number of initial points to generate for the prior distribution.
#' @param samplingSize The number of samples to take for the prior distribution.
#' @param nSD The number of points to generate for the standard deviation calculation.
#' @param nIter The number of iterations for the MCMC chain.
#'
#' @return A vector representing the MCMC chain.
#' @export
#'
#' @examples
#' MCMC_h_Pi(nIter = 100)
MCMC_h_Pi <- function(nInit = 1e6, samplingSize = 1e4, nSD = 1000, nIter) {
  # 0.) initialize result vector and get a value for d
  x <- rep(0, nIter)

  d <- sd(calc_ratio(nInit, nSD, FALSE))
  d <- 0.2

  # 1.) create a prior distribution

  # beta parameter from 1e6 ratios >> prior distribution params
  thetaBetaMLE <- get_beta_dist(calc_ratio(nInit, samplingSize, FALSE))

  # mean of beta distribution
  meanBeta <- unname(1 / (1 + (thetaBetaMLE[2] / thetaBetaMLE[1])))


  # 2.) Propose first move (start with mean of prior)
  x[1] <- meanBeta

  # 3.) Initiate loop

  for (iter in 2:nIter) {
    # 4.) propose a move with uniform proposal kernel
    x[iter] <- runif(n = 1, min = x[iter - 1] - d / 2, max = x[iter - 1] + d / 2)

    # 5.) hastings ratio
    current <- dbeta(x[iter], shape1 = thetaBetaMLE[1], shape2 = thetaBetaMLE[2])
    previous <- dbeta(x[iter - 1], shape1 = thetaBetaMLE[1], shape2 = thetaBetaMLE[2])

    h <- min(1, current / previous)

    # 6.) decide move
    u <- runif(1, 0, 1)
    if (u <= h) {
      next # accept >> next iteration
    } else {
      x[iter] <- x[iter - 1] # reject >> repeat
    }
  }
  return(x)
}


# Auxiliary Functions -----------------------------------------------------


# Methods to measure the accuracy of the estimates

#' Accuracy of pi estimate
#' The smaller the number the better.
#' @param pi_estimate An estimation of pi.
#'
#' @return The absolute difference between the estimate and the true value of pi.
#' @export
#'
#' @examples
#' accuracy_pi_estimate(3.14)
accuracy_pi_estimate <- function(pi_estimate) {
  return(abs(pi_estimate - pi))
}


#' Scoring function
#' The lower the absolute difference between pi and the estimate the better.
#' @param res The result of the accuracy_pi_estimate function.
#'
#' @return A score from 0 to 6.
#' @export
#'
#' @examples
#' scoring(0.05)
scoring <- function(res) {
  if (res > 0.1) {
    return(0)
  } else if (res > 0.01) {
    return(1)
  } else if (res > 0.001) {
    return(2)
  } else if (res > 0.0001) {
    return(3)
  } else if (res > 0.00001) {
    return(4)
  } else if (res > 0.000001) {
    return(5)
  } else {
    return(6)
  }
}

#' Test accuracy of pi estimation
#' Compare time and score of accuracy. Not for MCMC as we only consider the last values.
#' @param n The number of points to generate.
#' @param nIter The number of iterations.
#' @param type The type of estimation ("empirical" or "resampled").
#' @param samplingSize The number of samples to take for the ratio calculation.
#' @param outputLength The number of samples to generate from the distribution.
#' @param distr The distribution to use for resampling ("beta" or "gamma").
#'
#' @return A data frame with the accuracy scores.
#' @export
#'
#' @examples
#' test_accuracy(100, 10, "empirical")
test_accuracy <- function(n,
                          nIter,
                          type,
                          samplingSize = 1e4,
                          outputLength = 1e6,
                          distr = "beta") {
  # Scoring Output
  message("Scoring Calculation: Difference = estimated pi minus real pi \n")
  message("Scores:")
  message("Difference > 0.1 : Score = 0")
  message("Difference > 0.01 : Score = 1")
  message("Difference > 0.001 : Score = 2")
  message("Difference > 0.0001 : Score = 3")
  message("Difference > 0.00001 : Score = 4")
  message("Difference > 0.000001 : Score = 5")
  message("Difference < 0.000001 : Score = 6 \n")


  # create vector for accuracy measure
  vecAccuracy <- c()

  # loop for nIter
  if (type == "empirical") {
    message("Method : Empirical \n")
    for (i in 1:nIter) {
      estimate <- accuracy_pi_estimate(estimate_pi_empirical(n))
      score <- scoring(estimate)
      vecAccuracy <- c(vecAccuracy, score)
    }
  } else {
    message("Method : Resampled \n")
    for (i in 1:nIter) {
      estimate <- accuracy_pi_estimate(estimate_pi_resampled(n,
        samplingSize = samplingSize,
        outputLength = outputLength,
        distr = distr
      ))
      score <- scoring(estimate)
      vecAccuracy <- c(vecAccuracy, score)
    }
  }
  # output formatting
  DF <- data.frame(nIter, mean(vecAccuracy), sd(vecAccuracy), min(vecAccuracy), max(vecAccuracy))
  colnames(DF) <- c("Iterations", "Mean Score", "SD Score", "Min Score", "Max Score")

  message("Accuracy of the Pi Estimate: ")
  return(DF)
}

#' Mean estimate of pi
#' Descriptive statistics of the pi estimate collected from several iterations.
#' @param n The number of points to generate.
#' @param nIter The number of iterations.
#' @param type The type of estimation ("empirical" or "resampled").
#' @param samplingSize The number of samples to take for the ratio calculation.
#' @param outputLength The number of samples to generate from the distribution.
#' @param distr The distribution to use for resampling ("beta" or "gamma").
#'
#' @return A data frame with descriptive statistics of the pi estimates.
#' @export
#'
#' @examples
#' mean_estimate(100, 10, "empirical")
mean_estimate <- function(n,
                          nIter,
                          type,
                          samplingSize = 1e4,
                          outputLength = 1e6,
                          distr = "beta") {
  vecAccuracy <- c()

  if (type == "empirical") {
    for (i in 1:nIter) {
      estimate <- accuracy_pi_estimate(estimate_pi_empirical(n))
      vecAccuracy <- c(vecAccuracy, estimate)
    }
  } else {
    for (i in 1:nIter) {
      estimate <- accuracy_pi_estimate(estimate_pi_resampled(n,
        samplingSize = samplingSize,
        outputLength = outputLength,
        distr = distr
      ))
      vecAccuracy <- c(vecAccuracy, estimate)
    }
  }
  # output formatting
  DF <- data.frame(nIter, mean(vecAccuracy), sd(vecAccuracy), min(vecAccuracy), max(vecAccuracy))
  colnames(DF) <- c("Iterations", "Mean", "SD", "Min", "Max")
  return(DF)
}


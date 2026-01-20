# # Test functions ----------------------------------------------------------
#
# ### empirical
# estimate_pi_empirical(1e6)
#
# ### resampled
# estimate_pi_resampled(n = 1e6,
#                       outputLength = 1e6,
#                       samplingSize = 1e4,
#                       plot = FALSE,
#                       distr = "beta")
#
# accuracy_pi_estimate(
#   estimate_pi_resampled(n = 1e6,
#                         outputLength = 1e6,
#                                     samplingSize = 1e4,
#                                     plot = TRUE,
#                                     distr = "beta"))
# #
# ### MCMC-like cheat algo (super accurate)
# posterior <- MCMC_Pi(nIter = 1e4)
# approx_pi_resample(tail(posterior,n = 1))
#
# # plot trace
# plot(4*posterior, type = "l")
# abline(h=pi, col = "firebrick")
#
#
# ### Metropolis-Hastings Algo that explores ratio beta distribution
# posterior <- MCMC_h_Pi(nIter = 1e5)
# approx_pi_resample(posterior)
#
# # plot trace
# plot(4*posterior, type = "l")
# abline(h=pi, col = "firebrick")
#
#
#
# # BenchMark (Speed) -------------------------------------------------------
#
# n = 1e6
#
# test <- rbenchmark::benchmark("empirical" = {estimate_pi_empirical(n)},
#                   "resampled" = {estimate_pi_resampled(n)},
#                   "MCMC"      = {MCMC_Pi(nIter = 1e5)},
#                   replications = 10)
#
# test$meanTime <- test$elapsed/test$replications
# test
#
#
# # BenchMark (Accuracy) ----------------------------------------------------
#
#
# # Accurcay Scores
# gamma <- test_accuracy(n = 1e6, samplingSize = 1e4, outputLength = 1e6, distr = "gamma", nIter = 1000, type = "resampled")
# beta <- test_accuracy(n = 1e6, samplingSize = 1e4, outputLength = 1e6, distr = "beta", nIter = 1000, type = "resampled")
# norm <- test_accuracy(n = 1e6, samplingSize = 1e4, outputLength = 1e6, distr = "beta", nIter = 1000, type = "empirical")
#
#
# gamma
# beta
# norm
#
#
# # Accuracy of Estimate (raw)
# mean_estimate(n = 1e6, nIter = 10, type = "empirical")
# mean_estimate(n = 1e6, nIter = 10, type = "resampled")
#
# # As expected the accuracies are more or less the same
#

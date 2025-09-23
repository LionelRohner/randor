# Libs --------------------------------------------------------------------

library(testthat)
source("lib/primes.R", encoding = "UTF-8")

# Unit Tests --------------------------------------------------------------

testthat::test_that("Test that sieve_ov_Erathostenes works as intended", {
  
  # Dummy data
  x1 <- 10  # Primes up to ten
  x2 <- 1e6 # Primes less than 1,000,000 = 78,498 from here: https://www.mathematical.com/primes0to1000k.html
  # Edge cases
  x3 <- 2
  x4 <- 3
  # Exceptions
  x5 <- 1
  
  # Expected results
  exp_x1 <- c(2,3,5,7)
  exp_x2 <- 78498
  exp_x3 <- x3
  exp_x4 <- x4
  
  # Tests
  expect_equal(sieve_ov_Erathostenes(to = x1), exp_x1)
  expect_equal(sieve_ov_Erathostenes(to = x2) %>% length(), exp_x2)
  expect_equal(sieve_ov_Erathostenes(to = x3), exp_x3)
  expect_equal(sieve_ov_Erathostenes(to = x4), exp_x4)
  expect_error(sieve_ov_Erathostenes(to = x5))
  expect_error(sieve_ov_Erathostenes(to = x6))
})

testthat::test_that("Test that get_cyclic_primes works as intended", {
  
  # Dummy data
  p1 <- sieve_ov_Erathostenes(to = 10)   # Primes up to ten
  p2 <- sieve_ov_Erathostenes(to = 1000) # 
  p3 <- sieve_ov_Erathostenes(to = 1e6)  # https://projecteuler.net/problem=35
  # Exceptions
  p4 <- 6
  
  # Expected results
  exp_p1 <- c(2,3,5,7)
  # https://oeis.org/A068652
  exp_p2 <- c(2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, 97, 113, 131, 197,
              199, 311, 337, 373, 719, 733, 919, 971, 991) 
  exp_p3 <- 55 # https://projecteuler.net/index.php?section=problems&id=35
  
  # Tests
  expect_equal(get_cyclic_primes(primes = p1), exp_p1)
  expect_equal(get_cyclic_primes(primes = p2), exp_p2)
  expect_equal(get_cyclic_primes(primes = p3) %>% length(), exp_p3)
  expect_equal(get_cyclic_primes(primes = p4))
})

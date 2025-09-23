
#' Sieve of Erathostenes
#'
#' @param to upper bound of primes to return
#'
#' @return Vector of primes up to to-argument
#' @export
#'
#' @examples primes_to_11 <- sieve_ov_Erathostenes(11)
sieve_ov_Erathostenes <- function(to){
  
  # Exceptions
  assertthat::assert_that(to >= 2,
                          msg = "There are no primes below 2!!!")
  
  # Edge Cases
  if (to %in% c(2,3)){
    return(to)
  } 
  
  # We only need to check up to sqrt(to) as everything bigger than that
  upper_bound = round(sqrt(to))
  
  # Generate vec of boolean of length "to"
  vec_nat = rep(TRUE, to)
  
  # set 1 to false, edge case
  vec_nat[1] <- FALSE
  
  # sieve of erathostenes
  # start with 2
  last_prime = 2
  
  for (i in last_prime:upper_bound){
    # skip indeces that are already FALSE
    if (vec_nat[i] == FALSE){
      last_prime = last_prime + 1
      next
    }
    
    # find all multiples of primes as these must be composite
    multiples <- seq(last_prime*2, to, last_prime)
    vec_nat[multiples] <- FALSE
    
    # redefine new last_prime
    last_prime = last_prime + 1
  }
  
  # return primes, i.e. those that are still TRUE
  return(which(vec_nat))
}

#' Get circular primes - E.g. 113, 131, and 311, i.e. circular permutatios that
#' are also primes
#'
#' @param primes 
#'
#' @return
#' @export
#'
#' @examples primes_to_11 <- sieve_ov_Erathostenes(11)
#' get_circular_primes(primes_to_11)
get_circular_primes <- function(primes){
  
  # Cache vector for primes - reduces the number of iteration if primes are already in
  # this vectior. E.g. a circular prime with 3 digits will appear 3 times and should only
  # be evaluated once.
  rot_vec_cache <- c()
  
  # empty vector for output$
  circular_primes <- c()
  
  cnt = 0
  
  # nenad'sche Heuristik
  primes = primes[!grepl("0|[0-9]+2|4|6|8", primes)]
  
  # go through all rotations in all primes of the slice primesRot
  for(pot_circular_primes in primes){
    
    # transform prime to character vector to evaluate rotation
    rot <- as.character(pot_circular_primes)
    rot_vec <- unlist(strsplit(rot,""))
    
    # rotation
    check = 0
    
    rotation = length(rot_vec)
    
    # evaluate if all rotations are also primes 
    for (i in 1:rotation){
      
      last_idx <- length(rot_vec)
      
      # puts the last index at the first position and pushes the OG-vec to position 2
      rot_vec <- rot_vec[c(last_idx,1:last_idx-1)]
      
      # if rot_vec not in cache check whether rotations are primes
      rot_vec_num <- as.numeric(paste(rot_vec, collapse = "",sep = ""))
      cnt = cnt + 1
      
      # check if rotation is prime
      if(rot_vec_num %in% primes){
        # if rotation is in primes, add +1 to check counter  
        check = check + 1
      } else {
        break
      }
    }
    # if the number of checks is equal to the number of rotation all rotation are primes
    if (check == rotation){
      circular_primes <- append(circular_primes, pot_circular_primes)
    }
  }
  return(circular_primes)
}

#' Title
#'
#' @param coins 
#' @param N 
#'
#' @return
#' @export
#'
#' @examples

# TODO: Does not yet work
coin_change_algo <- function(coins, N){
  
  # create vars
  ways = rep(0,N+1)
  lenCoils = length(coins)
  lenWays = N+1
  
  # Initialize Algo
  ways[1] = 1
  
  # do dynamic programming
  for (i in 1:lenCoils){
    for (j in 1:length(ways)){
      if (coins[i] < j){
        ways[j] <- ways[j] + ways[(j - coins[i])]
      }
    }
  }
  return(ways[N])
}

# TODO: shouldnt this be 3? 1,1,1; 1,1,2; 2,2
coin_change_algo(c(1,2),4)



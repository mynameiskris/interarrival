
#' @title Obtained Interarrival Time from a simulated Poisson Process
#'
#' @description This function provides functionality for obtaining interarrival time
#' from a simulated Poisson Process.
#'
#' @param seed A user determined seed to randomly generate Poisson process
#' @param t time of Poisson Process
#' @param lambda lambda of Poisson Process
#' @param p probability of the truncated Poisson
#' @return Interarrival time from the simulated Poisson Process
#' @export
#'
#' @examples main(seed=723,t=12,lambda=2,p=1)
#' main(seed=111,t=12,lambda=1,p=1)
#' main()

main <- function(seed=723, t=24, lambda = 2, p = 1/3) {
  set.seed(seed)

  N = rpois(1,lambda*t*p)

  unifs = runif(N,0,t)
  arrivals = sort(unifs)
  difference = arrivals
  for(i in 1:N){
    if(i==1){
      difference[i]=arrivals[i]
    }
    else{
      difference[i]=arrivals[i]-arrivals[i-1]
    }
  }
  return(difference)
}


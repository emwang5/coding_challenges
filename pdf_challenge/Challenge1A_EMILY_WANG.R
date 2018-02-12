##RUN THIS CODE AT http://rextester.com/WIIN44690

## isPossible - tells you whether the target value can be reached with any combination of
## the specified bucket sizes.
## INPUTS:
##    buckets: vector of bucket sizes
##    target: target value
## OUTPUT:
##    boolean value
isPossible <- function(buckets,target){
  max_coeff=target/buckets #Vector of max number of bucket size that can be used.
  max_buckets=c() #Stores expansion of the buckets by the max number of them to be used
  for (ind in 1:length(buckets)){ #Iterate over the buxket sizes
    max_buckets=c(max_buckets,rep(buckets[ind],max_coeff[ind])) #Add each bucket its max coeff # of times 
  }
  for (comb_size in 1:length(max_buckets)){ #Iterate over all combination sizes possible
    combs=combn(max_buckets,comb_size) #Create matrix of all possible combinations of specified size
    for (col_ind in 1:dim(combs)[2]){ #Iterate over each combination
      if (sum(combs[,col_ind])==target) #Computes sum of combination and sees if it equals the target value
        return(TRUE) #Combination of buckets found to form target value.
    }
  }
  return(FALSE) #No combination of the given buckets could form the target value.
}

isPossible(c(5,7),5) #TRUE
isPossible(c(5,7),33) #TRUE
isPossible(c(5,7),9) #FALSE
isPossible(c(2,11),9) #FALSE
isPossible(c(2,9,17),19) #TRUE


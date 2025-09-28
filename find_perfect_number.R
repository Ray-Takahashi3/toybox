# step 1 : decompose integer
# step 2 : sum up all divisors
# step 3 : determine the condition

prime_numbers <- c(2,3,5,7,11,13,17,19)

determin_number <- function(number){
  
  sum <- 0
  
  for(i in 1:number){
    
    if(number %% i == 0){
      sum <- sum + i    
    }
  }
  
  if(sum == number * 2){
    print(number)
  }
}






find_perfect_numbers <- function(range_max){
  
  if(is.integer(range_max) & range_max > 0){
    
    for(i in 1:range_max){
      
    }
  }

}
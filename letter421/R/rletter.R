rletter <-
function(){
  
  # Create vector of all letters
  letters <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
  
  # Randomly generate a number from 1 to 26 using the runif() function
  x <- as.integer(runif(1,1,26))
  
  # Use the randomly generated number as the index for the letters vector
  r <- letters[x]
  
  # Return the letter
  return(r)
  
}

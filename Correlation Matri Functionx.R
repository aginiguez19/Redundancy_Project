


# Correlation Matrix Function ---------------------------------------------





cor.gen = function(nv = 5, EF = 0.50, edge.prob = 0.50){
  # Define a flag variable to stop your loop
  while(flag < 1){
    # Create A Diagonal Matrix with 1's on diagonal
    # Use the rbinom() function to randomly sample whether off-diagonal elements will be 0 or EF given by edge.prob
    # Take the vector you made above and turn the 1's into correlations of value EF and 0's stay 0
    # Populate the lower.triangular and reflect that to the upper.triangular to make a symmetric correlation matrix
    # Check if the matrix you made is PSD; if so, set flag to 1 otherwise, flag stays 0 and keeps going
  }
  return(correlation.matrix)
}
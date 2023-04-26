# Define the trigonometric function
trig_func <- function(x) {
  y <- sin(x/2) + cos(x/2)
  return(y)
}

# Get user input
x <- as.numeric(readline(prompt="Enter a numeric value for x: "))

# Calculate and print the result
y <- trig_func(x)
print(y)

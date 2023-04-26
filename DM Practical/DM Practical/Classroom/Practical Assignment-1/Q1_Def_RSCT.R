# Define the rectangle function
rectangle <- function(length, breadth) {
  return(length * breadth)
}

# Define the square function
square <- function(side) {
  return(side * side)
}

# Define the circle function
circle <- function(radius) {
  return(pi * radius^2)
}

# Define the triangle function
triangle <- function(base, height) {
  return(0.5 * base * height)
}

# Display options for user
print("1. Rectangle")
print("2. Square")
print("3. Circle")
print("4. Triangle")

# Get user choice
choice <- as.numeric(readline(prompt="Enter your choice: "))

# Call the appropriate function based on user choice
if (choice == 1) {
  length <- as.numeric(readline(prompt="Enter length: "))
  breadth <- as.numeric(readline(prompt="Enter breadth: "))
  print(rectangle(length, breadth))
} else if (choice == 2) {
  side <- as.numeric(readline(prompt="Enter side: "))
  print(square(side))
} else if (choice == 3) {
  radius <- as.numeric(readline(prompt="Enter radius: "))
  print(circle(radius))
} else if (choice == 4) {
  base <- as.numeric(readline(prompt="Enter base: "))
  height <- as.numeric(readline(prompt="Enter height: "))
  print(triangle(base, height))
} else {
  print("Invalid choice")
}

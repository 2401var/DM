#Q-1
vector<-c(1,2,3,4)
new_vector<-vector+7
cat("Original Vector:",vector,"\n")
cat("New Vector:",new_vector,"\n")

#Q-2
vector<-c(60,62,65,67,69)
cat("Result",vector>65)

#Q-3
vector<-sample(seq(-50,50),10,replace=TRUE)
cat("Vector:",vector)

#Q-4
v1<-c(1,2,3,4,5)
v2<-c(5,6,7,8,9)
cat("Union:",union(v1,v2))
cat("Intersection:",intersect(v1,v2))
cat("Set Difference:",setdiff(v1,v2))

#Q-5
v<-c(0:4,13,rep(c(2,-5.1,-23),3),7/42+3+35/42)
cat("Original vector:",v)
cat("Sorted vector:",sort(v))
cat("Length of Sorted vector:",length(sort(v)))
cat("Descending Sequence:",length(sort(v)):1)
cat("Final vector:",sort(v)+length(sort(v)):1)

#Q-6
list_of_heterogeneous_data<-list(c("a","abc"),c(1,2),c(TRUE,FALSE))
cat("List of heterogeneous data:\n")
print(list_of_heterogeneous_data)

#Q-7
a<-c(1,2,3)
b<-c(4,5,6)
c<-c(7,8,9)
matrix<-cbind(a,b,c)
cat("Matrix:\n")
print(matrix)

#Q-8
Newlist <- list(a=1:10, b="Good morning", c="Hi")
Newlist$a <- Newlist$a + 1
print(Newlist$a)

#Q-9
x <- list(a=5:10, c="Hello", d="AA")
cat("Before:")
print(x)
x$z <- "NewItem"
cat("After:")
print(x)

#Q-10
y <- list("a", "b", "c")
names(y) <- c("one", "two", "three")
print(y)

#Q-11
mat1<-matrix(c(7,9,12,2,4,13),nrow=2,ncol=3,byrow=TRUE)
print(mat1)
mat2<-matrix(c(1,7,12,19,2,8,13,20,3,9,14,21),nrow=3,ncol=4,byrow=TRUE)
print(mat2)
cat("Matrix after multiplication:\n")
print(mat1 %*% mat2)

#Q-12
# Function to implement binary search
binary_search <- function(vec, key) {
  # Sort the vector
  vec <- sort(vec)
  # Get the length of the vector
  n <- length(vec)
  # Set the start and end index for the search
  start <- 1
  end <- n
  
  # Continue the search as long as start <= end
  while (start <= end) {
    # Get the middle index
    mid <- floor((start + end) / 2)
    # If the key is found at the middle, return the index
    if (vec[mid] == key) {
      return(mid)
    } 
    # If the key is greater than the middle value, search in the right half
    else if (vec[mid] < key) {
      start <- mid + 1
    } 
    # If the key is smaller than the middle value, search in the left half
    else {
      end <- mid - 1
    }
  }
  
  # Return -1 if the key is not found
  return(-1)
}

# Test the binary_search function
vec <- c(1, 12, 23, 34, 56, 99)
cat("Vectors:",vec)
key <- as.integer(readline(prompt = "Enter number:"))
result <- binary_search(vec, key)
cat("Result:", result, "\n")

#Q-13
# Create a two-dimensional 5x3 array of sequence of even integers greater than 50
array <- matrix(seq(52, 92, by = 2), nrow = 5, ncol = 3, byrow = TRUE)
cat("Two-dimensional array:\n")
print(array)

#Q-14
# Create an ordered factor from data consisting of the names of months
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
months_factor <- factor(months, levels = months, ordered = TRUE)
cat("Ordered factor:\n")
print(months_factor)

#Q-15
A=matrix(c(1,-2,3,2,1,1,-3,2,-2),nrow=3,ncol=3,byrow=TRUE)
b=c(7,4,-10)
cat("Values of x,y & z are:\n")
print(solve(A,b))

#Q-16
dim <- as.integer(readline(prompt="Enter the dimensions of the matrix: "))

mat <- matrix(nrow=dim, ncol=dim)
for (i in 1:dim) {
  for (j in 1:dim) {
    mat[i,j] <- as.numeric(readline(prompt=paste0("Enter the value of (", i, ",", j, "): ")))
  }
}

# Check if the matrix is symmetric
if (all(mat == t(mat))) {
  print("The matrix is symmetric.")
} else {
  print("The matrix is not symmetric.")
}

#Q-17
#(i) Mean and median of x:
x <- c(0,1,1,2,3,5,8,13,21,34,NA,11,NA)
mean_x <- mean(x, na.rm=TRUE)
median_x <- median(x, na.rm=TRUE)
print(paste("Mean of x:", mean_x))
print(paste("Median of x:", median_x))

#(ii) 3X3 matrix of square of 1st nine natural numbers:
matrix_sq <- matrix(data=1:9, nrow=3, ncol=3, byrow=TRUE)^2
print("Matrix of square of 1st nine natural numbers:")
print(matrix_sq)

#(iii) Transpose of a matrix:
matrix_transpose<-t(matrix_sq)
print("Transpose of the matrix:")
print(matrix_transpose)

#(iv) Multiplying two 3X3 matrices:
matrix1 <- matrix(1:9, nrow=3, ncol=3, byrow=TRUE)
matrix2 <- matrix(2:10, nrow=3, ncol=3, byrow=TRUE)
r <- matrix1 %*% matrix2
print("Resultant matrix after multiplying two matrices:")
print(r)

#(v) Concatenating two lists L1 and L2:
L1 <- list(a=1:10, b="Good morning")
L2 <- list(c=2:5, d="Hi")
L3 <- c(L1, L2)
print("Concatenated list:")
print(L3)

#(vi) Printing all even elements of a vector:
vector <- 1:10
for(i in vector){
  if(i %% 2 == 0){
    print(i)
  }
}

#(vii) Sorting a vector in ascending order:
vector <- c(3, 5, 1, 10, 8, 2)
sorted_vector <- sort(vector)
print("Sorted vector in ascending order:")
print(sorted_vector)



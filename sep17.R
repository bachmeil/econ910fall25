x <- 1:10
x
twice <- function(z) {
  return(2*z)
}
twice(1376.5)
lapply(x, twice)
lapply(x, function(z) {
  return(2*z)
})

# Same as lapply, but stores the results as a vector
sapply(x, twice)
vapply(x, twice, double(1))
vapply(x, twice, character(1))

`y[t-1]` <- 4
`y[t-2]` <- 3
0.4*`y[t-1]` + 0.3*`y[t-2]`

x <- 1:5
y <- 6:10
Map(`+`, x, y)

# Fills by column
x <- matrix(1:16, nrow=4)
x
y <- matrix(1:16, nrow=4, byrow=TRUE)
y
# Calculate the sum of rows
apply(x, MARGIN=1, sum)
apply(x, MARGIN=2, sum)
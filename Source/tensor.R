# See https://keras3.posit.co/articles/getting_started.html
# Install Python if needed
# https://www.python.org/downloads/

Measure <- read_feather('Tables/Measure.feather')
# Set dimensions for the tenro
dim1 <- max(Measure$day)
dim2 <- max(Measure$step)
dim3 <- length(unique(Measure$room))
dim4 <- length(c('C', 'V', 'loss', 'ppm', 'bar'))
array4D <- array(0,dim=c(dim1, dim2, dim3, dim4))

# map room to an index
room <- rep(0,max(Measure$room))
temp <- unique(Measure$room)
# map room to an index
for (r in 1:dim3) {
  room[temp[r]] <- r
}

# Create array 
for (m in 1:dim(Measure)[1]) {
  i <- as.integer(Measure[m,9]) # day
  j <- as.integer(Measure[m,10]) # step
  indx <- as.integer(Measure[m,4]) # room index
  k <- room[indx] # room
  C <- as.double(Measure[m,3])
  array4D[i,j,k,1] <- C
  V <- as.double(Measure[m,5])
  array4D[i,j,k,1] <- V
  loss <- as.double(Measure[m,6])
  array4D[i,j,k,1] <- loss
  ppm <- as.double(Measure[m,7])
  array4D[i,j,k,1] <- ppm
  bar <- as.double(Measure[m,8])
  array4D[i,j,k,1] <- bar
}
save(array4D, file = 'array4D')
tensor4D <- as_tensor(array4D)
save(tensor4D, file = 'tensor4D')
# convert to a tensor









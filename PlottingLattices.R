library('sna')

# for an n by m grid if we lay out the indices 1:nm column wise
# eg., 1 3
#      2 4
# read the indices (i,j) as a matrix: (1,1) is top left, (n,m) is bottom right


#the function which draws the n by m grid
drawZxZ = function(length,width, potentialScale = 1){
  c = Map(getIJ, z=1:(length*width), n=length, m=width)
  c = Map(getXY, c, n=length, m=width)
  c = matrix(unlist(c), ncol = 2, byrow=T)
  A = matrix(0, nrow=(length*width), ncol=(length*width))
  A = addAllNeighbours(A, n=length, m=width)
  sinks = rnorm(length*width)
  gplot(A, jitter=T, gmode = 'graph', coord = c, vertex.cex = sinks)
}

#index to coordinates
getIJ =function(z, n, m){
 i = (z-1)%%n
 j = (z-1-i)/n
 c(i+1,j+1)
}

# coordinates to index
getIndex = function(IJ, n, m){
 return(IJ[1] + (IJ[2]-1)*n)   
}

#input index z in 1:(n*m)
#get indices of neighbours
neighbours = function(z,n,m){
 chums = c()
 IJ = getIJ(z,n,m)
 
 if (IJ[2] > 1){
 nL = getIndex(IJ + c(0,-1),n,m)
 chums = append(chums,nL)
 }
 
 if (IJ[2] < m){ 
  nR = getIndex(IJ + c(0,1),n,m)
  chums = append(chums, nR)
 }
 
 if (IJ[1] < n){
  nU = getIndex(IJ + c(1,0),n,m)
  chums = append(chums, nU)
 }
 
 if (IJ[1] > 1){ 
  nD = getIndex(IJ + c(-1,0),n,m)
  chums  = append(chums, nD)
 }
 
 return(chums) 
}

#add neighbour to the adjacency matrix A
addNeighbours = function(A, z, n, m){
  z.neigh = neighbours(z,n,m)
  for (chum in z.neigh) {
   A[z,chum] = 1
   A[chum,z] = 1
  }
  A
}

#add all neighbours
addAllNeighbours = function(A, n, m){
 for (index in 1:nrow(A)){
   A = addNeighbours(A,index,n,m)
 }
  A
}

#turn matrix coordinates to (x,y) coordinates
getXY = function(IJ, n, m){
  y = n-IJ[1]
  x = IJ[2] -1
  c(x,y)
}

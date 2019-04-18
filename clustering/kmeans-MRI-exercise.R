# MRI Image exercise

# Read in data
healthy <- read.csv("healthy.csv", header = FALSE)
healthyMatrix <- as.matrix(healthy)

str(healthyMatrix) # check structure

# see the image
image(healthyMatrix, axes=FALSE, col = grey(seq(0,1,length=256)))


# hierarchical clustering --------------------------

healthyVector <- as.vector(healthyMatrix)

# compute distances
distance <- dist(healthyVector, method = "euclidean") # returns error - R cannot allocate enough memory

str(healthyVector) # n = 365636 -> 67 billion values required

# Since we need to calculate so many distances, we can't actually use hierarchical clustering here


# k-means clustering ------------------------------------------------------

# the first step in k-means clustering involves specifying "k"
# But how do we select k?
# Our clusters would ideally assign each point in the image to a tissue class or particular substance (e.g. gray matter, white matter, etc.)
# Setting the number of clusters depends on your objective!
k <- 5 # set k - based on domain expertise

set.seed(1)
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000) # k-means algorithm is ITERATIVE - might take long to converge: iter.max = 1000

str(KMC)

healthyClusters <- KMC$cluster # extract information from the cluster vector in the KMC object

KMC$centers[2] # extract centroid of cluster 2

# convert healthyClusters from vector to matrix so that we can compile the image
dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix)) 
image(healthyClusters, axes=FALSE, col = rainbow(k)) # rainbow takes a numerical argument representing the different colors to use - k in this case

# Can we use the clusters to identify tumors in another MRI image of a sick patient?
tumor <- read.csv("tumor.csv", header = FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

# Instead of running the k-means clustering algorithm again, we'll just apply the results we found from the healthy MRI
# i.e. healthy is the training set, tumor is the test set

# we'll need another package first
install.packages("flexclust")
library(flexclust)

# flexclust contains the KCCA (k-centroids cluster analysis) object class
# we need to convert the information from the clustering algorithm to a KCCA object before we can use the predict function on the test (tumor) set
KMC.kcca <- as.kcca(KMC, healthyVector)

# cluster using the predict function
tumorClusters <- predict(KMC.kcca, newdata = tumorVector)

# convert and create image
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col = rainbow(k))

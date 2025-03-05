#' PCA-processed MNIST data
#'
#' MNIST data set pulled from the `dslabs` package. [prcomp()] used to compute a
#' 300-dimensional PCA embedding, then 2,000 rows were randomly sampled.
"MNIST_pca"

#' t-SNE embedding of MNIST data
#'
#' A 2-dimensional t-SNE embedding of [MNIST_pca] computed using [Rtsne::Rtsne()].
"MNIST_low"

#' MNIST class labels
#'
#' A vector of MNIST class labels pulled from the `dslabs` package. The labels
#' correspond to the handwritten digits.
"MNIST_labels"

#' MNIST ids
#'
#' A vector of ids used to keep track of which MNIST images were randomly
#' sampled.
"MNIST_id"

#' Output of k-means clustering on MNIST data
#'
#' A vector of class labels computed using [kmeans()] with 10 clusters, one for
#' each digit.
"MNIST_kmeans_cluster"

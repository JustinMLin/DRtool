#' PCA-processed MNIST data
#'
#' MNIST data set pulled from the `dslabs` package. [prcomp()] was used to compute a
#' 300-dimensional PCA embedding, then 2,000 samples were randomly selected.
"MNIST_pca"

#' t-SNE embedding of MNIST data
#'
#' A 2-dimensional t-SNE embedding of [MNIST_pca] computed using [Rtsne::Rtsne()].
"MNIST_low"

#' MNIST class labels
#'
#' A vector of MNIST class labels pulled from the `dslabs` package for [MNIST_pca]. The labels
#' correspond to the handwritten digits.
"MNIST_labels"

#' MNIST ids
#'
#' A vector of ids used to keep track of which MNIST images were randomly
#' sampled.
"MNIST_id"

#' Output of k-means clustering on MNIST data
#'
#' A vector of class labels computed using [kmeans()] on [MNIST_pca] with 10 clusters, one for
#' each digit.
"MNIST_kmeans_cluster"

#' Wong et al. (2016) data
#'
#' Mass cytometry data from [Wong et al. (2016)](https://doi.org/10.1016/j.immuni.2016.07.007).
#' The data was processed according to [Becht et al. (2018)](https://rdcu.be/edjyF),
#' then 3,000 samples were randomly selected.
"Wong_high"

#' UMAP embedding of Wong et al. (2016)
#'
#' A 2-dimensional UMAP embedding of [Wong_high] computed using [umap::umap()].
"Wong_low"

#' Wong cell type labels
#'
#' A vector of cell type labels for [Wong_high].
"Wong_cell_labels"

#' Wong tissue type labels
#'
#' A vector of tissue type labels for [Wong_high].
"Wong_organ_labels"

#' Output of k-means clustering on Wong et al. (2016) data
#'
#' A vector of class labels computed using [kmeans()] with 10 clusters on [Wong_high].
"Wong_kmeans"

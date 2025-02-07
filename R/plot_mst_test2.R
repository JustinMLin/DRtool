count_crossings2 <- function(mst, cluster) {
  count <- 0
  for (i in 1:igraph::ecount(mst)) {
    head_label <- cluster[igraph::head_of(mst, i)]
    tail_label <- cluster[igraph::tail_of(mst, i)]

    if (head_label != tail_label) count <- count + 1
  }

  count
}

sim_crossings <- function(n, p, var_ratio, b=100) {
  counts = vector(length=b)
  for (i in 1:b) {

    X <- matrix(runif(n, min=-var_ratio[1]/2, max=var_ratio[1]/2), ncol=1)
    for (j in 2:p) {
      X <- cbind(X, matrix(runif(n, min=-var_ratio[j]/2, max=var_ratio[j]/2), ncol=1))
    }

    mst <- get_mst(dist(X))

    count <- 0
    for (k in 1:igraph::ecount(mst)) {
      head <- X[igraph::head_of(mst, k),]
      tail <- X[igraph::tail_of(mst, k),]

      if (sign(head[1]) != sign(tail[1])) count <- count + 1
    }

    counts[i] <- count
  }

  counts
}

plot_mst_test2 <- function(Z, path, cluster) {
  path_ids <- as.numeric(path$vpath)

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  Z1 <- Z[cluster %in% c(first_label, last_label),]

  mst <- get_mst(dist(Z1))

  num_crossings <- count_crossings2(mst, cluster[cluster %in% c(first_label, last_label)])

  var_ratio <- prcomp(Z1)$sdev^2

  print("Bootstrapping null distribution...")
  counts <- sim_crossings(dim(Z1)[1], min(dim(Z1)[1], dim(Z1)[2]), var_ratio)
  print("Done!")

  null_mean <- mean(counts)
  null_se <- sd(counts)

  p_val <- pnorm(num_crossings, null_mean, null_se)

  df <- data.frame(x = seq(from=null_mean - 4*null_se, to=null_mean + 4*null_se, length.out = 100)) %>%
    dplyr::mutate(y = dnorm(x, mean=null_mean, sd=null_se))

  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_area(fill = "sky blue") +
    gghighlight::gghighlight(x < num_crossings) +
    ggplot2::labs(x="", y="") +
    ggplot2::geom_text(x=-Inf, y=Inf,
                       hjust=0, vjust=2,
                       size=5,
                       check_overlap=TRUE,
                       label=paste0("Expected Number of Crossings = ", round(null_mean,3))) +
    ggplot2::geom_text(x=-Inf, y=Inf,
                       hjust=0, vjust=4,
                       size=5,
                       check_overlap=TRUE,
                       label=paste0("Standard Error = ", round(null_se,3))) +
    ggplot2::geom_text(x=-Inf, y=Inf,
                       hjust=0, vjust=6,
                       size=5,
                       check_overlap=TRUE,
                       label=paste0("Number of Crossings = ", num_crossings)) +
    ggplot2::geom_text(x=-Inf, y=Inf,
                       hjust=0, vjust=8,
                       size=5,
                       check_overlap=TRUE,
                       label=paste0("p = ", round(p_val, 3)))
}

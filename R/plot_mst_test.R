count_crossings <- function(tree, path, cluster) {
  path_ids <- as.numeric(path$vpath)

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  if (first_label == last_label) return(NA)

  count <- 0
  for (i in 1:igraph::ecount(tree)) {
    head_label <- cluster[igraph::head_of(tree, i)]
    tail_label <- cluster[igraph::tail_of(tree, i)]

    if (all(sort(c(first_label, last_label)) == sort(c(head_label, tail_label)))) count <- count + 1
  }

  count
}

calc_test_vals = function(tree, path, cluster, p) {
  path_ids <- as.numeric(path$vpath)

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  n <- sum(cluster %in% c(first_label, last_label))
  t <- n^(1/p)
  t0 <- 400^(1/p)

  E_t0 <- if (p <= 50) {
    E_t0_small_p[p-1, 'E_t0']
  } else {
    m <- E_t0_param[1,2]
    k <- E_t0_param[2,2]
    C <- E_t0_param[3,2]
    -m * p ** -k + C
  }

  E_t = (t/t0)^(p-1) * E_t0

  if (p <= 20) {
    m <- se_param[p-1,2]
    k <- se_param[p-1,3]
    C <- se_param[p-1,4]
  } else {
    m <- se_param[20,2]
    k <- se_param[20,3]
    C <- se_param[20,4]
  }

  se <- m * n ** k + C

  num_crossings <- count_crossings(tree, path, cluster)

  list(E_t=E_t, se=se, num_crossings=num_crossings)
}

plot_mst_test <- function(tree, path, cluster, p) {
  vals <- calc_test_vals(tree, path, cluster, p)

  df <- data.frame(x = seq(from=vals$E_t - 4*vals$se, to=vals$E_t + 4*vals$se, length.out = 100)) %>%
    dplyr::mutate(y = dnorm(x, mean=vals$E_t, sd=vals$se))

  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_area(fill = "sky blue") +
    gghighlight::gghighlight(x < vals$num_crossings)
}

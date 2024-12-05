#' @rdname meta_data_plot
meta_data_plot_brush <- function(Z, g1, g2, meta_data, feature) {
  both_groups <- intersect(g1, g2)
  just_g1 <- g1[!(g1 %in% g2)]
  just_g2 <- g2[!(g2 %in% g1)]

  df <- data.frame(data = meta_data[[feature]][c(just_g1, both_groups, just_g2)],
                   group = factor(rep(c("Group 1", "Both Groups", "Group 2"), times=c(length(just_g1),
                                                                               length(both_groups),
                                                                               length(just_g2))),
                                  levels=c("Group 1", "Both Groups", "Group 2")))


  if (is.numeric(meta_data[[feature]])) {
    ggplot2::ggplot(df, ggplot2::aes(x=group, y=data, color=group)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(color = feature)
  }
  else {
    p1 <- ggplot2::ggplot(df[df$group == "Group 1",], ggplot2::aes(x=factor(1), fill=data)) +
      ggplot2::geom_bar(stat="count", width=1, color="white") +
      ggplot2::coord_polar(theta="y", start=0) +
      ggplot2::theme_void() +
      ggplot2::labs(title="Group 1", fill=feature)

    p2 <- ggplot2::ggplot(df[df$group == "Both Groups",], ggplot2::aes(x=factor(1), fill=data)) +
      ggplot2::geom_bar(stat="count", width=1, color="white") +
      ggplot2::coord_polar(theta="y", start=0) +
      ggplot2::theme_void() +
      ggplot2::labs(title="Both Groups", fill=feature)

    p3 <- ggplot2::ggplot(df[df$group == "Group 2",], ggplot2::aes(x=factor(1), fill=data)) +
      ggplot2::geom_bar(stat="count", width=1, color="white") +
      ggplot2::coord_polar(theta="y", start=0) +
      ggplot2::theme_void() +
      ggplot2::labs(title="Group 2", fill=feature)

    gridExtra::grid.arrange(p1, p2, p3, nrow=1)
  }
}

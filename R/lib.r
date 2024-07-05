#!/usr/bin/env Rscript

if (!require(metafor)) {
  install.packages("metafor")
}
library(metafor)

#' @export
basic_forest_plot <- function(
    x, se, width, height, name,
    header, slab, xlab, digits = 2,
    shade = TRUE, cex = 1,
    ...) {
  if (missing(x)) {
    stop("x is missing")
  }
  if (missing(se)) {
    stop("se is missing")
  }
  if (missing(width)) {
    stop("width is missing")
  }
  if (missing(height)) {
    stop("height is missing")
  }
  if (missing(name)) {
    stop("name is missing")
  }
  if (missing(header)) {
    stop("header is missing")
  }
  if (length(header) != 2) {
    stop("header must have 2 elements")
  }
  if (missing(slab)) {
    stop("slab is missing")
  }
  if (missing(xlab)) {
    stop("xlab is missing")
  }

  items <- length(x)
  if (length(se) != items) {
    stop("length of se must match length of x")
  }
  if (length(slab) != items) {
    stop("length of slab must match length of x")
  }

  png(paste0(name, ".png"), width = width, height = height)
  forest.default(
    x = x,
    sei = se,
    annosym = c(" [", ", ", "]", "\u2212"),
    psize = 1,
    digits = digits,
    layout = "JAMA",
    order = order(x, decreasing = TRUE),
    header = header,
    slab = slab,
    xlab = xlab,
    shade = shade,
    cex = cex,
    ...
  )
  dev.off()
}

#' @export
grouped_forest_plot <- function(
    x, se, width, height, name,
    header, slab, xlab, glab,
    digits = 2, cex = 1, col,
    order_by,
    ...) {
  if (missing(x)) {
    stop("x is missing")
  }
  if (missing(se)) {
    stop("se is missing")
  }
  if (missing(width)) {
    stop("width is missing")
  }
  if (missing(height)) {
    stop("height is missing")
  }
  if (missing(name)) {
    stop("name is missing")
  }
  if (missing(header)) {
    stop("header is missing")
  }
  if (length(header) != 2) {
    stop("header must have 2 elements")
  }
  if (missing(slab)) {
    stop("slab is missing")
  }
  if (missing(xlab)) {
    stop("xlab is missing")
  }
  if (missing(glab)) {
    stop("glab is missing")
  }

  group_size <- length(x)
  if (group_size <= 0) {
    stop("x must have at least one element")
  }
  if (missing(col)) {
    col <- rep("black", times = group_size)
  }
  if (length(se) != group_size) {
    stop("length of se must match length of x")
  }
  if (length(col) != group_size) {
    stop("length of col must match length of x")
  }
  if (length(glab) != group_size) {
    stop("length of glab must match length of x")
  }
  items <- length(x[[1]])
  if (length(slab) != items) {
    stop("length of slab must match length of x")
  }
  for (i in 1:group_size) {
    if (length(x[[i]]) != items) {
      stop("length of x must match for all groups")
    }
    if (length(se[[i]]) != items) {
      stop("length of se must match for all groups")
    }
  }

  rows_per_group <- group_size + 2
  rows <- c()
  for (i in 1:items) {
    rows <- c(
      rows,
      (1 + rows_per_group * (i - 1))
      :(rows_per_group * i - (rows_per_group - 3))
    )
  }
  gcol <- col
  col <- rep(col, times = items)
  if (missing(order_by)) {
    order_by <- group_size
  }
  orig_order <- order(x[[order_by]], decreasing = FALSE)
  raw_order <- (orig_order - 1) * group_size + 1
  order <- c()
  for (i in 1:items) {
    o <- raw_order[i]
    for (j in 1:group_size) {
      order <- c(order, o + (j - 1))
    }
  }
  xs <- c()
  ses <- c()
  for (i in 1:items) {
    for (j in 1:group_size) {
      xs <- c(xs, x[[j]][i])
      ses <- c(ses, se[[j]][i])
    }
  }

  png(paste0(name, ".png"), width = width, height = height)
  plot <- forest.default(
    x = xs[order],
    sei = ses[order],
    slab = NA,
    cex = cex,
    annosym = c(" [", ", ", "]", "\u2212"),
    psize = 1,
    digits = digits,
    rows = rows,
    col = col,
    layout = "JAMA",
    header = header,
    ylim = c(1, items * rows_per_group + 3),
    xlab = xlab,
    ...
  )

  slab <- slab[orig_order]
  for (i in 1:items) {
    text(
      plot$textpos[1],
      rows_per_group * (i - 1) + 4,
      slab[i],
      font = 2,
      pos = 4,
      cex = cex
    )
    for (j in 1:group_size) {
      text(
        plot$textpos[1],
        rows_per_group * (i - 1) + j,
        glab[j],
        pos = 4,
        cex = cex,
        col = gcol[j]
      )
    }
  }

  dev.off()
}

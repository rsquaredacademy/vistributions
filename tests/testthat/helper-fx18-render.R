# Calls are shared with the isolated baseline comparison in docs-dev.
fx18_calls <- function() {
  out <- list()
  add <- function(fun, ...) {
    args <- c(list(...), list(print_plot = FALSE))
    id <- paste(fun, paste(vapply(args, paste, "", collapse = ","), collapse = " "))
    out[[make.unique(c(names(out), id))[length(out) + 1L]]] <<-
      list(fun = fun, args = args)
  }
  for (family in c("chisquare", "f", "t", "normal", "binom")) {
    for (kind in c("plot", "perc", "prob")) add(paste0("vdist_", family, "_", kind))
  }
  probs <- c(.01, .05, .5, .99)
  for (df in c(.5, 1, 2, 5, 30, 200, 1000)) {
    add("vdist_chisquare_plot", df)
    for (pr in probs) for (type in c("lower", "upper")) {
      add("vdist_chisquare_perc", pr, df, type)
      add("vdist_chisquare_prob", stats::qchisq(pr, df), df, type)
    }
  }
  for (fd in list(c(1, 5), c(5, 10), c(500, 500))) {
    add("vdist_f_plot", fd[1], fd[2])
    for (pr in probs) for (type in c("lower", "upper")) {
      add("vdist_f_perc", pr, fd[1], fd[2], type)
      add("vdist_f_prob", stats::qf(pr, fd[1], fd[2]), fd[1], fd[2], type)
    }
  }
  for (df in c(1, 5, 30)) {
    add("vdist_t_plot", df)
    for (pr in probs) {
      for (type in c("lower", "upper", "both")) add("vdist_t_perc", pr, df, type)
      for (type in c("lower", "upper")) add("vdist_t_prob", stats::qt(pr, df), df, type)
    }
    for (x in c(0, .5, 1.96, 3)) for (type in c("both", "interval")) {
      add("vdist_t_prob", x, df, type)
    }
  }
  for (pr in probs) {
    for (type in c("lower", "upper", "both")) add("vdist_normal_perc", pr, 0, 1, type)
    for (type in c("lower", "upper")) add("vdist_normal_prob", stats::qnorm(pr), 0, 1, type)
  }
  for (x in list(c(-.35, .35), c(2, 3.5), c(-1.96, 1.96))) {
    add("vdist_normal_prob", x, 0, 1, "both")
  }
  for (n in c(10, 100, 1000)) {
    add("vdist_binom_plot", n, .3)
    for (type in c("lower", "upper", "exact", "interval")) {
      x <- if (type == "interval") c(floor(.2 * n), floor(.4 * n)) else floor(.3 * n)
      add("vdist_binom_prob", n, .3, x, type)
    }
    for (type in c("lower", "upper")) add("vdist_binom_perc", n, .3, .05, type)
  }
  # Exact named audit cases, including all three round-18 render reprexes.
  add("vdist_chisquare_prob", 13, 200, "lower")
  add("vdist_chisquare_prob", 1, 200, "lower")
  add("vdist_chisquare_prob", .4549, 1, "lower")
  add("vdist_f_prob", 2.35, 500, 500, "upper")
  add("vdist_t_perc", .01, 30, "both")
  add("vdist_t_perc", .05, 5, "both")
  add("vdist_t_perc", .975, 1, "lower")
  add("vdist_t_perc", .97, 1, "lower")
  add("vdist_t_perc", .99, 2, "lower")
  add("vdist_f_perc", .95, 2, 2, "upper")
  add("vdist_f_perc", .9, 200, 2, "lower")
  add("vdist_f_perc", .5, 1, 1, "upper")
  add("vdist_f_perc", .25, 1, 2, "lower")
  add("vdist_f_prob", 4052, 1, 1, "lower")
  for (df in c(1.5, 2, 5, 30)) add("vdist_t_prob", 10, df, "upper")
  out
}

# Read actual text glyph extents in the forced device display list. deviceLoc
# includes the parent viewport offsets; local coordinates alone miss clipping.
fx18_text_boxes <- function(p) {
  print(p)
  grid::grid.force()
  listing <- grid::grid.ls(print = FALSE, viewports = TRUE, grobs = TRUE)
  at <- which(listing$type == "grobListing" &
    grepl("axis-[blrt]|caption", listing$gPath) & grepl("^GRID.text", listing$name))
  boxes <- lapply(at, function(i) {
    g <- grid::grid.get(paste(listing$gPath[i], listing$name[i], sep = "::"))
    vp <- tail(strsplit(listing$vpPath[i], "::", fixed = TRUE)[[1]], 1)
    grid::seekViewport(vp)
    on.exit(grid::upViewport(0), add = TRUE)
    loc <- grid::deviceLoc(g$x, g$y, valueOnly = TRUE)
    do.call(rbind, lapply(seq_along(g$label), function(j) {
      glyph <- grid::textGrob(g$label[j], gp = g$gp, rot = g$rot)
      w <- grid::convertWidth(grid::grobWidth(glyph), "inches", TRUE)
      h <- grid::convertHeight(grid::grobHeight(glyph), "inches", TRUE)
      x <- rep_len(loc$x, length(g$label))[j]
      y <- rep_len(loc$y, length(g$label))[j]
      hj <- rep_len(g$hjust, length(g$label))[j]
      vj <- rep_len(g$vjust, length(g$label))[j]
      data.frame(kind = if (grepl("caption", listing$gPath[i])) "caption" else "tick",
        axis = sub(".*(axis-[blrt]).*", "\\1", listing$gPath[i]),
        label = as.character(g$label[j]), left = x - w * hj,
        right = x + w * (1 - hj), bottom = y - h * vj, top = y + h * (1 - vj))
    }))
  })
  do.call(rbind, boxes)
}

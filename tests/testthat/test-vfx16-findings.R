# Round 16: rendered label placement, percentage formatting and caption fixes.
#
# Every assertion below reads the glyph box that grid actually draws inside the
# panel, on a pdf device at the target size. Placement therefore cannot pass by
# testing anchors: overlaps, clipping and wrong-side labels are all measured.

vfx16_label_grobs <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  pidx <- which(gt$layout$name == "panel")
  grid::grid.newpage()
  grid::grid.draw(gt)
  l <- gt$layout[pidx, ]
  vpname <- paste0("panel.", l$t, "-", l$l, "-", l$b, "-", l$r)
  grid::seekViewport(vpname)
  grid::downViewport(gt$grobs[[pidx]]$vp$name)
  on.exit(grid::upViewport(0), add = TRUE)
  find <- function(g) {
    if (inherits(g, "vdist_labels")) {
      return(g)
    }
    if (!is.null(g$children)) {
      for (child in g$children) {
        found <- find(child)
        if (!is.null(found)) {
          return(found)
        }
      }
    }
    NULL
  }
  g <- find(gt$grobs[[pidx]])
  if (is.null(g)) {
    return(NULL)
  }
  g <- grid::makeContent(g)
  # The gTree holds a halo patch before each text grob; both are measured in the
  # panel viewport, so the boxes below are the boxes the device draws.
  do.call(rbind, lapply(g$children, function(ch) {
    is_halo <- is.null(ch$label)
    w <- grid::convertWidth(grid::grobWidth(ch), "npc", valueOnly = TRUE)
    h <- grid::convertHeight(grid::grobHeight(ch), "npc", valueOnly = TRUE)
    x <- grid::convertX(ch$x, "npc", valueOnly = TRUE)
    y <- grid::convertY(ch$y, "npc", valueOnly = TRUE)
    hj <- if (is_halo) 0.5 else ch$hjust
    vj <- if (is_halo) 0.5 else ch$vjust
    data.frame(halo = is_halo,
               has_halo = isTRUE(ch$vdist_halo),
               label = if (is_halo) NA_character_ else as.character(ch$label),
               left = x - w * hj, right = x + w * (1 - hj),
               bottom = y - h * vj, top = y + h * (1 - vj),
               side = if (is_halo) NA_integer_ else ch$vdist_side,
               cutoff = if (is_halo) NA_real_ else ch$vdist_cutoff,
               stacked = if (is_halo) NA else ch$vdist_stacked,
               row = if (is_halo) NA_integer_ else ch$vdist_row,
               fill = if (is_halo) as.character(ch$gp$fill) else NA_character_,
               alpha = if (is_halo) {
                 if (is.null(ch$gp$alpha)) 1 else ch$gp$alpha
               } else NA_real_,
               stringsAsFactors = FALSE)
  }))
}

vfx16_extents <- function(p) {
  d <- vfx16_label_grobs(p)
  if (is.null(d)) {
    return(NULL)
  }
  d <- d[!d$halo, c("label", "left", "right", "bottom", "top", "side",
                    "cutoff", "stacked", "row")]
  rownames(d) <- NULL
  d
}

vfx16_grid <- function() {
  grid <- list()
  add <- function(id, p) {
    grid[[length(grid) + 1L]] <<- list(id = id, p = p)
  }
  probs <- c(.01, .05, .25, .5, .75, .95, .99)

  for (df in c(1, 2, 4, 5, 30)) {
    for (pr in probs) for (type in c("lower", "upper", "both")) {
      add(sprintf("t_perc %.2f df %g %s", pr, df, type),
          vdist_t_perc(pr, df, type, FALSE))
    }
    for (pr in c(.01, .5, .99)) for (type in c("lower", "upper")) {
      add(sprintf("t_prob q%.2f df %g %s", pr, df, type),
          vdist_t_prob(stats::qt(pr, df), df, type, FALSE))
    }
    for (p in c(0, .5, 1.96, 3)) {
      for (type in c("interval", "both")) {
        add(sprintf("t_prob %.2f df %g %s", p, df, type),
            vdist_t_prob(p, df, type, FALSE))
      }
    }
  }

  for (pr in probs) for (type in c("lower", "upper", "both")) {
    add(sprintf("normal_perc %.2f %s", pr, type),
        vdist_normal_perc(pr, 0, 1, type, FALSE))
  }
  for (pr in c(.05, .5, .95)) for (type in c("lower", "upper")) {
    add(sprintf("normal_prob q%.2f %s", pr, type),
        vdist_normal_prob(stats::qnorm(pr), 0, 1, type, FALSE))
  }
  for (pair in list(c(-1, 1), c(-1.96, 1.96), c(0, 1.96))) {
    add(paste("normal_prob", paste(pair, collapse = ","), "both"),
        vdist_normal_prob(pair, 0, 1, "both", FALSE))
  }

  f_pairs <- list(c(1, 1), c(1, 2), c(2, 2), c(4, 5), c(200, 2), c(500, 500))
  for (fd in f_pairs) {
    for (pr in probs) for (type in c("lower", "upper")) {
      add(sprintf("f_perc %.2f %g %g %s", pr, fd[1], fd[2], type),
          vdist_f_perc(pr, fd[1], fd[2], type, FALSE))
    }
    for (pr in c(.05, .5, .95)) for (type in c("lower", "upper")) {
      add(sprintf("f_prob q%.2f %g %g %s", pr, fd[1], fd[2], type),
          vdist_f_prob(stats::qf(pr, fd[1], fd[2]), fd[1], fd[2], type, FALSE))
    }
  }

  for (df in c(1, 5, 30, 200)) {
    for (pr in probs) {
      add(sprintf("chi_perc %.2f df %g lower", pr, df),
          vdist_chisquare_perc(pr, df, "lower", FALSE))
      add(sprintf("chi_perc %.2f df %g %s", pr, df, type = "upper"),
          vdist_chisquare_perc(pr, df, "upper", FALSE))
    }
    for (pr in c(.05, .5, .95)) for (type in c("lower", "upper")) {
      add(sprintf("chi_prob q%.2f df %g %s", pr, df, type),
          vdist_chisquare_prob(stats::qchisq(pr, df), df, type, FALSE))
    }
  }

  # Every call in the round-15 M1/M2 tables and the named Carbon regressions.
  audit <- list(
    list("t_perc default", vdist_t_perc(print_plot = FALSE)),
    list("t_perc .95 30", vdist_t_perc(.95, 30, print_plot = FALSE)),
    list("t_perc .99 5 upper", vdist_t_perc(.99, 5, "upper", FALSE)),
    list("t_perc .99 2", vdist_t_perc(.99, 2, print_plot = FALSE)),
    list("f_perc default", vdist_f_perc(print_plot = FALSE)),
    list("f_perc .95 2 2 upper", vdist_f_perc(.95, 2, 2, "upper", FALSE)),
    list("f_perc .9 200 2", vdist_f_perc(.9, 200, 2, print_plot = FALSE)),
    list("f_perc .5 1 1 upper", vdist_f_perc(.5, 1, 1, "upper", FALSE)),
    list("f_perc .25 1 2 lower", vdist_f_perc(.25, 1, 2, "lower", FALSE)),
    list("f_perc .9 2 2 lower", vdist_f_perc(.9, 2, 2, "lower", FALSE)),
    list("chi_perc .95 1 upper", vdist_chisquare_perc(.95, 1, "upper", FALSE)),
    list("chi_prob .4549 1", vdist_chisquare_prob(.4549, 1, print_plot = FALSE)),
    list("f_prob qf(.05,2,2)", vdist_f_prob(stats::qf(.05, 2, 2), 2, 2, "upper", FALSE)),
    list("t_perc .975 1", vdist_t_perc(.975, 1, print_plot = FALSE)),
    list("t_perc .99 2 lower", vdist_t_perc(.99, 2, "lower", FALSE)),
    list("t_perc .99 1", vdist_t_perc(.99, 1, print_plot = FALSE)),
    list("t_prob 10 1.5 upper", vdist_t_prob(10, 1.5, "upper", FALSE)),
    list("t_prob 10 2 upper", vdist_t_prob(10, 2, "upper", FALSE)),
    list("t_prob 10 5 upper", vdist_t_prob(10, 5, "upper", FALSE)),
    list("t_prob 10 30 upper", vdist_t_prob(10, 30, "upper", FALSE)),
    list("t_prob 10 1.5 lower", vdist_t_prob(10, 1.5, "lower", FALSE)),
    list("f_prob 4052 1 1", vdist_f_prob(4052, 1, 1, print_plot = FALSE)),
    list("f_prob qf(.1,1,1)", vdist_f_prob(stats::qf(.1, 1, 1), 1, 1, "upper", FALSE)),
    list("chi_prob 13 200", vdist_chisquare_prob(13, 200, "lower", FALSE)),
    list("chi_prob 1 500 lower", vdist_chisquare_prob(1, 500, "lower", FALSE)),
    list("t_prob 0 5 interval", vdist_t_prob(0, 5, "interval", FALSE)),
    list("t_prob 0 5 both", vdist_t_prob(0, 5, "both", FALSE)),
    list("t_perc .95 1 lower", vdist_t_perc(.95, 1, "lower", FALSE)),
    list("t_perc .97 1 lower", vdist_t_perc(.97, 1, "lower", FALSE)),
    list("chi_prob 1 1 lower", vdist_chisquare_prob(1, 1, "lower", FALSE)),
    list("f_prob 2.35 500 500", vdist_f_prob(2.35, 500, 500, "upper", FALSE)),
    list("t_perc .05 5 both", vdist_t_perc(.05, 5, "both", FALSE))
  )
  audit <- lapply(audit, function(a) list(id = a[[1]], p = a[[2]]))
  c(grid, audit)
}

test_that("vfx16: cutoff-anchored labels stay separated, inside and on side", {
  # 1,320 rendered plots; run locally and in devtools::test(), not in the
  # CRAN check's own test pass.
  skip_on_cran()
  cases <- vfx16_grid()
  sizes <- list(c(4, 3), c(7, 5), c(10, 6))
  for (sz in sizes) {
    grDevices::pdf(NULL, width = sz[1], height = sz[2])
    for (case in cases) {
      info <- paste(case$id, paste(sz, collapse = "x"))
      e <- tryCatch(vfx16_extents(case$p), error = function(err) err)
      expect_false(inherits(e, "error"), info = info)
      if (inherits(e, "error") || is.null(e) || !nrow(e)) {
        next
      }
      tol <- 1e-7
      expect_true(all(e$left >= -tol & e$right <= 1 + tol), info = info)
      expect_true(all(e$bottom >= -tol & e$top <= 1 + tol), info = info)
      if (nrow(e) > 1) {
        for (i in seq_len(nrow(e) - 1L)) {
          for (j in (i + 1L):nrow(e)) {
            separated <- e$right[i] <= e$left[j] + tol ||
              e$right[j] <= e$left[i] + tol ||
              e$top[i] <= e$bottom[j] + tol ||
              e$top[j] <= e$bottom[i] + tol
            expect_true(separated,
                        info = paste(info, "overlap", e$label[i], e$label[j]))
          }
        }
      }
      # A label never crosses its own cutoff line. Centre labels have no
      # cutoff of their own: `cutoff` for them is the interval midpoint.
      edges <- e$side != 0
      if (any(edges)) {
        expect_true(all(e$right[edges] <= e$cutoff[edges] + tol |
                        e$left[edges] >= e$cutoff[edges] - tol),
                    info = paste(info, "crosses cutoff"))
      }
      # Labels that fit on their own side are asserted to be there. A label
      # deliberately stacked at a panel edge is on the other side, but on a
      # second line; this is the documented fallback.
      left_ok <- e$stacked | e$side >= 0 | e$right <= e$cutoff + tol
      right_ok <- e$stacked | e$side <= 0 | e$left >= e$cutoff - tol
      expect_true(all(left_ok & right_ok), info = paste(info, "wrong side"))
      centre <- e$side == 0
      if (any(centre)) {
        expect_true(all(abs((e$left[centre] + e$right[centre]) / 2 -
                            e$cutoff[centre]) <= 1e-6), info = paste(info, "centre"))
      }
    }
    grDevices::dev.off()
  }
})

test_that("vfx16: percentages near 0.1% agree across forms", {
  pairs <- list(
    c(vdist_t_perc(.001, 5, print_plot = FALSE)$labels$subtitle,
      vdist_t_prob(-5.893, 5, print_plot = FALSE)$labels$subtitle),
    c(vdist_chisquare_perc(.001, 5, print_plot = FALSE)$labels$subtitle,
      vdist_chisquare_prob(0.2102, 5, print_plot = FALSE)$labels$subtitle)
  )
  for (pair in pairs) {
    expect_equal(pair[1], pair[2])
  }
  expect_false(grepl("1e-01%", pairs[[1]][1], fixed = TRUE))
  expect_false(grepl("5e-02%", pairs[[1]][1], fixed = TRUE))
  expect_equal(vistributions:::vdist_fmt_pct(0.0005)$text, "5.00e-02")
  expect_equal(vistributions:::vdist_fmt_pct(0.000123)$text, "1.23e-02")
  expect_equal(vistributions:::vdist_fmt_pct(0.00005)$text, "5.00e-03")
})

test_that("vfx16: the sub-pixel caption does not repeat the same sentence", {
  cap <- vdist_chisquare_prob(1, 200, print_plot = FALSE)$labels$caption
  expect_match(cap, "narrower than one pixel", fixed = TRUE)
  expect_match(cap, "marked by the blue strip", fixed = TRUE)
  expect_false(grepl("too small to display", cap, fixed = TRUE))
  expect_equal(lengths(regmatches(cap, gregexpr("shaded region", cap))), 1L)
  # A region that is short but not marked keeps the generic disclosure.
  expect_match(vdist_chisquare_prob(13, 200, print_plot = FALSE)$labels$caption,
               "too small to display", fixed = TRUE)
})

test_that("vfx16: chi-square df has a documented upper resolution bound", {
  expect_error(vdist_chisquare_plot(1e34, print_plot = FALSE),
               "df must not exceed")
  expect_error(vdist_chisquare_perc(.95, 1e30, print_plot = FALSE),
               "df must not exceed")
  expect_error(vdist_chisquare_prob(1, 1e30, print_plot = FALSE),
               "df must not exceed")
  expect_match(vistributions:::vdist_contract("chisquare")$parameter, "2e20")
})

test_that("vfx16: large interval and cutoff text use scientific notation", {
  skip_on_cran()
  s <- vdist_t_perc(.998, .05, "both", FALSE)$labels$subtitle
  expect_match(s, "e\\+")
  expect_false(grepl("[0-9]{20,}", s))
  s <- vdist_t_prob(stats::qt(.001, .05, lower.tail = FALSE), .05, "both", FALSE)$labels$subtitle
  expect_match(s, "e\\+")
  expect_false(grepl("[0-9]{20,}", s))
})

# ---------------------------------------------------------------------------
# Round 17: label occlusion. The label layer must be drawn last, and each label
# must carry an opaque patch in the panel background colour, so the density
# polygon (which reaches the top of the panel for chi-square df < 2 and F
# num_df < 2) and the cutoff lines cannot erase or cut through the text.
#
# The check below is geometric: text boxes, halo boxes and the built polygon
# vertices are all read from the drawn grobs in the panel viewport.
# ---------------------------------------------------------------------------

# Panel-coordinate vertices of the filled event region (the darker fill), read
# from the built polygon layers. coord$transform() is the package's own coord,
# so a wide window on a transformed x scale is converted correctly.
vfx17_dark_points <- function(p) {
  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]
  out <- list()
  for (i in seq_along(p$layers)) {
    l <- p$layers[[i]]
    if (!inherits(l$geom, "GeomPolygon")) {
      next
    }
    fill <- l$aes_params$fill
    if (is.null(fill) || !identical(toupper(as.character(fill)), "#0000CD")) {
      next
    }
    d <- b$data[[i]]
    if (is.null(d) || !all(c("x", "y") %in% names(d))) {
      next
    }
    tr <- b$layout$coord$transform(d[, c("x", "y")], pp)
    tr <- tr[is.finite(tr$x) & is.finite(tr$y), , drop = FALSE]
    if (!nrow(tr)) {
      next
    }
    # Vertices alone can miss a wide, flat region; the edge midpoints fill the
    # gaps between consecutive polygon points.
    n <- nrow(tr)
    mid <- data.frame(x = (tr$x[-n] + tr$x[-1]) / 2,
                      y = (tr$y[-n] + tr$y[-1]) / 2)
    out[[length(out) + 1L]] <- rbind(tr, mid)
  }
  if (!length(out)) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  do.call(rbind, out)
}

vfx17_grid <- function() {
  out <- list()
  add <- function(id, p) out[[length(out) + 1L]] <<- list(id = id, p = p)

  # Singular-at-zero families: the density spike reaches the top of the panel
  # exactly where the labels are anchored.
  for (df in c(0.5, 1, 1.5)) {
    for (pr in c(.05, .5, .95)) for (type in c("lower", "upper")) {
      add(sprintf("chi_perc %.2f df %g %s", pr, df, type),
          vdist_chisquare_perc(pr, df, type, FALSE))
    }
    for (pr in c(.05, .95)) {
      add(sprintf("chi_prob q%.2f df %g", pr, df),
          vdist_chisquare_prob(stats::qchisq(pr, df), df, "upper", FALSE))
    }
  }
  for (nd in c(1, 1.5)) for (dd in c(2, 5, 10)) {
    for (pr in c(.05, .9, .95, .99)) for (type in c("lower", "upper")) {
      add(sprintf("f_perc %.2f %g %g %s", pr, nd, dd, type),
          vdist_f_perc(pr, nd, dd, type, FALSE))
    }
    for (pr in c(.1, .01)) {
      add(sprintf("f_prob q%.2f %g %g", pr, nd, dd),
          vdist_f_prob(stats::qf(pr, nd, dd), nd, dd, "upper", FALSE))
    }
  }

  # The named round-16 reprexes and the three-label forms, where the centre
  # label sits on the shaded strip between two cutoff lines.
  named <- list(
    list("f_perc .95 1 5 upper", vdist_f_perc(0.95, 1, 5, "upper", FALSE)),
    list("f_perc .90 1 5 upper", vdist_f_perc(0.90, 1, 5, "upper", FALSE)),
    list("f_perc .99 1 5 upper", vdist_f_perc(0.99, 1, 5, "upper", FALSE)),
    list("f_perc .95 1 2 upper", vdist_f_perc(0.95, 1, 2, "upper", FALSE)),
    list("chi_perc .95 1 upper", vdist_chisquare_perc(0.95, 1, "upper", FALSE)),
    list("f_prob .1 1 5 upper", vdist_f_prob(0.1, 1, 5, "upper", FALSE)),
    list("f_prob .01 1 5 upper", vdist_f_prob(0.01, 1, 5, "upper", FALSE)),
    list("t_perc .05 5 both", vdist_t_perc(.05, 5, "both", FALSE)),
    list("t_perc .01 30 both", vdist_t_perc(.01, 30, "both", FALSE)),
    list("normal_perc .1 both", vdist_normal_perc(.1, 0, 1, "both", FALSE)),
    list("t_prob 0 5 interval", vdist_t_prob(0, 5, "interval", FALSE)),
    list("t_perc default", vdist_t_perc(print_plot = FALSE)),
    list("f_perc default", vdist_f_perc(print_plot = FALSE)),
    list("chi_perc default", vdist_chisquare_perc(print_plot = FALSE))
  )
  c(out, lapply(named, function(a) list(id = a[[1]], p = a[[2]])))
}

test_that("vfx17: labels are drawn last, over an opaque halo", {
  skip_on_cran()
  cases <- vfx17_grid()
  sizes <- list(c(4, 3), c(7, 5), c(10, 6))
  dark_hits <- 0L
  for (sz in sizes) {
    grDevices::pdf(NULL, width = sz[1], height = sz[2])
    for (case in cases) {
      info <- paste(case$id, paste(sz, collapse = "x"))
      p <- case$p

      # The label layer is the last layer: polygons, lines and markers below it.
      labels <- vapply(p$layers,
        function(l) inherits(l$geom, "GeomVdistLabels"), logical(1))
      expect_true(any(labels), info = info)
      expect_true(labels[length(labels)], info = info)

      d <- tryCatch(vfx16_label_grobs(p), error = function(err) err)
      expect_false(inherits(d, "error"), info = info)
      if (inherits(d, "error") || is.null(d) || !nrow(d)) {
        next
      }
      txt <- d[!d$halo, , drop = FALSE]
      n_txt <- nrow(txt)
      expect_gt(n_txt, 0L)
      # Children alternate halo, text: each label owns the patch just below it.
      expect_equal(nrow(d), 2L * n_txt, info = info)
      expect_true(all(d$halo[seq(1L, nrow(d), by = 2L)]), info = info)
      expect_false(any(d$halo[seq(2L, nrow(d), by = 2L)]), info = info)
      # Every label has its own patch, painted just before it, opaque and in
      # the panel background colour, and the patch covers the glyph box.
      expect_true(all(txt$has_halo), info = paste(info, "no halo"))
      for (i in seq_len(n_txt)) {
        h <- d[2L * i - 1L, , drop = FALSE]
        expect_false(is.na(h$fill), info = paste(info, "halo fill"))
        expect_equal(h$alpha, 1, info = paste(info, "halo alpha"))
        expect_true(h$left <= txt$left[i] + 1e-9 &&
                    h$right >= txt$right[i] - 1e-9 &&
                    h$bottom <= txt$bottom[i] + 1e-9 &&
                    h$top >= txt$top[i] - 1e-9,
                    info = paste(info, "halo covers text"))
        # Strictly larger than the glyph box: a padded patch, not a repaint.
        expect_gt(h$right - h$left, txt$right[i] - txt$left[i] + 1e-6,
                  label = info)
        expect_gt(h$top - h$bottom, txt$top[i] - txt$bottom[i] + 1e-6,
                  label = info)
      }

      # Geometry is unchanged from round 16: inside the panel, no label-on-label
      # overlap, and an edge label never crosses its own cutoff.
      tol <- 1e-7
      expect_true(all(txt$left >= -tol & txt$right <= 1 + tol), info = info)
      expect_true(all(txt$bottom >= -tol & txt$top <= 1 + tol), info = info)
      if (n_txt > 1) {
        for (i in seq_len(n_txt - 1L)) {
          for (j in (i + 1L):n_txt) {
            separated <- txt$right[i] <= txt$left[j] + tol ||
              txt$right[j] <= txt$left[i] + tol ||
              txt$top[i] <= txt$bottom[j] + tol ||
              txt$top[j] <= txt$bottom[i] + tol
            expect_true(separated,
                        info = paste(info, "overlap", txt$label[i], txt$label[j]))
          }
        }
      }
      edges <- txt$side != 0
      if (any(edges)) {
        expect_true(all(txt$right[edges] <= txt$cutoff[edges] + tol |
                        txt$left[edges] >= txt$cutoff[edges] - tol),
                    info = paste(info, "crosses cutoff"))
      }

      # Occlusion: a label box that sits on the darker event fill is allowed
      # only because the halo is drawn. Count the hits so the check is not
      # vacuous.
      dark <- vfx17_dark_points(p)
      if (nrow(dark)) {
        for (i in seq_len(n_txt)) {
          inside <- dark$x >= txt$left[i] & dark$x <= txt$right[i] &
            dark$y >= txt$bottom[i] & dark$y <= txt$top[i]
          if (any(inside)) {
            dark_hits <- dark_hits + 1L
            expect_true(txt$has_halo[i],
                        info = paste(info, "dark region without halo", txt$label[i]))
          }
        }
      }
    }
    grDevices::dev.off()
  }
  # The grid does put labels on top of the dark fill (the reprexes above), so
  # the occlusion assertion is exercised rather than vacuous.
  expect_gt(dark_hits, 0L)
})

test_that("vfx17: a near-certain event prints a bound, not 100.0%", {
  fmt <- vistributions:::vdist_fmt_pct
  expect_equal(fmt(1)$text, "100.0")
  expect_equal(fmt(0.9997673)$text, "> 99.9")
  expect_equal(fmt(0.9999999999999)$text, "> 99.9999999999")
  expect_equal(fmt(0.9994)$text, "99.9")
  # A value that rounds to 100.0 at one decimal but can still fail.
  expect_equal(fmt(0.99951)$text, "> 99.9")

  subs <- list(
    vdist_normal_prob(3.5, print_plot = FALSE),
    vdist_normal_prob(13.291, 10, 1, print_plot = FALSE),
    vdist_normal_prob(13.291, 10, 1, "lower", FALSE),
    vdist_t_prob(50, 5, "lower", FALSE)
  )
  for (p in subs) {
    expect_match(p$labels$subtitle, "> 99.9%", fixed = TRUE)
    expect_false(grepl("= 100.0%", p$labels$subtitle, fixed = TRUE))
  }
  # The percentile caption for the same event prints the same bound.
  cap <- vdist_t_perc(.001, .5, print_plot = FALSE)$labels$caption
  expect_match(gsub("[[:space:]]+", " ", cap),
               "> 99.9% of the shaded region", fixed = TRUE)
  # The same event prints identical text in the percentile and probability
  # forms: at a halfway event the finer digits survive, and a bound event is a
  # bound in both forms.
  expect_equal(vdist_normal_perc(.9995, 10, 1, print_plot = FALSE)$labels$subtitle,
               "P(X < 13.2905267315) = 99.95%")
  expect_equal(vdist_normal_prob(13.2905267315, 10, 1, print_plot = FALSE)$labels$subtitle,
               "P(X < 13.2905267315) = 99.95%")
  expect_equal(vdist_normal_perc(.9997673, 0, 1, print_plot = FALSE)$labels$subtitle,
               vdist_normal_prob(3.4998677, 0, 1, print_plot = FALSE)$labels$subtitle)
  # An exact certainty keeps its equality.
  expect_equal(vdist_normal_prob(40, 0, 1, print_plot = FALSE)$labels$subtitle,
               "P(X < 40) = 100.0%")
})

test_that("vfx17: a zero-width t interval does not print a signed zero", {
  s <- vdist_t_prob(0, 5, "interval", print_plot = FALSE)$labels$subtitle
  expect_equal(s, "P(0 < X < 0) < 1e-10%")
  expect_false(grepl("-0", s, fixed = TRUE))
})

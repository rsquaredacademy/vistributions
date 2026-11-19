# Round 16: rendered label placement, percentage formatting and caption fixes.
#
# Every assertion below reads the glyph box that grid actually draws inside the
# panel, on a pdf device at the target size. Placement therefore cannot pass by
# testing anchors: overlaps, clipping and wrong-side labels are all measured.

vfx16_extents <- function(p) {
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
  do.call(rbind, lapply(g$children, function(ch) {
    w <- grid::convertWidth(grid::grobWidth(ch), "npc", valueOnly = TRUE)
    h <- grid::convertHeight(grid::grobHeight(ch), "npc", valueOnly = TRUE)
    x <- grid::convertX(ch$x, "npc", valueOnly = TRUE)
    y <- grid::convertY(ch$y, "npc", valueOnly = TRUE)
    data.frame(label = ch$label,
               left = x - w * ch$hjust, right = x + w * (1 - ch$hjust),
               bottom = y - h * ch$vjust, top = y + h * (1 - ch$vjust),
               side = ch$vdist_side, cutoff = ch$vdist_cutoff,
               stacked = ch$vdist_stacked, row = ch$vdist_row,
               stringsAsFactors = FALSE)
  }))
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
  s <- vdist_t_perc(.998, .05, "both", FALSE)$labels$subtitle
  expect_match(s, "e\\+")
  expect_false(grepl("[0-9]{20,}", s))
  s <- vdist_t_prob(stats::qt(.001, .05, lower.tail = FALSE), .05, "both", FALSE)$labels$subtitle
  expect_match(s, "e\\+")
  expect_false(grepl("[0-9]{20,}", s))
})

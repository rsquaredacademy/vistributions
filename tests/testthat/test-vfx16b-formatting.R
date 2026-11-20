# Round 16b: the printed event has one percentage spelling in every form.
vfx16b_cutoffs <- function(subtitle) {
  event <- sub("\\).*", "", subtitle)
  as.numeric(regmatches(event, gregexpr(
    "-?[0-9]+(?:\\.[0-9]*)?(?:[eE][+-]?[0-9]+)?", event, perl = TRUE))[[1]])
}

vfx16b_percent <- function(p) sub(".*= ", "", p$labels$subtitle)

# Read precision from the literal printed digits, including every trailing
# zero. This check is independent of vdist_fmt_pct() and its tolerance.
vfx16b_expect_precision <- function(text, truth, info) {
  parts <- strsplit(sub("%$", "", text), "e", fixed = TRUE)[[1]]
  dec <- if (grepl(".", parts[1], fixed = TRUE)) {
    nchar(sub("^[^.]*\\.", "", parts[1]))
  } else 0L
  ex <- if (length(parts) == 2L) as.numeric(parts[2]) else 0
  half <- 0.5 * 10 ^ (ex - dec)
  expect_true(abs(100 * truth - as.numeric(sub("%$", "", text))) <=
                half + 32 * .Machine$double.eps * max(100 * truth, half),
              info = info)
  if (length(parts) == 2L) {
    expect_match(text, "^[1-9]\\.[0-9]{2}e[+-][0-9]+%$", info = info)
  }
}

vfx16b_specs <- function() {
  c(list(list(family = "normal", args = list(mean = 0, sd = 1),
              cdf = pnorm, density = dnorm)),
    lapply(c(1, 5, 30, 100), function(df) {
      list(family = "t", args = list(df = df),
           cdf = function(x, lower.tail = TRUE) pt(x, df, lower.tail = lower.tail),
           density = function(x) dt(x, df))
    }),
    lapply(c(1, 5, 30), function(df) {
      list(family = "chisquare", args = list(df = df),
           cdf = function(x, lower.tail = TRUE) pchisq(x, df, lower.tail = lower.tail),
           density = function(x) dchisq(x, df))
    }),
    lapply(list(c(5, 10), c(500, 500), c(1e6, 1e6)), function(df) {
      list(family = "f", args = list(num_df = df[1], den_df = df[2]),
           cdf = function(x, lower.tail = TRUE) pf(x, df[1], df[2], lower.tail = lower.tail),
           density = function(x) stats::df(x, df[1], df[2]))
    }))
}

test_that("fx16b: scientific percentages retain three digits and their tolerance", {
  fmt <- vistributions:::vdist_fmt_pct
  for (pr in c(1e-6, 2e-4, 5e-4, 0.0009994)) {
    f <- fmt(pr)
    expect_identical(f$text, formatC(100 * pr, format = "e", digits = 2))
    ex <- as.numeric(sub(".*e", "", f$text))
    expect_equal(f$tol, 0.5 * 10 ^ (ex - 2) / 100)
  }
  # Either side of the three-digit rounding switch has a stable spelling.
  for (pr in c(.001, pt(qt(.001, 5), 5), .0009996, .0010003)) {
    expect_identical(fmt(pr)$text, "1.00e-01")
    expect_equal(fmt(pr)$tol, 5e-6)
  }
  expect_identical(fmt(0.0009994)$text, "9.99e-02")
})

test_that("fx16b: named switch and halfway events use consistent text", {
  pairs <- list(
    list(vdist_t_perc(.001, 5, print_plot = FALSE), vdist_t_prob(-5.893, 5, print_plot = FALSE)),
    list(vdist_normal_perc(.001, 100, 1, print_plot = FALSE), vdist_normal_prob(96.91, 100, 1, print_plot = FALSE)),
    list(vdist_chisquare_perc(.001, 5, print_plot = FALSE), vdist_chisquare_prob(.2102, 5, print_plot = FALSE)))
  for (pair in pairs) expect_identical(vfx16b_percent(pair[[1]]), vfx16b_percent(pair[[2]]))
  cases <- list(
    list("normal", list(.0505, 10, 1), 8.36),
    list("normal", list(.9995, 10, 1), 13.291),
    list("t", list(.0505, 10), -1.806),
    list("f", list(.8005, 5, 10), 1.805))
  for (z in cases) {
    perc <- getExportedValue("vistributions", paste0("vdist_", z[[1]], "_perc"))
    prob <- getExportedValue("vistributions", paste0("vdist_", z[[1]], "_prob"))
    p <- do.call(perc, c(z[[2]], list(print_plot = FALSE)))
    cut <- vfx16b_cutoffs(p$labels$subtitle)
    expect_false(identical(cut, z[[3]]))
    q <- do.call(prob, c(list(cut), z[[2]][-1], list(print_plot = FALSE)))
    expect_identical(vfx16b_percent(p), vfx16b_percent(q))
  }
  for (pr in c(.001, .0002, 1e-6)) {
    p <- vdist_f_perc(pr, 1e6, 1e6, "upper", FALSE)
    truth <- pf(vfx16b_cutoffs(p$labels$subtitle), 1e6, 1e6, lower.tail = FALSE)
    expect_identical(sprintf("%.2e", truth), sprintf("%.2e", pr))
  }
})

test_that("fx16b: all percentile types round trip through their printed event", {
  probs <- c(1e-6, 2e-4, 5e-4, .001, .0505, .05, .5, .8005, .95, .9995, .999)
  fmt <- vistributions:::vdist_pct_text
  for (spec in vfx16b_specs()) {
    types <- if (spec$family %in% c("normal", "t")) c("lower", "upper", "both") else c("lower", "upper")
    perc <- getExportedValue("vistributions", paste0("vdist_", spec$family, "_perc"))
    prob <- getExportedValue("vistributions", paste0("vdist_", spec$family, "_prob"))
    for (pr in probs) for (type in types) {
      info <- paste(spec$family, paste(unlist(spec$args), collapse = ","), pr, type)
      p <- do.call(perc, c(list(pr), spec$args, list(type = type, print_plot = FALSE)))
      cut <- vfx16b_cutoffs(p$labels$subtitle)
      prob_type <- if (type == "both" && spec$family == "t") "interval" else type
      prob_cut <- if (prob_type == "interval") cut[2] else cut
      q <- do.call(prob, c(list(prob_cut), spec$args, list(type = prob_type, print_plot = FALSE)))
      expect_identical(vfx16b_percent(p), vfx16b_percent(q), info = info)
      truth <- if (type == "both") {
        # Independent base-R quadrature avoids subtracting two near-0.5 CDFs
        # for the narrowest central intervals.
        integrate(spec$density, cut[1], cut[2], rel.tol = 1e-10)$value
      } else spec$cdf(cut, lower.tail = type == "lower")
      vfx16b_expect_precision(vfx16b_percent(p), truth, info)
      expect_identical(paste0(fmt(truth), "%"), vfx16b_percent(p), info = info)
      layers <- Filter(function(l) inherits(l$geom, "GeomVdistLabels"), p$layers)
      labels <- unname(unlist(lapply(layers, function(l) l$data$label)))
      label_probs <- if (type == "both") {
        c(spec$cdf(cut[1]), truth, spec$cdf(cut[2], lower.tail = FALSE))
      } else c(spec$cdf(cut), spec$cdf(cut, lower.tail = FALSE))
      expect_identical(labels, paste0(vapply(label_probs, fmt, ""), "%"), info = info)
    }
  }
})

vfx16b_rendered_colours <- function(p) {
  walk <- function(g) {
    if (inherits(g, "text") && any(grepl("%", g$label, fixed = TRUE))) {
      return(data.frame(label = g$label, colour = g$gp$col))
    }
    do.call(rbind, lapply(g$children, walk))
  }
  gt <- ggplot2::ggplotGrob(p)
  result <- walk(gt$grobs[[which(gt$layout$name == "panel")]])
  rownames(result) <- NULL
  result
}

test_that("fx16b: rendered percentage colours match c63f7de in every family", {
  reference <- read.csv(test_path("fixtures", "fx16b-label-colours.csv"))
  for (id in unique(reference$case)) {
    z <- strsplit(id, "_", fixed = TRUE)[[1]]
    arg <- if (z[2] == "perc") .95 else if (z[1] == "normal" && z[3] == "both") c(-1.96, 1.96) else 1.96
    fun <- getExportedValue("vistributions", paste0("vdist_", z[1], "_", z[2]))
    p <- fun(arg, type = z[3], print_plot = FALSE)
    got <- vfx16b_rendered_colours(p)
    expected <- subset(reference, case == id)
    expect_identical(sort(paste(got$label, got$colour)),
                     sort(paste(expected$label, expected$colour)), info = id)
    # A user colour scale remains free to colour other layers.
    scaled <- p + ggplot2::geom_point(data = data.frame(x = 0, y = 0),
      ggplot2::aes(x = x, y = y, colour = "other"), inherit.aes = FALSE) +
      ggplot2::scale_colour_manual(values = c(other = "black"))
    expect_identical(vfx16b_rendered_colours(scaled), got, info = id)
    expect_identical(tail(ggplot2::ggplot_build(scaled)$data, 1)[[1]]$colour, "black")
  }
})

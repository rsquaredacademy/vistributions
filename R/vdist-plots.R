#' Binomial distribution plot
#'
#' Build plot for binomial distribution.
#'
#' @param data Output returned by `bplot_data_prep()`.
#' @param n Number of trials.
#' @param p Aggregate probability.
#'
#' @noRd
#'
bplot_plot_build <- function(data, n, p) {

  p <-
    ggplot(data$plot_data) +
    vdist_col_layer(data, "blue") +
    ylab("Probability") +
    xlab("No. of success") +
    ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
            subtitle = paste("Mean =", data$bm, ", Std. Dev. =", data$bsd)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = vdist_count_breaks(n))

  if (isTRUE(data$binned)) {
    p <- p + labs(caption = vdist_binned_text())
  }

  return(p)

}

# Bars of a count distribution. Individual counts keep the ordinary narrow bar;
# a support too wide for individual bars is drawn from binned probabilities and
# the bars then touch, so each bar shows the mass of its own bin.
vdist_col_layer <- function(data, fill) {
  n <- df <- NULL
  if (isTRUE(data$binned)) {
    geom_col(aes(x = n, y = df), fill = fill, width = data$plot_data$w)
  } else {
    geom_col(aes(x = n, y = df), fill = fill)
  }
}

# Axis breaks for a count axis never number more than 31 ticks, so a large n
# does not materialise one label per count.
vdist_count_breaks <- function(n) {
  if (n <= 30) seq(0, n) else ggplot2::waiver()
}

vdist_binned_text <- function() {
  paste("Bars are grouped: each bar shows the probability of a range of counts,",
        "because individual bars would be narrower than a pixel at this scale")
}

bprob_plot_build <- function(data, method, n, p, s) {

  np <- vdist_fmt_binom_prob(data$k)
  relation <- if (grepl("^[<>]", np)) " " else " = "
  plot <-
    ggplot(data$plot_data) +
    vdist_col_layer(data, data$cols) +
    ylab("Probability") +
    xlab(paste("No. of success\n", "Mean =", data$bm, ", Std. Dev. =", data$bsd)) +
    scale_x_continuous(breaks = vdist_count_breaks(n)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  # The inequality belongs inside P(): P(X >= 4) = 0.35, not P(X) >= 4 = 0.35.
  if (method == "lower") {
    sub <- paste0("P(X <= ", s, ")", relation, np)
  } else if (method == "upper") {
    sub <- paste0("P(X >= ", s, ")", relation, np)
  } else if (method == "exact") {
    sub <- paste0("P(X = ", s, ")", relation, np)
  } else {
    sub <- paste0("P(", s[1], " <= X <= ", s[2], ")", relation, np)
  }

  plot <- plot +
    ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
            subtitle = sub)

  notes <- if (isTRUE(data$binned)) vdist_binned_text() else character(0)
  if (data$k < 1e-12) notes <- c(notes, "The shaded probability is too small to display")
  if (length(notes)) plot <- plot + labs(caption = paste(notes, collapse = "; "))

  return(plot)
}

bperc_plot_build <- function(data, method, n, p, tp) {

  plot <-
    ggplot(data$plot_data) +
    vdist_col_layer(data, data$cols) +
    ylab("Probability") +
    xlab("No. of success") +
    scale_x_continuous(breaks = vdist_count_breaks(n)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  # The threshold is printed with every digit needed to state the inequality:
  # paste0() prints 1 - 2^-53 as the excluded value 1.
  threshold <- vdist_fmt_pct(tp)$text
  extreme <- grepl("^[<>]", threshold)
  tpt <- if (extreme) "target" else vdist_fmt_number(tp)

  if (method == "lower") {
    k <- data$k
    sub <- paste0("P(X <= ", k - 1, ") < ", tpt,
                  ", but P(X <= ", k, ") >= ", tpt)
  } else {
    k <- data$k
    sub <- paste0("P(X >= ", k + 1, ") <= ", tpt,
                  ", but P(X >= ", k, ") > ", tpt)
  }

  if (extreme) sub <- paste0("target ", threshold, "%: ", sub)

  plot <- plot +
    ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
            subtitle = sub)

  notes <- if (isTRUE(data$binned)) vdist_binned_text() else character(0)
  shaded <- sum(data$plot_data$df[data$cols == "#0000CD"])
  if (shaded < 1e-12) notes <- c(notes, "The shaded probability is too small to display")
  if (length(notes)) plot <- plot + labs(caption = paste(notes, collapse = "; "))

  return(plot)

}

cplot_plot_build <- function(data, df, range, normal) {

  ydisp <- vdist_ylim_cap(if (normal) max(data$ycap, data$nline_data$y) else data$ycap)
  pp <-
    ggplot(data$plot_data) +
    geom_line(aes(x, chi),
              color = '#4682B4',
              linewidth  = 2) +
    ggtitle(label    = "Chi Square Distribution",
            subtitle = paste("df =", df)) +
    ylab('') +
    xlab(paste("Mean =", fmt_num(df), " Std Dev. =", fmt_num(sqrt(2 * df)))) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = data$breaks) +
    geom_polygon(data    = data$poly_data,
                 mapping = aes(x = y, y = z),
                 fill    = '#4682B4') +
    geom_point(data    = data$point_data,
               mapping = aes(x = x, y = y),
               shape   = 4,
               color   = 'red',
               size    = 3) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  if (normal) {

    pp <-
      pp +
      geom_line(data    = data$nline_data,
              mapping = aes(x = x, y = y),
              color   = '#FF4500')

  }

  return(pp)

}

cperc_plot_build <- function(data, method, probs, df) {

  other_text <- data$other_text
  ydisp      <- vdist_ylim_cap(data$ycap)
  ytext      <- ydisp[2] * 0.97

  plot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    xlab(paste("Mean =", fmt_num(df), " Std Dev. =", fmt_num(sqrt(2 * df)))) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    plot <-
      plot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label   = paste0(data$prob_text, "%"),
               x       = vdist_clamp(data$pc - data$chisd, data$view),
               y       = ytext,
               color   = "#0000CD",
               size    = 3) +
      annotate("text",
               label   = paste0(other_text, "%"),
               x       = vdist_clamp(data$pc + data$chisd, data$view),
               y       = ytext,
               color   = "#6495ED",
               size    = 3)

  } else {
    plot <-
      plot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label   = paste0(other_text, "%"),
               x       = vdist_clamp(data$pc - data$chisd, data$view),
               y       = ytext,
               color   = "#6495ED",
               size    = 3) +
      annotate("text",
               label   = paste0(data$prob_text, "%"),
               x       = vdist_clamp(data$pc + data$chisd, data$view),
               y       = ytext,
               color   = "#0000CD",
               size    = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    pol_data <- vdist_pol_seg(data, i)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(pol_data$x, pol_data$y)
    }
    plot <-
      plot +
      geom_polygon(data    = pol_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_region(dark, data$event_prob, "chi-square shaded region")

  plot <-
    plot +
    geom_vline(xintercept = data$pc,
               linetype   = 2,
               linewidth  = 1) +
    geom_point(data       = data$point_data,
               mapping    = aes(x = x, y = y),
               shape      = 4,
               color      = 'red',
               size       = 3) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = if (data$xm[2] <= 300) seq(0, data$xm[2], by = 5) else waiver()) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  plot <- plot + vdist_subpixel_layer(data, ydisp)

  if (!is.null(vdist_caption_text(data))) {
    plot <- plot + labs(caption = vdist_caption_text(data))
  }

  return(plot)

}

cprob_plot_build <- function(data, method, perc, df) {

  other_text <- data$other_text
  ydisp      <- vdist_ylim_cap(data$ycap)
  ytext      <- ydisp[2] * 0.97

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    xlab(paste("Mean =", fmt_num(df), " Std Dev. =", fmt_num(sqrt(2 * df)))) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))


  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - data$chisd, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + data$chisd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - data$chisd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + data$chisd, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    pol_data <- vdist_pol_seg(data, i)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(pol_data$x, pol_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = pol_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_region(dark, data$event_prob, "chi-square shaded region")

  gplot <-
    gplot +
    geom_vline(xintercept = data$pc,
               linetype   = 2,
               linewidth = 1) +
    geom_point(data       = data$point_data,
               mapping    = aes(x = x, y = y),
               shape      = 4,
               color      = 'red',
               size       = 3) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = if (data$l[data$ln] <= 300) seq(0, data$l[data$ln], by = 5) else waiver()) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  gplot <- gplot + vdist_subpixel_layer(data, ydisp)

  if (!is.null(vdist_caption_text(data))) {
    gplot <- gplot + labs(caption = vdist_caption_text(data))
  }

  return(gplot)
}

# Polygon of a continuous region. The builder in R/vdist-utils.R samples the
# region on a support/tail-aware grid and refines it until its own area matches
# the exact probability p(hi) - p(lo); this function only wraps the vertices.
vdist_pol_cont <- function(spec, lo, hi, tol = 1e-5) {
  # Only location-scale families are standardised in the region builder: for a
  # distribution on a support boundary the affine map to z would collapse the
  # many decades between the support and the body into one double, and the
  # support-edge mass would be lost.
  if (isTRUE(spec$standardise)) {
    r <- vdist_region(lo, hi, spec$dfun, spec$pfun, spec$qfun, tol = tol,
                      center = spec$center, scale = spec$scale)
  } else {
    r <- vdist_region(lo, hi, spec$dfun, spec$pfun, spec$qfun, tol = tol)
  }
  data.frame(x = r$x, y = r$y)
}

# One polygon of the event layout. The dark segments carry the probability and
# are built to the area tolerance; the light background segments are only
# geometry, so a looser tolerance keeps the plot cheap.
vdist_pol_seg <- function(data, i, bounds = NULL) {
  lc <- if (is.null(bounds)) data$lc else bounds
  vdist_pol_cont(data$spec, lc[data$l1[i]], lc[data$l2[i]],
                 tol = if (identical(data$col[i], "#0000CD")) 1e-5 else 1e-3)
}

fplot_plot_build <- function(data, num_df, den_df, normal) {

  ydisp <-
    vdist_ylim_cap(data$ycap)

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    geom_polygon(data    = data$poly_data,
                 mapping = aes(x = y, y = z),
                 fill    = '#4682B4') +
    geom_point(data    = data$point_data,
               mapping = aes(x = x, y = y),
               shape   = 4,
               color   = 'red',
               size    = 3) +
    xlab(paste("Mean =", fmt_moment(data$fm), " Std Dev. =", fmt_moment(data$fsd))) +
    ylab('') +
    ggtitle(label    = 'f Distribution',
            subtitle = paste("Num df =", num_df, "  Den df =", den_df)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = data$breaks) +
    scale_y_continuous(breaks = NULL) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  if (normal && nrow(data$nline_data) > 0) {
    gplot <-
      gplot +
      geom_line(data    = data$nline_data,
                mapping = aes(x = x, y = y),
                color   = '#FF4500')
  } else if (normal) {
    message(paste("Normal overlay omitted: the F distribution standard",
                  "deviation is undefined when den_df <= 4."))
  }

  return(gplot)
}

fperc_plot_build <- function(data, probs, num_df, den_df, method) {

  ydisp <- vdist_ylim_cap(data$ycap)
  ytext <- ydisp[2] * 0.97

  gplot <-
    ggplot(data$plot_data) +
    geom_line(data    = data$plot_data,
              mapping = aes(x = x, y = y),
              color   = 'blue') +
    xlab(paste("Mean =", fmt_moment(data$fm), " Std Dev. =", fmt_moment(data$fsd))) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))


  other_text <- data$other_text

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - 0.2, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + 0.2, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - 0.2, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + 0.2, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_seg(data, i)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_region(dark, data$event_prob, "F shaded region")

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    vdist_x_scale(data$view, breaks = data$breaks) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  gplot <- gplot + vdist_subpixel_layer(data, ydisp)

  if (!is.null(vdist_caption_text(data))) {
    gplot <- gplot + labs(caption = vdist_caption_text(data))
  }

  return(gplot)

}

fprob_plot_build <- function(data, perc, num_df, den_df, method) {

  ydisp <- vdist_ylim_cap(data$ycap)
  ytext <- ydisp[2] * 0.97

  gplot <-
    ggplot(data$plot_data) +
    geom_line(data    = data$plot_data,
              mapping = aes(x = x, y = y),
              color   = 'blue') +
    xlab(paste("Mean =", fmt_moment(data$fm), " Std Dev. =", fmt_moment(data$fsd))) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  fsd_offset <- if (!is.na(data$fsd)) data$fsd else 0.5
  other_text <- data$other_text

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - fsd_offset, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + fsd_offset, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - fsd_offset, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + fsd_offset, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_seg(data, i)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_region(dark, data$event_prob, "F shaded region")

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    vdist_x_scale(data$view, breaks = data$breaks) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  gplot <- gplot + vdist_subpixel_layer(data, ydisp)

  if (!is.null(vdist_caption_text(data))) {
    gplot <- gplot + labs(caption = vdist_caption_text(data))
  }

  return(gplot)

}


nplot_plot_build <- function(data, mean, sd) {

  ydisp <- vdist_ylim_cap(data$ycap)

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y)) +
    xlab('') +
    ylab('') +
    ggtitle(label    = "Normal Distribution",
            subtitle = paste("Mean:", mean, "     Standard Deviation:", sd)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_seg(data, i, data$ll)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }

  # Default labels round every break to "1" when the window is a tiny multiple
  # of sd around a non-zero mean. Use the distribution's own scale in that case;
  # an ordinary normal keeps the default labels (and snapshots) unchanged.
  if (is.finite(mean) && abs(mean) > 0 && sd / abs(mean) < 1e-6) {
    gplot <- gplot + scale_x_continuous(
      labels = function(v) fmt_num(v, sd)
    )
  }

  gplot <- gplot + coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  return(gplot)

}


nperc_plot_build <- function(data, probs, mean, sd, method) {

  ydisp <- vdist_ylim_cap(data$ycap)
  ytext <- ydisp[2] * 0.97

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y)) +
    xlab(paste("Mean:", mean, " Standard Deviation:", sd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  other_text <- data$other_text

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - sd, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + sd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - sd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + sd, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3)
  } else {
    tail_text <- vdist_pct_text(data$alpha)
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(", data$cut_text[1], " < X < ", data$cut_text[2], ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(mean(data$pc), data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(tail_text, "%"),
               x     = vdist_clamp(data$pc[1] - sd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(tail_text, "%"),
               x     = vdist_clamp(data$pc[2] + sd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_seg(data, i)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_region(dark, data$event_prob, "normal shaded region")

  pln <- length(data$pc)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = data$breaks,
      labels = if (abs(mean) > 0 && sd / abs(mean) < 1e-6) function(v) fmt_num(v, sd) else ggplot2::waiver()) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  gplot <- gplot + vdist_subpixel_layer(data, ydisp)

  if (!is.null(vdist_caption_text(data))) {
    gplot <- gplot + labs(caption = vdist_caption_text(data))
  }

  return(gplot)
}

nprob_plot_build <- function(data, perc, mean, sd, method) {

  ydisp <- vdist_ylim_cap(data$ycap)
  ytext <- ydisp[2] * 0.97

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y)) +
    xlab(paste("Mean:", mean, " Standard Deviation:", sd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  other_text <- data$other_text

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - sd, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + sd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - sd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + sd, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3)
  } else {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(", data$cut_text[1], " < X < ", data$cut_text[2], ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(mean(data$pc), data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp1), "%"),
               x     = vdist_clamp(data$pc[1] - sd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp2), "%"),
               x     = vdist_clamp(data$pc[2] + sd, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_seg(data, i)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_region(dark, data$event_prob, "normal shaded region")

  pln <- length(data$pc)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = data$breaks,
      labels = if (abs(mean) > 0 && sd / abs(mean) < 1e-6) function(v) fmt_num(v, sd) else ggplot2::waiver()) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  gplot <- gplot + vdist_subpixel_layer(data, ydisp)

  if (!is.null(vdist_caption_text(data))) {
    gplot <- gplot + labs(caption = vdist_caption_text(data))
  }

  return(gplot)

}

tplot_plot_build <- function(data, df) {

  ydisp <- vdist_ylim_cap(data$ycap)

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = 'blue') +
    ggtitle(label    = 't Distribution',
            subtitle = paste("df =", df)) +
    xlab('') +
    ylab('') +
    geom_polygon(data    = data$poly_data,
                 mapping = aes(x = y, y = z),
                 fill    = '#4682B4') +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = -4:4) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  return(gplot)

}

tperc_plot_build <- function(data, probs, df, method) {

  ydisp <- vdist_ylim_cap(data$ycap)
  ytext <- ydisp[2] * 0.97

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = 'blue') +
    xlab(paste("df =", df)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  other_text <- data$other_text

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - 0.3, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + 0.3, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - 0.3, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + 0.3, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3)
  } else {
    tail_text <- vdist_pct_text(data$alpha)
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(", data$cut_text[1], " < X < ", data$cut_text[2], ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(mean(data$pc), data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(tail_text, "%"),
               x     = vdist_clamp(data$pc[1] - 0.3, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(tail_text, "%"),
               x     = vdist_clamp(data$pc[2] + 0.3, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_seg(data, i)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_region(dark, data$event_prob, "t shaded region")

  pln <- length(data$pc)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    vdist_x_scale(data$view, breaks = data$breaks) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  gplot <- gplot + vdist_subpixel_layer(data, ydisp)

  if (!is.null(vdist_caption_text(data))) {
    gplot <- gplot + labs(caption = vdist_caption_text(data))
  }

  return(gplot)
}

tprob_plot_build <- function(data, perc, df, method) {

  ydisp <- vdist_ylim_cap(data$ycap)
  ytext <- ydisp[2] * 0.97

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = 'blue') +
    xlab(paste("df =", df)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    vdist_x_scale(data$view, breaks = data$breaks) +
    scale_y_continuous(breaks = NULL) +
    coord_cartesian(xlim = data$view, ylim = ydisp, expand = FALSE)

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_seg(data, i)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_region(dark, data$event_prob, "t shaded region")

  other_text <- data$other_text

  if (method == "lower") {

    point_data <- data.frame(x = data$pc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - 1, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + 1, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      geom_vline(xintercept = data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)

  } else if (method == "upper") {

    point_data <- data.frame(x = data$pc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - 1, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + 1, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      geom_vline(xintercept = data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)

  } else if (method == "interval") {

    point_data <- data.frame(x1 = data$pc, x2 = -data$pc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(", paste0("-", data$cut_text), " < X < ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = 0,
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp1), "%"),
               x     = vdist_clamp(-data$pc - 1, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp2), "%"),
               x     = vdist_clamp(data$pc + 1, data$view),
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      geom_vline(xintercept = data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_vline(xintercept = -data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x1, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3) +
      geom_point(data    = point_data,
                 mapping = aes(x = x2, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  } else {

    point_data <- data.frame(x1 = data$pc, x2 = -data$pc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(|X| > ", data$cut_text, ") ", if (grepl("^[<>]", data$prob_text)) "" else "= ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = 0,
               y     = ytext,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp1), "%"),
               x     = vdist_clamp(-data$pc - 1, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp2), "%"),
               x     = vdist_clamp(data$pc + 1, data$view),
               y     = ytext,
               color = "#0000CD",
               size  = 3) +
      geom_vline(xintercept = data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_vline(xintercept = -data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x1, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3) +
      geom_point(data    = point_data,
                 mapping = aes(x = x2, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <- gplot + vdist_subpixel_layer(data, ydisp)

  if (!is.null(vdist_caption_text(data))) {
    gplot <- gplot + labs(caption = vdist_caption_text(data))
  }

  return(gplot)

}



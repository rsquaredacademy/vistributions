# Usage (from the worktree):
# Rscript docs-dev/vist-fx18-validate.R capture <tree> <output.rds>
# Rscript docs-dev/vist-fx18-validate.R compare <baseline.rds> <current.rds>
# Rscript docs-dev/vist-fx18-validate.R render <output-directory>
args <- commandArgs(TRUE)
source("tests/testthat/helper-fx18-render.R")
if (args[1] == "capture") {
  pkgload::load_all(args[2], quiet = TRUE)
  cases <- fx18_calls()
  results <- lapply(cases, function(case) {
    tryCatch({
      p <- do.call(getExportedValue("vistributions", case$fun), case$args)
      b <- ggplot2::ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      labels <- setNames(lapply(names(p$labels), function(nm) p$labels[[nm]]), names(p$labels))
      # A caption may differ only in whitespace; all other text is exact.
      if (!is.null(labels$caption)) labels$caption <- gsub("[[:space:]]+", " ", labels$caption)
      list(error = NULL, labels = labels, windows = list(pp$x.range, pp$y.range),
        layers = b$data, source_data = p$data,
        geoms = vapply(p$layers, function(l) class(l$geom)[1], ""),
        params = lapply(p$layers, function(l) l$aes_params),
        breaks = pp$x$get_breaks())
    }, error = function(e) list(error = conditionMessage(e)))
  })
  saveRDS(results, args[3])
  cat("Captured", length(results), "calls; errors:",
      sum(vapply(results, function(x) !is.null(x$error), TRUE)), "\n")
} else if (args[1] == "compare") {
  base <- readRDS(args[2]); current <- readRDS(args[3])
  stopifnot(identical(names(base), names(current)), length(base) >= 300L)
  fields <- c("labels", "windows", "layers", "source_data", "geoms", "params")
  counts <- setNames(integer(length(fields)), fields)
  errors <- breaks <- 0L
  for (i in seq_along(base)) {
    a <- base[[i]]; b <- current[[i]]
    # Cross-process S7 class registries have different identities. Compare the
    # text values in plain named lists, without those registry attributes.
    for (obj in c("a", "b")) {
      x <- get(obj)
      x$labels <- setNames(lapply(names(x$labels), function(nm) x$labels[[nm]]), names(x$labels))
      assign(obj, x)
    }
    errors <- errors + !is.null(a$error) + !is.null(b$error)
    for (field in fields) {
      if (!identical(a[[field]], b[[field]])) {
        counts[field] <- counts[field] + 1L
        cat("DIFF", names(base)[i], field, "\n")
      }
    }
    breaks <- breaks + !identical(a$breaks, b$breaks)
  }
  cat("Calls:", length(base), "Errors:", errors, "Changed break vectors:", breaks, "\n")
  print(counts)
  stopifnot(errors == 0L, all(counts == 0L))
} else if (args[1] == "render") {
  pkgload::load_all(".")
  out <- args[2]
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  cases <- list(chi_axis = vdist_chisquare_prob(13, 200, "lower", FALSE),
    f_caption = vdist_f_prob(2.35, 500, 500, "upper", FALSE),
    t_caption = vdist_t_perc(.01, 30, "both", FALSE))
  measurements <- list()
  for (name in names(cases)) for (size in list(c(4, 3), c(7, 5), c(10, 6))) {
    id <- paste0(name, "-", paste(size, collapse = "x"))
    path <- file.path(out, paste0(id, ".png"))
    ragg::agg_png(path, width = size[1], height = size[2], units = "in", res = 150)
    d <- fx18_text_boxes(cases[[name]])
    grDevices::dev.off()
    measurements[[id]] <- d
    axis <- d[d$kind == "tick" & d$axis == "axis-b", ]
    axis <- axis[order(axis$left), ]
    overlaps <- if (nrow(axis) > 1) sum(axis$right[-nrow(axis)] > axis$left[-1]) else 0
    clips <- sum(d$left < 0 | d$right > size[1] | d$bottom < 0 | d$top > size[2])
    cat(id, "ticks", nrow(axis), "overlaps", overlaps, "clips", clips, "\n")
    stopifnot(overlaps == 0L, clips == 0L)
  }
  saveRDS(measurements, file.path(out, "named-boxes.rds"))
} else stop("Unknown mode")

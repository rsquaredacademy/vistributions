label_values <- function(g) {
  unlist(lapply(g$layers, function(layer) {
    if ("label" %in% names(layer$data)) return(layer$data$label)
    if (!is.null(layer$aes_params$label)) return(layer$aes_params$label)
    NULL
  }))
}

test_that("supported domain limits are stable at the normal ratio and binomial jumps", {
  expect_error(vdist_normal_plot(1, 0.999e-10, FALSE), "sd / \\|mean\\|")
  expect_error(vdist_normal_perc(1e-12, print_plot = FALSE), "probs")
  expect_s3_class(vdist_binom_perc(1, .5, .5 + 2^-53, print_plot = FALSE), "ggplot")
  g <- vdist_binom_perc(1, .5, .5 + 2e-12, print_plot = FALSE)
  d <- ggplot2::layer_data(g, 1)
  expect_equal(sum(d$y[d$fill == "#0000CD"]), pbinom(1, 1, .5))
})

test_that("cutoffs preserve both normal tail labels", {
  g <- vdist_normal_prob(4.00049, print_plot = FALSE)
  text <- sub(".*X < ([^)]*)\\).*", "\\1", g$labels$subtitle)
  cutoff <- as.numeric(text)
  expect_equal(round(100 * pnorm(cutoff, lower.tail = FALSE), 5),
               round(100 * pnorm(4.00049, lower.tail = FALSE), 5))
  expect_true("3.16e-03%" %in% unname(label_values(g)))
})

test_that("chi-square views follow the distribution and retain full density grids", {
  g <- vdist_chisquare_plot(200, TRUE, print_plot = FALSE)
  d <- ggplot2::layer_data(g, 4)
  expect_lte(max(d$y), g$coordinates$limits$y[2])
  expect_equal(d$y[which.min(abs(d$x - 200))], dnorm(d$x[which.min(abs(d$x - 200))], 200, 20))
  d <- ggplot2::layer_data(vdist_chisquare_plot(500, xaxis_range = 11, print_plot = FALSE), 1)
  expect_gt(nrow(d), 100)
  expect_lt(abs(approx(d$x, d$y, 10.89)$y / dchisq(10.89, 500) - 1), 1e-5)
})

test_that("singular F panel includes the body", {
  for (dfs in list(c(1, 1), c(1, 2), c(2, 1))) {
    for (type in c("lower", "upper")) {
      g <- vdist_f_perc(.95, dfs[1], dfs[2], type, FALSE)
      expect_gte(g$coordinates$limits$y[2], df(1, dfs[1], dfs[2]))
    }
  }
})

test_that("t interval endpoints use the same decimal format", {
  old <- options(OutDec = ","); on.exit(options(old))
  g <- vdist_t_prob(.000123, 7, "interval", FALSE)
  expect_match(g$labels$subtitle, "-0\\.000123 < X < 0\\.000123")
  expect_equal(round(100 * diff(pt(c(-.000123, .000123), 7)), 4), .0095)
})

test_that("event polygons meet relative error for supported tails", {
  area <- function(g) sum(vapply(Filter(function(layer)
    identical(layer$aes_params$fill, "#0000CD") ||
      identical(layer$geom_params$fill, "#0000CD"), g$layers), function(layer)
        vistributions:::vdist_area(layer$data$x, layer$data$y), numeric(1)))
  cases <- list(
    list(vdist_normal_prob(7, type = "upper", print_plot = FALSE), pnorm(7, lower.tail = FALSE)),
    list(vdist_t_perc(1e-10, 1, "upper", FALSE), pt(qt(1e-10, 1, lower.tail = FALSE), 1, lower.tail = FALSE)),
    list(vdist_f_perc(.95, 1, 1, "upper", FALSE), .95))
  for (case in cases) expect_lt(abs(area(case[[1]]) / case[[2]] - 1), 1e-6)
})

test_that("under-floor probabilities are disclosed and subtraction uses its operands", {
  g <- vdist_normal_prob(c(0, 992), 1000, 1, print_plot = FALSE)
  expect_lt(100 * diff(pnorm(c(0, 992), 1000, 1)), 1e-10)
  expect_match(g$labels$subtitle, "< 1e-10%")
  expect_equal(vistributions:::vdist_prob_between(pnorm, dnorm, -100, -7), pnorm(-7), tolerance = 1e-12)
})

test_that("translated normal ticks are distinguishable on all three plots", {
  plots <- list(vdist_normal_plot(1, 1e-10, FALSE),
                vdist_normal_perc(.5, 1, 1e-10, print_plot = FALSE),
                vdist_normal_prob(1, 1, 1e-10, print_plot = FALSE))
  for (g in plots) {
    labels <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]$x$get_labels()
    labels <- labels[!is.na(labels)]
    expect_equal(length(unique(labels)), length(labels))
  }
  expect_equal(pnorm(1, 1, 1e-10), .5)
})


test_that("underflowed normal tails use floor and disclose omission", {
  g <- vdist_normal_prob(40, type = "upper", print_plot = FALSE)
  expect_equal(pnorm(40, lower.tail = FALSE), 0)
  expect_match(g$labels$subtitle, "< 1e-10%")
  expect_match(g$labels$caption, "too small")
  g <- vdist_normal_prob(40, type = "lower", print_plot = FALSE)
  # pnorm(40) is 1 exactly: an exact certainty keeps its equality.
  expect_match(g$labels$subtitle, "= 100\\.0%")
})

test_that("exact binomial jumps keep qbinom convention", {
  for (type in c("lower", "upper")) {
    g <- vdist_binom_perc(1, .5, .5, type, FALSE)
    d <- ggplot2::layer_data(g, 1)
    k <- qbinom(.5, 1, .5, lower.tail = type == "lower")
    truth <- if (type == "lower") pbinom(k, 1, .5) else pbinom(k, 1, .5, lower.tail = FALSE)
    expect_equal(sum(d$y[d$fill == "#0000CD"]), truth)
  }
})


test_that("binomial displays use the probability floor", {
  g <- vdist_binom_prob(1000, .5, 0, "exact", FALSE)
  expect_lt(100 * dbinom(0,1000,.5), 1e-10)
  expect_match(g$labels$subtitle, "< 1e-10%")
  g <- vdist_binom_perc(1,.5,1e-13,"lower",FALSE)
  expect_match(g$labels$subtitle, "target < 1e-10%")
})

test_that("singular F and chi-square bodies remain visible in every export", {
  for (dfs in list(c(1,1), c(1,2), c(2,1))) {
    a <- dfs[1]; b <- dfs[2]
    plots <- list(vdist_f_plot(a,b,print_plot=FALSE),
                  vdist_f_perc(.95,a,b,"lower",FALSE), vdist_f_perc(.95,a,b,"upper",FALSE),
                  vdist_f_prob(1,a,b,"lower",FALSE), vdist_f_prob(1,a,b,"upper",FALSE))
    for (g in plots) expect_gte(g$coordinates$limits$y[2], df(1,a,b))
  }
  plots <- list(vdist_chisquare_plot(1,print_plot=FALSE),
                vdist_chisquare_perc(.95,1,"lower",FALSE), vdist_chisquare_perc(.95,1,"upper",FALSE),
                vdist_chisquare_prob(1,1,"lower",FALSE), vdist_chisquare_prob(1,1,"upper",FALSE))
  for (g in plots) expect_gte(g$coordinates$limits$y[2], dchisq(1,1))
})

test_that("heavy-tailed polygons and F tail bounds preserve probability", {
  expect_error(vdist_t_perc(1e-10,.1,print_plot=FALSE),
               "probs must be between 0.001 and 0.999", fixed = TRUE)
  g <- vdist_t_perc(1e-10,1,print_plot=FALSE)
  layers <- Filter(function(l) identical(l$aes_params$fill,"#0000CD") ||
                     identical(l$geom_params$fill,"#0000CD"), g$layers)
  mass <- sum(vapply(layers,function(l) vistributions:::vdist_area(l$data$x,l$data$y),numeric(1)))
  expect_lt(abs(mass / pt(qt(1e-10,1),1) - 1), 1e-6)
  g <- vdist_f_prob(1e6,1e6,1e6,print_plot=FALSE)
  expect_equal(pf(1e6,1e6,1e6), 1)
  expect_match(g$labels$subtitle, "= 100\\.0%")
})


test_that("small interval probabilities use stable tail differences", {
  pf <- function(x, lower.tail=TRUE) pnorm(x,1000,1,lower.tail=lower.tail)
  dens <- function(x) dnorm(x,1000,1)
  got <- vistributions:::vdist_prob_between(pf,dens,0,992)
  expect_lt(abs(got / diff(pnorm(c(0,992),1000,1)) - 1), 1e-10)
  a <- .2; b <- a + .Machine$double.eps / 8
  got <- vistributions:::vdist_prob_between(pnorm,dnorm,a,b)
  truth <- integrate(dnorm,a,b,rel.tol=1e-12)$value
  expect_lt(abs(got / truth - 1), 1e-10)
})

test_that("all continuous families floor and disclose extreme tails", {
  cases <- list(
    list(vdist_t_prob(1000,100,"upper",FALSE), pt(1000,100,lower.tail=FALSE)),
    list(vdist_chisquare_prob(100,1,"upper",FALSE), pchisq(100,1,lower.tail=FALSE)),
    list(vdist_f_prob(30,5,200,"upper",FALSE), pf(30,5,200,lower.tail=FALSE)))
  for (case in cases) {
    expect_lt(100*case[[2]],1e-10)
    expect_match(case[[1]]$labels$subtitle,"< 1e-10%")
    expect_match(case[[1]]$labels$caption,"outside|too small")
  }
})

test_that("normal ratio boundary is stable at large magnitudes", {
  for (mean in c(1,1e200)) {
    sd <- mean * 1e-10
    expect_true(sd / abs(mean) >= 1e-10)
    expect_s3_class(vdist_normal_plot(mean,sd,FALSE),"ggplot")
    expect_error(vdist_normal_plot(mean,sd*.999,FALSE),"sd / \\|mean\\|")
  }
})


test_that("printed cutoffs support a floor bound near its boundary", {
  x <- qnorm(9.999e-13, lower.tail=FALSE)
  g <- vdist_normal_prob(x,type="upper",print_plot=FALSE)
  cut <- as.numeric(regmatches(g$labels$subtitle,
    regexec("P\\(X > ([^)]+)\\)",g$labels$subtitle))[[1]][2])
  expect_match(g$labels$subtitle,"< 1e-10%")
  expect_lt(pnorm(cut,lower.tail=FALSE),1e-12)
  g <- vdist_normal_prob(x,type="lower",print_plot=FALSE)
  cut <- as.numeric(regmatches(g$labels$subtitle,
    regexec("P\\(X < ([^)]+)\\)",g$labels$subtitle))[[1]][2])
  expect_lt(pnorm(cut,lower.tail=FALSE),1e-12)
  expect_true(any(grepl("< 1e-10%", label_values(g), fixed = TRUE)))
})

test_that("binomial exact jumps and targets outside the exclusion match qbinom", {
  for (n in c(1,10,1000,10000)) for (p in c(.01,.1,.5,.9,.99)) {
    for (type in c("lower","upper")) {
      lower <- type == "lower"
      k <- min(n,max(0,round(n*p)))
      jump <- pbinom(k,n,p,lower.tail=lower)
      for (offset in c(-2e-12,0,2e-12)) {
        tp <- jump + offset
        if (tp <= 0 || tp >= 1) next
        g <- vdist_binom_perc(n,p,tp,type,FALSE)
        d <- ggplot2::layer_data(g,1)
        observed <- sum(d$y[d$fill == "#0000CD"])
        expected <- pbinom(qbinom(tp,n,p,lower.tail=lower),n,p,lower.tail=lower)
        expect_equal(observed,expected,tolerance=1e-10,
                     info=paste("n",n,"p",p,"type",type,"offset",offset))
      }
    }
  }
})

test_that("printed cutoffs support both labels for all continuous families", {
  check_label <- function(text, truth) {
    if (startsWith(text,"<")) { expect_lt(truth,1e-12); return(invisible(NULL)) }
    if (startsWith(text,">")) {
      # The printed text is a bound: the exact probability must be beyond it.
      bound <- as.numeric(sub("%$","",sub("^>[[:space:]]*","",text)))
      expect_gt(100*truth,bound)
      return(invisible(NULL))
    }
    num <- sub("%$","",text)
    parts <- strsplit(tolower(num),"e",fixed=TRUE)[[1]]
    decimals <- if (grepl("\\.",parts[1])) nchar(sub("^[^.]*\\.","",parts[1])) else 0
    exponent <- if (length(parts)==2) as.numeric(parts[2]) else 0
    expect_lte(abs(100*truth-as.numeric(num)),.5*10^(exponent-decimals)+1e-12)
  }
  families <- list(
    list(prob=function(x,t) vdist_normal_prob(x,type=t,print_plot=FALSE), perc=function(p,t) vdist_normal_perc(p,type=t,print_plot=FALSE), pf=pnorm, qf=qnorm),
    list(prob=function(x,t) vdist_t_prob(x,5,t,FALSE), perc=function(p,t) vdist_t_perc(p,5,t,FALSE), pf=function(x,lower.tail=TRUE) pt(x,5,lower.tail=lower.tail), qf=function(p,lower.tail=TRUE) qt(p,5,lower.tail=lower.tail)),
    list(prob=function(x,t) vdist_chisquare_prob(x,2.5,t,FALSE), perc=function(p,t) vdist_chisquare_perc(p,2.5,t,FALSE), pf=function(x,lower.tail=TRUE) pchisq(x,2.5,lower.tail=lower.tail), qf=function(p,lower.tail=TRUE) qchisq(p,2.5,lower.tail=lower.tail)),
    list(prob=function(x,t) vdist_f_prob(x,5,10,t,FALSE), perc=function(p,t) vdist_f_perc(p,5,10,t,FALSE), pf=function(x,lower.tail=TRUE) pf(x,5,10,lower.tail=lower.tail), qf=function(p,lower.tail=TRUE) qf(p,5,10,lower.tail=lower.tail)))
  for (family in families) for (type in c("lower","upper")) for (p in c(.95,.99996839427)) {
    x <- family$qf(p,lower.tail=type=="lower")
    for (g in list(family$prob(x,type),family$perc(p,type))) {
      cut <- as.numeric(regmatches(g$labels$subtitle,
        regexec("P\\(X [<>] ([^)]+)\\)",g$labels$subtitle))[[1]][2])
      labels <- label_values(g)
      expect_gte(length(labels),2)
      check_label(labels[if (type == "lower") 1 else 2], family$pf(cut,lower.tail=type=="lower"))
      check_label(labels[if (type == "lower") 2 else 1], family$pf(cut,lower.tail=type!="lower"))
    }
  }
})


test_that("large translated normal ticks remain distinct in all builders", {
  for (mean in c(1e16,1e200)) {
    sd <- mean * 1e-10
    plots <- list(vdist_normal_plot(mean,sd,FALSE),
      vdist_normal_perc(.5,mean,sd,print_plot=FALSE),
      vdist_normal_prob(mean,mean,sd,print_plot=FALSE))
    for (g in plots) {
      labels <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]$x$get_labels()
      labels <- labels[!is.na(labels)]
      expect_equal(length(unique(labels)),length(labels))
    }
    expect_equal(pnorm(mean,mean,sd),.5)
    expect_equal(pnorm(mean+sd,mean,sd),pnorm(1),tolerance=1e-6)
  }
})

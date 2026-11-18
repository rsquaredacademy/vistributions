test_that("binomial jump cutoffs follow qbinom and shade its event", {
  cases <- list(c(9,.5,.5),c(1,.01,.01),c(1,.1,.9),c(1,.1,.1))
  for (z in cases) for (type in c("lower", "upper")) {
    n <- z[1]; p <- z[2]; tp <- z[3]; lower <- type == "lower"
    g <- vdist_binom_perc(n,p,tp,type,FALSE)
    k <- qbinom(tp,n,p,lower.tail=lower)
    bars <- ggplot2::layer_data(g,1)
    expect_equal(sum(bars$y[bars$fill == "#0000CD"]),
                 pbinom(k,n,p,lower.tail=lower), tolerance=1e-12)
    expect_match(g$labels$subtitle, paste0(if (lower) "X <= " else "X >= ", k), fixed=TRUE)
    re <- "P\\(X (<=|>=) (-?[0-9]+)\\) (<=|>=|<|>|=) ([^, ]+)"
    clauses <- regmatches(g$labels$subtitle, gregexpr(re, g$labels$subtitle, perl=TRUE))[[1]]
    expect_length(clauses, 2)
    for (clause in clauses) {
      v <- regmatches(clause, regexec(re, clause, perl=TRUE))[[1]]
      j <- as.numeric(v[3]) - as.integer(v[2] == ">=")
      invert <- tp > .5
      actual <- pbinom(j,n,p,lower.tail=if (invert) v[2] != "<=" else v[2] == "<=")
      target <- if (invert) 1-tp else tp
      op <- v[4]
      if (invert) op <- switch(op, "<"=">", ">"="<", "<="=">=", ">="="<=", "="="=")
      expect_true(switch(op, "<"=actual<target, ">"=actual>target,
                         "<="=actual<=target, ">="=actual>=target, "="=actual==target))
    }
  }
})

test_that("two cutoff labels agree with both printed tails", {
  fmt <- vistributions:::vdist_fmt_pct
  for (x in c(1.64049,4.00049, seq(.2,4.2,by=.2)+.00049)) {
    d <- vistributions:::nprob_data_prep(c(-x,x),0,1,"both")
    z <- as.numeric(d$cut_text)
    expect_identical(fmt(pnorm(z[1]))$text,fmt(pnorm(-x))$text)
    expect_identical(fmt(pnorm(z[2],lower.tail=FALSE))$text,fmt(pnorm(x,lower.tail=FALSE))$text)
  }
  for (x in c(5.00049,seq(.5,6,by=.5)+.00049)) for (method in c("interval","both")) {
    d <- vistributions:::tprob_data_prep(x,10,method)
    z <- as.numeric(d$cut_text)
    expect_identical(fmt(pt(-z,10))$text,fmt(pt(-x,10))$text)
    expect_identical(fmt(pt(z,10,lower.tail=FALSE))$text,fmt(pt(x,10,lower.tail=FALSE))$text)
  }
  for (x in c(1.64049,4.00049)) {
    p <- 1-2*pnorm(-x)
    d <- vistributions:::nperc_data_prep(p,0,1,"both")
    expect_identical(fmt(pnorm(as.numeric(d$cut_text[1])))$text,fmt(pnorm(qnorm((1-p)/2)))$text)
    g <- vdist_normal_perc(p, type="both", print_plot=FALSE)
    labels <- unlist(lapply(g$layers, function(layer) layer$aes_params$label))
    expect_identical(unname(labels[2:3]), paste0(c(fmt(pnorm(as.numeric(d$cut_text[1])))$text,
      fmt(pnorm(as.numeric(d$cut_text[2]),lower.tail=FALSE))$text), "%"))
  }
  p <- 1-2*pt(-5.00049,10)
  d <- vistributions:::tperc_data_prep(p,10,"both")
  expect_identical(fmt(pt(as.numeric(d$cut_text[1]),10))$text,fmt(pt(qt((1-p)/2,10),10))$text)
  g <- vdist_t_perc(p,10,"both",FALSE)
  labels <- unlist(lapply(g$layers, function(layer) layer$aes_params$label))
  expect_identical(unname(labels[2:3]), paste0(c(fmt(pt(as.numeric(d$cut_text[1]),10))$text,
    fmt(pt(as.numeric(d$cut_text[2]),10,lower.tail=FALSE))$text), "%"))
})

test_that("F near-one upper quantiles reproduce the smaller tail", {
  for (den in c(1,2,10)) for (p in 1-c(1e-8,1e-9,1e-11)) {
    d <- vistributions:::fperc_data_prep(p,1,den,"upper")
    expect_lt(abs(pf(d$pp,1,den)/(1-p)-1),1e-6)
    expect_false(startsWith(d$prob_text,">"))
    g <- vdist_f_perc(p,1,den,"upper",FALSE)
    expect_false(grepl("> 99.9999999999%",g$labels$subtitle,fixed=TRUE))
  }
})

test_that("F upper views show the cutoff and disclose omitted tails", {
  dfs <- c(lapply(c(1,2,3,5,30,500),function(n)c(n,2)),
           lapply(c(1,2,3,5,30,500),function(n)c(n,3)),list(c(1,4),c(2,4)))
  for (v in dfs) {
    cut <- qf(.5,v[1],v[2])
    for (d in list(vistributions:::fperc_data_prep(.5,v[1],v[2],"upper"),
                   vistributions:::fprob_data_prep(cut,v[1],v[2],"upper"))) {
      expect_gte(d$pc,d$view[1]); expect_lte(d$pc,d$view[2])
      expect_gt((d$view[2]-d$pc)/diff(d$view),.01)
      expect_gt(qf(.75,v[1],v[2])/diff(d$view),.25)
      expect_gt(d$omitted,.01)
      expect_match(vistributions:::vdist_caption_text(d),"outside the plotted range")
    }
  }
})

test_that("certain binomial events, undefined F moments and clipped lines are clear", {
  g <- vdist_binom_prob(10,.3,10,"lower",FALSE)
  expect_equal(pbinom(10,10,.3),1)
  expect_identical(g$labels$subtitle,"P(X <= 10) = 1.000")
  f <- vdist_f_plot(1,2,print_plot=FALSE)
  expect_match(f$labels$x,"Mean = undefined  Std Dev. = undefined",fixed=TRUE)
  g <- vdist_chisquare_perc(1-1e-11,30,print_plot=FALSE)
  expect_gt(qchisq(1-1e-11,30),g$coordinates$limits$x[2])
  expect_match(g$labels$caption,"cutoff line is clipped",ignore.case=TRUE)
})

test_that("NEWS describes the actual percentile limits and split-aware bins", {
  path <- system.file("NEWS.md", package="vistributions")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "NEWS.md")
  news <- paste(readLines(path), collapse="\n")
  expect_false(grepl("at most 2001 equal-width bins", news, fixed=TRUE))
  expect_false(grepl("[1e-6, 1-1e-6]", news, fixed=TRUE))
  expect_match(news,"approximately 2001 bins",fixed=TRUE)
  g <- vdist_binom_prob(10000,.01,101,"exact",FALSE)
  bars <- ggplot2::layer_data(g,1)
  expect_equal(nrow(bars),2003)
  expect_equal(sum(bars$y[bars$fill == "#0000CD"]),dbinom(101,10000,.01),tolerance=1e-10)
  x <- qchisq(1e-8,5)
  expect_equal(pchisq(x,5),1e-8,tolerance=1e-12)
  expect_s3_class(vdist_chisquare_perc(1e-8,5,print_plot=FALSE),"ggplot")
})

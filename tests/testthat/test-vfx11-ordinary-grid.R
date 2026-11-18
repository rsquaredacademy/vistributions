# Ordinary teaching inputs: compare every shaded event with base R. These
# cases cover the stated parameter ranges and every public plot type.
test_that("ordinary distribution grid matches base R", {
  mass <- function(g) {
    layers <- Filter(function(l) inherits(l$geom, "GeomPolygon") &&
      (identical(l$aes_params$fill, "#0000CD") ||
       identical(l$geom_params$fill, "#0000CD")), g$layers)
    sum(vapply(layers, function(l) vistributions:::vdist_area(l$data$x, l$data$y), numeric(1)))
  }
  check_area <- function(g, truth, id) {
    expect_lt(abs(mass(g) / truth - 1), 1e-6, label = id)
    text <- regmatches(g$labels$subtitle,
      regexec("= ([0-9.eE+-]+)%", g$labels$subtitle))[[1]]
    expect_equal(length(text), 2L, info = id)
    if (length(text) == 2L) {
      shown <- as.numeric(text[2])
      bits <- strsplit(tolower(text[2]), "e", fixed=TRUE)[[1]]
      decimals <- if (grepl("\\.", bits[1])) nchar(sub("^[^.]*\\.", "", bits[1])) else 0
      exponent <- if (length(bits) == 2) as.numeric(bits[2]) else 0
      half_unit <- .5 * 10^(exponent - decimals)
      expect_lte(abs(100 * truth - shown), half_unit + 1e-10, label = id)
    }
  }
  shape_mass <- function(g) {
    polygons <- Filter(function(l) inherits(l$geom, "GeomPolygon"), g$layers)
    sum(vapply(polygons, function(l) {
      d <- l$data
      if (all(c("y", "z") %in% names(d))) vistributions:::vdist_area(d$y,d$z)
      else vistributions:::vdist_area(d$x,d$y)
    }, numeric(1)))
  }
  probs <- c(.01, .05, .25, .5, .75, .95, .99)
  for (m in c(0, 1, 10, 100, 1000)) for (sd in c(.01, .1, 1, 10, 100, 1000)) {
    g <- vdist_normal_plot(m, sd, FALSE)
    bands <- Filter(function(l) inherits(l$geom, "GeomPolygon"), g$layers)
    bounds <- list(c(-1,1), c(-2,-1), c(-3,-2), c(1,2), c(2,3))
    for (i in seq_along(bands)) {
      got <- vistributions:::vdist_area(bands[[i]]$data$x,bands[[i]]$data$y)
      expected <- diff(pnorm(bounds[[i]]))
      expect_lt(abs(got / expected - 1),1e-6,label=paste("normal band",m,sd,i))
    }
    for (p in probs) for (type in c("lower", "upper", "both"))
      check_area(vdist_normal_perc(p, m, sd, type, FALSE), p,
                 paste("normal percentile", m, sd, p, type))
    for (z in c(-1.5, 0, 1.5)) for (type in c("lower", "upper")) {
      x <- m + sd * z
      truth <- pnorm(x, m, sd, lower.tail = type == "lower")
      check_area(vdist_normal_prob(x, m, sd, type, FALSE), truth,
                 paste("normal probability", m, sd, z, type))
    }
    x <- m + sd * c(-1.2, .8)
    check_area(vdist_normal_prob(x, m, sd, "both", FALSE), diff(pnorm(x, m, sd)),
               paste("normal interval", m, sd))
  }
  for (df in c(1, 1.5, 5.5, 30, 100.25, 500)) {
    g <- vdist_t_plot(df, FALSE)
    expect_lt(abs(shape_mass(g) / diff(pt(c(-4,4),df)) - 1),1e-6,label=paste("t plot",df))
    for (p in probs) for (type in c("lower", "upper", "both"))
      check_area(vdist_t_perc(p, df, type, FALSE), p, paste("t percentile", df, p, type))
    for (x in c(-1.5, .5, 2.1)) for (type in c("lower", "upper", "interval", "both")) {
      truth <- switch(type, lower=pt(x,df), upper=pt(x,df,lower.tail=FALSE),
                      interval=diff(pt(c(-abs(x),abs(x)),df)), both=2*pt(-abs(x),df))
      check_area(vdist_t_prob(x, df, type, FALSE), truth, paste("t probability",df,x,type))
    }
    g <- vdist_chisquare_plot(df, TRUE, print_plot=FALSE)
    truth <- diff(pchisq(g$coordinates$limits$x,df))
    expect_lt(abs(shape_mass(g) / truth - 1),1e-6,label=paste("chi plot",df))
    for (p in probs) for (type in c("lower", "upper"))
      check_area(vdist_chisquare_perc(p, df, type, FALSE), p, paste("chi percentile",df,p,type))
    for (p in c(.1,.5,.9)) for (type in c("lower", "upper")) {
      x <- qchisq(p,df)
      check_area(vdist_chisquare_prob(x,df,type,FALSE),
                 pchisq(x,df,lower.tail=type=="lower"),paste("chi probability",df,p,type))
    }
  }
  for (dfs in list(c(1,1),c(1,500),c(500,1),c(3.5,7.25),c(10,50),c(200,500),c(500,500))) {
    a <- dfs[1]; b <- dfs[2]
    g <- suppressMessages(vdist_f_plot(a,b,TRUE,FALSE))
    expect_lt(abs(shape_mass(g) / pf(4,a,b) - 1),1e-6,label=paste("F plot",a,b))
    for (p in probs) for (type in c("lower", "upper"))
      check_area(vdist_f_perc(p,a,b,type,FALSE),p,paste("F percentile",a,b,p,type))
    for (p in c(.1,.5,.9)) for (type in c("lower", "upper")) {
      x <- qf(p,a,b)
      check_area(vdist_f_prob(x,a,b,type,FALSE),pf(x,a,b,lower.tail=type=="lower"),
                 paste("F probability",a,b,p,type))
    }
  }
  bars <- function(g) {d <- ggplot2::layer_data(g,1); sum(d$y[d$fill == "#0000CD"])}
  for (n in c(1,10,100,2000,5000,10000)) for (p in c(.01,.1,.5,.9,.99)) {
    expect_s3_class(vdist_binom_plot(n,p,FALSE), "ggplot")
    k <- round(n*p)
    for (type in c("lower","upper","exact","interval")) {
      x <- if (type == "interval") c(max(0,k-1), min(n,k+1)) else k
      truth <- switch(type, lower=pbinom(k,n,p), upper=pbinom(k-1,n,p,lower.tail=FALSE),
                      exact=dbinom(k,n,p), interval=sum(dbinom(x[1]:x[2],n,p)))
      expect_equal(bars(vdist_binom_prob(n,p,x,type,FALSE)),truth,tolerance=1e-10,
                   info=paste("binomial probability",n,p,type))
    }
    for (tp in probs) for (type in c("lower","upper")) {
      lower <- type == "lower"
      k <- qbinom(tp,n,p,lower.tail=lower)
      ks <- unique(pmax(0,pmin(n,k+c(-1,0,1))))
      jumps <- pbinom(ks,n,p,lower.tail=lower)
      if (!any(tp == jumps) && any(abs(tp-jumps) <= 1e-12)) {
        expect_error(vdist_binom_perc(n,p,tp,type,FALSE),"tp.*1e-12")
      } else {
        truth <- pbinom(k,n,p,lower.tail=lower)
        expect_equal(bars(vdist_binom_perc(n,p,tp,type,FALSE)),truth,tolerance=1e-10,
                     info=paste("binomial percentile",n,p,tp,type))
      }
    }
  }
})

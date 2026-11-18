test_that("unrepresentable central normal mass is rejected with the documented remedy", {
  expect_error(vdist_normal_perc(.01,1,1e-10,"both",FALSE),
               "probs.*central probability.*cannot be represented at this scale.*sd.*larger sd or probs")
  expect_error(vdist_normal_perc(1.01e-12,1000,.01,"both",FALSE),
               "probs.*central probability.*cannot be represented at this scale.*sd.*larger sd or probs")
})

test_that("ordinary central normal percentiles never trigger the resolution error", {
  for (m in c(0,1,10,100,1000)) for (sd in c(.01,.1,1,10,100,1000)) {
    for (p in c(.01,.05,.25,.5,.75,.95,.99)) {
      g <- vdist_normal_perc(p,m,sd,"both",FALSE)
      dark <- Filter(function(layer) inherits(layer$geom,"GeomPolygon") &&
        identical(layer$aes_params$fill,"#0000CD"),g$layers)
      area <- sum(vapply(dark,function(layer)
        vistributions:::vdist_area(layer$data$x,layer$data$y),numeric(1)))
      expect_lt(abs(area/p-1),1e-6)
    }
  }
})

test_that("F quantiles invert the requested tail over the supported grid", {
  for (dfs in list(c(1,1),c(1,2),c(2,1),c(5,10))) {
    a <- dfs[1]; b <- dfs[2]
    for (p in c(1e-9,1e-8,.01,.5,.99)) for (type in c("lower","upper")) {
      lower <- type == "lower"
      g <- vdist_f_perc(p,a,b,type,FALSE)
      lines <- which(vapply(g$layers,function(layer) inherits(layer$geom,"GeomVline"),logical(1)))
      built <- ggplot2::ggplot_build(g)
      inverse <- built$layout$panel_scales_x[[1]]$get_transformation()$inverse
      cut <- inverse(ggplot2::layer_data(built,lines[1])$xintercept[1])
      expect_lt(abs(pf(cut,a,b,lower.tail=lower)/p-1),1e-6,
                label=paste("F quantile",a,b,p,type))
      dark <- Filter(function(layer) inherits(layer$geom,"GeomPolygon") &&
        identical(layer$aes_params$fill,"#0000CD"),g$layers)
      mass <- sum(vapply(dark,function(layer)
        vistributions:::vdist_area(layer$data$x,layer$data$y),numeric(1)))
      expect_lt(abs(mass/p-1),1e-6,label=paste("F area",a,b,p,type))
    }
  }
})

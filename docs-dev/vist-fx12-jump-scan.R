pkgload::load_all('.',quiet=TRUE)
check <- function(n,p,tp,type) {
  lower <- type == 'lower'
  d <- bperc_data_prep(n,p,tp,type)
  expected <- qbinom(tp,n,p,lower.tail=lower)
  if (d$k != expected) stop('qbinom mismatch')
  mass <- sum(d$plot_data$df[d$cols == '#0000CD'])
  if (abs(mass-pbinom(expected,n,p,lower.tail=lower)) > 1e-10) stop('mass mismatch')
  g <- bperc_plot_build(d,type,n,p,tp)
  text <- g$labels$subtitle
  re <- 'P\\(X (<=|>=) (-?[0-9]+)\\) (<=|>=|<|>|=) ([^, ]+)'
  matches <- regmatches(text,gregexpr(re,text,perl=TRUE))[[1]]
  if (length(matches)!=2) stop(paste('caption parse', text))
  for (m in matches) {
    parts <- regmatches(m,regexec(re,m,perl=TRUE))[[1]]
    j <- as.numeric(parts[3]) - as.integer(parts[2] == '>=')
    lower_event <- parts[2] == '<='
    inv <- tp > .5
    actual <- pbinom(j,n,p,lower.tail=if(inv) !lower_event else lower_event)
    target <- if(inv) 1-tp else tp
    op <- parts[4]
    if (inv) op <- switch(op,'<'='>','>'='<','<='='>=','>='='<=','='='=')
    good <- switch(op,'<'=actual<target,'>'=actual>target,'<='=actual<=target,'>='=actual>=target,'='=actual==target)
    if (!good) stop(paste('caption false',n,p,format(tp,digits=17),type,text))
  }
}
ns <- c(1,2,3,5,9,10,20,50,100,500,1000,10000)
ps <- c(.01,.1,.25,.3,.5,.7,.9,.99)
probs <- c(.01,.1,.25,.5,.75,.9,.99)
offsets <- c(-1e-10,-2e-12,-1e-12,-.Machine$double.eps,0,.Machine$double.eps,1e-12,2e-12,1e-10)
count <- 0L; bad <- 0L; first <- NULL
for(n in ns) for(p in ps) for(pr in probs) for(type in c('lower','upper')) for(off in offsets) {
  k <- qbinom(pr,n,p,lower.tail=type=='lower')
  jump <- pbinom(k,n,p,lower.tail=type=='lower')
  tp <- min(1-1e-10,max(1e-10,jump+off))
  result <- tryCatch({check(n,p,tp,type); NULL},error=function(e) conditionMessage(e))
  count <- count+1L; if(!is.null(result)) {bad <- bad+1L; if(is.null(first)) first<-result}
}
cat('jump scan',count,'failures',bad,'first',first,'\n')
count <- bad <- 0L; first <- NULL
for(p in seq(.01,.99,.01)) for(tp in c(p,1-p)) for(type in c('lower','upper')) {
  result <- tryCatch({check(1,p,tp,type); NULL},error=function(e) conditionMessage(e))
  count <- count+1L; if(!is.null(result)) {bad <- bad+1L; if(is.null(first)) first<-result}
}
cat('Bernoulli scan',count,'failures',bad,'first',first,'\n')

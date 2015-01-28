#options(shiny.trace = TRUE)

#check for and/or install dependencies
need<-c("shiny","googleVis") # some more should auto load...
for(i in 1:length(need)){
  if(require(need[i], character.only = TRUE)==FALSE){install.packages(need[i]);library(need[i], character.only = TRUE)} else { library(need[i],character.only = TRUE)}
}

library(googleVis)
library(shiny)


# load code for custom functions
#source("plot.R") # for using ggplot

#data <- read.csv("dataset.csv",headers=T)

pwr.2p2n.test <- function (h = NULL, n1 = NULL, n2 = NULL, sig.level = 0.05, 
                           power = NULL, alternative = c("two.sided", "less", "greater")){
  if (sum(sapply(list(h, n1, n2, power, sig.level), is.null)) != 
        1) 
    stop("exactly one of h, n1, n2, power, and sig.level must be NULL")
  if (!is.null(n1) && n1 < 2) 
    stop("number of observations in the first group must be at least 2")
  if (!is.null(n2) && n2 < 2) 
    stop("number of observations in the second group must be at least 2")
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
                                                             sig.level | sig.level > 1)) 
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 > power | power > 1)) 
    stop(sQuote("power"), " must be numeric in [0, 1]")
  alternative <- match.arg(alternative)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)
  if (tside == 2 && !is.null(h)) 
    h <- abs(h)
  if (tside == 3) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = FALSE) - h * sqrt((n1 * n2)/(n1 + n2)), lower = FALSE)
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = TRUE) - h * sqrt((n1 * n2)/(n1 + n2)), lower = TRUE)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      pnorm(qnorm(sig.level/2, lower = FALSE) - h * sqrt((n1 *  n2)/(n1 + n2)), lower = FALSE) + pnorm(qnorm(sig.level/2, 
              lower = TRUE) - h * sqrt((n1 * n2)/(n1 + n2)), lower = TRUE)
    })
  }
  if (is.null(power)){
    power <- eval(p.body) 
  }else if (is.null(h)) {
    if (tside == 2) {
      h <- uniroot(function(h) eval(p.body) - power, c(1e-10, 10))$root
    }
    if (tside == 1) {
      h <- uniroot(function(h) eval(p.body) - power, c(-10, 5))$root
    }
    if (tside == 3) {
      h <- uniroot(function(h) eval(p.body) - power, c(-5,  10))$root
    }
  }else if (is.null(n1)){
    n1 <- uniroot(function(n1) eval(p.body) - power, c(2 + 1e-10, 1e+05))$root
  }else if (is.null(n2)){
    n2 <- uniroot(function(n2) eval(p.body) - power, c(2 +  1e-10, 1e+05))$root
  } else if (is.null(sig.level)){
    res <- class(try(sig.level <-uniroot(function(sig.level) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root,T))
    if(res=="try-error"){
      sig.level <- 0
    }
  }else{
    stop("internal error")
  } 
  NOTE <- "different sample sizes"
  METHOD <- "difference of proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(h = h, n1 = n1, n2 = n2, sig.level = sig.level, 
                 power = power, alternative = alternative, method = METHOD, 
                 note = NOTE), class = "power.htest")
}

pwr.2p.test <- function (h = NULL, n = NULL, sig.level = 0.05, power = NULL, 
                         alternative = c("two.sided", "less", "greater")) {
  if (sum(sapply(list(h, n, power, sig.level), is.null)) != 
        1) 
    stop("exactly one of h, n, power, and sig.level must be NULL")
  if (!is.null(n) && n < 1) 
    stop("number of observations in each group must be at least 1")
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
                                                             sig.level | sig.level > 1)) 
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 > power | 
                                                     power > 1)) 
    stop(sQuote("power"), " must be numeric in [0, 1]")
  alternative <- match.arg(alternative)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)
  if (tside == 2 && !is.null(h)) 
    h <- abs(h)
  if (tside == 3) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = FALSE) - h * sqrt(n/2), lower = FALSE)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      pnorm(qnorm(sig.level/2, lower = FALSE) - h * sqrt(n/2), 
            lower = FALSE) + pnorm(qnorm(sig.level/2, lower = TRUE) - 
                                     h * sqrt(n/2), lower = TRUE)
    })
  }
  if (tside == 1) {
    p.body <- quote({
      pnorm(qnorm(sig.level, lower = TRUE) - h * sqrt(n/2), lower = TRUE)
    })
  }
  if (is.null(power)) 
    power <- eval(p.body)
  else if (is.null(h)) {
    if (tside == 2) {
      h <- uniroot(function(h) eval(p.body) - power, c(1e-10, 10))$root
    }
    if (tside == 1) {
      h <- uniroot(function(h) eval(p.body) - power, c(-10, 5))$root
    }
    if (tside == 3) {
      h <- uniroot(function(h) eval(p.body) - power, c(-5,  10))$root
    }
  }
  else if (is.null(n)) 
    n <- uniroot(function(n) eval(p.body) - power, c(2 + 
                                                       1e-10, 1e+05))$root
  else if (is.null(sig.level)) 
    sig.level <- uniroot(function(sig.level) eval(p.body) - 
                           power, c(1e-10, 1 - 1e-10))$root
  else stop("internal error")
  NOTE <- "same sample sizes"
  METHOD <- "Difference of proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(h = h, n = n, sig.level = sig.level, power = power, 
                 alternative = alternative, method = METHOD, note = NOTE), 
            class = "power.htest")
}

sim_post <- function (x, n, alpha = 1, beta = 1, ndraws = 10000){
  k <- length(x)
  ans <- matrix(nrow = ndraws, ncol = k)
  no = n - x
  for (i in (1:k)) ans[, i] = rbeta(ndraws, x[i] + alpha, no[i] + beta)
  return(ans)
}

prob_winner <- function (post){
  k = ncol(post)
  w = table(factor(max.col(post), levels = 1:k))
  return(w/sum(w))
}
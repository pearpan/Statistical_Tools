#options(shiny.trace = TRUE)

#check for and/or install dependencies
need<-c("shiny") # some more should auto load...
for(i in 1:length(need)){
  if(require(need[i], character.only = TRUE)==FALSE){install.packages(need[i]);library(need[i], character.only = TRUE)} else { library(need[i],character.only = TRUE)}
}

library(shiny)


# load code for custom functions
#source("plot.R") # for using ggplot

#data <- read.csv("dataset.csv",headers=T)

pwr.2p2n.test <- function (h = NULL, n1 = NULL, n2 = NULL, sig.level = 0.05, 
                           power = NULL, alternative = c("two.sided", "less", "greater")){
  if (sum(sapply(list(h, n1, n2, power, sig.level), is.null)) !=  1) 
    stop("exactly one of h, n1, n2, power, and sig.level must be NULL")
  if (!is.null(n1) && n1 < 2) 
    stop("number of observations in the first group must be at least 2")
  if (!is.null(n2) && n2 < 2) 
    stop("number of observations in the second group must be at least 2")
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)) 
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
    n1 <- uniroot(function(n1) eval(p.body) - power, c(2 + 1e-10, 1e+10))$root
  }else if (is.null(n2)){
    n2 <- uniroot(function(n2) eval(p.body) - power, c(2 +  1e-10, 1e+10))$root
  } else if (is.null(sig.level)){
    sig.level <-uniroot(function(sig.level) eval(p.body) - power, c(0, 1))$root
  }else{
    stop("internal error")
  } 
  
  METHOD <- "difference of proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(h = h, n1 = n1, n2 = n2, sig.level = round(sig.level,3), 
                 power = round(power,3), alternative = alternative, method = METHOD), class = "power.htest")
}

pwr.2p.test <- function (h = NULL, n = NULL, sig.level = 0.05, power = NULL, 
                         alternative = c("two.sided", "less", "greater")) {
  if (sum(sapply(list(h, n, power, sig.level), is.null)) != 1) 
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
    n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+10))$root
  else if (is.null(sig.level)) 
    sig.level <- uniroot(function(sig.level) eval(p.body) - power, c(0, 1))$root
  else stop("internal error")
  NOTE <- "same sample sizes"
  METHOD <- "Difference of proportion power calculation for binomial distribution (arcsine transformation)"
  structure(list(h = h, n = n, sig.level = round(sig.level,3), power = round(power,3), 
                 alternative = alternative, method = METHOD), class = "power.htest")
}

pwr.t2n.test <- function (n1 = NULL, n2 = NULL, d = NULL, sig.level = 0.05, 
                          power = NULL, alternative = c("two.sided", "less", "greater")){
  if (sum(sapply(list(n1, n2, d, power, sig.level), is.null)) != 1) 
    stop("exactly one of n1, n2, d, power, and sig.level must be NULL")
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)) 
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 > power | power > 1)) 
    stop(sQuote("power"), " must be numeric in [0, 1]")
  if (!is.null(n1) && n1 < 2) 
    stop("number of observations in the first group must be at least 2")
  if (!is.null(n2) && n2 < 2) 
    stop("number of observations in the second group must be at least 2")
  alternative <- match.arg(alternative)
  tsample <- 2
  ttside <- switch(alternative, less = 1, two.sided = 2, greater = 3)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 1)
  if (tside == 2 && !is.null(d)) 
    d <- abs(d)
  if (ttside == 1) {
    p.body <- quote({
      nu <- n1 + n2 - 2
      pt(qt(sig.level/tside, nu, lower = TRUE), nu, ncp = d * 
           (1/sqrt(1/n1 + 1/n2)), lower = TRUE)
    })
  }
  if (ttside == 2) {
    p.body <- quote({
      nu <- n1 + n2 - 2
      qu <- qt(sig.level/tside, nu, lower = FALSE)
      pt(qu, nu, ncp = d * (1/sqrt(1/n1 + 1/n2)), lower = FALSE) + 
        pt(-qu, nu, ncp = d * (1/sqrt(1/n1 + 1/n2)), lower = TRUE)
    })
  }
  if (ttside == 3) {
    p.body <- quote({
      nu <- n1 + n2 - 2
      pt(qt(sig.level/tside, nu, lower = FALSE), nu, ncp = d * 
           (1/sqrt(1/n1 + 1/n2)), lower = FALSE)
    })
  }
  if (is.null(power)) 
    power <- eval(p.body)
  else if (is.null(n1)) 
    n1 <- uniroot(function(n1) eval(p.body) - power, c(2 + 1e-10, 1e+10))$root
  else if (is.null(n2)) 
    n2 <- uniroot(function(n2) eval(p.body) - power, c(2 + 1e-10, 1e+10))$root
  else if (is.null(d)) {
    if (ttside == 2) {
      d <- uniroot(function(d) eval(p.body) - power, c(1e-07, 10))$root
    }
    if (ttside == 1) {
      d <- uniroot(function(d) eval(p.body) - power, c(-10, 5))$root
    }
    if (ttside == 3) {
      d <- uniroot(function(d) eval(p.body) - power, c(-5, 10))$root
    }
  }
  else if (is.null(sig.level)) 
    sig.level <- uniroot(function(sig.level) eval(p.body) -  power, c(0, 1))$root
  else stop("internal error")
  METHOD <- c("t test power calculation")
  structure(list(n1 = n1, n2 = n2, d = d, sig.level = round(sig.level,3), 
                 power = round(power,3), alternative = alternative, method = METHOD), 
            class = "power.htest")
}

pwr.t.test <- function (n = NULL, d = NULL, sig.level = 0.05, power = NULL, 
                        type = c("two.sample", "one.sample", "paired"), 
                        alternative = c("two.sided", "less", "greater")){
  if (sum(sapply(list(n, d, power, sig.level), is.null)) !=  1) 
    stop("exactly one of n, d, power, and sig.level must be NULL")
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 >  sig.level | sig.level > 1)) 
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 > power |  power > 1)) 
    stop(sQuote("power"), " must be numeric in [0, 1]")
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
  ttside <- switch(alternative, less = 1, two.sided = 2, greater = 3)
  tside <- switch(alternative, less = 1, two.sided = 2, greater = 1)
  if (tside == 2 && !is.null(d)) 
    d <- abs(d)
  if (ttside == 1) {
    p.body <- quote({
      nu <- (n - 1) * tsample
      pt(qt(sig.level/tside, nu, lower = TRUE), nu, ncp = sqrt(n/tsample) * d, lower = TRUE)
    })
  }
  if (ttside == 2) {
    p.body <- quote({
      nu <- (n - 1) * tsample
      qu <- qt(sig.level/tside, nu, lower = FALSE)
      pt(qu, nu, ncp = sqrt(n/tsample) * d, lower = FALSE) + 
        pt(-qu, nu, ncp = sqrt(n/tsample) * d, lower = TRUE)
    })
  }
  if (ttside == 3) {
    p.body <- quote({
      nu <- (n - 1) * tsample
      pt(qt(sig.level/tside, nu, lower = FALSE), nu, ncp = sqrt(n/tsample) * d, lower = FALSE)
    })
  }
  if (is.null(power)) 
    power <- eval(p.body)
  else if (is.null(n)) 
    n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+10))$root
  else if (is.null(d)) {
    if (ttside == 2) {
      d <- uniroot(function(d) eval(p.body) - power, c(1e-07, 10))$root
    }
    if (ttside == 1) {
      d <- uniroot(function(d) eval(p.body) - power, c(-10, 5))$root
    }
    if (ttside == 3) {
      d <- uniroot(function(d) eval(p.body) - power, c(-5, 10))$root
    }
  }
  else if (is.null(sig.level)) 
    sig.level <- uniroot(function(sig.level) eval(p.body) -  power, c(0, 1))$root
  else stop("internal error")
  NOTE <- switch(type, paired = "n is number of *pairs*",  two.sample = "n is number in *each* group", NULL)
  METHOD <- paste(switch(type, one.sample = "One-sample", 
                         two.sample = "Two-sample", paired = "Paired"), "t test power calculation")
  structure(list(n = n, d = d, sig.level = round(sig.level,3), power = round(power,3), 
                 alternative = alternative, note = NOTE, method = METHOD), class = "power.htest")
}


sim_post <- function (x, n, alpha = 1, beta = 1, ndraws = 100000){
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

ES.h <- function (p1, p2){
  2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
}

win_prob_test <- function (x, n){
  num_groups = length(x)
  p = x/n
  my_rank = rank(-p)
  pt = prop.test(x = x, n = n)
  lower = rep(NA, num_groups)
  upper = rep(NA, num_groups)
  significance = rep(NA, num_groups)
  best = rep(0, num_groups)
  b = best_binomial_bandit(x, n)
  if (pt$p.value < 0.05) {
    is_best = 1
    for (cur_rank in sort(unique(my_rank))) {
      cur_indices = which(my_rank == cur_rank)
      if (length(cur_indices) >= 1) {
        cur_index = max(cur_indices)
        ranks_above = my_rank[my_rank > cur_rank]
        if (length(ranks_above) > 0) {
          comparison_index = min(which(my_rank == min(ranks_above)))
          pt = prop.test(x = x[c(cur_index, comparison_index)], 
                         n = n[c(cur_index, comparison_index)], conf.level = (1 - 0.05))
          significance[cur_indices] = pt$p.value
          lower[cur_indices] = pt$conf.int[1]
          upper[cur_indices] = pt$conf.int[2]
          best[cur_indices] = is_best
          if (pt$p.value < 0.05) {
            is_best = 0
          }
        }
      }
    }
  }
  return(data.frame(successes = x, totals = n, estimated_proportion = p, 
                    lower = lower, upper = upper, significance = significance, 
                    rank = my_rank, best = best, p_best = b))
}

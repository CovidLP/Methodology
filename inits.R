pars <- names(init$fit)
all_iterations <- as.array(init$fit)
last_iter <- colMeans(all_iterations[, 1, ])

a.old = last_iter[is.element(pars, paste("a[", 1:init$n_waves, "]", sep = ""))]
b.old = last_iter[is.element(pars, paste("b[", 1:init$n_waves, "]", sep = ""))]
c.old = last_iter[is.element(pars, paste("c[", 1:init$n_waves, "]", sep = ""))]
alpha.old = last_iter[is.element(pars, paste("alpha[", 1:init$n_waves, "]", sep = ""))]
delta.old = last_iter[is.element(pars, paste("delta[", 1:init$n_waves, "]", sep = ""))]

genLog = function(t, a, b, c) log(a)+log(c)-(c*t)-2 * log( b+exp(-c*t) )
probit = function(t, alpha, delta) pnorm(alpha * (t - delta), log = TRUE)

n = nrow(init$Y$data)

mu.w = vector(mode = "list", length = init$n_waves) %>% lapply(function(x) rep(0, n))
for(time in 1:n){
  for (curve in 1:init$n_waves){
    mu.w[[curve]][time] =  + exp(
      probit(time, alpha.old[curve], delta.old[curve]) + 
        genLog(time, a.old[curve], b.old[curve], c.old[curve]) )
  }
}

if(extension == "n"){
  date.aux = which(init$Y$data$date == last.date)
  par(mfrow = c(2, ceiling(init$n_waves/2)))
  for(wave in 1:init$n_waves){
    ts.plot(init$Y$data$new_cases, col = "gray50", main = wave)
    points(seq(1, n), mu.w[[wave]], col = "red", type = "l", lwd = 2)
    abline(v = date.aux, col = "blue", lwd = 2)
  }
} else{
  date.aux = which(init$Y$data$date == last.date)
  par(mfrow = c(2, ceiling(init$n_waves/2)))
  for(wave in 1:init$n_waves){
    ts.plot(init$Y$data$new_deaths, col = "gray50", main = wave)
    points(seq(1, n), mu.w[[wave]], col = "red", type = "l", lwd = 2)
    abline(v = date.aux, col = "blue", lwd = 2)
  }
}

nwaves = function(data, new_cases = TRUE){
  
  data = list(data)
  
  if(new_cases){
    
    npeaks = data %>% lapply(function(x) x$data$new_cases) %>%
      lapply(function(x) nrow(findpeaks(x, minpeakdistance  = 95)))
    
  } else{
    
    npeaks = data %>% lapply(function(x) x$data$new_deaths) %>%
      lapply(function(x) nrow(findpeaks(x, minpeakdistance  = 95)))
    
  }
  
  npeaks %>% unlist
  
}

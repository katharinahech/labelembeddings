# model functions ------------------------------------

log_posterior <- function(z_i, mu, sigma, y_i){
  J <- sum(y_i)
  log_post <- log_prior(z_i, mu=mu, sigma=sigma) + log_likelihood(z_i, y_i)
  return(log_post)
}

log_prior <- function(z, mu, sigma){
  p <- mvtnorm::dmvnorm(z, mean=mu, sigma=sigma, log=TRUE)
  return(p)
}

log_likelihood <- function(z, y){
  ll <- extraDistr::ddirmnom(y, sum(y), alpha=exp(z), log = TRUE)
  return(ll)
}

# algorithm functions ---------------------------------

tune_acceptance <- function(y, 
                            mu_current, sigma_current,
                            tune_val, optim_val='Nelder-Mead', seed_val=123,
                            burnin_val=100, mcmc_val=2000, thin_val=20){
  
  sample <- MCMCmetrop1R(log_posterior, 
                         mu=mu_current,
                         sigma=sigma_current,
                         y_i=y[1,],
                         theta.init=mu_current,
                         logfun=TRUE, 
                         burnin=burnin_val,mcmc=mcmc_val,thin=thin_val,
                         tune=tune_val,
                         optim.method=optim_val,
                         seed=seed_val
  )
}

mcmc_simulation <- function(y,
                           z_current,
                           mu_current, sigma_current,
                           tune_val, optim_val, seed_val=123,
                           burnin_val=100, mcmc_val=2000, thin_val=20){
  
  K <- ncol(y)
  
  z_hat <- matrix(data=NA, nrow=nrow(y), ncol=K)
  z_samples <- list()
  
  for (i in 1:nrow(y)){
    z_current_i <- z_current[i,]
    y_i <- y[i,]
    J <- sum(y_i)
    
    R.utils::captureOutput(expr={
      
      sample <- MCMCmetrop1R(log_posterior, 
                             mu=mu_current,
                             sigma=sigma_current+diag(0.001,K,K),
                             y_i=y_i,
                             theta.init=z_current_i,
                             logfun=TRUE, 
                             burnin=burnin_val,mcmc=mcmc_val,thin=thin_val,
                             tune=tune_val,
                             optim.method=optim_val,
                             seed=seed_val
      )
    })
    
    z_hat[i,] <- colMeans(sample)
    z_samples[[i]] <- sample
  }
  
  return(list(z_hat, z_samples))
  
}

m_step <- function(z_hat, weights){
  
  mu <- apply(z_hat, 2, function(x) weighted.mean(x, weights))
  sigma <- (1/(nrow(z_hat)-1))*(t((sweep(z_hat,2,mu)))%*%(sweep(z_hat,2,mu)))
  
  return(list(mu, sigma))
  
}

train_once <- function(y, weights,
                       mu_init, sigma_init,
                       max_iter=50, rtol=0.003, 
                       seed_val=123,
                       tune_val=1.5, optim_val='Nelder-Mead', 
                       burnin_val=100, mcmc_val=5000, thin_val=50,
                       fixed_mu = TRUE){
  
  # empty list for saving results
  z_hat_list <- list()
  mcmc_list <- list()
  
  # initialize parameters
  K <- ncol(y)
  mu_current <- mu_init
  sigma_current <- sigma_init
  z_current <- matrix(0,nrow(y),ncol(y))
  loss_current <- -1000
  
  # EM iterations
  for (iter in 1:max_iter){
      
    loss_prev <- loss_current
      
    print(paste0('Iteration ', iter))
    
    tryCatch(
      mcmc_results <- mcmc_simulation(y, 
                                     z_current,
                                     mu_current, sigma_current, 
                                     tune_val, optim_val, 
                                     seed_val, 
                                     burnin_val, mcmc_val, thin_val) , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    # save results
    z_hat_list[[iter]] <- mcmc_results[[1]]
    z_current <- mcmc_results[[1]]
    
    mcmc_list[[iter]] <- mcmc_results[[2]]
    
    # M step
    estimates <- m_step(z_current, weights)
    
    if (fixed_mu == FALSE){
      mu_current <- estimates[[1]]
      sigma_current <- estimates[[2]]
    }
    else{
      mu_current <- mu_init
      sigma_current <- estimates[[2]]
    }
    
    # compute loss
    loss_current <- compute_loss(z_current, estimates[[1]], estimates[[2]], y)
    
    if (abs((loss_prev - loss_current)/loss_prev) < rtol){
      break
    }
    print(loss_current)
    print(sigma_current)
    
  }
  
  return(list(z_current, loss_current, z_hat_list, mcmc_list))
  
}

compute_loss <- function(z_hat, mu_hat, sigma_hat, y_patterns){
  loss <- c()
  for (i in nrow(y_patterns)){
    x <- log_posterior(z_hat[i,], mu_hat, sigma_hat, y_patterns[i,])
    loss <- c(loss,x)
  }
  return(sum(loss))
}

fit_model <- function(y, weights,
                      mu_init, sigma_init,
                      restarts=10,
                      max_iter=50, rtol=0.1, 
                      tune_val=1.5, optim_val='Nelder-Mead', seed_val=123,
                      burnin_val=100, mcmc_val=5000, thin_val=50,
                      fixed_mu = TRUE){
  
  best_loss <- -Inf
  
  for (r in 1:restarts){
    
    print(paste0('Restart ', r))
    
    # change seed for each restart
    r_seed <- round(seed_val + rnorm(1, mean=50, sd=10))
    r_results <- train_once(y, weights, mu_init, sigma_init, 
                            seed_val = r_seed,
                            max_iter, rtol, tune_val, optim_val, 
                            burnin_val, mcmc_val, thin_val, fixed_mu)
    r_loss <- r_results[[2]]
    
    # assign current values to smallest loss
    if (r_loss > best_loss){
      best_results <- r_results
      best_loss <- r_loss
    }
  }
  
  return(best_results)
  
}



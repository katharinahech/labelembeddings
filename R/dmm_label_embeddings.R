# Dirichlet Multinomial Model

likelihood <- function(z, y_i, J){
  alpha = exp(z)
  alpha0 = sum(alpha)
  f1 = lgamma(alpha0) - lgamma(alpha0 + J)
  f2 = sum(lgamma(alpha + y_i) - lgamma(alpha))
  lh = exp(f1+f2)
  return(lh)
}

prior <- function(z, mu, sigma){
  p = exp(-0.5*(t(z-mu)%*%solve(sigma)%*%(z-mu)))
  return(p)
}

posterior <- function(z,mu,sigma,J, y_i){
  post = prior(z, mu=mu, sigma=sigma)*likelihood(z, J=J,y_i=y_i)
  return(post)
}

run_sem_embeddings <- function(y_patterns,
                    weights,
                    K,
                    seed_val=1234, n_iters=50,
                    sigma_init = diag(K)*10, mu_init = rep.int(0,times=K),
                    mcmc_burnin=500, mcmc_samples=10000, mcmc_thin=100,
                    mcmc_tune=0.4, mcmc_optim='Nelder-Mead')
{
  samples_MCMC <- list()
  z_hat <- matrix(data=NA, nrow=nrow(y_patterns), ncol=K)

  for (p in 1:n_iters){
    out1 <- paste0("Iteration ", p)
    print(out1)

    if (p == 1){
      mu <- mu_init
      sigma <- sigma_init
    }

    tryCatch(

      for (i in 1:nrow(y_patterns)){

        y_i <- array(unlist(y_patterns[i,]))
        J <- sum(unlist(y_patterns[i,]))

        R.utils::captureOutput(expr={

          sample <- MCMCpack::MCMCmetrop1R(posterior,
                                 mu=mu,
                                 sigma=sigma,
                                 J=J,
                                 y_i=y_i,
                                 theta.init=rep(0,K) ,
                                 logfun=FALSE,
                                 burnin=mcmc_burnin, mcmc=mcmc_samples, thin=mcmc_thin,
                                 #burnin=100,mcmc=5000,thin=50,
                                 tune=mcmc_tune,
                                 seed=seed_val,
                                 optim.method=mcmc_optim)
        })

        samples_MCMC[[paste0("sample", i)]] <- sample
        z_i_new <- colMeans(sample)
        z_hat[i,] <- z_i_new
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

    mu <- apply(z_hat, 2, function(x) weighted.mean(x, weights))
    sigma <- (1/nrow(z_hat))*(t((z_hat-mu))%*%(z_hat-mu))
  }

  return(list(z_hat, samples_MCMC, mu, sigma))
}



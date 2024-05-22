library(MCMCpack)

# Chaosnli -----------------

chaosnli_one_hot <- read.csv('data/chaosnli_one_hot.csv')
chaosnli_one_hot <- chaosnli_one_hot[2:4]

chaosnli_results <- fit_model(y=chaosnli_one_hot, weights=rep(1, nrow(chaosnli_one_hot)),
                              mu_init=c(0,0,0), sigma_init=diag(10,3,3),
                              restarts=1,
                              max_iter=30, rtol=0.05, 
                              tune_val=1.8, optim_val='Nelder-Mead', seed_val=123,
                              burnin_val=50, mcmc_val=5000, thin_val=20,
                              fixed_mu = FALSE)


for (z in chaosnli_results[[3]]){
  print(compute_loss(z, colMeans(z), var(z), chaosnli_one_hot))
}

saveRDS(chaosnli_results, "results/chaosnli_results.rds")

# So2Sat LCZ42 -----------------

so2sat_results <-  fit_model(so2sat_patterns[,1:16], so2sat_patterns[,17],
                          mu_init=rep(0,16), sigma_init=diag(10,16,16),
                          restarts=1,
                          max_iter=20, rtol=0.01, 
                          tune_val=1.0, optim_val='Nelder-Mead', seed_val=123,
                          burnin_val=50, mcmc_val=5000, thin_val=20,
                          fixed_mu = FALSE)

for (z in so2sat_results[[3]]){
  print(compute_loss(z, colMeans(z), var(z), so2sat_patterns[,1:16]))
}

saveRDS(so2sat_results, "results/so2sat_results.rds")

# Cifar-10H -------------------

cifar_results <-  fit_model(cifar_patterns[,1:10], cifar_patterns[,11],
                         mu_init=rep(0,10), sigma_init=diag(10,10,10),
                         restarts = 1,
                         max_iter=20, rtol=0.01, 
                         tune_val=1.0, optim_val='Nelder-Mead', seed_val=123,
                         burnin_val=50, mcmc_val=1000, thin_val=10,
                         fixed_mu = FALSE)

for (z in cifar_results[[3]]){
  print(compute_loss(z, colMeans(z), var(z), cifar_patterns[,1:10]))
}

saveRDS(cifar_results, "results/cifar_results.rds")

# Chaosnli mixed --------------------

# create dataset
chaosnli_one_hot_mixed <- chaosnli_one_hot
set.seed(12)
ids_to_sample <- sample(1:nrow(chaosnli_df), 1009, replace=FALSE)
ids_to_sample_5 <- ids_to_sample[1:503]
ids_to_sample_25 <- ids_to_sample[504:1009]

for (id in ids_to_sample_5){
  sample_annotations <- rowSums(rmultinom(5, 1, chaosnli_one_hot[id,]/100))
  chaosnli_one_hot_mixed[id,] <- sample_annotations
}

for (id in ids_to_sample_25){
  sample_annotations <- rowSums(rmultinom(25, 1, chaosnli_one_hot[id,]/100))
  chaosnli_one_hot_mixed[id,] <- sample_annotations
}

# run algorithm
chaosnli_mixed_results <- fit_model(y=chaosnli_one_hot_mixed, weights=rep(1, nrow(chaosnli_one_hot_mixed)),
                                         mu_init=c(0,0,0), sigma_init=diag(10,3,3),
                                         restarts=1,
                                         max_iter=30, rtol=0.01, 
                                         tune_val=1.8, optim_val='Nelder-Mead', seed_val=123,
                                         burnin_val=50, mcmc_val=5000, thin_val=20,
                                         fixed_mu = FALSE)


for (z in chaosnli_mixed_results[[3]]){
  print(compute_loss(z, colMeans(z), var(z), chaosnli_one_hot_mixed))
}

saveRDS(chaosnli_mixed_results, "results/chaosnli_mixed_results.rds")

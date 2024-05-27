library(MCMCpack)

set.seed(123)

# Chaosnli -----------------

chaosnli_one_hot <- read.csv('datasets/data/chaosnli_one_hot.csv')
chaosnli_one_hot <- chaosnli_one_hot[2:4]
K <- ncol(chaosnli_one_hot)

chaosnli_results_full <- fit_model(y=chaosnli_one_hot, weights=rep(1, nrow(chaosnli_one_hot)),
                              mu_init=rep(0,K), sigma_init=diag(10,K,K),
                              restarts=3,
                              max_iter=20, rtol=0.05, 
                              tune_val=1.8, optim_val='Nelder-Mead', seed_val=1234,
                              burnin_val=50, mcmc_val=1000, thin_val=10,
                              fixed_mu = FALSE)


for (z in chaosnli_results[[3]]){
  print(compute_loss(z, colMeans(z), var(z), chaosnli_one_hot))
}

# save full results (incl. all iterations)
saveRDS(chaosnli_results_full, "embeddings/results/full/chaosnli_results_full.rds")

# save best z + mcmc samples in a smaller object 

chaosnli_z <- chaosnli_results_full[[1]]
chaosnli_samples <- chaosnli_results_full[[4]][[length(chaosnli_results_full[[4]])]]
chaosnli_results <- list(chaosnli_z, chaosnli_samples)
saveRDS(chaosnli_results, "embeddings/results/chaosnli_results.rds")

# So2Sat LCZ42 -----------------

K <- ncol(so2sat_patterns[,1:16]) # last column is frequency of patterns 

so2sat_results_full <-  fit_model(y=so2sat_patterns[,1:16], weights=so2sat_patterns[,17],
                          mu_init=rep(0,K), sigma_init=diag(10,K,K),
                          restarts=3,
                          max_iter=20, rtol=0.05, 
                          tune_val=1.0, optim_val='Nelder-Mead', seed_val=1234,
                          burnin_val=50, mcmc_val=1000, thin_val=10,
                          fixed_mu = FALSE)

for (z in so2sat_results[[3]]){
  print(compute_loss(z, colMeans(z), var(z), so2sat_patterns[,1:16]))
}

# save full results (incl. all iterations)
saveRDS(so2sat_results_full, "embeddings/results/full/so2sat_results_full.rds")

# save best z + mcmc samples 
so2sat_z <- so2sat_results_full[[1]]
so2sat_samples <- so2sat_results_full[[4]][[length(so2sat_results_full[[4]])]]
so2sat_results <- list(so2sat_z, so2sat_samples)
saveRDS(so2sat_results, "embeddings/results/so2sat_results.rds")

# Cifar-10H -------------------

K <- ncol(cifar_patterns[,1:10])

cifar_results_full <-  fit_model(y=cifar_patterns[,1:10], weights=cifar_patterns[,11],
                         mu_init=rep(0,K), sigma_init=diag(10,K,K),
                         restarts = 3,
                         max_iter=20, rtol=0.05, 
                         tune_val=1.0, optim_val='Nelder-Mead', seed_val=1234,
                         burnin_val=50, mcmc_val=1000, thin_val=10,
                         fixed_mu = FALSE)

for (z in cifar_results[[3]]){
  print(compute_loss(z, colMeans(z), var(z), cifar_patterns[,1:10]))
}

# save full results (incl. all iterations)
saveRDS(cifar_results_full, "embeddings/results/full/cifar_results_full.rds")

# save best z + mcmc samples 
cifar_z <- cifar_results_full[[1]]
cifar_samples <- cifar_results_full[[4]][[length(cifar_results_full[[4]])]]
cifar_results <- list(cifar_z, cifar_samples)
saveRDS(cifar_results, "embeddings/results/cifar_results.rds")

# Chaosnli mixed --------------------

# create dataset
chaosnli_one_hot_mixed <- chaosnli_one_hot
K <- ncol(chaosnli_one_hot_mixed)
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

# write to csv
write.csv(chaosnli_one_hot_mixed, "datasets/data/chaosnli_one_hot_mixed.csv", row.names = FALSE)

# run algorithm
chaosnli_mixed_results_full <- fit_model(y=chaosnli_one_hot_mixed, weights=rep(1, nrow(chaosnli_one_hot_mixed)),
                                         mu_init=rep(0,K), sigma_init=diag(10,K,K),
                                         restarts=3,
                                         max_iter=20, rtol=0.05, 
                                         tune_val=1.8, optim_val='Nelder-Mead', seed_val=1234,
                                         burnin_val=50, mcmc_val=1000, thin_val=10,
                                         fixed_mu = FALSE)


# save full results (incl. all iterations)
saveRDS(chaosnli_mixed_results_full, "embeddings/results/full/chaosnli_mixed_results_full.rds")

# save best z + mcmc samples 
chaosnli_mixed_z <- chaosnli_mixed_results_full[[1]]
chaosnli_mixed_samples <- chaosnli_mixed_results_full[[4]][[length(chaosnli_mixed_results_full[[4]])]]
chaosnli_mixed_results <- list(chaosnli_mixed_z, chaosnli_mixed_samples)
saveRDS(chaosnli_mixed_results, "embeddings/results/chaosnli_mixed_results.rds")

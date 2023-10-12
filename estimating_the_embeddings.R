library(devtools)
install_github('katharinahech/labelembedding')
library(labelembeddings)

# additional packages
library(rjson)
library(jsonlite)
library(data.table)

######################################################################
####### Estimating the Embeddings ####################################
######################################################################

# set working directory to your local data folder

getwd()
setwd('/Users/katharina/Documents/PhD/Scripts_Paper_2/R_Code')

# set seed for reproducability
set.seed(1997)

# 1. so2sat lcz42 --------------------------------------------------

# prepare data:
#   format: column 1 to K contain number of annotations for respective class,
#           column K+1 contains the frequency of the annotation pattern

#   NOTE: Dataset will be made available in near future along with a soon-to-be-published paper

# todo: edit path to data

so2sat_votes <- read.csv('Data/so2sat/patterns_freq_11.csv')
so2sat_votes <- so2sat_votes[,-1]
LCZs <- c( "X1","X2","X3","X4","X5","X6","X8","X9","X10","X11",
           "X12","X13","X14","X15","X16","X17") # lcz 7 omitted
colnames(so2sat_votes) <- c(LCZs, 'Freq')

# run algorithm
so2sat_embeddings <- labelembeddings::run_sem_embeddings(y_patterns = so2sat_votes[,1:16],
                                                         K = 16,
                                                         weights = so2sat_votes$Freq)

# 2. Plankton --------------------------------------------------------

# prepare data

# extract relevant infos from json
plankton_full_data <- rjson::fromJSON(file='Data/plankton/annotations.json')
json_relevant <- plankton_full_data[[1]][5][[1]]
plankton_df <- rbindlist(json_relevant, fill=TRUE)

labels <- 1:length(unique(plankton_df$image_path))
plankton_df$image_path <- factor(plankton_df$image_path, labels = unique(plankton_df$image_path))
plankton_full <- plankton_df[,1:2]

# convert to one hot for voting patterns
plankton_one_hot <- dcast(data = plankton_full, image_path ~ class_label, length)[,1:11]

# unique voting patterns with frequency
row_strings <- apply(plankton_one_hot[,2:11], 1, paste, collapse = ",")
df <- as.data.frame(table(row_strings))
df$row_strings <- strsplit(as.character(df$row_strings), ",")
result_matrix <- cbind(do.call(rbind, df$row_strings),Freq = df$Freq)
plankton_votes <- apply(result_matrix, c(1, 2), as.integer)

# run algorithm

plankton_embeddings <- labelembeddings::run_sem_embeddings(y_patterns = plankton_votes[,1:10],
                                                           K = 10,
                                                           weights = plankton_votes[,11],
                                                           mcmc_optim = "BFGS",
                                                           mcmc_tune =0.5)

# 3. Cifar10-H--------------------------------------------------------

# prepare data

# load dataset
cifar_df_full <- read.csv('Data/cifar/cifar10h-raw.csv')
cifar_df_full_without_attention = cifar_df_full[cifar_df_full$is_attn_check == 0,] # delete attention checks
cifar_df <- cifar_df_full_without_attention[c('annotator_id', 'chosen_label', 'image_filename')]

# convert to one hot for voting patterns
cifar_one_hot <- cifar_df[order(cifar_df$image_filename),][,c(2,3)]
cifar_one_hot <- data.table::dcast(data = cifar_one_hot, image_filename ~ chosen_label, length)

# unique voting patterns with frequency
row_strings <- apply(cifar_one_hot[,2:11], 1, paste, collapse = ",")
df <- as.data.frame(table(row_strings))
df$row_strings <- strsplit(as.character(df$row_strings), ",")
result_matrix <- cbind(do.call(rbind, df$row_strings),Freq = df$Freq)
cifar_votes <- apply(result_matrix, c(1, 2), as.integer)

# run algorithm
# NOTE: here, the function is run based on the non-aggregated version of the dataset to
#       keep the information about the respective image for visual inspection

cifar_embeddings <- labelembeddings::run_sem_embeddings(y_patterns = cifar_one_hot[,2:11],
                                                        K=10,
                                                        weights = rep(1,nrow(cifar_one_hot)),
                                                        mcmc_tune = 0.5)

cifar_embeddings_unique <- labelembeddings::run_sem_embeddings(y_patterns = cifar_votes[,1:10],
                                                               K=10,
                                                               weights = cifar_votes[,11],
                                                               mcmc_tune = 0.5,
                                                               mcmc_burnin = 50,
                                                               mcmc_samples = 1000,
                                                               mcmc_thin = 50)

# X. Export annotation patterns and estimated embeddings -------------

saveRDS(cifar_embeddings, "RData/cifar_embeddings.rds")
saveRDS(cifar_embeddings_unique, "RData/cifar_embeddings_unique.rds")
saveRDS(plankton_embeddings, "RData/plankton_embeddings.rds")
saveRDS(so2sat_embeddings, "RData/so2sat_embeddings.rds")

saveRDS(cifar_votes, "RData/cifar_votes.rds")
saveRDS(cifar_one_hot, "RData/cifar_one_hot.rds")
saveRDS(plankton_votes, "RData/plankton_votes.rds")
saveRDS(so2sat_votes, "RData/so2sat_votes.rds")

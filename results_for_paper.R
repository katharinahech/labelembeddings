library(factoextra)
library(ggmatplot)
library(ramify)
library(corrplot)
library(ggplot2)
library(MCMCpack)
setwd('/Users/katharina/Documents/PhD/Scripts_Paper_2/Github/labelembeddings')

# Import results and datasets

chaosnli_results <- readRDS("embeddings/results/chaosnli_results.rds")
so2sat_results <- readRDS("embeddings/results/so2sat_results.rds")
cifar_results <- readRDS("embeddings/results/cifar_results.rds")
chaosnli_mixed_results <- readRDS("embeddings/results/chaosnli_mixed_results.rds")

chaosnli_one_hot <- read.csv("datasets/data/chaosnli_one_hot.csv")
chaosnli_one_hot_mixed <- read.csv("datasets/data/chaosnli_one_hot_mixed.csv")
so2sat_patterns <- read.csv("datasets/data/so2sat_patterns.csv")
cifar_patterns <- read.csv("datasets/data/cifar_patterns.csv")
chaosnli_df <- read.csv("datasets/data/chaosnli_df.csv")
cifar_one_hot <- read.csv("datasets/data/cifar_one_hot.csv")

# Chaosnli ----------------------

# 1. exploratory stats ------
nrow(unique(chaosnli_one_hot))

# 2. exemplary embeddings -----
chaosnli_example_ids <- c(34,1168,1177,1371)
chaosnli_df[chaosnli_example_ids,]

# extract embeddings + MCMC samples 
chaosnli_z <- chaosnli_results[[1]]
chaosnli_samples <- chaosnli_results[[2]]

simulations_plot_chaosnli(samples=chaosnli_samples, id=34, z=chaosnli_z, y_patterns=chaosnli_one_hot)
simulations_plot_chaosnli(samples=chaosnli_samples, id=1177, z=chaosnli_z, y_patterns=chaosnli_one_hot)
simulations_plot_chaosnli(samples=chaosnli_samples, id=1168, z=chaosnli_z, y_patterns=chaosnli_one_hot)
simulations_plot_chaosnli(samples=chaosnli_samples, id=1371, z=chaosnli_z, y_patterns=chaosnli_one_hot)

# save plots 
ggsave(
  "plots/chaosnli/exemplary_z_34.png",
  simulations_plot_chaosnli(samples=chaosnli_samples, id=34, z=chaosnli_z, y_patterns=chaosnli_one_hot),
)
ggsave(
  "plots/chaosnli/exemplary_z_1177.png",
  simulations_plot_chaosnli(samples=chaosnli_samples, id=1177, z=chaosnli_z, y_patterns=chaosnli_one_hot),
)
ggsave(
  "plots/chaosnli/exemplary_z_1168.png",
  simulations_plot_chaosnli(samples=chaosnli_samples, id=1168, z=chaosnli_z, y_patterns=chaosnli_one_hot),
)
ggsave(
  "plots/chaosnli/exemplary_z_1371.png",
  simulations_plot_chaosnli(samples=chaosnli_samples, id=1371, z=chaosnli_z, y_patterns=chaosnli_one_hot),
)

# 3. biplot -----

colnames(chaosnli_z) <- c('contradiction','neutral','entailment')
chaosnli_pca <- prcomp(chaosnli_z, scale=TRUE)
majority_vote <- chaosnli_df$majority_label
ground_truth <- chaosnli_df$ground_truth

fviz_pca_biplot(chaosnli_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var', 
                habillage = majority_vote) + 
  scale_color_manual(values=c("#ff7f00", "#4daf4a","#377eb8")) +
  ggtitle('') +
  theme(axis.text=element_text(size=15), legend.text = element_text(size=15))

fviz_pca_biplot(chaosnli_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var', 
                habillage = ground_truth) + 
  scale_color_manual(values=c("#ff7f00", "#4daf4a","#377eb8")) +
  ggtitle('') +
  theme(axis.text=element_text(size=15), legend.text = element_text(size=15))

# save plots 
ggsave(
  "plots/chaosnli/biplot_majority_vote.png",
  fviz_pca_biplot(chaosnli_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var', 
                  habillage = majority_vote) + 
    scale_color_manual(values=c("#ff7f00", "#4daf4a","#377eb8")) +
    ggtitle('') +
    theme(axis.text=element_text(size=15), legend.text = element_text(size=15)),
)
ggsave(
  "plots/chaosnli/biplot_ground_truth.png",
  fviz_pca_biplot(chaosnli_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var', 
                  habillage = ground_truth) + 
    scale_color_manual(values=c("#ff7f00", "#4daf4a","#377eb8")) +
    ggtitle('') +
    theme(axis.text=element_text(size=15), legend.text = element_text(size=15)),
)

# 4. biplot (mixed annotations)

# extract z
chaosnli_mixed_z <- chaosnli_mixed_results[[1]]

# create plot
chaosnli_mixed_pca <- prcomp(chaosnli_mixed_z, scale=TRUE)
var_j <- rowSums(chaosnli_one_hot_mixed)

fviz_pca_biplot(chaosnli_mixed_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var', 
                habillage = var_j, addEllipses=TRUE, ellipse.level=0.95) + 
  ggtitle('') +
  scale_color_manual(values=c("#34c0ff", "#377eb8", '#0e194f')) +
  theme(axis.text=element_text(size=15), legend.text = element_text(size=15))

# save plots 
ggsave(
  "plots/chaosnli/biplot_mixed_annotations.png",
  fviz_pca_biplot(chaosnli_mixed_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var', 
                  habillage = var_j, addEllipses=TRUE, ellipse.level=0.95) + 
    ggtitle('') +
    scale_color_manual(values=c("#34c0ff", "#377eb8", '#0e194f')) +
    theme(axis.text=element_text(size=15), legend.text = element_text(size=15)),
)

# So2Sat ----------------------------

# 1. unique voting patterns -----

nrow(so2sat_patterns)

# 2. example embeddings -----

so2sat_example_ids <- c(18,66,294,349)
so2sat_patterns[so2sat_example_ids,]

# extract embeddings + MCMC samples 

so2sat_z <- so2sat_results[[1]]
so2sat_samples <- so2sat_results[[2]]

simulations_plot_so2sat(samples=so2sat_samples, id=18, z=so2sat_z, y_patterns=so2sat_patterns[,1:16])
simulations_plot_so2sat(samples=so2sat_samples, id=66, z=so2sat_z, y_patterns=so2sat_patterns[,1:16])
simulations_plot_so2sat(samples=so2sat_samples, id=185, z=so2sat_z, y_patterns=so2sat_patterns[,1:16])
simulations_plot_so2sat(samples=so2sat_samples, id=349, z=so2sat_z, y_patterns=so2sat_patterns[,1:16])

# save plots 

ggsave(
  "plots/so2sat/exemplary_z_18.png",
  simulations_plot_so2sat(samples=so2sat_samples, id=18, z=so2sat_z, y_patterns=so2sat_patterns[,1:16]),
  width = 20, height = 15, units = "cm"
)
ggsave(
  "plots/so2sat/exemplary_z_66.png",
  simulations_plot_so2sat(samples=so2sat_samples, id=66, z=so2sat_z, y_patterns=so2sat_patterns[,1:16]),
  width = 20, height = 15, units = "cm"
)
ggsave(
  "plots/so2sat/exemplary_z_185.png",
  simulations_plot_so2sat(samples=so2sat_samples, id=185, z=so2sat_z, y_patterns=so2sat_patterns[,1:16]),
  width = 20, height = 15, units = "cm"
)
ggsave(
  "plots/so2sat/exemplary_z_349.png",
  simulations_plot_so2sat(samples=so2sat_samples, id=349, z=so2sat_z, y_patterns=so2sat_patterns[,1:16]),
  width = 20, height = 15, units = "cm"
)

# 3. biplot -----

colnames(so2sat_z) <- c('01','02','03','04','05','06','08','09','10','A','B','C','D','E','F','G')
so2sat_pca <- prcomp(so2sat_z, scale=TRUE)

fviz_pca_biplot(so2sat_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var') + 
  #scale_color_manual(values=c("#ff7f00", "#4daf4a","#377eb8")) +
  ggtitle('') +
  theme(axis.text=element_text(size=15), legend.text = element_text(size=15))

# save plots 
ggsave(
  "plots/so2sat/biplot_so2sat.png",
  fviz_pca_biplot(so2sat_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var') + 
    #scale_color_manual(values=c("#ff7f00", "#4daf4a","#377eb8")) +
    ggtitle('') +
    theme(axis.text=element_text(size=15), legend.text = element_text(size=15)),
)

# 4. correlation matrix -----

# correlation matrix 

m <- cor(so2sat_z)
colnames(m) <- paste(c(1:6,8:10, 'A', 'B', 'C', 'D', 'E', 'F', 'G'), sep="")
rownames(m) <- paste(c(1:6,8:10, 'A', 'B', 'C', 'D', 'E', 'F', 'G'), sep="")
col <- colorRampPalette(c("#BB4444", "#fbe8e4","#fbe8e4", "#FFFFFF","#FFFFFF","#FFFFFF","#edf2f8", "#edf2f8", "#4477AA"))

png("plots/so2sat/correlation_so2sat.png", height=1500, width=1500, res=200)
corrplot(m, method="color", col=col(200),
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex=1, #tl.pos='lt',#Text label color and rotation
         number.cex = 1, cl.pos='b'
)
dev.off()

# standard deviation of entries 

samples <- so2sat_samples
N <- length(samples)
K <- 16
P <- nrow(samples[[1]])

# convert into suitable format
MCMC_samples_list <- list()
for (p in 1:P){
  MCMC_sample <- matrix(nrow=N,ncol=K)
  for (i in 1:N){
    MCMC_sample[i,] <- samples[[i]][p,]
  }
  MCMC_samples_list[[p]] <- MCMC_sample
}

MCMC_corr <- list()
for (p in 1:P){
  MCMC_corr[[p]] <- cor(matrix(unlist(MCMC_samples_list[p]), nrow=N, ncol=K))
}

MCMC_corr_array <- array(unlist(MCMC_corr),
                         dim = c(K,K,length(MCMC_corr)))

MCMC_var_corr <- apply(MCMC_corr_array,c(1,2), var)
MCMC_std_corr <- apply(MCMC_corr_array,c(1,2), sd)

png("plots/so2sat/correlation_std_so2sat.png",height=1500, width=1500, res=200)

col2 <- colorRampPalette(c("#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF"))
colnames(MCMC_std_corr) <- paste(c(1:6,8:10, 'A', 'B', 'C', 'D', 'E', 'F', 'G'), sep="")
rownames(MCMC_std_corr) <- paste(c(1:6,8:10, 'A', 'B', 'C', 'D', 'E', 'F', 'G'), sep="")
corrplot(MCMC_std_corr, method="color", #col=col2(10),
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex=1, #tl.pos='lt',#Text label color and rotation
         number.cex = 1, cl.pos='n'
)
dev.off()

# cifar10H ---------------------

# 1. unique voting patterns ------

nrow(cifar_patterns)
nrow(cifar_one_hot)

# 2. example embeddings -----

cifar_individual_example_ids <- c(4,3953,4047,8941) # ids für unique setting!!
cifar_one_hot[cifar_individual_example_ids,]

pattern_4 <- cifar_one_hot[4,2:11]
pattern_3953 <- cifar_one_hot[3953,2:11]
pattern_4047 <- cifar_one_hot[4047,2:11]
pattern_8941 <- cifar_one_hot[8941,2:11]

# search by hand 
cifar_patterns_example_ids <- c(30, 2843, 2847, 265)

# extract embeddings + MCMC samples 

cifar_z <- cifar_results[[1]]
cifar_samples <- cifar_results[[2]]

simulations_plot_cifar(samples=cifar_samples, id=30, z=cifar_z, y_patterns=cifar_patterns[,1:10])
simulations_plot_cifar(samples=cifar_samples, id=2843, z=cifar_z, y_patterns=cifar_patterns[,1:10])
simulations_plot_cifar(samples=cifar_samples, id=2847, z=cifar_z, y_patterns=cifar_patterns[,1:10])
simulations_plot_cifar(samples=cifar_samples, id=265, z=cifar_z, y_patterns=cifar_patterns[,1:10])

# save plots 

ggsave(
  "plots/cifar/exemplary_z_30_ship.png",
  simulations_plot_cifar(samples=cifar_samples, id=30, z=cifar_z, y_patterns=cifar_patterns[,1:10]),
  width = 20, height = 10, units = "cm"
)
ggsave(
  "plots/cifar/exemplary_z_2843_deer.png",
  simulations_plot_cifar(samples=cifar_samples, id=2843, z=cifar_z, y_patterns=cifar_patterns[,1:10]),
  width = 20, height = 10, units = "cm"
)
ggsave(
  "plots/cifar/exemplary_z_2847_cat.png",
  simulations_plot_cifar(samples=cifar_samples, id=2847, z=cifar_z, y_patterns=cifar_patterns[,1:10]),
  width = 20, height = 10, units = "cm"
)
ggsave(
  "plots/cifar/exemplary_z_265_toad.png",
  simulations_plot_cifar(samples=cifar_samples, id=265, z=cifar_z, y_patterns=cifar_patterns[,1:10]),
  width = 20, height = 10, units = "cm"
)

# 3. biplot ------

colnames(cifar_z) <- c('plane','auto','bird','cat','deer','dog','frog','horse','ship','truck')
cifar_pca <- prcomp(cifar_z, scale=TRUE)

fviz_pca_biplot(cifar_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var') + 
  #scale_color_manual(values=c("#ff7f00", "#4daf4a","#377eb8")) +
  ggtitle('') +
  theme(axis.text=element_text(size=15), legend.text = element_text(size=15))

ggsave(
  "plots/cifar/biplot_cifar.png",
  fviz_pca_biplot(cifar_pca, repel = TRUE, col.ind='gray', col.var = 'black', label='var') + 
    #scale_color_manual(values=c("#ff7f00", "#4daf4a","#377eb8")) +
    ggtitle('') +
    theme(axis.text=element_text(size=15), legend.text = element_text(size=15)),
)

# 4. correlation matrix ------

m <- cor(cifar_z)
colnames(m) <- c('plane','auto','bird','cat','deer','dog','frog','horse','ship','truck')
rownames(m) <- c('plane','auto','bird','cat','deer','dog','frog','horse','ship','truck')
col <- colorRampPalette(c("#BB4444", "#fbe8e4","#fbe8e4", "#FFFFFF","#FFFFFF","#FFFFFF","#edf2f8", "#edf2f8", "#4477AA"))

png("plots/cifar/correlation_cifar.png",
    height=1500, width=1500, res=200)
corrplot(m, method="color", col=col(200),
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex=1, #tl.pos='lt',#Text label color and rotation
         number.cex = 1, cl.pos='b'
)
dev.off()

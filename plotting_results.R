######################################################################
####### Plotting the results##### ####################################
######################################################################

# 0. Preparations ----------------------------------------------------

library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(dplyr)
library(bssm)
library(corrplot)
library(ggmatplot)
library(Rtsne)

set.seed(1234)
getwd()
setwd('/Users/katharina/Documents/PhD/Scripts_Paper_2/R_Code')

so2sat_embeddings = readRDS("RData/so2sat_embeddings.rds")
cifar_embeddings = readRDS(file = "RData/cifar_embeddings.rds")
cifar_embeddings_unique = readRDS(file = "RData/cifar_embeddings_unique.rds")
plankton_embeddings = readRDS(file = "RData/plankton_embeddings.rds")

so2sat_votes = readRDS("RData/so2sat_votes.rds")
cifar_votes = readRDS(file = "RData/cifar_votes.rds")
cifar_one_hot = readRDS(file = "RData/cifar_one_hot.rds")
plankton_votes = readRDS(file = "RData/plankton_votes.rds")

# 1. Correlation Matrices ----------------------------------------------------

col <- colorRampPalette(c("#BB4444", "#fbe8e4", "#FFFFFF","#edf2f8", "#4477AA"))

# 1.1 So2Sat
s <- cor(so2sat_embeddings[[1]])
colnames(s) <- paste(c(1:6,8:10, 'A', 'B', 'C', 'D', 'E', 'F', 'G'), sep="")
rownames(s) <- paste(c(1:6,8:10, 'A', 'B', 'C', 'D', 'E', 'F', 'G'), sep="")
corrplot(s, method="color", col=col(200),
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex=1, #tl.pos='lt',#Text label color and rotation
         number.cex = 1, cl.pos='b'
)

# 1.2 Plankton
p <- cor(plankton_embeddings[[1]])
corrplot(p, method="color", col=col(200),
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex=1.5, #tl.pos='lt',#Text label color and rotation
         number.cex = 1.5, cl.pos='b',cl.cex=1.3
)

# 1.3 Cifar10
c <- cor(cifar_embeddings[[1]])
corrplot(c, method="color", col=col(200),
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex=1.5, #tl.pos='lt',#Text label color and rotation
         number.cex = 1.5, cl.pos='b',cl.cex=1.3
)

# 2. Exemplary Embeddings -------------------------------------------------

simulations_plot_so2sat <- function(MCMC_samples_df, image_id, z_hat, y_patterns,K){

  i <- as.integer(image_id)
  MCMC <- MCMC_samples_df[i]
  df_simulations <- t(as.data.frame(MCMC))
  rownames(df_simulations) <- c(1:K)
  LCZs <- rownames(df_simulations)
  LCZs <- factor(LCZs, levels = c(1:K))
  value <- df_simulations # only plot 100 MCMC iterations for clarity
  rownames(value) <- LCZs

  p_sim_z <- ggmatplot(
    x = LCZs,
    y = value,
    plot_type = "line",
    size = 0.01,
    xlab = "LCZ",
    ylab = "Estimated z",
    color = "darkgreen",
    linetype = "solid"
  ) +
    theme(legend.position = "none") +
    #scale_x_continuous(breaks = c(1:16)) +
    ggtitle(paste0('Simulated values for image ',i)) +
    ylim(-15,12.5) +
    theme(plot.title=element_text(size=16))


  mean_df <- data.frame(x = 1:nrow(df_simulations), y = z_hat[i,], votes=as.numeric(y_patterns[i,]))
  p_sim_z_mean <- p_sim_z +
    geom_line(data = mean_df,
              aes(x = as.factor(LCZs), y = y, group = 1),
              color = "orange", size = 0.5) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    scale_x_discrete() +
    theme(plot.margin = unit(c(0.3, 0, 0, 0.3), "cm")) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))


  y_df <- data.frame(x=1:nrow(df_simulations), votes=as.numeric(y_patterns[i,]))
  p_y <- ggplot(data = mean_df,
                aes(x = LCZs, y = votes, group = 1)) +
    geom_col(fill='gray70')+
    geom_text(aes(label = ifelse(votes>0,votes,'')),
              position = position_dodge(width = 1),
              vjust = 1.8, size = 3) +
    theme(plot.margin = unit(c(-5,0, 0, 0), "cm")) +
    #theme(axis.title.y=element_blank(),
    #      axis.ticks.y=element_blank()) +
    scale_y_continuous(position = "right") +
    scale_x_discrete(labels=c(1:6,8:10, 'A','B','C','D','E','F','G'))+
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))

  p_final <- cowplot::plot_grid(p_sim_z_mean, p_y, align = "v", ncol = 1,
                                rel_heights = c(0.95, 0.05))

  return(p_final)

}

simulations_plot_additional <- function(MCMC_samples_df, image_id, z_hat, y_patterns, K){

  i <- as.integer(image_id)
  MCMC <- MCMC_samples_df[i]
  df_simulations <- t(as.data.frame(MCMC))
  rownames(df_simulations) <- c(1:K)
  LCZs <- rownames(df_simulations)
  LCZs <- factor(LCZs, levels = c(1:K))
  value <- df_simulations # only plot 100 MCMC iterations for clarity
  rownames(value) <- LCZs

  p_sim_z <- ggmatplot(
    x = LCZs,
    y = value,
    plot_type = "line",
    size = 0.01,
    xlab = "LCZ",
    ylab = "Estimated z",
    color = "darkgreen",
    linetype = "solid"
  ) +
    theme(legend.position = "none") +
    #scale_x_continuous(breaks = c(1:16)) +
    ggtitle(paste0('Simulated values for image ',i)) +
    ylim(-15,15) +
    theme(plot.title=element_text(size=16))


  mean_df <- data.frame(x = 1:nrow(df_simulations), y = z_hat[i,], votes=as.numeric(y_patterns[i,]))
  p_sim_z_mean <- p_sim_z +
    geom_line(data = mean_df,
              aes(x = as.factor(LCZs), y = y, group = 1),
              color = "orange", size = 1) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    scale_x_discrete() +
    theme(plot.margin = unit(c(0.3, 0, 0, 0.3), "cm")) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))


  y_df <- data.frame(x=1:nrow(df_simulations), votes=as.numeric(y_patterns[i,]))
  p_y <- ggplot(data = mean_df,
                aes(x = LCZs, y = votes, group = 1)) +
    geom_col(fill='gray70')+
    geom_text(aes(label = ifelse(votes>0,votes,'')),
              position = position_dodge(width = 1),
              vjust = 1.8, size = 3) +
    theme(plot.margin = unit(c(-5,0, 0, 0), "cm")) +
    #theme(axis.title.y=element_blank(),
    #      axis.ticks.y=element_blank()) +
    scale_y_continuous(position = "right") +
    scale_x_discrete(labels=c(1:K))+
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))

  p_final <- cowplot::plot_grid(p_sim_z_mean, p_y, align = "v", ncol = 1,
                                rel_heights = c(0.95, 0.05))

  return(p_final)

}

# 2.1 So2Sat
chosen_ids <- c(349, 66, 185,17, 18)

so2sat_samples = so2sat_embeddings[[2]]
so2sat_z = so2sat_embeddings[[1]]
so2sat_y = so2sat_votes[,1:16]

for (id in chosen_ids){
  p <- simulations_plot_so2sat(so2sat_samples, id, so2sat_z, so2sat_y, K=16)
  print(p)
}

# 2.2 Plankton
chosen_ids = list(362,303,1376,2369)

plankton_samples = plankton_embeddings[[2]]
plankton_z = plankton_embeddings[[1]]
plankton_y = plankton_votes[,1:10]

for (id in chosen_ids){
  p <- simulations_plot_additional(plankton_samples, id, plankton_z, plankton_y,K=10)
  print(p)
}

# 2.3 Cifar
chosen_ids = list(4, 8941, 4047, 3953)

cifar_samples = cifar_embeddings[[2]]
cifar_z = cifar_embeddings[[1]]
cifar_y = cifar_one_hot[,2:11]

for (id in chosen_ids){
  p <- simulations_plot_additional(cifar_samples, id, cifar_z, cifar_y, K=10)
  print(p)
}


# 3. T-SNE Plots for 2-dimensional projection --------------------------------

custom_palette_distinct <- c(
  "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00",
  "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99",
  "#fdbf6f", "#cab2d6", "#ffff99", "#fdbf6f",
  "#ff7f00", "#b15928", "#e41a1c", "#f781bf"
)

custom_palette_similar <- c(
  'gold', #1
  RColorBrewer::brewer.pal(7, 'Blues')[3:7], #2-6
  'limegreen', 'light green', #8-9
  'orange',# 10
  RColorBrewer::brewer.pal(3, 'Reds'),# A-C
  'plum', # D
  'olive drab', 'yellow green', # E-F
  'dark orchid' # G
)

custom_palette_binary <- c(
  rep('orange',9),
  rep('darkgreen',7)
)

# 3.1 So2Sat

tsne_result <- Rtsne::Rtsne(so2sat_z, pca=FALSE, perplexity=20)
weights <- so2sat_votes$Freq
z_tsne <- as.data.frame(tsne_result$Y)
m_vote <- apply(so2sat_y, 1, which.max)
z_tsne['m_vote'] <- factor(m_vote, labels=paste(c(1:6,8:10, 'A', 'B', 'C', 'D', 'E', 'F', 'G'), sep=""))
z_tsne['weights'] <- weights

tsne_similar <- ggplot(z_tsne, aes(x = V1, y = V2, color = m_vote, size=weights)) +
  geom_point() +
  #stat_ellipse(aes(fill = m_vote), alpha = 0.2, geom = "polygon", type='t') +
  labs(x = "V1", y = "V2") +
  scale_color_manual(values = custom_palette_similar) +
  guides(size='none') +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14))

tsne_binary <- ggplot(z_tsne, aes(x = V1, y = V2, color = m_vote, size=weights)) +
  geom_point() +
  #stat_ellipse(aes(fill = m_vote), alpha = 0.2, geom = "polygon", type='t') +
  labs(x = "V1", y = "V2") +
  scale_color_manual(values = custom_palette_binary) +
  guides(size='none') +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14))

tsne_distinct <- ggplot(z_tsne, aes(x = V1, y = V2, color = m_vote, size=weights)) +
  geom_point() +
  #stat_ellipse(aes(fill = m_vote), alpha = 0.2, geom = "polygon", type='t') +
  labs(x = "V1", y = "V2") +
  scale_color_manual(values = custom_palette_distinct) +
  guides(size='none') +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14))

tsne_similar
tsne_distinct
tsne_binary

# 3.2 Plankton

tsne_result <- Rtsne::Rtsne(plankton_z, pca=FALSE, perplexity=20)
weights <- plankton_votes[,11]
z_tsne <- as.data.frame(tsne_result$Y)
m_vote <- apply(plankton_votes[,1:10], 1, which.max)
z_tsne['m_vote'] <- factor(m_vote, labels=paste(c(1:10), sep=""))
z_tsne['weights'] <- weights

plankton_tsne_distinct <- ggplot(z_tsne, aes(x = V1, y = V2, color = m_vote, size=weights)) +
  geom_point() +
  #stat_ellipse(aes(fill = m_vote), alpha = 0.2, geom = "polygon", type='t') +
  labs(x = "V1", y = "V2") +
  scale_color_manual(values = custom_palette_distinct) +
  guides(size='none') +
  ggtitle("Plankton") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14))

plankton_tsne_distinct

# 3.3 Cifar

cifar_z_unique <- cifar_embeddings_unique[[1]]
tsne_result <- Rtsne::Rtsne(cifar_z_unique[,1:10], pca=FALSE, perplexity=20)
weights <- cifar_votes[,11]
z_tsne <- as.data.frame(tsne_result$Y)
m_vote <- apply(cifar_votes[,1:10], 1, which.max)
z_tsne['m_vote'] <- factor(m_vote, labels=paste(c(1:10), sep=""))
z_tsne['weights'] <- weights

cifar_tsne_distinct <- ggplot(z_tsne, aes(x = V1, y = V2, size=weights, color = m_vote)) +
  geom_point() +
  #stat_ellipse(aes(fill = m_vote), alpha = 0.2, geom = "polygon", type='t') +
  labs(x = "V1", y = "V2") +
  scale_color_manual(values = custom_palette_distinct) +
  guides(size='none') +
  ggtitle("Cifar10-H") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14))

cifar_tsne_distinct

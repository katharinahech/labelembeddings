simulations_plot_chaosnli <- function(samples, id, z, y_patterns){
  
  i <- as.integer(id)
  MCMC <- samples[i]
  df_simulations <- t(as.data.frame(MCMC))
  value <- df_simulations 
  rownames(value) <- colnames(y_patterns)
  df_mean <- data.frame(x = colnames(y_patterns), y = z[i,], votes=as.numeric(y_patterns[i,]))
  
  p_sim_z <- ggmatplot(
    x = colnames(y_patterns),
    y = value,
    plot_type = "line",
    linewidth = 0.01,
    xlab = "Class",
    ylab = "Estimated z",
    color = "darkgreen",
    linetype = "solid") +
    theme(legend.position = "none") +
    ggtitle(paste0('Simulated values for observation ',i)) +
    ylim(-5,5) +
    theme(plot.title=element_text(size=25)) +
    scale_x_discrete(limits=colnames(y_patterns))
  
  p_sim_z_mean <- p_sim_z +
    geom_hline(yintercept = 0, color = "darkgrey") +
    geom_line(data = df_mean,
              aes(x = x, y = y, group = 1),
              color = "orange", linewidth = 0.5) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    scale_x_discrete() +
    theme(plot.margin = unit(c(0.3, 0, 0, 0.3), "cm")) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text=element_text(size=25),
          axis.title=element_text(size=25)) +
    scale_x_discrete(limits=colnames(y_patterns))
  
  p_y <- ggplot(data = df_mean,
               aes(x = rownames(value), y = votes, group = 1)) +
  geom_col(fill='gray70', alpha=.6) +
  theme(axis.title.x=element_blank()) +
  geom_text(aes(label = ifelse(votes>0,votes,'')),
           position = position_dodge(width = 1),
           vjust = 1.8, size = 6) +
  theme(plot.margin = unit(c(-5,0, 0, 0), "cm")) +
  scale_y_continuous(position = "right") +
  scale_x_discrete(labels=colnames(y_patterns))+
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.text=element_text(size=25),
       axis.title=element_text(size=25))+
  scale_x_discrete(limits=colnames(y_patterns))
  
  p_final <- cowplot::plot_grid(p_sim_z_mean, p_y, align = "v", ncol = 1,
                                rel_heights = c(0.95, 0.05))

  
  return(p_final)
  
}





simulations_plot_so2sat <- function(samples, id, z, y_patterns){
  
  i <- as.integer(id)
  MCMC <- samples[i]
  df_simulations <- t(as.data.frame(MCMC))
  value <- df_simulations 
  LCZs <- c('01','02','03','04','05','06','08','09','10','A','B','C','D','E','F','G')
  rownames(value) <- LCZs
  df_mean <- data.frame(x = LCZs, y = z[i,], votes=as.numeric(y_patterns[i,]))
  
  p_sim_z <- ggmatplot(
    x = LCZs,
    y = value,
    plot_type = "line",
    size = 0.01,
    xlab = "Class",
    ylab = "Estimated z",
    color = "darkgreen",
    linetype = "solid") +
    theme(legend.position = "none") +
    ggtitle(paste0('Simulated values for observation ',i)) +
    ylim(-22,17) +
    theme(plot.title=element_text(size=16)) 
  
  
  p_sim_z_mean <- p_sim_z +
    geom_hline(yintercept = 0, color = "darkgrey") +
    geom_line(data = df_mean,
              aes(x = x, y = y, group = 1),
              color = "orange", size = 0.5) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    scale_x_discrete() +
    theme(plot.margin = unit(c(0.3, 0, 0, 0.3), "cm")) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))
  
  p_y <- ggplot(data = df_mean,
                aes(x = LCZs, y = votes, group = 1)) +
    theme(axis.title.x=element_blank()) +
    geom_col(fill='gray70', alpha=.6)+
    geom_text(aes(label = ifelse(votes>0,votes,'')),
              position = position_dodge(width = 1),
              vjust = 1.8, size = 3) +
    theme(plot.margin = unit(c(-3,0, 0, 0), "cm")) +
    scale_y_continuous(position = "right") +
    scale_x_discrete(labels=LCZs)+
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10))
  
  p_final <- cowplot::plot_grid(p_sim_z_mean, p_y, align = "v", ncol = 1,
                                rel_heights = c(0.95, 0.05))
  
  return(p_final)
  
}

simulations_plot_cifar <- function(samples, id, z, y_patterns){
  
  i <- as.integer(id)
  MCMC <- samples[i]
  df_simulations <- t(as.data.frame(MCMC))
  value <- df_simulations 
  classes <- c('plane','auto','bird','cat','deer','dog','frog','horse','ship','truck')
  rownames(value) <- classes
  df_mean <- data.frame(x = classes, y = z[i,], votes=as.numeric(y_patterns[i,]))
  
  p_sim_z <- ggmatplot(
    x = classes,
    y = value,
    plot_type = "line",
    size = 0.01,
    xlab = "Class",
    ylab = "Estimated z",
    color = "darkgreen",
    linetype = "solid") +
    theme(legend.position = "none") +
    ggtitle(paste0('Simulated values for observation ',i)) +
    ylim(-15,15) +
    theme(plot.title=element_text(size=16)) +
    scale_x_discrete(limits=classes)
  
  
  p_sim_z_mean <- p_sim_z +
    geom_hline(yintercept = 0, color = "darkgrey") +
    geom_line(data = df_mean,
              aes(x = x, y = y, group = 1),
              color = "orange", size = 0.5) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    scale_x_discrete(limits=classes) +
    theme(plot.margin = unit(c(0.3, 0, 0, 0.3), "cm")) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))
  
  p_y <- ggplot(data = df_mean,
                aes(x = classes, y = votes, group = 1)) +
    theme(axis.title.x=element_blank()) +
    geom_col(fill='gray70', alpha=.6)+
    geom_text(aes(label = ifelse(votes>0,votes,'')),
              position = position_dodge(width = 1),
              vjust = 1.8, size = 3) +
    theme(plot.margin = unit(c(-3,0, 0, 0), "cm")) +
    scale_y_continuous(position = "right") +
    scale_x_discrete(limits=classes)+
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10))
  
  p_final <- cowplot::plot_grid(p_sim_z_mean, p_y, align = "v", ncol = 1,
                                rel_heights = c(0.95, 0.05))
  
  return(p_final)
  
}

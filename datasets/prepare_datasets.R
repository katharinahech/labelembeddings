library(jsonlite)
setwd('/Users/katharina/Documents/PhD/Scripts_Paper_2/R_Code/new (revision)/final')

# Chaosnli -----------------------

chaosnli_df <- stream_in(file("data/original_files/chaosNLI_snli.jsonl"))

chaosnli_df['n'] <- chaosnli_df$label_counter$n
chaosnli_df['e'] <- chaosnli_df$label_counter$e
chaosnli_df['c'] <- chaosnli_df$label_counter$c
chaosnli_df <- subset(chaosnli_df, select = -label_counter)

# replace NA with zero
chaosnli_df$n[is.na(chaosnli_df$n)] <- 0
chaosnli_df$e[is.na(chaosnli_df$e)] <- 0
chaosnli_df$c[is.na(chaosnli_df$c)] <- 0

# add ground truth column
gt <- unlist(lapply(chaosnli_df$old_labels, function(l) l[[1]]))
chaosnli_df['ground_truth'] <- substr(gt, start = 1 , stop = 1)

# only keep relevant columns 
chaosnli_df['hypothesis'] <- chaosnli_df$example$hypothesis
chaosnli_df['premise'] <- chaosnli_df$example$premise
chaosnli_df <- subset(chaosnli_df, select = -example)
chaosnli_df <- subset(chaosnli_df, select = -old_labels)
chaosnli_df <- subset(chaosnli_df, select = -label_dist)
chaosnli_df <- subset(chaosnli_df, select = -label_count)

# only extract annotations for model
chaosnli_one_hot <- chaosnli_df[c('c', 'n', 'e')]

# write to csv
write.csv(chaosnli_df, "data/chaosnli_df.csv")
write.csv(chaosnli_one_hot, "data/chaosnli_one_hot.csv")


# So2Sat ------------------------------------------

# note: we use unique patterns here and include their frequency for faster computation!

so2sat_votes <- read.csv('data/original_files/so2sat_patterns_16.csv')
so2sat_patterns <- so2sat_votes[,-1]
LCZs <- c( "X1","X2","X3","X4","X5","X6","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17")
colnames(so2sat_patterns) <- c(LCZs, 'Freq')

write.csv(so2sat_patterns, "data/so2sat_patterns.csv")

# Cifar-10H ------------------------------

# load dataset
cifar_df_full <- read.csv('data/original_files/cifar10h-raw.csv')
cifar_df_full_without_attention <- cifar_df_full[cifar_df_full$is_attn_check == 0,] # delete attention checks 
cifar_df <- cifar_df_full_without_attention[c('annotator_id', 'chosen_label', 'image_filename')] 

# convert to one hot for voting patterns
cifar_one_hot <- cifar_df[order(cifar_df$image_filename),][,c(2,3)]
cifar_one_hot <- data.table::dcast(data = cifar_one_hot, image_filename ~ chosen_label, length)

# unique voting patterns with frequency
row_strings <- apply(cifar_one_hot[,2:11], 1, paste, collapse = ",")
df <- as.data.frame(table(row_strings))
df$row_strings <- strsplit(as.character(df$row_strings), ",")
result_matrix <- cbind(do.call(rbind, df$row_strings),Freq = df$Freq)
cifar_patterns <- apply(result_matrix, c(1, 2), as.integer)

write.csv(cifar_df, "data/cifar_df.csv")
write.csv(cifar_patterns, "data/cifar_patterns.csv")

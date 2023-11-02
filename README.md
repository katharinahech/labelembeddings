# labelembeddings

The R package allows to estimate label embeddings from multiple annotations. If a single ground truth label is too restrictive and multiple annotations are available for each instance,
these can be embedded into the K-dimensional space. The embedding is derived from the voting distribution in a Bayesian setup, modelled via a Dirichlet-Multinomial model. 
We estimate the model and posteriors using a stochastic Expectation Maximisation (EM) algorithm with Markov Chain Monte Carlo (MCMC) steps. 
Details on the methodology can be found in the paper "Towards Label Embedding - Measuring Classification Difficulty" by Hechinger et al. 

Additionally, this repository contains the scripts for estimating the embeddings for three exemplary datasets and producing the results and plots presented in the paper. 

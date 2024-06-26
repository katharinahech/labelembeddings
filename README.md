# Human-in-the-loop: Towards Label Embeddings for Measuring Classification Difficulty

Uncertainty in machine learning models is a timely and vast field of research. In supervised learning, uncertainty can already occur in the first stage of the training process, the annotation phase. This scenario is particularly evident when some instances cannot be definitively classified. In other words, there is inevitable ambiguity in the annotation step and hence, not necessarily a "ground truth" associated with each instance. The main idea of this work is to drop the assumption of a ground truth label and instead embed the annotations into a multidimensional space. This embedding is derived from the empirical distribution of annotations in a Bayesian setup, modeled via a Dirichlet-Multinomial framework. We estimate the model parameters and posteriors using a stochastic Expectation Maximisation algorithm with Markov Chain Monte Carlo steps. The methods developed in this paper readily extend to various situations where multiple annotators independently label instances. To showcase the generality of the proposed approach, we apply our approach to three benchmark datasets for image classification and Natural Language Inference. Besides the embeddings, we can investigate the resulting correlation matrices, which reflect the semantic similarities of the original classes very well for all three exemplary datasets.

Full paper can be found here: todo

## Repository

The project is structured as follows.

The folder "datasets" contains:
- data_raw: raw version of the datasets, as downloadable via the URLs provided in the paper
- data: transformed versions of the datasets to one-hot-encoded annotations
- prepare_datasets: script for data transformation

The folder "embeddings" contains: 
- results: csv files containing the estimated embeddings for all three datasets
- run_model: script for estimation of the embeddings

The folder "results" contains the script that produces the final results reported in the paper. 

The folder "functions" contains: 
- model_functions: script with all functions needed for the model setup and estimation
- plotting_functions: script with all functions needed for producing the plots shown in the paper. 

To reproduce the results, follow the following steps: 
1. Run scripts with functions.
2. Prepare and save the datasets with prepare_datasets.R
3. Run the model and save the embeddings with run_model.R
4. Run results_for_paper.R to produce plots and graphics.

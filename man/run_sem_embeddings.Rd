\name{run_sem_embeddings}
\alias{run_sem_embeddings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw MCMC samples from the posterior distribution
}
\description{
This function samples from the posterior specified by the Dirichlet Multinomial Model iteratively based on the Empirical Bayes appraoch. This way, MCMC samples are created and the final embeddings can be estimated as mean value.
}
\usage{
run_sem_embeddings(y_patterns,
                    weights,
                    K,
                    seed_val=1234, n_iters=50,
                    sigma_init = diag(K)*10, mu_init = rep.int(0,times=K),
                    mcmc_burnin=500, mcmc_samples=10000, mcmc_thin=100,
                    mcmc_tune=0.4, mcmc_optim='Nelder-Mead')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y_patterns}{Matrix of unique annotations, columns are annotated classes, rows are instances}
  \item{weights}{Vector of frequency of annotation patterns, i.e. each entry corresponds to the line of y_patterns (only relevant if unique patterns are used which occur more than once, otherwise assign weights=rep(1,nrow(y_patterns))) }
  \item{K}{Number of classes}
  \item{seed_val}{Seed for reproducibility}
  \item{sigma_init, mu_init}{Initialization of gaussian prior distribution}
  \item{n_iters}{Max. number of iterations of the algorithm}
  \item{mcmc_burnin,...}{Options for sampling from posterior, see function "MCMCmetropR()"}
}
\details{
The function starts by initializing the parameters of the Gaussian prior distribution of Z. Then, MCMC samples are drawn from the posterior based on the data and current parameter values. The current embeddings are then estimated as mean value of the samples. Based on the estimated embeddings, new input parameters for the Gaussian can be calculated. The process is repeated until convergence.
}
\value{
Returns a list of values:
z_hat, samples_MCMC, mu, sigma
\item{z_hat}{matrix of estimated embeddings}
\item{samples_MCMC}{explicit samples for each instance of the last iteration before convergence, "z_hat"" is calculated as column-wise mean of the samples}
\item{mu, sigma}{estimated parameters for the final Gaussian distribution of the embeddings}
}
\references{
}
\author{
Katharina Hechinger
}

\examples{
set.seed(1234)

# create random votes
K <- 10
N <- 20
J <- 12
votes <- rmultinom(n=K, size=J, prob=rep(1/20,20))
weights <- rep(1,nrow(votes))

# run function
embeddings <- run_sem_embeddings(votes, weights, K=10,
                                 n_iters = 3,
                                 mcmc_burnin = 10, mcmc_samples = 100, mcmc_thin = 5,
                                 mcmc_optim='BFGS')
}

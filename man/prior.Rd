\name{prior}
\alias{prior}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gaussian Prior Distribution of Embeddings
}
\description{
Auxiliary function for sampling from the posterior: the embeddings are assumed to follow a K-dimensional Gaussian distribution, with parameters mu and Sigma, i.e. the prior distribution. The function calculates the density based on a given embedding z.
}
\usage{
prior(z = NULL, mu = c(), sigma = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{Estimated embedding for an instance, K-dimensional vector}
  \item{mu}{Expectation, K-dimensional vector }
  \item{sigma}{Variance-Covariance Matrix, KxK-dimensional matrix}
}
\details{

}
\value{
probability of z stemming from the specified distribution
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Katharina Hechinger
}



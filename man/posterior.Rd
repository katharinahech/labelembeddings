\name{posterior}
\alias{posterior}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Posterior Probability of Embedding
}
\description{
The function calculates the posterior probability of an embedding, given the parameter values and the annotations. This is the function we need to sample from.
}
\usage{
posterior(z = NULL,mu = c(), sigma = c(), J, y_i = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{vector embedding of respecive instance i}
  \item{y_i}{vector of observed annotations of instance i}
  \item{J}{number of annotations of instance i}
  \item{mu}{Expectation, K-dimensional vector }
  \item{sigma}{Variance-Covariance Matrix, KxK-dimensional matrix}
}
}

\author{
Katharina Hechinger
}


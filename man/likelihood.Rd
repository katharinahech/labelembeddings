\name{likelihood}
\alias{likelihood}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Likelihood of Embedding
}
\description{
The function calculates the likelihood contribution of one instance i, based on its true embedding z, the observed annotations and the number of annotations.
}
\usage{
likelihood(z = NULl , y_i = c(), J)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{vector embedding of respecive instance i}
  \item{y_i}{vector of observed annotations of instance i}
  \item{J}{number of annotations of instance i}
}
\details{}
\value{
  function returns the likelihood contribution of instance i
}

\author{
  Katharina Hechinger
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}


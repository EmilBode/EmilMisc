% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Misc.R
\name{parent.nenv}
\alias{parent.nenv}
\title{Access environments further in the definition chain}
\usage{
parent.nenv(env, n)
}
\arguments{
\item{env}{an environment}

\item{n}{how many steps to go up the chain, >= 0}
}
\value{
An enclosing environment
}
\description{
Get the parent, grandparent, grandgrandparent of an environment
}
\examples{
myfun <- function(x) {
  myfun2 <- function(x2) {
    x+x2
  }
  x <- x+1
  return(myfun2)
}
parent.env(environment(myfun(1))) # That is myfuns environment
parent.env(parent.env(environment(myfun(1))))
  # This is the environment in which myfun was defined, normally .GlobalEnv
parent.nenv(environment(myfun(1)), 2) # The same call, but clearer

}

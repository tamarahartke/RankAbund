\name{spec.ranks}
\alias{spec.ranks}
\title{
Calculating species ranks
}
\description{
Species are ranked by abundance separately for each level of the grouping variable.
}
\usage{
spec.ranks(df, gv)
}
\arguments{
  \item{df}{
Community data matrix, with species as columns and sites as rows.
}
  \item{gv}{
Grouping variable, a vector with one entry corresponding to each row of the data. Currently accepts a dataframe without warning, grouping by every combination of levels.
}
}
\details{
}
\value{
A list of dataframes containing the rank abundances, one for each level of the grouping variable.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Tamara Hartke
}
\note{

}


\seealso{

}
\examples{
library(vegan)
data(dune)
data(dune.env)
dune_ranks <- spec.ranks(dune, dune.env$Use)
}

\keyword{ dplot }%

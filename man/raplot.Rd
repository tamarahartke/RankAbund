\name{ra.plot}
\alias{ra.plot}
\title{
Plotting rank abundance curves
}
\description{
ggplot of species rank by abundance, with one line per level of the grouping variable.
}
\usage{
ra.plot(ra.df, gv, mycolours)
}
\arguments{
  \item{ra.df}{
List of dataframes containing rank abundances.
}
  \item{gv}{
Grouping variable, a vector with one entry corresponding to each row (site) of the original data.
}
  \item{mycolours}{
Colours for the lines.
}
}
\details{
ra.df is the result of [spec.ranks()] or similar list of data frames containing rank abundance data.
I may get rid of gv at some point, but I have it there right now to use any levels you may have already established for other plots.
Colours is a list of 1 or the same length as the number of dataframes in the list/levels in the grouping variable. Maybe I'll make this optional in the next version.
}
\value{
A ggplot of rank abundances with one line for each dataframe in the list/level of the grouping variable. y-axis (abundance) is on pseudo-log scale. Currently the breakpoints are fixed, but maybe I'll fancy that up later, too.
}
\references{

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
dune_ranks <- spec.ranks(dune, dune.env$Use) # generate rank abundance dataframes
ra.plot(dune_ranks, dune.env$Use, c("red", "gold", "green"))
}

\keyword{ hplot }%

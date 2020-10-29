\name{ra.plot}
\alias{ra.plot}
\title{
Plotting rank abundance curves
}
\description{
ggplot of species rank by abundance, with one line per level of a grouping variable.
}
\usage{
ra.plot(ra.df, mycolours)
}
\arguments{
  \item{ra.df}{
List of dataframes containing rank abundances.
}
  \item{mycolours}{
Optional list of colours for the lines.
}
}
\details{
ra.df is the result of [spec.ranks()] or a similar list of data frames containing rank abundance data. The names of the data frames in the list are taken to be the names of the groups to be plotted. If ra.df was generated using [spec.ranks()], these will be the levels of the grouping variable gv.
mycolours is an optional list the same length as the number of dataframes in ra.df, i.e. levels in the grouping variable. Defaults to a [viridis()] pallete with a different colour for each group.
}
\value{
A ggplot of rank abundances with one line for each dataframe in the list, i.e. level of the grouping variable. y-axis (abundance) is plotted on pseudo-log scale with fixed breakpoints. Most aspects of the plot can be customised by adding elements in the usual ggplot manner.
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
ra.plot(dune_ranks, c("red", "gold", "green"))
}

\keyword{ hplot }%

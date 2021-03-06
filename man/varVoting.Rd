% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varVoting.R
\name{varVoting}
\alias{varVoting}
\title{Voting Win method for variable selection and importance}
\usage{
varVoting(variableWeightageMatrix, candidateColumn = NULL, isRanked = FALSE)
}
\arguments{
\item{variableWeightageMatrix}{A matrix of variables and there scores
based on methods}

\item{candidateColumn}{The column which should be treated as column
for candidates. If NULL first clumn will be treated as candidate column.}

\item{isRanked}{A logical. Is the matrix supplied with ranks or raw scores?}
}
\value{
A Ranked data frame with average rank scores
}
\description{
Voting Win method for variable selection and importance
}
\examples{
namedVotes <- read.table(header = T,text = " Candidates Vote_A Vote_B Vote_C Vote_D Vote_E Vote_F Vote_G Vote_H
 Albert      1      2      1      3      2      1      3      4
 Bruce      2      4      5      4      5      4      1      2
 Charles      3      1      3      1      1      2      4      5
 David      4      5      2      5      4      5      2      1
 Edward      5      3      4      2      3      3      5      3")

varVoting(namedVotes,isranked = TRUE)
}

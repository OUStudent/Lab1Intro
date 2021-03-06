% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampleCorrelationMatrix.R
\name{SampleCorrelationMatrix}
\alias{SampleCorrelationMatrix}
\title{Sample Correlation Coefficent}
\usage{
SampleCorrelationMatrix(x)
}
\arguments{
\item{x}{A Matrix, where each variable represents a column}
}
\value{
A vector of the sample means Test
}
\description{
Returns a matrix of the correlation coefficents of the given matrix.
}
\details{
Given a matrix of data, an algorithm is employed to calculate the
sample correlation coefficient matrix. Note: Uses SampleBiasedCovarianceVarianceMatrix function.
}
\examples{
myMat = c(3,4,2,6,8,2,5, 5,5.5,4,7,10,5,7.5)
dim(myMat) = c(7,2) # Creates a 2x7 matrix
mySampleCorrelationVariable = SampleCorrelationMatrix(myMat)
}

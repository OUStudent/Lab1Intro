#' Sample Mean Vector
#'
#' Returns the mean vector of the given matrix.
#'
#'
#' Given a matrix of data, an algorithm is employed to calculate the
#' sample mean vector.
#'
#' @param x A Matrix, where each variable represents a column
#'
#' @return A vector of the sample means
#' @export
#'
#' @examples
#' myMat = c(3,4,2,6,8,2,5, 5,5.5,4,7,10,5,7.5)
#' dim(myMat) = c(7,2) # Creates a 2x7 matrix
#' mySampleMeanVariable = SampleMeanVector(myMat)
SampleMeanVector = function(x) {
  d= dim(x)
  row = d[1]
  col=d[2]
  xMean = vector(length=col)
  for(i in 1:col) {
    xMean[i] = mean(x[,i])
  }
  xMean
}

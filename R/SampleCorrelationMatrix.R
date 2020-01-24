#' Sample Correlation Coefficent
#'
#' Returns a matrix of the correlation coefficents of the given matrix.
#'
#'
#' Given a matrix of data, an algorithm is employed to calculate the
#' sample correlation coefficient matrix. Note: Uses SampleBiasedCovarianceVarianceMatrix function.
#'
#' @param x A Matrix, where each variable represents a column
#'
#' @return A vector of the sample means Test
#' @export
#'
#' @examples
#' myMat = c(3,4,2,6,8,2,5, 5,5.5,4,7,10,5,7.5)
#' dim(myMat) = c(7,2) # Creates a 2x7 matrix
#' mySampleCorrelationVariable = SampleCorrelationMatrix(myMat)
SampleCorrelationMatrix = function(x) {
  d= dim(x)
  row = d[1]
  col=d[2]
  covarianceMat = SampleBiasedCovarianceVariance(x)
  correlationMat = matrix(nrow=col, ncol=col)

  for(i in 1:col) {
    for(j in 1:col) {
      while(i < j) {
        i = i +1
      }
      if(i==j) {
       correlationMat[i,j] = 1
      }else {
        sum = 0
        sum = covarianceMat[i,j] / (sqrt(covarianceMat[j,j])*sqrt(covarianceMat[i,i]))
        correlationMat[i,j] = sum
        correlationMat[j,i] = sum
      }
    }
  }
  correlationMat
}

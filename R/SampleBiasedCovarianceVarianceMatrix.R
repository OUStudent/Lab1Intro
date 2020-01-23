#' Sample (Biased) Covariance with Variance
#'
#' Returns a matrix of the covariance withvariance (baised) of the given matrix.
#'
#'
#' Given a matrix of data, an algorithm is employed to calculate the
#' sample covariance and variance (biased) matrix. The variance values
#' are located on the diagonal of the matrix, x11,x22...xpp; while the
#' covariance values are located in the other positions.
#' Note: Uses SampleMeanVector function.
#'
#' @param x A Matrix, where each variable represents a column
#'
#' @return A vector of the sample means
#' @export
#'
#' @examples
#' myMat = c(3,4,2,6,8,2,5, 5,5.5,4,7,10,5,7.5)
#' dim(myMat) = c(7,2) # Creates a 2x7 matrix
#' mySampleCovarianceVarianceVariable = SampleBiasedCovarianceVariance(myMat)
SampleBiasedCovarianceVariance = function(x) {
  d= dim(x)
  row = d[1]
  col=d[2]
  covarianceMat = matrix(nrow=col, ncol=col)
  sampleMean = SampleMeanVector(x)

  for(i in 1:col) {
    for(j in 1:col) {
      while(i < j) {
        i = i +1
      }
      if(i==j) {
        sum = 0
        for(k in 1:row) {
          sum = sum + (x[k,i]-sampleMean[i])^2
        }
        sum = sum / row
        covarianceMat[j,j] = sum
      }else {
        sum = 0
        for(k in 1:row) {
          sum = sum+ (x[k,i]-sampleMean[i])*(x[k,j]-sampleMean[j])
        }
        sum = sum / row
        covarianceMat[i,j] = sum
        covarianceMat[j,i] = sum
      }
    }
  }
  covarianceMat
}


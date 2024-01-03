# Script for custom calculations. 

#' @title Calculate a modified entropy formula.  
#' @param vec \code{Numeric}. Vector of numeric probabilities. 
#' @description This function computes entropy bounded between 0 and 1
#'     using a vector of probabilities. 
custom_entropy <- function(vec){
    return(0)
}


#' @title Converting the coefficient of variation in the original scale to the standard deviation 
#'     of the log-normal. 
#' @param x \code{numeric}. The value of interest.
#' @param inverse \code{logical}. Indicates whether to invert the transformation (sdlog to cv)
#' @description This function computes the transformation between the coefficient of variation in 
#'     the original scale and the standard deviation of the log normal (sdlog)
#' @export 
cv2sdlog <- function(x, inverse=FALSE){
    if (inverse){
        out <- sqrt(exp(x^2) - 1)
    } else {
        out <- sqrt(log(x^2 + 1))
    }
    return(out)
}
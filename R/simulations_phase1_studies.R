# Script containing some extra functions to help with simulating phase 1 studies. 

#' @title Obtain the rates of toxicity events by grade. 
#' @description Function to obtain toxicity rates by grade (grade 0-1, grade 2, grade 3 and above). 
#'     This uses the model assumed by Simon et al. for demonstrating the utility of accelerated titration
#'     design. This method implements the assumptions made in Mi et al. 2021. This is a function factory. 
#' @param dlt_rates \code{Vector}. DLT rate for the first two doses in order. 
#' @param ... Not used. 
#' @return A list of functions (vectorized) taking into account argument \code{dose} as an 
#'     integer specifying the dose level (e.g. 1,2, 3, 4, or 5). It will return a vector of probabilities
#'     per dose 
#' \itemize{
#'     \item{\code{grade1}: }{Generating probabilities for grade 0 - 1}
#'     \item{\code{grade2}: }{Generating probabilities for grade 2 events or higher (not DLT)}
#'     \item{\code{dlt}: }{Generating probabilities for DLT}
#' }
#' 
#' @details Details can be found in the supplemental section of the following manuscript \cr
#'     Mi et al. 2021, SPA: Single patient acceleration in oncology dose-escalation trials, 
#'     Contemporary Clinical Trials. ISSN 1551-7144, https://doi.org/10.1016/j.cct.2021.106378. 
#' @importFrom stats qnorm pnorm
#' @export
toxicity_by_grade <- function(dlt_rates, ...){
    checkmate::check_vector(dlt_rates, len = 2)
    p_dlt_events <- 1 - sqrt(1 - dlt_rates)
    sigma_b <- log(1.4)/(qnorm(p_dlt_events[2]) - qnorm(p_dlt_events[1]))
    
    n2 <- 2 - ((qnorm(p_dlt_events[2]) * sigma_b)/log(1.4))
    n1 <- n2 - 2
    
    const <- log(1.4)/sigma_b
    
    return(list(
        grade1 = function(dose){
            if (any(dose < 1)) stop("Dose has to be an integer that is 1 or greater since it's an index")
            if (any(!is.integer(dose))) stop("Dose has to be an integer that is 1 or greater since it's an index")
            pnorm((n1 - dose)*const)
        }, 
        grade2 = function(dose){
            if (any(dose < 1)) stop("Dose has to be an integer that is 1 or greater since it's an index")
            if (any(!is.integer(dose))) stop("Dose has to be an integer that is 1 or greater since it's an index")
            pnorm((n2 - dose)*const) - pnorm((n1 - dose)*const)
        }, 
        dlt = function(dose){
            if (any(dose < 1)) stop("Dose has to be an integer that is 1 or greater since it's an index")
            if (any(!is.integer(dose))) stop("Dose has to be an integer that is 1 or greater since it's an index")
            1 - ((1 - pnorm((dose - n2)*const))^2)
        }
    ))
    
}
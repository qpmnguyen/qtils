% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phase1_sim.R
\name{toxicity_by_grade}
\alias{toxicity_by_grade}
\title{Obtain the rates of toxicity events by grade.}
\usage{
toxicity_by_grade(dlt_rates, ...)
}
\arguments{
\item{dlt_rates}{\code{Vector}. DLT rate for the first two doses in order.}

\item{...}{Not used.}
}
\value{
A list of functions (vectorized) taking into account argument \code{dose} as an
integer specifying the dose level (e.g. 1,2, 3, 4, or 5). It will return a vector of probabilities
per dose
\itemize{
\item{\code{grade1}: }{Generating probabilities for grade 0 - 1}
\item{\code{grade2}: }{Generating probabilities for grade 2 events or higher (not DLT)}
\item{\code{dlt}: }{Generating probabilities for DLT}
}
}
\description{
Function to obtain toxicity rates by grade (grade 0-1, grade 2, grade 3 and above).
This uses the model assumed by Simon et al. for demonstrating the utility of accelerated titration
design. This method implements the assumptions made in Mi et al. 2021. This is a function factory.
}
\details{
Details can be found in the supplemental section of the following manuscript \cr
Mi et al. 2021, SPA: Single patient acceleration in oncology dose-escalation trials,
Contemporary Clinical Trials. ISSN 1551-7144, https://doi.org/10.1016/j.cct.2021.106378.
}

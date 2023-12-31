% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_output_simple_models.R
\name{qtab_lm_anova}
\alias{qtab_lm_anova}
\title{Function to provide a quick table for ANCOVA analyses with linear models}
\usage{
qtab_lm_anova(df, var, arm, covariates = NULL, ...)
}
\arguments{
\item{df}{\code{tibble}/\code{DataFrame} The data frame of interest}

\item{var}{\code{string} Name of the outcome variable}

\item{arm}{\code{string} Name of the primary treatment variable (or any other factor variable)
that is of primary interest}

\item{covariates}{\code{vector}. A vector of covariates for adjustment}

\item{...}{Additional arguments passed to \code{lm} function}
}
\value{
A \code{flextable} table
}
\description{
This function takes a continuous outcome (specified by \code{var}) and a binary covariate
(likely a treatment assignment variable specified by \code{arm}), and fit a linear regression adjusted
for additional covariates (specified by \code{covariates}, which is a vector of string values. The function
will output the observed means and standard deviations stratified by \code{arm}, as well as LS means and standard
errors. The p-value reported will be the overall ANOVA p-value of the term specified in \code{arm}.
}
\examples{
data("rand_adlb")
proc_df <- rand_adlb |> 
    dplyr::rename_all(tolower) |> 
    dplyr::filter(paramcd == "ALT") |> 
    dplyr::filter(avisit == "WEEK 5 DAY 36")
qtab_lm_anova(df = proc_df, var = "aval", 
    arm = "trt01a", covariates = NULL)
}

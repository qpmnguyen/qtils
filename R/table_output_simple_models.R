# Simple functionality to provide nice table outputs for simple models (such as glm)

#' @title Function to provide a quick table for ANCOVA analyses with linear models
#' @param df \code{tibble}/\code{DataFrame} The data frame of interest
#' @param var \code{string} Name of the outcome variable
#' @param arm \code{string} Name of the primary treatment variable (or any other factor variable)
#'     that is of primary interest
#' @param covariates \code{vector}. A vector of covariates for adjustment
#' @param ... Additional arguments passed to \code{lm} function
#' @return A \code{flextable} table
#' @description This function takes a continuous outcome (specified by \code{var}) and a binary covariate
#'    (likely a treatment assignment variable specified by \code{arm}), and fit a linear regression adjusted
#'    for additional covariates (specified by \code{covariates}, which is a vector of string values. The function 
#'    will output the observed means and standard deviations stratified by \code{arm}, as well as LS means and standard
#'    errors. The p-value reported will be the overall ANOVA p-value of the term specified in \code{arm}.   
#' @importFrom rlang sym
#' @export
#' @examples
#' data("rand_adlb")
#' proc_df <- rand_adlb |> 
#'     dplyr::rename_all(tolower) |> 
#'     dplyr::filter(paramcd == "ALT") |> 
#'     dplyr::filter(avisit == "WEEK 5 DAY 36")
#' qtab_lm_anova(df = proc_df, var = "aval", 
#'     arm = "trt01a", covariates = NULL)
qtab_lm_anova <- function(df, var, arm, covariates=NULL, ...){
    sd <- emmean <- SE <- `LS Means (SE)` <- term <- p.value <- NULL
    
    checkmate::assert_factor(df[[arm]])
    checkmate::assert_numeric(df[[var]])
    if (!is.null(covariates)){
        checkmate::assert_vector(covariates)
        covar <- paste(covariates, collapse = " + ")
        
        form <- stats::as.formula(glue::glue(
            "{var} ~ {arm} + {covariates}", 
            var = var, arm = arm, covariates = covar
        ))
    } else {
        form <- stats::as.formula(glue::glue(
            "{var} ~ {arm}", 
            var = var, arm = arm
        ))
    }
    
    
    mod <- stats::lm(formula = form, data = df, ...)
    mod$call$formula <- form
    ls_means <- emmeans::emmeans(mod, specs = c(arm), type = "response")
    p_value <- broom::tidy(stats::anova(mod)) |> dplyr::filter(term == arm) |> 
        dplyr::pull(p.value)

    out <- dplyr::left_join(
        df |> dplyr::group_by(!!sym(arm)) |> 
            dplyr::summarise(N = sum(!is.na(!!sym(var))), 
                      mean = mean(!!sym(var), na.rm = TRUE), 
                      sd = sd(!!sym(var), na.rm = TRUE)) |> 
            dplyr::mutate(`Mean (SD)` = paste0(round(mean,2), " (", round(sd,2), ")")) |> 
            dplyr::select(-c(mean, sd)),
        tibble::as_tibble(ls_means) |> 
            dplyr::mutate(`LS Means (SE)` = paste0(round(emmean,2), " (", round(SE,2), ")")) |> 
            dplyr::select(!!sym(arm), `LS Means (SE)`), 
        by = arm
    )  
    p_vec <- rep(NA, nrow(out))
    p_vec[1] <- round(p_value,4)
    
    out <- out |>     
        dplyr::mutate(`p-value` = p_vec) |>
        dplyr::rename("Treatment" = !!arm) |>
        flextable::flextable() |> 
        flextable::autofit()
    
    return(out)
}




#' @title Function to provide quick flextable to estimate differences in response rate from a negative binomial model 
#'     with offset 
#' @param offset \code{symbol}. The offset variable
#' @inheritParams qtab_lm_anova
qtab_glm_nb <- function(df, var, arm, offset=NULL, covariates = NULL,  ...){
    return(0)
}


#' @title Table for generating a table combining multiple glm calls   
#' @param modlist \code{List}. A list of models for combination
qtabl_mglm_coef <- function(modlist){
    return(0)
}

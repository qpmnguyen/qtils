# Simple functionality to provide nice table outputs for simple models (such as glm)

#' @title Function to provide a quick table for ANCOVA analyses with linear models
#' @param df (Tibble/DataFrame) The data frame of interest
#' @param var (String) Name of the outcome variable
#' @param arm (String) Name of the primary treatment variable (or any other factor variable)
#'     that is of primary interest
#' @param covariates (Vector of Strings) A vector of covariates for adjustment
#' @param ... Additional arguments passed to \code{lm} function
#' @return A \code{flextable} table
#' @details This function will fit a linear model of the form var ~ arm + covariates
#'   the table will have LS means per strata of \code{arm}, as well as means and
#'   standard deviations of the primary outcome \code{var} stratified by \code{arm}. 
#'   The p-value is the ANOVA p-value associated with \code{arm}, since \code{arm}
#'   might have multiple levels. 
#' @importFrom rlang sym
quick_lm_ancova_table <- function(df, var, arm, covariates=NULL, ...){
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
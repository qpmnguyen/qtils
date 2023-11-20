# This script contains plot outputs for longitudinal data sets


#' @title Plot observed trajectories of longitudinal data  
#' @description This function takes a data frame and plots longitudinal observations
#'     over time. We can plot mean trajectories as well as spaghetti plots. 
#' @param df The data frame to plot
#' @param ptype \code{string}. This can be either 'mean' or 'spaghetti'
#' @param var \code{symbol}. Unquoted variable name representing the primary variable
#' @param tvar \code{symbol}. Unquoted variable name representing the primary time variable. 
#' @param id \code{symbol}. Unquoted variable name representing the identifier column. 
#' @param col_by \code{symbol}. Unquoted variable name representing the colors. Can be NULL. 
#' @param lt_by \code{symbol}. Unquoted variable name representing the line type. Can be NULL.
#' @param facet_row \code{symbol}. Unquoted variable name representing row facets. Can be NULL
#' @param facet_col \code{symbol}. Unquoted variable name representing column facets. Can be NULL
#' 
#' @return A \code{ggplot2} plot
#' 
#' @details For mean trajectories, a table representing the number of non-NA observations
#'     will be reported along-side the table. The confidence interval will be computed based on the 
#'     normal approximation: \eqn{m + 1.96 * SE}. This function does not label the axes and does not
#'     specify a color scheme or themeing. In other words, themes and labels should be added by the 
#'     analyst after. 
#' @export
#' @examples
#' data(rand_adlb)
#' df <- rand_adlb |> dplyr::filter(PARAMCD == "ALT") |> 
#'     dplyr::mutate(AVISIT = factor(AVISIT))
#' qplot_obs_long(df = df, ptype = "mean", var = AVAL, 
#'     tvar = AVISIT, id = USUBJID, col_by = TRT01A)
#' 
#'     
#' @importFrom ggplot2 aes
#'     
qplot_obs_long <- function(df, ptype, var, tvar, id, 
                           col_by = NULL, 
                           lt_by = NULL,
                           facet_row = NULL, 
                           facet_col = NULL){
    
    n <- m <- sd <- se <- upper <- lower <- NULL
    
    ptype <- match.arg(ptype, c("mean", "spaghetti"))
    
    cmiss <- rlang::quo_is_null(rlang::enquo(col_by))
    lmiss <- rlang::quo_is_null(rlang::enquo(lt_by))
    
    if (ptype == "mean"){
        
        # mental gymnastics for a group expression :(
        if (cmiss & lmiss){
            grp_expr <- 1 
        } else if (!cmiss & lmiss){
            grp_expr <- rlang::expr({{ col_by }})
        } else if (!cmiss & !lmiss){
            grp_expr <- rlang::expr(interaction({{ col_by }}, {{ lt_by }}))
        } else if (cmiss & !lmiss){
            stop("col_by has to be specified first before lt_by")
        }
        
        plot_df <- df |> dplyr::group_by({{ tvar }}, {{ col_by }}, {{ lt_by }}, {{ facet_row }}, 
                                         {{ facet_col }}) |> 
            dplyr::summarise(n = dplyr::n_distinct({{ id }}), 
                      m = mean({{ var }}, na.rm = TRUE),
                      sd = sd({{ var }}, na.rm = TRUE),
                      se = sd/sqrt(n), 
                      upper = m + 1.96 * se, 
                      lower = m - 1.96 * se
            )
        out <- ggplot2::ggplot(plot_df, aes(
                x = {{ tvar }}, y = m, 
                col = {{ col_by }}, linetype = {{ lt_by }}, 
                fill = {{ col_by }},
                group = !!grp_expr,
            )) + ggplot2::geom_line() + 
            ggplot2::geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2) +
            ggrepel::geom_text_repel(aes(label = round(m,2)), seed = 1234) +
            ggplot2::geom_point() +
            ggplot2::facet_grid(rows = ggplot2::vars({{ facet_row }}), 
                                cols = ggplot2::vars({{ facet_col }}))
        
    } else if (ptype == "spaghetti"){
        out <- ggplot2::ggplot(df, aes(
            x = {{ tvar }}, y = {{ var }}, 
            col = {{ col_by }}, linetype = {{ lt_by }}, 
            fill = {{ col_by }}, 
            group = {{ id }},
        )) + ggplot2::geom_line(alpha = 0.2) +
            ggplot2::facet_grid(rows = {{ facet_row }}, cols = {{ facet_col }})
    } else {
        stop("Not supported")
    }
    
    return(out)
    
}    



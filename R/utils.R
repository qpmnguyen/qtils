# Utilities for project management. 

#' @title This function returns a string that can be added to trace the provenance of a table. 
#' @param manual_path This is a manual path. If not, the function will attempt to use the \code{rstudioapi}
#'     function to get the current path of the active file (might not work outside RStudio).
#' @param rmd (Logical). Indiciate whether an RMD is being rendered. This will affect how the function retrieves
#'     the current file in non-interactive mode.  
#' @param ... Not currently used. 
#' @details It is recommended that this function be used with a manual path, preferably using the 
#'     function \code{here}
#' 
#' @importFrom glue glue
#' @examples
#'     qtils::generate_provenance_string("/this/is/the/path/t1.1.R")
#' @export
generate_provenance_string <- function(manual_path = NULL, rmd = FALSE, ...){
    version <- R.Version()$version.string
    info <- Sys.info()
    machine_name <- glue::glue("{sysname} {machine}", sysname = info["sysname"], machine = info['machine'])
    
    if (is.null(manual_path)){
        if (interactive()){
            if (Sys.getenv("RSTUDIO") == 1 & .Platform$GUI == "RStudio"){
                manual_path <- normalizePath(rstudioapi::documentPath())
            } else {
                warning("Manual path not provided and Rstudio not used. Setting to NULL")
            }
        } else {
            if (rmd == FALSE){
                manual_path <- file.path(normalizePath(dirname(knitr::current_input())), knitr::current_input()) 
            } else {
                manual_path <- get_this_script()
            }
        }
    }
    
    prov_string <- glue::glue("{path} ({usr} {date} {r_ver} {machine})", machine = machine_name,
                              date = Sys.Date(),
                              r_ver = version, 
                              usr = info['user'], 
                              path = manual_path)
    return(prov_string)

}


#' @title Retrieving the path of a script using workarounds
#' @param ... Not used. 
#' @description Get the directory of the current file either through Rscript or through sourcing within RStudio
#' @keywords internal
#' @references https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/1816487#1816487
get_this_script <- function(...){
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    needle <- "--file="
    match <- grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        # 'source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
    }
}
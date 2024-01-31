# Utilities for project management. 

#' @title This function returns a string that can be added to trace the provenance of a table. 
#' @param manual_path This is a manual path. If not, the function will attempt to use the \code{rstudioapi}
#'     function to get the current path of the active file (might not work outside RStudio). 
#' @param ... Not currently used. 
#' @details
#'     It is recommended that this function be used with a manual path, preferably using the 
#'     function \code{here::here}. 
#' 
#' @importFrom glue glue
#' @examples
#'     qtils::generate_provenance_string("/this/is/the/path/t1.1.R")
#' @export
generate_provenance_string <- function(manual_path = NULL, ...){
    version <- R.Version()$version.string
    info <- Sys.info()
    machine_name <- glue::glue("{sysname} {machine}", sysname = info["sysname"], machine = info['machine'])
    
    if (is.null(manual_path)){
        manual_path <- rstudioapi::documentPath()
    }
    
    prov_string <- glue::glue("{path} ({usr} {date} {r_ver} {machine})", machine = machine_name,
                              date = Sys.Date(),
                              r_ver = version, 
                              usr = info['user'], 
                              path = manual_path)
    return(prov_string)

}
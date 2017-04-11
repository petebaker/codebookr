## Filename: zzz.R
## Purpose: Non-documented functions for 'codebookr' package
##
## To run in terminal use:  R CMD BATCH --vanilla zzz.R

## Created at:  Wed Apr  5 15:52:23 2017
## Author:      Peter Baker
## Hostname:    clearwell2
## Directory:   /home/pete/Data/dev/codebookr/R/
## Licence:     GPLv3 see <http://www.gnu.org/licenses/>
##
## Change Log: 
##

##' Internal function called when library is loaded to set options
##' @keywords internal
.onLoad <- function(libname, pkgname){
  op <- options()
  
  op.codebookr <-
    list(codebookr.column_names = c(variable_name = "Variable",
                                    variable_label = "Label",
                                    variable_original = "Original_Name",
                                    variable_levels = "Levels",
                                    variable_limits = "Limits",
                                    min = "Minimum", max = "Maximum",
                                    missing_values = "Missing_Values",
                                    factor_type = "Factor_Type",
                                    comments = "Comments")) #,
  ## codebookr.na = c("", "NA", ".", " ")) # keep simpler for now
  ## could have na.extra parameter
  toset <- !(names(op.codebookr) %in% names(op))
  if(any(toset)) options(op.codebookr[toset])
  
  invisible()
}


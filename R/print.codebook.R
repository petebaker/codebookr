##' Print an S3 object of class \code{codebook}
##'
##' \code{read_codebook} reads a code book stored as a \code{csv} file
##' for either checking against a data file or relabelling factor
##' levels or labelling variables.  \code{read_codebook} returns an S3
##' object of  class \code{codebook}
##'
##' @aliases codebook
##'
##' @param x object of class \code{codebook}
##' @param extra logical: whether to print extra information. Default: FALSE 
##' @param ... extra arguments passed to specific printing functions
##' 
##' @seealso \code{\link{read_codebook}}
##' @author Peter Baker \email{pete@@petebaker.id.au}
##' @examples
##' file.copy(system.file('demoFiles', 'data1_codebook.csv',
##'                       package='codebookr'), 'data1_codebook.csv')
##' data1_codebook <- read_codebook("data1_codebook.csv",
##'          column_names = list(variable_levels = "Factor.Levels",
##'                              variable_original = "Old.Variable",
##'                              min = "Min", max = "Max"))
##' print(data1_codebook)
##' @export
print.codebook <-
  function(x, extra = FALSE, ...)
{
  ## check class of object -------------------------------------
  if (class(x) != "codebook")
    stop(paste0("Object '", deparse(substitute(x)),
                "' not of class 'codebook'"))

  cat("Codebook:", deparse(substitute(x)), "\n\n")

  if (!is.null(x$file_info)){
    file_info <- x$file_info
    cat("Codebook read from file:", file_info$codebook_filename,
        "\nRead at:", file_info$codebook_read_time, "\nColumn names:\n")
    print(file_info$column_names)
  }

  if (extra & !is.null(x$renamed_variables)){
    cat("Renamed Variables:\n")
    print(x$renamed_variables)
  }

  if(!is.null(x$variable_labels)){
    cat("\nVariable Labels:\n")
    print(x$variable_labels)
  }

  if(!is.null(x$factor_levels)){
    cat("\nFactor Levels:\n")
    print(x$factor_levels)
  }

  if(!is.null(x$limits_continuous)){
    cat("\nLimits for Continuous Variables:\n")
    print(x$limits_continuous)
  }

  if (extra & !is.null(x$data_management_plan)){
    cat("\nData Management Plan details:\n")
    print(x$data_management_plan, ...)
  }

}

## data1_codebook <- read_codebook("../inst/demoFiles/data1_codebook.csv",
##          column_names = list(variable_levels = "Factor.Levels",
##                              variable_original = "Old.Variable",
##                              min = "Min", max = "Max"))
## x <- data1_codebook

## x
## print(x, extra = TRUE)

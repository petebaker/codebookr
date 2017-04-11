##' Use codebook to check continuous variables for validity
##'
##' Uses a codebook which is an S3 class \code{codebook}, possibly
##' read in by \code{read_codebook}, to check for data within valid
##' ranges.
##'
##' REPEAT: Often, when analysing data, data dictionaries or code books are
##' provided with data files. Rather than a \code{word} \code{doc} or
##' \code{pdf} files, the format required here is in a very specific
##' format stored as a \code{csv} file. Once read in, attributes such
##' as factor labels/levels and variable labels can be added to the
##' \code{data.frame} and/or also used to check factor labels and
##' variable names are consistent with the code book. Note that while
##' various methods may be available which attempt to convert word
##' docs or pdf's to a spreadsheet and/or csv file, extreme care
##' should be taken as these are far from perfect.
##'
##' @param x \code{tibble} to which \code{codebook} is applied
##' @param code_book \code{codebook} containing names of continuous
##'   variables and limits for checking data are within range
##' @param column_names character vector of column names for checking
##'   data in range. Default: All continuous variables defined in
##'   \code{codebook}.
##' @return object of type class \code{tibble} containing data out of
##'   limits
##' @author Peter Baker \email{pete@@petebaker.id.au}
##' @examples
##' file.copy(system.file('demoFiles', 'data1_codebook.csv',
##'                       package='codebookr'), 'data1_codebook.csv')
##' file.copy(system.file('demoFiles', 'data1-yr21.csv',
##'                       package='codebookr'), 'data1-yr21.csv')
##' data1_codebook <- read_codebook("data1_codebook.csv",
##'          column_names = list(variable_levels = "Factor.Levels",
##'                              variable_original = "Old.Variable",
##'                              min = "Min", max = "Max"))
##' data1 <- readr::read_csv('data1-yr21.csv')
##' data1
##' non_valid <- validate_limits(data1, data1_codebook)
##' @export
validate_limits <-
  function(x, code_book, column_names = NULL)
{
  ## set and/or check names for limits ------------------------------
  limits <- code_book$limits_continuous
  limits_names <- limits[["variable_name"]]

  if (is.null(column_names)){
    column_names <- limits_names
  }
  if (any(!column_names %in% code_book$continuous_names)){
    cat("Variable names not in list of continuous variables:\n")
    print(column_names[column_names %in% code_book$continuous_names])
    cat("List of continuous variables:\n")
    print(code_book$continuous_names)
    stop("One or more 'column_names' not in codebook")    
  }

  ## apply limits to variables ----------------------------
  newData <- x

  out_of_range <- vector(mode = "list", length = length(column_names))
  names(out_of_range) <- column_names
  
  for (I in 1:length(column_names)){
    
    ## extract min and max
    var_name <- column_names[I]
    cat("\n+++ Processing variable:", var_name, "\n")
    minMax <- dplyr::filter(limits, variable_name == var_name)

    sumData <- summary(x[[var_name]])
    smin <- sumData["Min."]
    smax <- sumData["Max."]
    cmin <- minMax[["min"]]
    cmax <- minMax[["max"]]

    if (smin<cmin | smax>cmax){
      outOfRange <- dplyr::select(x, c(dplyr::starts_with(var_name))) %>%
        dplyr::filter(!is.na(.)) %>% dplyr::filter(. < cmin | . > cmax)
      cat("Out of range values:\n")
      print(outOfRange)
      cat("\n")
    } else {
      outOfRange <- NA
      cat("No values out of range.\n\n")
    }
    out_of_range[[I]] <- outOfRange
  }
  out_of_range
}

## data1_codebook <- read_codebook("../inst/demoFiles/data1_codebook.csv",
##          column_names = list(variable_levels = "Factor.Levels",
##                              variable_original = "Old.Variable",
##                              min = "Min", max = "Max"))
## code_book <- data1_codebook
## data1 <- readr::read_csv('../inst/demoFiles/data1-yr21.csv')
## x <- data1

## non_valid <- validate_limits(data1, data1_codebook)

##' Use codebook to create factors and check levels for validity
##'
##' Uses a codebook which is an S3 class \code{codebook}, possibly
##' read in by \code{read_codebook}, to convert numerical vectors into
##' \code{factors} in a \code{tibble}.
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
##' @param code_book \code{codebook} containing factor names and factor levels
##' to convert numeric or character vectors to \code{factors} and also test
##' for valid levels.
##' @param column_names character vector of column names for conversion to
##' factors. Default: All \code{factors} defined in \code{codebook}.
##' @return object of type class \code{tibble}
##' @author Peter Baker \email{pete@@petebaker.id.au}
##' @examples
##' file.copy(system.file('demoFiles', 'data1_codebook.csv',
##'                       package='codebookr'), 'data1_codebook.csv')
##' file.copy(system.file('demoFiles', 'data1-birth.csv',
##'                       package='codebookr'), 'data1-birth.csv')
##' data1_codebook <- read_codebook("data1_codebook.csv",
##'          column_names = list(variable_levels = "Factor.Levels",
##'                              variable_original = "Old.Variable",
##'                              min = "Min", max = "Max"))
##' data1 <- readr::read_csv('data1-birth.csv')
##' data1
##' myData <- create_factors(data1,  data1_codebook)
##' str(myData)
##' @export
create_factors <-
  function(x, code_book, column_names = NULL)
{
  ## set and/or check factor names ------------------------------
  if (is.null(column_names)){
    column_names <- code_book$factor_names
  }
  if (any(!column_names %in% code_book$factor_names)){
    cat("Variable names not in list of factors:\n")
    print(column_names[column_names %in% code_book$factor_names])
    cat("List of factors:\n")
    print(code_book$factor_names)
    stop("One or more 'column_names' not in codebook factors")    
  }

  ## apply factor levels to variables ----------------------------
  flevels <- code_book$factor_levels
  newData <- x

  for (I in 1:length(column_names)){
    
    ## try the labels first
    var_name <- column_names[I]
    xlev <- flevels[[var_name]]
    levs <- xlev$fac.level
    labs <- xlev$fac.label
    if (all(is.na(labs)) | all(levs == labs)) labs <- levs
    
    test.labs <- unique(newData[var_name])
    
    cat("Processing variable:", var_name, "\n\n")
    
    if (all(as.character(unlist(test.labs)) %in% labs)){
      newData[[var_name]] <- 
        readr::parse_factor(newData[[var_name]], levels = labs)
      cat("Factor:", var_name, "set up with levels:\n")
      if (length(labs)<6){
        extra <- ""
      } else {
        extra <- "... [truncated] ..."
      }
      print(utils::head(labs))
      cat(extra, "\n")
    } else if (all(as.character(unlist(test.labs)) %in% levs)) {
      newData[[var_name]] <- 
        readr::parse_factor(newData[[var_name]], levels = labs)
      cat("Factor:", var_name, "set up with levels:\n")
      if (length(labs)<6){
        extra <- ""
      } else {
        extra <- "... [truncated] ..."
      }
      print(utils::head(labs))
      cat(extra, "\n")
    } else {
      cat("Warning: Some factor levels in data not in codebook\n",
          "         These will be set to NA\n\nCodebook Levels:\n")
      print(labs)
      cat("Levels in dataset:\n")
      test.labs <- test.labs %>% .[[1]]  # untibble!
      print(test.labs)
      cat("present in dataset but not codebook:\n")
      print(setdiff(test.labs, labs))
      newData[[var_name]] <- 
        readr::parse_factor(newData[[var_name]], levels = labs)
    }
  }
  newData
}

## data1_codebook <- read_codebook("../inst/demoFiles/data1_codebook.csv",
##          column_names = list(variable_levels = "Factor.Levels",
##                              variable_original = "Old.Variable",
##                              min = "Min", max = "Max"))
## code_book <- data1_codebook
## data1 <- readr::read_csv('../inst/demoFiles/data1-birth.csv')
## x <- data1

## myData <- create_factors(data1,  data1_codebook)
## str(myData)

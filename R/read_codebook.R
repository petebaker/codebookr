##' Read a code book in standard format(s) as a csv file
##'
##' \code{read_codebook} reads a code book stored as a \code{csv} file
##' for either checking against a data file or relabelling factor
##' levels or labelling variables.
##'
##' Often, when analysing data, data dictionaries or code books are
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
##' @param x filename of codebook to parse
##' @param codebook_directory directory containing codebook. Default :
##'   current directory
##' @param column_names named character vector containing column names
##'   in Code Book file. The vector contains components
##'   \code{variable_name} = variable name,
##'   \code{variable_original} = original name (if variable_name was
##'   changed), \code{label} for printing/plotting,
##'   \code{variable_levels} = factor levels, \code{variable_limits} or
##'   \code{min} and \code{max} for continuous measurements,
##'   \code{missing_values} = numeric or strings for values of
##'   variable to be set as missing \code{comments} = comments about
##'   the variable which may include the measurement instrument or
##'   references about the measurement. Note that default values may
##'   be found with \code{options(codebookr.column_names)}
##' @param na a character vector of strings which are to be
##'   interpreted as \sQuote{NA} values.  Blank fields are also
##'   considered to be missing values in logical, integer, numeric and
##'   complex fields. Default: \code{c("", "NA", ".", " ")}
##' @param data_management_plan a list containing information like url,
##'   location, authors, date, version and so on.
##'   Default: "All possible details should be here"
##' @return S3 object of type class \code{codebook}
##' @author Peter Baker \email{pete@@petebaker.id.au}
##' @examples
##' file.copy(system.file('demoFiles', 'data1_codebook.csv',
##'                       package='codebookr'), 'data1_codebook.csv')
##' data1_codebook <- read_codebook("data1_codebook.csv",
##'          column_names = list(variable_levels = "Factor.Levels",
##'                              variable_original = "Old.Variable",
##'                              min = "Min", max = "Max"))
##' @export
read_codebook <-
  function(x, codebook_directory = NULL, 
           column_names = NULL,
           na = c("", "NA", ".", " "),
           data_management_plan = "All possible details should be here")
{

  ## check directory ------------------------------------------------
  if (is.null(codebook_directory)) {
    codebook_directory <- "."
  }

  if (!file.exists(codebook_directory))
    stop(paste("Error: 'codebook_directory'", codebook_directory, "not found."))

  ## check for codebook ---------------------------------------------
  codebook_filename <- x
  codebook_file <- file.path(codebook_directory, x)
  if (!file.exists(codebook_file))
    stop(paste("Error: 'codebook_file'", codebook_file, "not found."))

  ## set up column names for processing -----------------------------
  default_names <- options("codebookr.column_names")[[1]]
  if (!is.null(column_names)){
    ##set_names <- match.arg(names(column_names), choices =  names(default_names),
    ##                       several.ok = TRUE)

    ## get names to be changed
    set_names <- column_names
    column_names <- default_names
    ## set names to be changed
    column_names[names(set_names)] <- set_names

    ## check that correct names
    if (!all(names(column_names) %in% names(default_names))){
      cat("User provided names for 'column_names':\n")
      print(names(column_names))
      cat("Should be in:\n")
      print(names(default_names))
      stop("Please provide correct names.")
    }
  } else {
    column_names <- default_names
  }

  ## read in codebook ----------------------------------------
  ## cat("\nFunction 'read_codebook' largely untested: beware!\n\n")
  xCodes <- readr::read_csv(codebook_file, na = na)
  dfCodes <- as.data.frame(xCodes)
  fileName <- deparse(substitute(x))
  colNames <- names(xCodes)

  ## take md5sum of file and other attributes --------------------
  codebook_read_time <- Sys.time()
  codebook_digest <- digest::digest(codebook_file)
  codebook_time <-
    file.info(codebook_file)[c("size", "mtime", "ctime", "atime")]
  
  ## check names present and not -------------------------------------------
  definedNames <- column_names %in% colNames  # are these present
  presentNames <- column_names[definedNames]  # names that are present
  presentNames2 <- as.character(column_names[definedNames]) # not named
  absentNames <- column_names[!definedNames]
  
  ## are codebook column names same as specified  ------------------
  if (!(all(colNames %in% column_names))){
    cat(stringr::str_c("File: '", fileName, "'"), "\n")
    cat("Column Names:\n")
    print(colNames)
    cat("Warning: some column names in codebook not defined:\n")
    print(absentNames)
    cat("\nColumns present (which may be all that are necessary):\n")
    print(presentNames)
    extraCols <- setdiff(names(xCodes), presentNames)
    if (length(extraCols) > 0){
      cat("Extra columns that perhaps should be used for codebook:\n")
      print(extraCols)
    }
  }

  ## variable_levels ------------------------------------------------------  
  if ("variable_levels" %in% names(absentNames)){
    cat("Warning: factor levels column not found.\n",
        "This should be set if any factors present\n")
    isFactorLevels <- FALSE
  } else {
    isFactorLevels <- TRUE    
  }
  
  ## variable labels ------------------------------------------------
  if (length(presentNames["variable_label"]) > 0){
    hhh <- presentNames[c("variable_name", "variable_label")]
    varLabels <- xCodes[as.character(hhh)]
    ## varLabels <-
    ## dplyr::select(xCodes,
    ##                dplyr::starts_with(presentNames["variable_name"]),
    ##                dplyr::starts_with(presentNames["variable_label"]))
    varLabels <-  dplyr::filter(varLabels,
                                !is.na(dplyr::select(varLabels, 1)))
  } else {
    varLabels <-  NA
  }
  
  ## renamed variables: --------------------------------------------------
  ## if variable renamed then construct table with new and old name
  if ("variable_original" %in% names(presentNames)){
    ## extract old/new variable names
    hhh <- presentNames[c("variable_name", "variable_original")]
    renamed_table <- xCodes[as.character(hhh)]
    ##renamed_table <-
    ##  dplyr::select(xCodes,
    ##                dplyr::starts_with(presentNames["variable_name"]),
    ##                dplyr::starts_with(presentNames["variable_original"]))
    renamed_table <-  dplyr::filter(renamed_table,
                                    !is.na(dplyr::select(renamed_table, 1)))
    names(renamed_table) <- c("variable_name", "variable_original")
  } else {
    renamed_table <- NA
  }

  ## set factor levels ------------------------------------------------
  if (isFactorLevels){
    dfCodes$variable_name.filled <-
      as.character(zoo::na.locf(dfCodes[,as.character(presentNames["variable_name"])]))
    ## appears more than twice then is a factor
    factors <- rle(dfCodes$variable_name.filled)
    n.levels <- factors$lengths
    factors <- factors$values[factors$lengths>1]
    n.levels <- n.levels[n.levels > 1]
    names(n.levels) <- factors
    
    factor.info <- dfCodes[dfCodes[, "variable_name.filled"] %in% factors, ]
    tmp <-
      strsplit(factor.info[,
                           as.character(presentNames["variable_levels"])], "=")
    factor.info$fac.level <- sapply(tmp, function(y) y[1])
    factor.info$fac.label <- sapply(tmp, function(y) y[2])
    factor.info$Factors <- factor.info$variable_name.filled
    ## hadley doesn't like dots so variable_name.filled messes up VNF ok
    ## plyr::dlply(factor.info, #.(factor.info$Factors),
    ##              FACTOR,
    ##              function(y) list(fac.level = y$fac.level,
    ##                               fac.label = y$fac.label))
    ## but really weird plyr interaction is driving me mad - use by instead
    factorLevels <- 
      by(factor.info, factor.info$Factors, function(y)
        list(fac.level = y$fac.level, fac.label = y$fac.label))
  }
  
  ## determine continuous variables ----------------------------------
  contVars <-
    dfCodes[grep("[Cc]ont",
                 dfCodes[,as.character(presentNames["variable_levels"])]),
                      as.character(presentNames["variable_name"])]
  contVars2 <-
    dfCodes[is.na(dfCodes[,as.character(presentNames["variable_levels"])]),
                        as.character(presentNames["variable_name"])]
  
  contVars <- unique(c(contVars, contVars2))
  
  ## min and max for continuous -------------------------------------
  ## min and max specified or limits - need a sensible consistent appoach

  ## choose cols and drop variables without limits
  if (any(c("min", "max", "variable_limits") %in% names(presentNames))){
    vars4 <- c("variable_name", "min", "max", "variable_limits")
    whichVars4 <- vars4[vars4 %in% names(presentNames)]
    chooseCols <- presentNames[whichVars4]
    chooseCols <- unlist(chooseCols)
    limitsContinuous <- dfCodes[, chooseCols]

    ## rename to standard names
    names(limitsContinuous) <- whichVars4
    ## drop if all of min, max, limits missing
    limitsContinuous <-
      limitsContinuous[apply(!is.na(limitsContinuous[,whichVars4[-1]]),
                             1, any), ]
    ## put limits in min max
    limits_continuous <- 
      limitsContinuous %>%
      dplyr::mutate(lims = strsplit(variable_limits, "\\s+")) # %>%

    limits_continuous$l_min <- sapply(limits_continuous$lims,
                                      function(x) as.numeric(x[1]))
    limits_continuous$l_max <- sapply(limits_continuous$lims,
                                      function(x) as.numeric(x[2]))
    limits_continuous$min <- with(limits_continuous, ifelse(!is.na(min),
                                                            min, l_min))
    limits_continuous$max <- with(limits_continuous, ifelse(!is.na(max),
                                                            max, l_max))
    limits_continuous <- limits_continuous[,1:3]
  }

  
  ## store all codebook data away in a S3 "codebook" class
  code_book <- list(variable_names = varLabels[,1],
                    variable_labels = varLabels,
                    factor_names = factors,
                    factor_levels = factorLevels,
                    factor_info = factor.info,
                    continuous_names = contVars,
                    limits_continuous = limits_continuous, 
                    renamed_variables = renamed_table,
                    names_info = list(present_names = presentNames,
                                      absent_names = absentNames),
                    file_info = list(codebook_filename = codebook_filename,
                                     codebook_directory = codebook_directory,
                                     codebook_file = codebook_file,
                                     codebook_time = codebook_time,
                                     codebook_read_time = codebook_read_time,
                                     column_names = column_names,
                                     code_book = xCodes),
                    data_management_plan = data_management_plan
                    )
  comment(code_book) <- paste0("Codebook read from '", codebook_file,
                               "' at ", date())
  ## S3 class object containing codebook
  ## 
  ## @method default codebook
  class(code_book) <- "codebook"
  code_book
}

## for development --------------------

## codebook_directory <-  "../inst/demoFiles"
## x <- "data1_codebook.csv"
## column_names <- c(variable_original = "Old.Variable", variable_levels = "Factor.Levels", min = "Min", max = "Max")

## data1_codebook <- read_codebook("../inst/demoFiles/data1_codebook.csv",
##          column_names = list(variable_levels = "Factor.Levels",
##                              variable_original = "Old.Variable",
##                              min = "Min", max = "Max"))


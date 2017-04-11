# codebookr

**UNDER CONSTRUCTION**

**codebookr** is an *R* package under development to automate
cleaning, checking and formatting data using metadata from Codebooks
or Data Dictionaries. It is primarily aimed at epidemiological
research and medical studies but can be easily used in other research
areas.

Researchers collecting primary, secondary or tertiary data from RCTs
or government and hospital administrative systems often have different
data documentation and data cleaning needs to those scraping data off
the web or collecting in-house data for business analytics. However,
all studies will benefit from using codebooks which comprehensively
document all study variables including derived variables. Codebooks
document data formats, variable names, variable labels, factor levels,
valid ranges for continuous variables, details of measuring
instruments and so on.

For statistical consultants, each new data set has a new codebook.
While statisticians may get a photocopied codebook or pdf, my
preference is a spreadsheet so that the metadata can be used
directly. Many data analysts are happy to use this metadata to code
syntax to read, clean and check data. I prefer to automate this
process by reading the codebook into *R* and then using the metadata
directly for data checking, cleaning, factor level definitions.

While there is considerable interest in the data wrangling and
cleaning (Jonge and Loo 2013; Wickham 2014; Fischetti 2017), there
appear to be few tools available to read codebooks (see [here](http://jason.bryer.org/posts/2013-01-10/Function_for_Reading_Codebooks_in_R.html))
and even less to automatically apply the metadata to datasets.

Codebook examples are from research projects undertaken at University
of Queensland's School of Public Health and have subsequently been
used in biostatistics courses.

** References

Fischetti, Tony. 2017. Assertr: Assertive Programming for R Analysis Pipelines. [www](https://CRAN.R-project.org/package=assertr.)

Jonge, Edwin de, and Mark van der Loo. 2013. “An Introduction to Data Cleaning with R.” Technical Report 201313. Statistics Netherlands. [www](http://cran.vinastat.com/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf.)

Wickham, Hadley. 2014. “Tidy Data.” The Journal of Statistical Software 59 (10). [www](http://www.jstatsoft.org/v59/i10/.)

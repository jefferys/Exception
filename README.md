
<!-- README.md is generated from README.Rmd. Please edit that file -->
exception
=========

Provides exception objects for use in an exception hierarchy, and a means of stopping, warning, and messaging with these objects. By defining an explicit hierarchy, catching and handling errors in groups becomes possible. Common errors have default messages, with only meaningful parameters needed.

``` r
library("exception")

nsfEx <- NoSuchFileException( path= "waldo.txt" )
class(nsfEx)
#> [1] "NoSuchFileException" "FileException"       "PathException"      
#> [4] "FileSystemException" "IOException"         "Exception"          
#> [7] "condition"

# Anything in Hierarchy caught
tryCatch(
   stopWith( nsfEx ),
   NoSuchFileException= function(e) {print("Caught a NoSuchFileException")},
   IOException= function(e) {print("Caught an IOException.")},
   error= function(e) {print("Caught some exception")}
)
#> [1] "Caught a NoSuchFileException"

tryCatch(
   stopWith( nsfEx ),
   IOException= function(e) {print("Caught an IOException.")},
   error= function(e) {print("Caught some exception")}
)
#> [1] "Caught an IOException."

tryCatch(
   stopWith( nsfEx ),
   error= function(e) {print("Caught some exception")}
)
#> [1] "Caught some exception"

# Note that it is the first to match, not best match.
tryCatch(
   stopWith( nsfEx ),
   error= function(e) {print("Caught some exception")},
   NoSuchFileException= function(e) {print("Caught a NoSuchFileException")}
)
#> [1] "Caught some exception"

# Normal stop(), warning(), and message() signalling function do not wrapp
# objects as error, warning, or message, although will still trigger halts,  print warnings, or print messages.
tryCatch(
   stop( nsfEx ),
   error= function(e) {print("Caught some exception")},
   NoSuchFileException= function(e) {print("Caught a NoSuchFileException")}
)
#> [1] "Caught a NoSuchFileException"
```

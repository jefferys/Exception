#' Data accessors for IOExceptions
#'
#' Data elemets used in the \code{IOException} hierarchy each have there own S3
#' based accessor methods which can be used to extract data from an exception if
#' caught. All accessors can be used on any \code{Exception} derived object,
#' returning \code{NULL} if the data element is not defined for that exception.
#'
#' @name IOExceptionData
#'
#' @param e The \code{Exception} to query for a data element.
#'
#' @param ... Required for generic S3 method implementation, not used by any
#'   method defined here.
#'
#' @return \describe{
#'   \item{\code{exceptionPath}}{Returns a file system path (as a one-element character vector).}
#'   \item{\code{exceptionTarget}}{Returns the target of a link (as a one-element character vector).}
#' }
#'
#' @seealso \code{\link{IOException}}
NULL

#' @rdname IOExceptionData
#' @export
exceptionPath <- function (...) {
   UseMethod( "exceptionPath" )
}

#' @rdname IOExceptionData
#' @export
exceptionPath.Exception <- function (e, ...) { return (e$path) }

#' @rdname IOExceptionData
#' @export
exceptionTarget <- function (...) {
   UseMethod( "exceptionTarget" )
}

#' @rdname IOExceptionData
#' @export
exceptionTarget.Exception <- function (e, ...) { return (e$target) }

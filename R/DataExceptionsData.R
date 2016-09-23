#' Data accessors for DataExceptions
#'
#' Data elements used in the \code{DataException} hierarchy each have there own S3
#' based accessor methods which can be used to extract data from an exception if
#' caught. All accessors can be used on any \code{Exception} derived object,
#' returning \code{NULL} if the data element is not defined for that exception.
#'
#' @name DataExceptionData
#'
#' @param e The \code{Exception} to query for a data element.
#'
#' @param ... Required for generic S3 method implementation, not used by any
#'   method defined here.
#'
#' @return \describe{
#'   \item{\code{exceptionData}}{Returns the data causing the exception, format varies).}
#'   \item{\code{exceptionPath}}{Returns a file system path (as a one-element character vector).}
#'   \item{\code{exceptionLine}}{Returns a file line number(as a one-element integer vector).}
#' }
#'
#' @seealso \code{\link{DataException}}
NULL

#' @rdname DataExceptionData
#' @export
exceptionData <- function (...) {
	UseMethod( "exceptionData" )
}

#' @rdname DataExceptionData
#' @export
exceptionData.Exception <- function (e, ...) { return (e$data) }

#' @rdname DataExceptionData
#' @export
exceptionLine <- function (...) {
	UseMethod( "exceptionLine" )
}

#' @rdname DataExceptionData
#' @export
exceptionLine.Exception <- function (e, ...) { return (e$line) }

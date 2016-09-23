#' The DataException hierarchy
#'
#' \code{DataException}s and their sub-classes describe problems with data
#' values, names, or formats. Subclasses extended the specificity of the
#' \code{DataException}. Each takes a \code{data} argument and possibly others.
#' See \code{\link{Exception}}. These exceptions are intended to be signaled,
#' i.e. with \code{stop} and exist both to provide pre-defined error messages
#' and so that handlers in try/catch blocks have latitude to choose what
#' granularity of exception to catch.
#'
#' @param message The message associated with the exception, for reading by
#'   humans. The default message is usually good enough, but can be overridden
#'   through this parameter if needed. When constructed inside packages, the
#'   message displayed will have the name of the package prepended as
#'   \code{'[\var{package}] \var{message}'} due to \code{conditionMessage} being
#'   over-ridden for \code{Exception} subclasses.
#' @param call The call or call stack associated with the exception. By default
#'   this is NUL. Can be overridden to provide the \code{\link{Sys.calls}} call
#'   stack or a single \code{\link{Sys.call}}.
#' @param package The package where this exception is generated. May be
#'   \code{NULL} when exceptions are generated and signaled from interactive or
#'   script level code and not from a package. Attempts to guess the package by
#'   default. Will usually include the package in the displayed \code{message}.
#' @param data The data element associated with an exception. Format varies and
#'   should be described in subclass documentation. This may be \code{NA} but
#'   should not be \code{NULL}. Accessible via the the \code{exceptionData} S3
#'   accessor.
#' @param path The path associated with an exception, as a one element character
#'   vector This is required for a \code{FileFormatException} or sub-class. It
#'   may be \code{NA} but should not be \code{NULL}. Accessible via the the
#'   \code{exceptionPath} S3 accessor.
#' @param line The line associated with an exception, as a one element integer
#'   vector This is required for a \code{FileFormatException} or sub-class. It
#'   may be \code{NA} but should not be \code{NULL}. Accessible via the the
#'   \code{exceptionLine} S3 accessor.
#' @param ... Additional arguments defining \code{\var{arg}= \var{value}} data
#'   to include in the Exception (accessible as list elements, but preferably
#'   by S3 accessor functions).
#'
#' @describeIn DataException
#'
#' This exception and sub-classes are \code{Exception}s that describe
#' problems with data values, names, or formats.
#' \itemize{
#'    \item message: "\code{[\var{package}] A DataException occurred.}"
#'    \item class: \code{c( "DataException", "Exception", "condition" )}
#'    \item data: Defaults: \code{data = NA}.
#' }
#'
#' @return An \code{DataException} or descendant describing some problematic
#'   event involving a data values, names, or formats. Always is/extends
#'   class \code{c( "DataException", "Exception", "condition" )}.
#'
#' @seealso \code{condition}, \code{conditionMessage()}, \code{conditionCall},
#'   \code{Exception}, \code{exceptionPackage}, \code{exceptionData}
#'
#' @examples
#' dEx <- IOException()
#' conditionMessage(dEx)
#' conditionCall(dEx)
#' exceptionPackage(dEx)
#' exceptionData(dEx)
#'
#' @export
DataException <- function( data=NA, message= 'A DataException occurred.',
								 call= NULL, package=packageName(), ...
) {
	e <- Exception( message= message, call= call, package= package, data=data, ... )
	extendException( "DataException", e )
}

FileFormatMsg <- function (path= NA, line= NA, data= NA, wd= getwd() ) {
	sprintf( 'A FileFormatException occurred at line %i in file: "%s".'
				%pp% '(Running in: "%s").\n'
				%p% 'That line began: "%s".',
				line, path, wd, substr(data, 1, 80))
}

#' @describeIn DataException
#'
#' This exception and sub-classes are \code{Exception}s that describe
#' problems with data files
#' \itemize{
#'    \item message: "\code{[\var{package}] A FileFormatException occurred at
#'    	line \var{line} in file: "\var{file}". (Running in: "\var{getwd()}").
#'    	That line began: "\var{first 80 char}".}"
#'    \item class: \code{c( "FileFormatException", "DataException", "Exception",
#'       "condition" )}
#'    \item data: Defaults: \code{path = NA, line = NA, data = NA}.
#' }
#'
#' @usage \code{ FileFormatException( path = NA, line = NA, data = NA,
#'    message = *, call = NULL, package = packageName(), ...)}
#'
#' @export
FileFormatException <- function( path=NA, line=NA, data= NA,
	message= FileFormatMsg(line=line, path=path, data=data),
	call= NULL, package= packageName(), ...
) {
	dEx <- DataException( message=message, call= call, package= package,
			data=data, path=path, line=line, ... )
	extendException( "FileFormatException", dEx )
}

EmptyFileMsg <- function (path= NA, wd= getwd() ) {
	sprintf( 'An EmptyFileException occurred. File is unexpectedly empty: "%s".'
				%pp% '(Running in: "%s").', path, wd)
}

#' @describeIn DataException
#'
#' This exception indicates a data file was unexpectedly empty
#' \itemize{
#'    \item message:  [\var{package}] An EmptyFileException occurred. File
#'    is unexpectedly empty: "\var{file}". (Running in: "\var{getwd()}").
#'    \item class: \code{c( "EmptyFileException", "FileFormatException",
#'    	"DataException", "Exception", "condition" )}
#'    \item data: Defaults: \code{path = NA, data= NA, line= NA}.
#' }
#'
#' @usage \code{ EmptyException( path = NA, line = NA, data = NA,
#'    message = *, call = NULL, package = packageName(), ...)}
#'
#' @export
EmptyFileException <- function( path=NA, data=NA, line=NA,
	message= EmptyFileMsg( path=path ),
	call= NULL, package= packageName(), ...
) {
	ffEx <- FileFormatException( message=message, call= call, package= package,
								 data=data, path=path, line=line, ... )
	extendException( "EmptyFileException", ffEx )
}

FileNotEmptyMsg <- function (path= NA, wd= getwd() ) {
	sprintf( 'A FileNotEmptyException occurred. File was expected to be empty'
				%pp% 'but wasn\'t: "%s".'
				%pp% '(Running in: "%s").', path, wd)
}

#' @describeIn DataException
#'
#' This exception indicates a data file that should have been empty but wasn't.
#' \itemize{
#'    \item message:  [\var{package}] A FileNotEmptyException occurred. File was
#'    expectedly to be empty but wasn't: "\var{file}". (Running in:
#'    "\var{getwd()}").
#'    \item class: \code{c( "FileNotEmptyException", "FileFormatException",
#'    	"DataException", "Exception", "condition" )}
#'    \item data: Defaults: \code{path = NA, data= NA, line= NA}.
#' }
#'
#' @usage \code{ FileNotEmptyException( path = NA, line = NA, data = NA,
#'    message = *, call = NULL, package = packageName(), ...)}
#'
#' @export
FileNotEmptyException <- function( path=NA, data=NA, line=NA,
	message= FileNotEmptyMsg( path=path ),
	call= NULL, package= packageName(), ...
) {
	ffEx <- FileFormatException( message=message, call= call, package= package,
										 data=data, path=path, line=line, ... )
	extendException( "FileNotEmptyException", ffEx )
}

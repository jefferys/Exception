# Exceptions.R - Exceptions and associated routines
#
# Probably should be split out as a separate package at some point, but needs
# some work involving functionals and non-stantdard evaluations a-la assertthat
# https://github.com/hadley/assertthat and some work on easier creation of
# exception class hierarchies. non-standard evaluation can help both with
# bundling tests and the exception they throw and in handling exception
# parameters.

#' Create a condition object.
#'
#' Create an S3 object of class \code{"condition"} for use in \code{R}'s
#' condition handling system and as a base class for an exception class hierarchy.
#'
#' @param message A one-element character vector describing this
#'   condition/event. By default this is \code{"condition"}. It is fine to set
#'   this NULL as some subclasess will not need it (i.e. an extension class
#'   that only holds named data.)
#'
#' @param call The call or calls that were in process when this condition or
#'   event was generated. By default this is \code{NULL} as conditions need not
#'   be code related events (e.g. may be due to events triggered by
#'   environmental conditions or timers, etc.). \code{sys.calls()} can be used
#'   to obtain the whole call stack, and \code{sys.call()} can be used to access
#'   a single call.
#'
#' @return A condition object, of class "condition".
#'
#' @examples
#' e <- condition()
#' conditionMessage( e )
#' conditionCall( e )
#'
#' cond <- condition( "Something happened.", call= sys.calls() )
#'
#' \dontrun{
#'    # Catch a "condition" class error and turn it into a warning instead.
#'    tryCatch(
#'         { stop(cond) },
#'         condition= function(e) { warning(e) }
#'    )
#' }
#' @export
condition <- function( message= 'condition', call= NULL ) {
   structure( class= c("condition"),
              list( message= message, call= call ))
}

#' Create a base Exception object.
#'
#' The generated \code{Exception} object is a subclass of
#' \code{\link{condition}}. It describes an anomalous event and is intended to
#' be the basis of a hierarchy of exception classes. Exceptions are signaled by
#' calling \code{\link{stop}}, \code{\link{warning}}, or \code{\link{message}}
#' on an \code{Exception} or \code{Exception}-derived object and can be caught
#' individually or in groups using try/catch signal handlers. \code{Exception}
#' objects inherit the \code{message} and \code{call} fields from
#' \code{\link{condition}} and add a \code{package} field that
#' describes what package this exception event happened in. They also add any
#' unused \code{\var{argName}= \var{value}} arguments as extra data, storing
#' this inside the exception object for internal use.
#'
#' @section Extra data:
#'   Data provided through extra arguments are accessible by treating an
#'   \code{Exception} as a list. This is intended to be used only internally in
#'   the package or script that signaled the \code{Exception}. If data is
#'   included for use by handlers in external code (from other packages or
#'   users), S3 accessors should be defined. Similarly, external code should
#'   never need to include extra data in exceptions for use by internal code.
#'   Custom exception generating functions with appropriate arguments should be
#'   exported. See \code{\link{extendException}}.
#'
#' @section Exceptions are part of your API:
#'   If exceptions are visible to users they are part of that functions public
#'   API just like arguments are. Any exception that a function signals by e.g.
#'   \code{\link{stop}}, \code{\link{warning}}, \code{\link{message}}, etc, that
#'   are not handled within that function are visible to users. These should be
#'   clearly documented and generally can be changed only in a
#'   backward-compatible manner. User code that catches exceptions will depend
#'   on them being thrown with the same class name and having both the same
#'   parent classes and the same accessible data, if any. Defining custom
#'   exception classes and using S3 accessors for any data makes this easier.
#'
#' @param message
#'   The message associated with the exception. By default this is \code{"An
#'   Exception occurred."}. \code{conditionMessage(e)} is overridden for
#'   \code{Exception} and its extensions so that most of the time the
#'   name of package where the exception was generated will be prepended upon
#'   access, making the message appear to be \code{"[\var{package}]
#'   \var{message}"}.
#' @param call
#'   The call or call stack associated with the exception. By default this is
#'   the list returned by calling \code{\link{sys.calls}} at the point where
#'   this object was constructed. Set \code{NULL} when an exception does not
#'   involve code, or with \code{\link{sys.call}} when only a specific code line
#'   is involved.
#' @param package
#'   The package where this exception is generated; tries to guess this by
#'   default. When not called from within a package, this will likely be
#'   \code{NULL}, e.g. when exceptions are constructed interactively or from
#'   script. If given or guessed and not \code{NULL}, the package is
#'   automatically prepended to the message.
#' @param ...
#'   Additional arguments defining \code{\var{key}=\var{value}} data to include
#'   in the object. These are accessible by treating the generated exception as
#'   a list.
#' @param e
#'    An exception object to query for a message, will prepend '\code{[\var{package}]}'
#'    if the exception was generated from some package's code.
#'
#' @return \code{Exception} returns an \code{Event} object, which is an object
#'   of class \code{c("Exception", "condition")} describing some problematic
#'   event, usually signaled from within a package e.g. with \code{\link{stop}}.
#'
#'   \code{conditionMessage} called on a \code{Exception} object returns the message
#'   associated with that exception, with the \code{package} of the exception
#'   prepended.
#'
#' @seealso \code{\link{condition}}, \code{\link{conditionMessage}},
#'   \code{\link{conditionCall}}, \code{\link{exceptionPackage}, \code{\link{tryCatch}}}
#'
#' @examples
#' e <- Exception()
#' conditionMessage(e)
#' conditionCall(e)
#' exceptionPackage(e)
#'
#' e <- Exception( "Extra data included", x=1, y=2, z=list(A=1, B="two") )
#' x <- e$x
#' y <- e$y
#' z <- e$z
#'
#' \dontrun{
#'   stop(Exception( "Oops, that didn't work." ))
#'
#'   # Convert a fatal Exception into a waning.
#'   e <- Exception( "Lying about the package", package="NotMine" )
#'   tryCatch( stop(e), Exception= function(e) {warning(e)})
#' }
#'
#' @export
Exception <- function ( message= 'An Exception occurred.',
   call= sys.calls(), package= packageName(), ...
) {
   structure(
      class= c("Exception","condition"),
      merge(
         list(message=message, call=call, package=package),
         list(...)
      )
   )
}

#' @rdname Exception
#' @export
conditionMessage.Exception <- function(e) {
   if (is.character(e$package) && nchar(e$package) > 0) {
      return( paste0( '[', e$package, '] ', e$message ))
   }
   else {
      return( e$message )
   }
}

#' Access the package data in an Exception
#'
#' An S3 method to extract the package in which an Exception occurred. Can be
#' applied to any Exception derived object, but there is no default version.
#'
#' @param e The Exception object to query
#' @param ... Future-proof the S3 generic method; not used here.
#' @return The package where the \code{Exception} was generated.
#'
#' @seealso \code{Exception}
#'
#' @examples
#' e <- Exception()
#' package <- exceptionPackage(e)
#'
#' e <- IOException()
#' package <- exceptionPackage(e)
#'
#' @export
exceptionPackage <- function (...) {
   UseMethod( "exceptionPackage" )
}

#' @rdname exceptionPackage
#' @export
exceptionPackage.Exception <- function (e, ...) { return (e$package) }

#' Create a new exception from an existing one.
#'
#' This is used mainly to create exception constructors. Given a base exception,
#' this builds a new exception based on it. The base exception is usually
#' constructed inline as the \code{message}, \code{call}, \code{package}, and
#' any data properties in the new exception are inherited from the base
#' exception. They can only be set when the base exception is constructed and
#' can not be overriden by this extension constructor. The new exception can
#' have any additional specified exception class(es), but will also inherit the
#' classes of the base exception.
#'
#' It is not recommended to use \code{"extendException"} to directly create new
#' exceptions unless those exceptions are documented and used only in one place.
#' Repeatedly constructing the same class of exception is the job of a
#' constructor function, and an exception constructor function provides an
#' obvious place to put documentation.
#'
#' @param exception The new exception's class (name). Usually just a single
#'   class, but can be a vector of classes. This is prepended to the class
#'   vector of the \code{base} exception to create the complete class of the
#'   new exception
#' @param base An existing exception to base a new exception on. Often
#'   constructed in-line to allow setting parameters. By default this is the
#'   exception generated by a no-parameter call to "Exception()."
#'
#' @return \code{extendException} returns a new exception class inheriting the
#'   \code{message}, \code{call}, and class hierarchy of the \code{base=}
#'   exception, and all of that base class's data elements.
#'
#' @examples
#' # Function to create a new FruitException
#' FruitException <- function( fruit,
#'     message= sprintf("Sorry, %s is a fruit", fruit),
#'     call=NULL, package= packageName(), ...
#' ){
#'    e <- extendException( c("FruitException", "PedanticException"),
#'       base= Exception( message=message, call= call, package= package,
#'          fruit= fruit, ...)
#'    )
#'    return(e)
#' }
#'
#' fEx <- FruitException( fruit= "tomato" )
#' inherits(fEx, "FruitException")
#' inherits(fEx, "PedanticException")
#' inherits(e, "Exception")
#' inherits(e, "condition")
#' fEx$fruit == "tomato"   # Internal use only
#'
#' # Making fruit accessible by external users.
#' exceptionFruit <- function(...)  UseMethod( "exceptionFruit" )
#' exceptionFruit.Exception(e, ...) e$fruit
#'
#' exceptionFruit( fEx )
#'
#' # Will work for any Exception derived object
#' ex <- Exception( fruit= "Green Pepper", package="testing" )
#' exceptionFruit( ex ) == ex$fruit
#' conditionMessage( ex ) == "[testing] An Exception occurred."
#'
#' # Can use data variables in messages
#' ex <- Exception( message= paste0( "I hate ", fruit ),
#'    fruit= "Green Pepper", package="testing")
#' conditionMessage( ex ) == "[testing] I hate Green Pepper."
#'
#' \dontrun{
#'    # User putting in extra data for internal use
#'    withComplainerFEx <- FruitException( fruit= "tomato", complainer= "Bob")
#'    tryCatch(
#'       { stop(withComplainerFEx) },
#'       FruitException= function(e) {
#'          if (e$complainer == "Bob") {
#'             message(paste0( "Ignoring Bob's complaint: \"",
#'                             conditionMessage(e), "\"" ))
#'          }
#'       }
#'    )
#' }
#'
#' @export
extendException <- function( exception, base= Exception() ) {
   class(base) <- c(exception, class(base))
   return(base)
}

#' Better condition signaling with objects
#'
#' Use the \code{stopWith}, \code{warningWith}, and \code{messageWith} functions
#' as replacements for the base \code{\link{stop}}, \code{\link{warning}}, and
#' \code{\link{message}} functions when signaling with condition-based objects.
#' These re-base the object to the correct "condition" subclass based on how
#' they were signaled, adding either "error", "warning" and "message"
#' (respectively) just before "condition. They then matching this new object
#' with the matchin base \R function. This allows handlers to catch
#' signaled objects based on how they were signaled, and to either group simple
#' message based signals in with object signals, or to differentiate them.
#'
#' Normally, when a signal is caught, the handler that process it is based on
#' the class of the signal object. \R's familiar signaling functions
#' (\code{\link{stop}}, \code{\link{warning}}, and \code{\link{message}}) create
#' objects that have different classes based on which was used. However, if you
#' pass one of these signaling function an object rather than a simple message,
#' it does \emph{not} change the class. If you don't catch it, somehow \R still
#' knows they are different signals and does the right thing. However if the
#' signal is caught in user code, this information is lost. A handler can not
#' then recall \code{\link{stop}}, for instance, if it does not, after all, know
#' how to handle a signal (a common problem).
#'
#' @param condition The condition object being signaled with. If the root class
#'   (last) is not "condition", the object is resignaled as is.
#'
#' @return Nothing, called only for their signal generating and processing side
#'   effects.
#'
#' @name exceptionSignaling
NULL

#' @describeIn exceptionSignaling Moves or adds class \code{c("error",
#'   "condition")} to the end of the class of a condition object.
#'
#'   The \code{\link{stop}} function converts simple messages into objects of
#'   class \code{c("simpleError", "error", "condition")}; both can thus be
#'   caught with an \code{error=} handler, or they can be distinguished by the
#'   \code{"simpleError"} class that (should) only be on a simple message based
#'   signal.
#
#' @export
stopWith <- function ( condition ) {
	wasClass <- class( condition )
	if (wasClass[length(wasClass)] != "condition") {
		stop( condition )
	}
	else {
		baseClass <- wasClass[! wasClass %in% c("error", "condition")]
		newClass <- c( baseClass, "error", "condition" )
		class(condition) <- newClass
		stop( condition )
	}
}

#' @describeIn exceptionSignaling Moves or adds class \code{c("warning",
#'   "condition")} to the end of the class of the signal object.
#'
#'   The \code{\link{warning}} function converts simple messages into objects of
#'   class \code{c("simpleWarning", "warning", "condition")}; both can thus be
#'   caught with a \code{warning=} handler, or they can be distinguished by the
#'   \code{"simpleWarning"} class that (should) only be on a simple message
#'   based signal.
#
#' @export
warningWith <- function ( condition ) {
	wasClass <- class( condition )
	if (wasClass[length(wasClass)] != "condition") {
		warning( condition )
	}
	else {
		baseClass <- wasClass[! wasClass %in% c("warning", "condition")]
		newClass <- c( baseClass, "warning", "condition" )
		class(condition) <- newClass
		warning( condition )
	}
}

#' @describeIn exceptionSignaling Moves or adds class \code{c("message",
#'   "condition")} to the end of the class of the signal object.
#'
#'   The \code{\link{message}} function converts simple messages into objects of
#'   class \code{c("simpleMessage", "message", "condition")}; both can thus be
#'   caught with a \code{message=} handler, or they can be distinguished by the
#'   \code{"simpleMessage"} class that (should) only be on a simple message
#'   based signal.
#
#' @export
messageWith <- function ( condition ) {
	wasClass <- class( condition )
	if (wasClass[length(wasClass)] != "condition") {
		message( condition )
	}
	else {
		baseClass <- wasClass[! wasClass %in% c("message", "condition")]
		newClass <- c( baseClass, "message", "condition" )
		class(condition) <- newClass
		message( condition )
	}
}

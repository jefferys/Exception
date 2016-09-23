#' The IOException heierarchy.
#'
#' \code{IOException}s and their sub-classes are \code{Exception}s that describe
#' problems encoutered reading or writting to a file, network socket, console,
#' connection, etc. Subclasses extended the specificity of the
#' \code{IOException} and can include data elements. See
#' \code{\link{Exception}}. These exceptions are intended to be signaled, i.e.
#' with \code{stop} and exist both to provide pre-defined error messages and so
#' that handlers in try/catch blocks have latitude to choose what granularity of
#' exception to catch.
#'
#' @param message The message associated with the exception, for reading by
#'   humans. The default message is usually good enough, but can be overriden
#'   through this parameter if needed. When constructed inside packages, the
#'   message displayed will have the name of the package prepended as
#'   \code{'[\var{package}] \var{message}'} due to \code{conditionMessage} being
#'   over-ridden for \code{Exception} subclasses.
#' @param call The call or call stack associated with the exception. By default
#'   this is NULL as \code{IOException}s are usually environment problems. Can
#'   be overriden to provide the \code{\link{Sys.calls}} call stack or a single
#'   \code{\link{Sys.call}}.
#' @param package The package where this exception is generated. May be
#'   \code{NULL} when exceptions are generated and signaled from interactive or
#'   script level code and not from a package. Attempts to guess the package by
#'   default. Will usually include the package in the dispalyed \code{message}.
#' @param path The path associated with an exception, as a one element character
#'   vector This is required for a \code{PathException} or sub-class, but may be
#'   \code{NULL} or \code{NA}. Accessible via the the \code{exceptionPath} S3
#'   accessor.
#' @param target The target associated with a problematic file system link. This
#'   is required for a \code{LinkException} or sub-class, but may be \code{NULL}
#'   or \code{NA}. Accessible via the the \code{exceptionTarget} S3 accessor
#' @param ... Additional arguments defining \code{\var{arg}= \var{value}} data
#'   to include in the Exception (accessible as list elements, but preferably
#'   by S3 accessor functions).
#'
#' @describeIn IOException
#'
#' This exception and sub-classes are \code{Exception}s that describe
#' problems encoutered reading or writting to a file, network socket, console,
#' connection, etc.
#' \itemize{
#'    \item message: "\code{[\var{package}] An IOException occurred.}
#'    \item class: \code{c( "IOException", "Exception", "condition" )}
#'    \item data: None
#' }
#'

#' @return An \code{IOException} or descendant describing some problematic
#'   event involving a file system, a network socket, etc. Always has or extends
#'   class \code{c( "IOException", "Exception", "condition" )}.
#'
#' @seealso \code{condition}, \code{conditionMessage()}, \code{conditionCall},
#'   \code{Exception}, \code{exceptionPackage}
#'
#' @examples
#' ioEx <- IOException()
#' conditionMessage(e)
#' conditionCall(e)
#' exceptionPackage(e)
#'
#' ioEx <- IOException( "Including the call stack", call=Sys.calls() )
#' ioEx <- IOException( "Lying about the package", package="NotMine" )
#' ioEx <- IOException( "Extra data included", x=1, y=list(A=1, B="two"), path="../" )
#' x <- ioEx$x
#' y <- ioEx$y
#' path <- exceptionPath(ioEx)
#' path == ioEx$path
#'
#' pEx <- pathException( "/some/path" )
#' thePath <- exceptionPath(pEx)
#'
#' \dontrun{
#'   wantToCreate <- "theFile"
#'   if ( file.exists( wantToCreate )) {
#'      if (is.dir) { stop( DirectoryExists( wantToCreate )) }
#'      else { stop( FileExists( wantToCreate )) }
#'   }
#' }
#' @export
IOException <- function( message= 'An IOException occurred.',
                         call= NULL, package=packageName(), ...
) {
   e <- Exception( message= message, call= call, package= package, ... )
   extendException( "IOException", e )
}

#' @describeIn IOException
#'
#' This exception and sub-classes are \code{Exception}s that describe
#' problems encoutered reading or writting to a file system, disk, etc.
#' \itemize{
#'    \item message: "\code{[\var{package}] A FileSystemException occurred.}"
#'    \item class: \code{c( "FileSystemException", "IOException", "Exception",
#'                       "condition" )}
#'    \item data: None
#' }
#'
#' @export
FileSystemException <- function( message= 'A FileSystemException occurred.',
                                 call= NULL, package= packageName(), ...
) {
   ioEx <- IOException( message=message, call= call, package= package, ... )
   extendException( "FileSystemException", base= ioEx)
}

#' @describeIn IOException
#'
#' This exception and sub-classes are \code{FileSystemException}s that describe
#' problems encoutered in the context of an operation on a file system path.
#' Always includes a "path" data element, accessible with
#' \code{exceptionPath}.
#' \itemize{
#'    \item message: '\code{[\var{package}] A PathException occurred involving
#'                         path: "\var{path}". (Running in: "\var{getwd()}").}'
#'    \item class: \code{c( "PathException", "FileSystemException",
#'                 "IOException", "Exception", "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#' }
#'
#' @export
PathException <- function( path,
   message= sprintf(
      'A PathException occurred involving path: "%s". (Running in: "%s").',
      path, getwd()
   ), call= NULL, package= packageName(), ...
) {
   fsEx <- FileSystemException( message=message, call= call, package= package, path=path, ... )
   extendException( "PathException", base= fsEx)
}

#' @describeIn IOException
#'
#' This exception and sub-classes are \code{PathException}s that describe
#' problems reading, writing, creating, or deleting a file (not a directory).
#' Always includes a "path" data element, accessible with
#' \code{exceptionPath}. Issues with the \emph{contents} of a file are
#' not \code{IOExceptions}, those are likely \code{DataFormatException}s or
#' \code{DataValueException}s. The path may be a valid link to a file, or a
#' nested series of links, but this should be transparent. The code generating
#' the exception should identify \code{LinkExceptions} separately.
#' \itemize{
#'    \item message: \code{'[\var{package}] A FileException occurred involving
#'                         file: "\var{path}". (Running in: "\var{getwd()}").'}
#'    \item class: \code{c( "FileException", "PathException",
#'             "FileSystemException", "IOException", "Exception", "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#' }
#'
#' @export
FileException <- function( path,
   message= sprintf(
      'A FileException occurred involving file: "%s". (Running in: "%s").',
                              path, getwd()
                           ), call= NULL, package= packageName(), ...
) {
   fEx <- PathException( message=message, call= call, package= package, path=path, ... )
   extendException( "FileException", base= fEx )
}

#' @describeIn IOException
#'
#' This exception and sub-classes are \code{PathException}s that describe
#' problems reading, writing, creating, or deleting a directory. Always includes
#' a "path" data element, accessible with \code{exceptionPath}. The
#' path may be a valid link to a directory, or a nested series of links, but
#' this should be transparent. The code generating the exception should identify
#' \code{LinkExceptions} separately.
#' \itemize{
#'    \item message: \code{'[\var{package}] A DirectoryException occurred
#'            involving directory: "\var{path}". (Running in: "\var{getwd()}").'}
#'    \item class: \code{c( "DirectoryException", "PathException",
#'            "FileSystemException", "IOException", "Exception", "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#' }
#'
#' @export
DirectoryException <- function( path,
   message= sprintf(
      'A DirectoryException occurred involving directory: "%s". (Running in: "%s").',
      path, getwd()
   ), call= NULL, package= packageName(), ...
) {
   dEx <- PathException( message=message, call= call, package= package, path=path, ... )
   extendException( "DirectoryException", base= dEx )
}

#' @describeIn IOException
#'
#' This exception and sub-classes are \code{PathException}s that describe
#' problems creating, deleating, or using a hard or soft/symbolic link to a file
#' system object. Always includes a "path" data element, accessible with
#' \code{exceptionPath} and a "target" data element, accessible with
#' \code{exceptionTarget}. Problems with the target (so long as it
#' exists) are not a problem with the link but a problem with the target only.
#' The fact that the path is a link should be ignored in this case and the
#' exception should be based on the target object (file, directory, or
#' possibly a link.)
#' \itemize{
#'    \item message: \code{'[\var{package}] A LinkException occurred involving
#'             link: "\var{path}" with target: "\var{target}". (Running in:
#'             "\var{getwd()}").'}
#'    \item class: \code{c( "LinkException", "PathException",
#'            "FileSystemException", "IOException", "Exception", "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#'    \item data: \code{target} - see \code{exceptionTarget}.
#' }
#'
#' @export
LinkException <- function( path, target,
   message= sprintf(
      'A LinkException occurred involving link: "%s" with target: "%s". (Running in: "%s").',
      path, target, getwd()
   ), call= NULL, package= packageName(), ...
) {
   linkEx <- PathException( message=message, call= call, package= package,
                         path=path, target=target, ... )
   extendException( "LinkException", base= linkEx )
}

#' @describeIn IOException
#'
#' If a link's target is invalid
#' \itemize{
#'    \item message: \code{'[\var{package}] The link target does not exist;
#'             link: "\var{path}" with target: "\var{target}". (Running in:
#'             "\var{getwd()}").'}
#'    \item class: \code{c( "NoSuchLinkTargetException", "LinkException", "PathException",
#'            "FileSystemException", "IOException", "Exception", "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#'    \item data: \code{target} - see \code{exceptionTarget}.
#' }
#'
#' @export
NoSuchLinkTargetException <- function( path, target,
   message= sprintf(
      'The link target does not exist; link: "%s" with target: "%s". (Running in: "%s").',
      path, target, getwd()
   ), call= NULL, package= packageName(), ...
) {
   nsltEx <- LinkException( message=message, call= call, package= package,
                            path=path, target=target, ... )
   extendException( "NoSuchLinkTargetException", base= nsltEx )
}

#' @describeIn IOException
#'
#' The specified file was not found on the file system.
#' \itemize{
#'    \item message: \code{'[\var{package}] No such file: "\var{path}".
#'                          (Running in: "\var{getwd()}").'}
#'    \item class: \code{c( "NoSuchFileException", "FileException",
#'        "PathException", "FileSystemException", "IOException", "Exception",
#'        "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#' }
#'
#' @export
NoSuchFileException <- function( path,
   message= sprintf(
      'No such file: "%s". (Running in: "%s").',
      path, getwd()
   ), call= NULL, package= packageName(), ...
) {
   nsfEx <- FileException( message=message, call= call, package= package, path=path, ... )
   extendException( "NoSuchFileException", base= nsfEx )
}

#' @describeIn IOException
#'
#' The specified directory was not be found on the file system.
#' \itemize{
#'    \item message: \code{'[\var{package}] No such directory: "\var{path}".
#'                          (Running in: "\var{getwd()}").'}
#'    \item class: \code{c( "NoSuchDirectoryException", "DirectoryException",
#'        "PathException", "FileSystemException", "IOException", "Exception",
#'        "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#' }
#'
#' @export
NoSuchDirectoryException <- function( path,
   message= sprintf(
      'No such directory: "%s". (Running in: "%s").',
      path, getwd()
   ), call= NULL, package= packageName(), ...
) {
   nsdEx <- DirectoryException( message=message, call= call, package= package, path=path, ... )
   extendException( "NoSuchDirectoryException", base= nsdEx )
}

#' @describeIn IOException
#'
#' The specified link was not be found on the file system. Should only be used
#' when the path is expected by the user to be a link. Usually a link is used
#' transparently to identify a file or directory, in which case a
#' \code{NoSuchFileException} or \code{NoSuchDirectoryException} should be used.
#' \itemize{
#'    \item message: \code{'[\var{package}] No such link: "\var{path}".
#'                          (Running in: "\var{getwd()}").'}
#'    \item class: \code{c( "NoSuchLinkException", "LinkException",
#'        "PathException", "FileSystemException", "IOException", "Exception",
#'        "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#'    \item data: \code{target} - see \code{exceptionTarget}.
#' }
#'
#' @export
NoSuchLinkException <- function( path, target=NA,
   message= sprintf(
      'No such link: "%s". (Running in: "%s").',
      path, getwd()
   ), call= NULL, package= packageName(), ...
) {
   nslEx <- LinkException( message=message, call= call, package= package,
                           path=path, target=target, ... )
   extendException( "NoSuchLinkException", base= nslEx )
}

#' @describeIn IOException
#'
#' The specified file already exists on the file system.
#' \itemize{
#'    \item message: \code{'[\var{package}] File already exists: "\var{path}".
#'                          (Running in: "\var{getwd()}").'}
#'    \item class: \code{c( "FileExistsException", "FileException",
#'        "PathException", "FileSystemException", "IOException", "Exception",
#'        "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#' }
#'
#' @export
FileExistsException <- function( path,
   message= sprintf(
      'File already exists: "%s". (Running in: "%s").',
      path, getwd()
   ), call= NULL, package= packageName(), ...
) {
   feEx <- FileException( message=message, call= call, package= package, path=path, ... )
   extendException( "FileExistsException", base= feEx )
}

#' @describeIn IOException
#'
#' The specified directory already existins on the file system.
#' \itemize{
#'    \item message: \code{'[\var{package}] Directory already exists: "\var{path}".
#'                          (Running in: "\var{getwd()}").'}
#'    \item class: \code{c( "DirectoryExistsException", "DirectoryException",
#'        "PathException", "FileSystemException", "IOException", "Exception",
#'        "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#' }
#'
#' @export
DirectoryExistsException <- function( path,
   message= sprintf(
      'Directory already exists: "%s". (Running in: "%s").',
      path, getwd()
   ), call= NULL, package= packageName(), ...
) {
   deEx <- DirectoryException( message=message, call= call, package= package, path=path, ... )
   extendException( "DirectoryExistsException", base= deEx )
}

#' @describeIn IOException
#'
#' The specified link already exists on the file system. Should only be used
#' when the path is expected by the user to be a link. Usually a link is used
#' transparently to identify a file or directory, in which case a
#' \code{FileExistsException} or \code{DirectoryExistsException} should be used.
#' \itemize{
#'    \item message: \code{'[\var{package}] Link already exists: "\var{path}".
#'                          (Running in: "\var{getwd()}").'}
#'    \item class: \code{c( "LinkExistsException", "LinkException",
#'        "PathException", "FileSystemException", "IOException", "Exception",
#'        "condition" )}
#'    \item data: \code{path} - see \code{exceptionPath}.
#'    \item data: \code{target} - see \code{exceptionTarget}.
#' }
#'
#' @export
LinkExistsException <- function( path, target,
   message= sprintf(
      'Link already exists: "%s" with target: "%s". (Running in: "%s").',
      path, target, getwd()
   ), call= NULL, package= packageName(), ...
) {
   leEx <- LinkException( message=message, call= call, package= package,
                           path=path, target=target, ... )
   extendException( "LinkExistsException", base= leEx )
}

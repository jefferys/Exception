context("Testing IOExceptions and sub-classes.")

# Specific test fixture - test that a constructed IOexception has the correct
# content via all S3 accessors and for any additional data as well. Requires
# a list with the expected elements, see desc$ references below.
expect_exception_like <- function(ex, desc) {
   expect_equal( class( ex ), desc$class )
   expect_equalOrNull( conditionMessage( ex ), desc$displayMessage )
   expect_equalOrNull( conditionCall(    ex ), desc$call )
   expect_equalOrNull( exceptionPackage( ex ), desc$package )
   expect_equalOrNull( exceptionPath(    ex ), desc$path )
   expect_equalOrNull( exceptionTarget(  ex ), desc$target )
   for( datum in names(desc$data)) {
      expect_equal( ex[datum], desc$data[datum] )
   }
}

# Default description of an IO exception, including S3 data fields and extra
# data as a list
exceptionDesc <- function(c) {
   list(
      class= c,
      rawMessage="A test message",
      displayMessage="[testMe] A test message",
      call= sys.calls(),
      package= "testMe",
      path= "/some/path",
      target= "/some/target",
      data= list( A=1, B=list(a="amy", b="bob"))
   )
}

# Convert a list description of an exception to an argument for an
# exception constructor.
descToExecArgsArg <- function( desc ) {
   merge(
      list(
         message= desc$rawMessage,
         call= desc$call,
         package= desc$package,
         path= desc$path,
         target= desc$target
      ),
      desc$data
   )
}

# To be definitive in testing.
thisPackage <- utils::packageName()

describe( "IOException", {
   IOExceptionClasses = c( "IOException", "Exception", "condition" )
   it( "Constructs the expected default object", {
      got <- IOException()
      desc <- list(
         class=    IOExceptionClasses,
         displayMessage= sprintf("[%s] An IOException occurred.", thisPackage),
         call=     NULL,
         package=  thisPackage
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( IOExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "IOException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( IOException(package= NULL ))
      want <- "An IOException occurred."
      expect_equal( got, want )
      got <- conditionMessage( IOException(message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "FileSystemException", {
   FileSystemExceptionClasses = c( "FileSystemException", "IOException",
                                 "Exception", "condition" )
   it( "Constructs the expected default object", {
      got <- FileSystemException()
      desc <- list(
         class=    FileSystemExceptionClasses,
         displayMessage=  sprintf("[%s] A FileSystemException occurred.", thisPackage),
         call=     NULL,
         package=  thisPackage
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( FileSystemExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "FileSystemException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( FileSystemException(package= NULL ))
      want <- "A FileSystemException occurred."
      expect_equal( got, want )
      got <- conditionMessage( FileSystemException(message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "PathException", {
   PathExceptionClasses = c( "PathException", "FileSystemException",
                                 "IOException", "Exception", "condition" )
   somePath <- "../"
   rawMessage = paste0(
      sprintf( 'A PathException occurred involving path: "%s". ', somePath ),
      sprintf( '(Running in: "%s").', getwd() )
   )
   it( "Constructs the expected default object", {
      got <- PathException(path= somePath)
      desc <- list(
         class=    PathExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( PathExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "PathException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( PathException(path=somePath, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( PathException(somePath, message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "FileException", {
   FileExceptionClasses = c( "FileException", "PathException",
            "FileSystemException", "IOException", "Exception", "condition" )
   somePath <- "../"
   rawMessage = paste0(
      sprintf( 'A FileException occurred involving file: "%s". ', somePath ),
      sprintf( '(Running in: "%s").', getwd() )
   )
   it( "Constructs the expected default object", {
      got <- FileException(path= somePath)
      desc <- list(
         class=    FileExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( FileExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "FileException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( FileException(path=somePath, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( FileException(somePath, message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "DirectoryException", {
   DirectoryExceptionClasses = c( "DirectoryException", "PathException",
            "FileSystemException", "IOException", "Exception", "condition" )
   somePath <- "../"
   rawMessage = paste0(
      sprintf( 'A DirectoryException occurred involving directory: "%s". ', somePath ),
      sprintf( '(Running in: "%s").', getwd() )
   )
   it( "Constructs the expected default object", {
      got <- DirectoryException(path= somePath)
      desc <- list(
         class=    DirectoryExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( DirectoryExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "DirectoryException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( DirectoryException(path=somePath, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( DirectoryException(
         somePath, message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "LinkException", {
   LinkExceptionClasses = c( "LinkException", "PathException",
               "FileSystemException", "IOException", "Exception", "condition" )
   somePath <- "../"
   someTarget <- "not/me"
   rawMessage = paste0(
      sprintf( 'A LinkException occurred involving link: "%s" ', somePath ),
      sprintf( 'with target: "%s". (Running in: "%s").', someTarget, getwd() )
   )
   it( "Constructs the expected default object", {
      got <- LinkException(path= somePath, target= someTarget)
      desc <- list(
         class=    LinkExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath,
         target= someTarget
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( LinkExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "LinkException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( LinkException(path= somePath, target= someTarget,
                                             package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( LinkException(
         somePath, someTarget, message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "NoSuchLinkTargetException", {
   NoSuchLinkTargetExceptionClasses = c( "NoSuchLinkTargetException", "LinkException",
      "PathException", "FileSystemException", "IOException", "Exception",
      "condition" )
   somePath <- "../"
   someTarget <- "not/me"
   rawMessage = paste0(
      sprintf( 'The link target does not exist; link: "%s" ', somePath ),
      sprintf( 'with target: "%s". (Running in: "%s").', someTarget, getwd() )
   )
   it( "Constructs the expected default object", {
      got <- NoSuchLinkTargetException(path= somePath, target= someTarget)
      desc <- list(
         class=    NoSuchLinkTargetExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath,
         target= someTarget
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( NoSuchLinkTargetExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "NoSuchLinkTargetException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( NoSuchLinkTargetException(path= somePath, target= someTarget,
                                             package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( NoSuchLinkTargetException(
         somePath, someTarget, message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "NoSuchFileException", {
   NoSuchFileExceptionClasses = c( "NoSuchFileException", "FileException",
      "PathException", "FileSystemException", "IOException", "Exception",
      "condition" )
   somePath <- "../"
   rawMessage = paste0(
      sprintf( 'No such file: "%s". ', somePath ),
      sprintf( '(Running in: "%s").', getwd() )
   )
   it( "Constructs the expected default object", {
      got <- NoSuchFileException(path= somePath)
      desc <- list(
         class=    NoSuchFileExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( NoSuchFileExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "NoSuchFileException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( NoSuchFileException(path=somePath, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( NoSuchFileException( somePath,
                                       message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "NoSuchDirectoryException", {
   NoSuchDirectoryExceptionClasses = c( "NoSuchDirectoryException",
      "DirectoryException", "PathException", "FileSystemException",
      "IOException", "Exception", "condition" )
   somePath <- "../"
   rawMessage = paste0(
      sprintf( 'No such directory: "%s". ', somePath ),
      sprintf( '(Running in: "%s").', getwd() )
   )
   it( "Constructs the expected default object", {
      got <- NoSuchDirectoryException(path= somePath)
      desc <- list(
         class=    NoSuchDirectoryExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( NoSuchDirectoryExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "NoSuchDirectoryException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( NoSuchDirectoryException(path=somePath, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( NoSuchDirectoryException( somePath,
                                                    message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "NoSuchLinkException", {
   NoSuchLinkExceptionClasses = c(  "NoSuchLinkException", "LinkException",
      "PathException", "FileSystemException", "IOException", "Exception",
      "condition" )
   somePath <- "../"
   rawMessage = paste0(
      sprintf( 'No such link: "%s". ', somePath ),
      sprintf( '(Running in: "%s").', getwd() )
   )
   it( "Constructs the expected default object", {
      got <- NoSuchLinkException(path= somePath)
      desc <- list(
         class=    NoSuchLinkExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath,
         target= NA
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( NoSuchLinkExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "NoSuchLinkException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( NoSuchLinkException(path=somePath, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( NoSuchLinkException( somePath,
            message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "FileExistsException", {
   FileExistsExceptionClasses = c( "FileExistsException", "FileException",
      "PathException", "FileSystemException", "IOException", "Exception",
      "condition" )
   somePath <- "../"
   rawMessage = paste0(
      sprintf( 'File already exists: "%s". ', somePath ),
      sprintf( '(Running in: "%s").', getwd() )
   )
   it( "Constructs the expected default object", {
      got <- FileExistsException(path= somePath)
      desc <- list(
         class=    FileExistsExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( FileExistsExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "FileExistsException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( FileExistsException(path=somePath, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( FileExistsException( somePath,
                                                    message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "DirectoryExistsException", {
   DirectoryExistsExceptionClasses = c( "DirectoryExistsException",
      "DirectoryException", "PathException", "FileSystemException",
      "IOException", "Exception", "condition" )
   somePath <- "../"
   rawMessage = paste0(
      sprintf( 'Directory already exists: "%s". ', somePath ),
      sprintf( '(Running in: "%s").', getwd() )
   )
   it( "Constructs the expected default object", {
      got <- DirectoryExistsException(path= somePath)
      desc <- list(
         class=    DirectoryExistsExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( DirectoryExistsExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "DirectoryExistsException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( DirectoryExistsException(path=somePath, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( DirectoryExistsException( somePath,
                                                    message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})
describe( "LinkExistsException", {
   LinkExistsExceptionClasses = c( "LinkExistsException", "LinkException",
      "PathException", "FileSystemException", "IOException", "Exception",
      "condition" )
   somePath <- "../"
   someTarget <- "not/me"
   rawMessage = paste0(
      sprintf( 'Link already exists: "%s" ', somePath ),
      sprintf( 'with target: "%s". (Running in: "%s").', someTarget, getwd() )
   )
   it( "Constructs the expected default object", {
      got <- LinkExistsException(path= somePath, target= someTarget)
      desc <- list(
         class=    LinkExistsExceptionClasses,
         displayMessage=  sprintf("[%s] %s", thisPackage, rawMessage),
         call=     NULL,
         package=  thisPackage,
         path= somePath,
         target= someTarget
      )
      expect_exception_like( got, desc )
   })
   it( "Can set message, call, path, target, and extra data", {
      desc <- exceptionDesc( LinkExistsExceptionClasses )
      argList <- descToExecArgsArg( desc )
      got <- do.call( "LinkExistsException", argList )
      expect_exception_like( got, desc )
   })
   it( "Displays raw message if package is null", {
      got <- conditionMessage( LinkExistsException( path= somePath,
         target= someTarget, package= NULL ))
      want <- rawMessage
      expect_equal( got, want )
      got <- conditionMessage( LinkExistsException( somePath, someTarget,
         message= "new message", package= NULL ))
      want <- "new message"
      expect_equal( got, want )
   })
})


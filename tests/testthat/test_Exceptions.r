context( "Testing exceptions." )

# To be definitive in testing.
thisPackage <- utils::packageName()

describe( "condition() object constructor", {

   defaultCondition <- condition()
   messageCondition <- condition( message= "a message" )

   aCallStack <- sys.calls()
   callCondition <- condition( message="Now with calls.", call= aCallStack)

   it( "generates  'condition' class object", {
      expect_equal( class( defaultCondition ), 'condition' )
      expect_equal( class( messageCondition ), 'condition' )
      expect_equal( class(    callCondition ), 'condition' )
   })
   it( "generates objects with the expected message content", {
      expect_equal( conditionMessage( defaultCondition ),       'condition' )
      expect_equal( conditionMessage( messageCondition ),       'a message' )
      expect_equal( conditionMessage(    callCondition ), 'Now with calls.' )
   })
   it( "generates objects with the expected calls content", {
      expect_null(  conditionCall( defaultCondition ))
      expect_null(  conditionCall( messageCondition ))
      expect_equal( conditionCall( callCondition ), aCallStack )
   })
})
describe( "Exception() object constructor and accessors", {

   defaultException <- Exception()
   messageException <- Exception( message= "a message" )
   packageException <- Exception( message= "a package", package="Bob" )

   aCallStack <- sys.calls()
   callException <- Exception( message="Now without calls.", call= NULL, package= NULL)

   xVec <- c(1,2)
   yList <- list(A=1,B=list(C=1, D=2))
   dataException <- Exception( message="With X and Y", X=xVec, Y=yList )

   it( "generates objects with expected class hierarchy", {
      want <- c('Exception', 'condition')
      expect_equal( class( defaultException ), want )
      expect_equal( class( messageException ), want )
      expect_equal( class( packageException ), want )
      expect_equal( class(    callException ), want )
      expect_equal( class(    dataException ), want )
   })
   it( "generates objects with expected accessible internal data", {
      expect_equal( defaultException$message, 'An Exception occurred.' )
      expect_equal( messageException$message, 'a message' )
      expect_equal( packageException$message, 'a package' )
      expect_equal( callException$message, 'Now without calls.' )
      expect_equal( dataException$message, 'With X and Y' )
      expect_equal( defaultException$call[1:length(aCallStack)], aCallStack[] )
      expect_equal( messageException$call[1:length(aCallStack)], aCallStack[] )
      expect_equal( packageException$call[1:length(aCallStack)], aCallStack[] )
      expect_null( callException$call )
      expect_equal( dataException$call[1:length(aCallStack)], aCallStack[] )
      expect_equal( defaultException$package, thisPackage )
      expect_equal( messageException$package, thisPackage )
      expect_equal( packageException$package, 'Bob' )
      expect_null( callException$package )
      expect_equal( dataException$package, thisPackage )
      expect_null( defaultException$X )
      expect_null( defaultException$Y )
      expect_null( messageException$X )
      expect_null( messageException$Y )
      expect_null( packageException$X )
      expect_null( packageException$Y )
      expect_null( callException$X )
      expect_null( callException$Y )
      expect_equal( dataException$X, xVec )
      expect_equal( dataException$Y, yList )
   })
   it( "generates objects with the expected conditionMessage() message content", {
      expect_equal( conditionMessage( defaultException ),
                    '[' %p% thisPackage %p% '] An Exception occurred.' )
      expect_equal( conditionMessage( messageException ),
                    '[' %p% thisPackage %p% '] a message' )
      expect_equal( conditionMessage( packageException ), '[Bob] a package' )
      expect_equal( conditionMessage(    callException ), 'Now without calls.' )
   })
   it( "generates objects with the expected conditionCall() call content", {
      # aCallStack[] is required as subseting pairlists converts them to lists...
      expect_equal( conditionCall( defaultException )[1:length(aCallStack)], aCallStack[] )
      expect_equal( conditionCall( messageException )[1:length(aCallStack)], aCallStack[] )
      expect_equal( conditionCall( packageException )[1:length(aCallStack)], aCallStack[] )
      expect_null(  conditionCall( callException ))
   })
   it( "generates objects with the expected exceptionPackage() package content", {
      expect_equal( exceptionPackage( defaultException ), thisPackage )
      expect_equal( exceptionPackage( messageException ), thisPackage )
      expect_equal( exceptionPackage( packageException ), 'Bob' )
      expect_null(  exceptionPackage( callException    ))
   })
})
describe( "Extending an exception with extendException()", {
   it( "Extends to the correct class", {
      e <- extendException( "ChildException" )
      got <- class( e )
      want <- c("ChildException", "Exception", "condition")
      expect_equal( got, want )

      ee <- extendException( "NewException", base=e )
      got <- class( ee )
      want <- c("NewException", "ChildException", "Exception", "condition")
      expect_equal( got, want )

   })
   it( "Extends to the correct class with multiple classes", {

      e <- extendException( c( "NewException1", "NewException2" ))
      got <- class( e )
      want <- c("NewException1", "NewException2", "Exception", "condition")
      expect_equal( got, want )

      ee <- extendException( c("newNewException1", "newNewException2"), base=e )
      got <- class( ee )
      want <- c("newNewException1", "newNewException2",
                "NewException1", "NewException2", "Exception", "condition")
      expect_equal( got, want )

   })
   it( "Extending an exception retains or overides the base message.", {
      e <- extendException( "NewException", base=Exception() )
      got <- conditionMessage( e )
      want <- '[' %p% thisPackage %p% '] An Exception occurred.'
      expect_equal( got, want )

      e <- extendException( "NewException", base=Exception("New message.") )
      got <- conditionMessage( e )
      want <- '[' %p% thisPackage %p% '] New message.'
      expect_equal( got, want )
   })
   it( "Can over-ride package", {
      e <- extendException( "NewException", Exception(package= "testPackageException" ))
      got <- exceptionPackage( e )
      want <- "testPackageException"
      expect_equal( got, want )
      got <- conditionMessage( e )
      want <- '[testPackageException] An Exception occurred.'
      expect_equal( got, want )

      ee <- extendException( "newNewException", base= e )
      got <- exceptionPackage( ee )
      want <- "testPackageException"
      expect_equal( got, want )
      got <- conditionMessage( ee )
      want <- '[testPackageException] An Exception occurred.'
      expect_equal( got, want )
   })
   it( "Extending an exception overrides the call, if specified", {
      calls <- sys.calls()

      e <- extendException( "NewException", base=Exception() )
      want <- calls[]
      got <- conditionCall( e )[1:length(want)]
      expect_equal( got, want )

      ee <- extendException( "NewNewException", base=e )
      want <- calls[]
      got <- conditionCall( ee )[1:length(want)]
      expect_equal( got, want )

      e <- extendException( "NewException", base=Exception( call= NULL ))
      expect_null( conditionCall( e ))

      ee <- extendException( "NewNewException", base=e )
      expect_null( conditionCall( ee ))

   })
   it( "Extending an exception sets or overrides data arguments, if specified", {

      e <- extendException( "NewException", base=Exception(
                            arg1= "arg1 value", arg2= "arg2 value" ))
      got <- c(e$arg1, e$arg2, e$arg3)
      want <- c("arg1 value", "arg2 value", NULL)
      expect_equal( got, want )

      ee <- extendException( "NewNewException", base=e )
      got <- c(ee$arg1, ee$arg2, ee$arg3)
      want <- c("arg1 value", "arg2 value", NULL)
      expect_equal( got, want )

   })
})
describe( "Signaling with object", {
	it( 'signals as expected if not a condition object', {
		bob <- structure( class="bobbob", list( theBobs=c( "bob", "bob" )))
		expect_error(      stopWith( bob ))
		expect_warning( warningWith( bob ))
		expect_message( messageWith( bob ))
	})
	it( 'signals as expected if is a condition object', {
		expect_error( stopWith( simpleError(     "bob" )), "^bob$" )
		expect_error( stopWith( simpleWarning(   "bob" )), "^bob$" )
		expect_error( stopWith( simpleMessage(   "bob" )), "^bob$" )
		expect_error( stopWith( simpleCondition( "bob" )), "^bob$" )

		expect_warning( warningWith( simpleError(     "bob" )), "^bob$" )
		expect_warning( warningWith( simpleWarning(   "bob" )), "^bob$" )
		expect_warning( warningWith( simpleMessage(   "bob" )), "^bob$" )
		expect_warning( warningWith( simpleCondition( "bob" )), "^bob$" )

		expect_message( messageWith( simpleError(     "bob" )), "^bob$" )
		expect_message( messageWith( simpleWarning(   "bob" )), "^bob$" )
		expect_message( messageWith( simpleMessage(   "bob" )), "^bob$" )
		expect_message( messageWith( simpleCondition( "bob" )), "^bob$" )

	})
	it( 'Does not preserve class of non-condition objects', {
		bob <- structure( class="bobbob", list( theBobs=c( "bob", "bob" )))
		got <- tryCatch(
			{stopWith( bob ); 42},
			Bob= function (c) fail('Should not have caught as "Bob"'),
			error= function (c) {
				expect_true( inherits(c, "simpleError") && inherits(c, "error")
								 && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
		got <- tryCatch(
			{ warningWith( bob ); 42 },
			Bob= function (c) fail('Should not have caught as "Bob"'),
			warning= function (c) {
				expect_true( inherits(c, "simpleWarning") && inherits(c, "warning")
								 && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
		got <- tryCatch(
			{ messageWith( bob ); 42 },
			Bob= function (c) fail('Should not have caught as "Bob"'),
			message= function (c) {
				expect_true( inherits(c, "simpleMessage") && inherits(c, "message")
								 && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
	})
	it( 'Adds correct classes based on signal, without loosing existing classes' )
		got <- tryCatch(
			{stopWith( simpleError("bob") ); 42},
			error= function (c) {
				expect_true( inherits(c, "simpleError") && inherits(c, "error")
								 && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
		got <- tryCatch(
			{stopWith( simpleWarning("bob") ); 42},
			error= function (c) {
				expect_true( inherits(c, "simpleWarning") && inherits(c, "warning")
								 && inherits(c, "error") && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
		got <- tryCatch(
			{warningWith( simpleError("bob") ); 42},
			warning= function (c) {
				expect_true( inherits(c, "simpleError") && inherits(c, "error")
								 && inherits(c, "warning") && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
		got <- tryCatch(
			{warningWith( simpleWarning("bob") ); 42},
			warning= function (c) {
				expect_true( inherits(c, "simpleWarning") && inherits(c, "warning")
								 && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
		got <- tryCatch(
			{messageWith( simpleWarning("bob") ); 42},
			message= function (c) {
				expect_true( inherits(c, "simpleWarning") && inherits(c, "message")
								 && inherits(c, "warning") && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
		got <- tryCatch(
			{messageWith( simpleMessage("bob") ); 42},
			message= function (c) {
				expect_true( inherits(c, "simpleMessage") && inherits(c, "message")
								 && inherits(c, "condition" ))
			}
		)
		expect_false( got == 42 )
})

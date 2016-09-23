context("Testing DataExceptions and sub-classes.")

# Specific test fixture - test that a constructed IOexception has the correct
# content via all S3 accessors and for any additional data as well. Requires
# a list with the expected elements, see desc$ references below.
expect_DataException_like <- function(ex, desc) {
	expect_equal( class( ex ), desc$class )
	expect_equalOrNull( conditionMessage( ex ), desc$displayMessage )
	expect_equalOrNull( conditionCall(    ex ), desc$call )
	expect_equalOrNull( exceptionPackage( ex ), desc$package )
	expect_equalOrNull( exceptionData(    ex ), desc$data )
	expect_equalOrNull( exceptionPath(    ex ), desc$path )
	expect_equalOrNull( exceptionLine(    ex ), desc$line )
	for( datum in names(desc$extData)) {
		expect_equal( ex[datum], desc$extData[datum] )
	}
}

dataExceptionDescription <- function(c) {
	list(
		class= c,
		rawMessage="A test message",
		displayMessage="[testMe] A test message",
		call= sys.calls(),
		package= "testMe",
		data= c(1,2,3),
		extData= list( A=1, B=list(a="amy", b="bob")),
		path= "/not/me.txt",
		line= 4242
	)
}

# Convert a list description of an exception to a list argument for use in a
# call("exceptionName").
dataExceptionDescriptionAsArguments <- function( desc ) {
	merge(
		list(
			message= desc$rawMessage,
			call= desc$call,
			package= desc$package,
			data= desc$data,
			path= desc$path,
			line= desc$line
		),
		desc$extData
	)
}

# To be definitive in testing.
thisPackage <- utils::packageName()

describe( 'Format string accessors generate correct messages', {
	it( "FileFormatMsg()", {
		got <- FileFormatMsg( line=123, path='FILE', wd='DIR', data='TEXT')
		want <- 	('A FileFormatException occurred at line 123 in file:'
					 %pp% '"FILE". (Running in: "DIR").\n'
					 %p% 'That line began: "TEXT".')
		expect_equal(got, want)
	})
	it( "EmptyFileMsg()", {
		got <- EmptyFileMsg( path='FILE', wd='DIR')
		want <- 	('An EmptyFileException occurred. File is unexpectedly empty:'
					 %pp% '"FILE". (Running in: "DIR").')
		expect_equal(got, want)
	})
	it( "FileNotEmptyMsg()", {
		got <- FileNotEmptyMsg( path='FILE', wd='DIR')
		want <- 	('A FileNotEmptyException occurred. File was expected to be empty'
					 %pp% 'but wasn\'t: "FILE". (Running in: "DIR").')
		expect_equal(got, want)
	})
})
describe( "DataException", {
	DataExceptionClasses = c( "DataException", "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- DataException()
		desc <- list(
			class=    DataExceptionClasses,
			displayMessage= sprintf("[%s] A DataException occurred.", thisPackage),
			call=     NULL,
			data= NA,
			package=  thisPackage
		)
		expect_DataException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- dataExceptionDescription( DataExceptionClasses )
		argList <- dataExceptionDescriptionAsArguments( desc )
		got <- do.call( "DataException", argList )
		expect_DataException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( DataException(package= NULL ))
		want <- "A DataException occurred."
		expect_equal( got, want )
		got <- conditionMessage( DataException(message= "new message", package= NULL, data=42 ))
		want <- "new message"
		expect_equal( got, want )
	})
})
describe( "FileFormatException", {
	FileFormatExceptionClasses = c( "FileFormatException","DataException",
											  "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- FileFormatException()
		desc <- list(
			class=    FileFormatExceptionClasses,
			displayMessage= ('[' %p% thisPackage %p% ']' %pp% FileFormatMsg()),
			call= NULL,
			path= NA,
			data= NA,
			line= NA,
			package= thisPackage
		)
		expect_DataException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- dataExceptionDescription( FileFormatExceptionClasses )
		argList <- dataExceptionDescriptionAsArguments( desc )
		got <- do.call( "FileFormatException", argList )
		expect_DataException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( FileFormatException(path=NA))
		want <- ('[' %p% thisPackage %p% ']' %pp% FileFormatMsg(wd= getwd()))
		expect_equal( got, want )
		got <- conditionMessage( FileFormatException(
			message= "new message", package= NULL, path="bob", data=42 ))
		want <- "new message"
		expect_equal( got, want )
	})
})
describe( "EmptyFileException", {
	EmptyFileExceptionClasses = c( "EmptyFileException", "FileFormatException",
		"DataException", "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- EmptyFileException()
		desc <- list(
			class=    EmptyFileExceptionClasses,
			displayMessage= ('[' %p% thisPackage %p% ']' %pp% EmptyFileMsg()),
			call=     NULL,
			path= NA,
			data= NA,
			line= NA,
			package=  thisPackage
		)
		expect_DataException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- dataExceptionDescription( EmptyFileExceptionClasses )
		argList <- dataExceptionDescriptionAsArguments( desc )
		got <- do.call( "EmptyFileException", argList )
		expect_DataException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( EmptyFileException(path=NA))
		want <- ('[' %p% thisPackage %p% ']' %pp% EmptyFileMsg())
		expect_equal( got, want )
		got <- conditionMessage( EmptyFileException(
			message= "new message", package= NULL, path="bob", data=42 ))
		want <- "new message"
		expect_equal( got, want )
	})
})
describe( "FileNotEmptyException", {
	FileNotEmptyExceptionClasses = c( "FileNotEmptyException", "FileFormatException",
		"DataException", "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- FileNotEmptyException()
		desc <- list(
			class=    FileNotEmptyExceptionClasses,
			displayMessage= ('[' %p% thisPackage %p% ']' %pp% FileNotEmptyMsg()),
			call=     NULL,
			path= NA,
			data= NA,
			line= NA,
			package=  thisPackage
		)
		expect_DataException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- dataExceptionDescription( FileNotEmptyExceptionClasses )
		argList <- dataExceptionDescriptionAsArguments( desc )
		got <- do.call( "FileNotEmptyException", argList )
		expect_DataException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( FileNotEmptyException(path=NA))
		want <- ('[' %p% thisPackage %p% ']' %pp% FileNotEmptyMsg())
		expect_equal( got, want )
		got <- conditionMessage( FileNotEmptyException(
			message= "new message", package= NULL, path="bob", data=42 ))
		want <- "new message"
		expect_equal( got, want )
	})
})

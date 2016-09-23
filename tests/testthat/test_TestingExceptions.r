context( "Tests for Exception tests")

describe( "Checking conditions with expect_condition", {
   it( "Passes when condition of expected type is signaled", {
      expect_success( expect_condition( stop( "Oops" )))
      expect_success( expect_condition( stop( "Oops" ), "condition" ))
      expect_success( expect_condition( stop( "Oops" ), "error" ))
      expect_success( expect_condition( warning( "Oops" )))
      expect_success( expect_condition( warning( "Oops" ), "condition" ))
      expect_success( expect_condition( warning( "Oops" ), "warning" ))
      expect_success( expect_condition( stop( Exception( call= NULL ))))
      expect_success( expect_condition( stop( Exception( call= NULL )), "condition" ))
      expect_success( expect_condition( stop( Exception( call= NULL )), "Exception" ))
      expect_success( expect_condition( warning( Exception( call= NULL )), "condition" ))
      expect_success( expect_condition( warning( Exception( call= NULL )), "Exception" ))
      expect_success(
         expect_condition( stop( Exception( call= NULL )), c( "Foo", "condition" ))
      )
      expect_success(
         expect_condition( warning( Exception( call= NULL )), c( "Exception", "Foo" ))
      )
   })
   it( "Fails when condition of unexpected type is thrown", {
      wantRE <- "\\QCondition inherits from `simpleError/error/condition` not `warning`\\E"
      expect_failure( expect_condition( stop( "Oops" ), "warning" ), wantRE )
      wantRE <- "\\QCondition inherits from `simpleWarning/warning/condition` not `error`\\E"
      expect_failure( expect_condition( warning( "Oops" ), "error" ), wantRE )
      wantRE <- "\\QCondition inherits from `Exception/condition` not `Bob`\\E"
      expect_failure( expect_condition( stop( Exception( call= NULL )), "Bob" ))
      wantRE <- "\\QCondition inherits from `Exception/condition` not `Amy/Bob`\\E"
      expect_failure( expect_condition( stop( Exception( call= NULL )), c( "Amy","Bob" )))
      wantRE <- "\\QCondition inherits from `Exception/condition` not `error`\\E"
      expect_failure( expect_condition( stop( Exception( call= NULL )), "error" ))
   })
   it( "Fails when no condition is thrown", {
      wantRE <- "\\QNo condition was signaled.\\E"
      expect_failure( expect_condition( 1+2, "Exception" ), wantRE )
      expect_failure( expect_condition( "Exception", "Exception" ), wantRE )
      expect_failure( expect_condition( Exception(call=NULL), "Exception" ), wantRE )
   })
})

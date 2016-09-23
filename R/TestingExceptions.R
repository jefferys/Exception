#' Are conditions signaled correctly?
#'
#' @param object Code that is expected to signal a condition.
#' @param class Class of the condition signaled, as a character vector. At least
#'   one of these class names must be amongst the condition's classes. By
#'   default this checks for "condition" which is the ubiquitous base class all
#'   signals must inherit.
#' @param label The object label. NULL is treated as the string "Condition".
#' @param info Extra information to be included in the message
#'
#' @return Called only for its side effects during testing.
#' @examples
#' \dontrun{
#' ### Pass
#' expect_condition( stop( "Oops" ))   # Checks if condition by default
#' expect_condition( stop( "Oops" ), "error" )
#' expect_condition( stop( "Oops" ), "simple-error" )
#' expect_condition( warning( "Oops" ), "warning" )
#' expect_condition( warning( "Oops" ), c("Foo", error, "Bar", "simple-error"))
#'
#' ### Fail
#' # Nothing signaled
#' expect_condition( "Oops" )
#'
#' # error, simple-error, or condition, not warning
#' expect_condition( stop("Oops"), "warning" )
#'
#' # error, simple-error, or condition, neither Foo nor Bar
#' expect_condition( stop( "Oops" ), c( "Foo", "Bar" ))
#' }
#' @export
expect_condition <- function( object, class="condition", label= NULL, info= NULL ) {
   cond <- tryCatch({
      object
      NULL
   },
   condition= function(c) c
   )
   if (is.null(label)) {label <- "Condition"}
   if (is.null(cond)) {
      testthat::expect( ! is.null(cond), "No condition was signaled.", info=info )
   }
   else {
      testthat::expect_is(cond, class, label=label, info=info)
   }
}

# Adding a field TransactionLog would require that the
# $clone method does a deep copy of the class to prevent errors
# as $clone itself isn't recursive by default.

#' @title Account Class
#'
#' @description
#' This base class for account objects.
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_number
#'
#' @details
#' An Account has a balance.
#' One can deposit or withdraw some money from an account.
#'
#' @family Account
#' @export
Account <- R6Class("Account",
  public = list(

    #' @field balance Account balance
    balance = 0,

    #' @description
    #' deposit a certain amount (>0) into the account
    #'
    #' @param amount (`integer()` >0) amount be deposited.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**
    deposit = function(amount = 0) {
      checkmate::assert_number(amount, lower = 0)
      self$balance <- self$balance + amount
      invisible(self)
    },

    #' @description
    #' withdraw a certain amount (>0) from the account.
    #'
    #' @param amount (`integer()` >0) amount be withdrawn.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**
    withdraw = function(amount = 0) {
      checkmate::assert_number(amount, lower = 0)
      self$balance <- self$balance - amount
      invisible(self)
    }
  )
)

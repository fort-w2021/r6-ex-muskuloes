# Active binding have the advantage
# that they work as getters and setters for (private) variables
# by implementing a single function

#' @title Giro Account Class
#'
#' @include Account.R
#'
#' @description
#' This Account uses active binding to define its methods.
#'
#' @family Account
#' @export
SafeAccount <- R6Class("SafeAccount",
  private = list(
    balance = 0
  ),
  active = list(

    #' @field deposit (`integer()` >0)\cr
    #' a certain amount deposited from the account.
    #' Returns the current balance if not assigned
    deposit = function(amount) {
      if (missing(amount)) {
        private$balance
      } else {
        checkmate::assert_number(amount, lower = 0)
        private$balance <- private$balance + amount
        invisible(self)
      }
    },

    #' @field withdraw (`integer()` >0)\cr
    #' a certain amount withdrawn from the account.
    #' Returns the current balance if not assigned
    withdraw = function(amount) {
      if (missing(amount)) {
        private$balance
      } else {
        checkmate::assert_number(amount, lower = 0)
        private$balance <- private$balance - amount
        invisible(self)
      }
    }
  ),
)

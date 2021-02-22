#' @title Giro Account Class
#'
#' @include Account.R
#'
#' @description
#' This Account extends [Account] with a withdrawal limit.
#'
#' @family Account
#' @export
GiroAccount <- R6Class("GiroAccount",
  inherit = Account,
  public = list(
    #' @field limit (`integer()` >0) withdraw limit.
    limit = 0,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param limit (`integer()` >0) withdraw limit.
    initialize = function(limit = 0) {
      self$limit <- limit
    },

    #' @description
    #' withdrawals above the limit are not possible.
    #' withdrawals from an account with negative balance is penalized
    #'
    #' @param amount (`integer()` >0) amount be withdrawn.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**
    withdraw = function(amount = 0) {
      if (self$limit < amount) {
        stop("Withdraw limit exceeded")
      }
      if (self$balance < 0) {
        super$withdraw(amount + 10)
      } else {
        super$withdraw(amount)
      }
    }
  )
)

# Using s3 or s4 to implement a deck of cards will be problematic because
# they don't store internal state (copy-on-modify semantics).
# So it won't be possible to save the deck's state in the same object.
# This is easily achieved through R6's reference semantics.

# Card deck class
Deck <- R6::R6Class("Cards",
  private = list(
    color = c("G", "H", "E", "S"),
    value = c(6:10, "U", "O", "K", "A"),
    deck = NA,
    shuffle = function(cards) {
      sample(cards)
    },
    get_deck = function() {
      paste0(rep(private$color, each = 9), rep(private$value, times = 4))
    }
  ),
  public = list(
    initialize = function() {
      fill()
    },
    draw_cards = function(num) {
      checkmate::assert_number(num, lower = 1, upper = length(private$deck))
      cards <- sample(private$deck, num)
      private$deck <- setdiff(self$deck, cards)
      cards
    },
    fill = function() {
      deck <- private$get_deck()
      private$deck <- private$shuffle(deck)
    },
    cut = function(num) {
      checkmate::assert_number(num, lower = 1, upper = length(private$deck))
      private$deck <- c(
        private$deck[(num + 1):length(private$deck)],
        private$deck[1:num]
      )
    }
  )
)

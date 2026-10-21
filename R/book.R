#' Create an empty limit order book
#'
#' A limit order book holds resting buy (bid) and sell (ask) limit orders and
#' matches incoming orders against them with price-time priority: the best
#' price trades first and, at the same price, the oldest order trades first.
#' Prices are stored internally as integer multiples of `tick_size`.
#'
#' The book is a mutable object (an environment): functions such as
#' [lob_submit()] and [lob_cancel()] change it in place.
#'
#' @param tick_size Minimum price increment. A positive number.
#' @return An object of class `lob`.
#' @seealso [lob_submit()], [lob_cancel()], [lob_quotes()], [lob_depth()]
#' @examples
#' book <- lob_new(tick_size = 0.01)
#' lob_submit(book, "buy", qty = 10, price = 99.98)
#' lob_submit(book, "sell", qty = 5, price = 100.02)
#' lob_quotes(book)
#' @export
lob_new <- function(tick_size = 0.01) {
  if (!is.numeric(tick_size) || length(tick_size) != 1L || !is.finite(tick_size) ||
      tick_size <= 0) {
    stop("`tick_size` must be a single positive number.", call. = FALSE)
  }
  b <- new.env(parent = emptyenv())
  b$tick_size <- tick_size
  b$next_id <- 1L
  b$time <- 0
  b$event <- 0L
  b$last_tick <- NA_real_
  # Resting orders, kept in arrival order so that position encodes time priority.
  b$o_id <- integer()
  b$o_side <- integer()
  b$o_tick <- numeric()
  b$o_qty <- numeric()
  b$o_agent <- integer()
  b$o_time <- numeric()
  # Trade log, grown geometrically.
  b$n_tr <- 0L
  b$tr_event <- integer(256L)
  b$tr_time <- numeric(256L)
  b$tr_tick <- numeric(256L)
  b$tr_qty <- numeric(256L)
  b$tr_side <- integer(256L)
  b$tr_buyer <- integer(256L)
  b$tr_seller <- integer(256L)
  b$tr_passive <- integer(256L)
  class(b) <- "lob"
  b
}

#' Submit an order to a limit order book
#'
#' A *limit order* (`price` given) first trades against resting orders on the
#' opposite side at prices at least as good as `price`; any remainder rests in
#' the book. A *market order* (`price = NULL`) trades against the best
#' available prices until it is filled or the opposite side is empty; any
#' unfilled remainder is discarded.
#'
#' Trades always execute at the price of the resting (passive) order.
#'
#' @param book A `lob` created by [lob_new()].
#' @param side `"buy"` or `"sell"`.
#' @param qty Order size. A positive whole number.
#' @param price Limit price, or `NULL` for a market order. Must lie on the
#'   tick grid.
#' @param agent Integer identifier of the submitter, recorded in the trade log.
#' @return Invisibly, a list with `id` (the id of the resting order, or `NA`
#'   if nothing rests), `filled` (quantity executed) and `resting` (quantity
#'   left in the book).
#' @examples
#' book <- lob_new()
#' lob_submit(book, "sell", qty = 5, price = 100.01)
#' lob_submit(book, "sell", qty = 5, price = 100.02)
#' res <- lob_submit(book, "buy", qty = 7)   # market order walks the book
#' res$filled
#' lob_trades(book)
#' @export
lob_submit <- function(book, side, qty, price = NULL, agent = 0L) {
  check_book(book)
  s <- parse_side(side)
  check_qty(qty)
  limit <- if (is.null(price)) NA_real_ else price_to_tick(book, price)
  res <- book_submit(book, s, qty, limit, as.integer(agent))
  invisible(res)
}

#' Cancel a resting order
#'
#' @param book A `lob`.
#' @param id Id of the order, as returned by [lob_submit()].
#' @return Invisibly, `TRUE` if the order was found and removed, `FALSE`
#'   otherwise (for example because it has already been filled).
#' @examples
#' book <- lob_new()
#' o <- lob_submit(book, "buy", qty = 3, price = 99.99)
#' lob_cancel(book, o$id)
#' lob_cancel(book, o$id)   # already gone
#' @export
lob_cancel <- function(book, id) {
  check_book(book)
  i <- match(id, book$o_id)
  if (is.na(i)) {
    return(invisible(FALSE))
  }
  book_remove(book, i)
  invisible(TRUE)
}

#' Best quotes of a limit order book
#'
#' @param book A `lob`.
#' @return A list with `bid`, `ask`, `mid`, `spread` (price units),
#'   `bid_size` and `ask_size` (quantity resting at the best prices). Prices
#'   are `NA` when a side of the book is empty.
#' @examples
#' book <- lob_new()
#' lob_submit(book, "buy", qty = 2, price = 99.99)
#' lob_submit(book, "sell", qty = 4, price = 100.01)
#' lob_quotes(book)
#' @export
lob_quotes <- function(book) {
  check_book(book)
  q <- book_best(book)
  ts <- book$tick_size
  list(
    bid = q[["bid"]] * ts,
    ask = q[["ask"]] * ts,
    mid = (q[["bid"]] + q[["ask"]]) / 2 * ts,
    spread = (q[["ask"]] - q[["bid"]]) * ts,
    bid_size = q[["bid_size"]],
    ask_size = q[["ask_size"]]
  )
}

#' Depth of a limit order book by price level
#'
#' @param book A `lob`.
#' @param levels Number of price levels to report on each side, counted from
#'   the best price.
#' @return A data frame with columns `side`, `level` (1 = best), `price`,
#'   `qty` and `orders`.
#' @examples
#' book <- lob_new()
#' for (k in 1:3) {
#'   lob_submit(book, "buy", qty = k, price = 100 - k / 100)
#'   lob_submit(book, "sell", qty = k, price = 100 + k / 100)
#' }
#' lob_depth(book)
#' @export
lob_depth <- function(book, levels = 10L) {
  check_book(book)
  out <- lapply(c(1L, -1L), function(s) {
    i <- which(book$o_side == s)
    if (!length(i)) {
      return(NULL)
    }
    tk <- book$o_tick[i]
    qty <- tapply(book$o_qty[i], tk, sum)
    n <- tapply(book$o_qty[i], tk, length)
    p <- as.numeric(names(qty))
    ord <- order(p, decreasing = s == 1L)
    ord <- utils::head(ord, levels)
    data.frame(
      side = if (s == 1L) "bid" else "ask",
      level = seq_along(ord),
      price = p[ord] * book$tick_size,
      qty = as.numeric(qty[ord]),
      orders = as.integer(n[ord]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(side = character(), level = integer(), price = numeric(),
                      qty = numeric(), orders = integer())
  }
  rownames(out) <- NULL
  out
}

#' Resting orders and trade log of a limit order book
#'
#' @param book A `lob`.
#' @return `lob_orders()` returns a data frame of resting orders in time
#'   priority. `lob_trades()` returns a data frame of executed trades; `side`
#'   is the side of the aggressive (incoming) order.
#' @examples
#' book <- lob_new()
#' lob_submit(book, "sell", qty = 5, price = 100.01, agent = 1)
#' lob_submit(book, "buy", qty = 2, agent = 2)
#' lob_orders(book)
#' lob_trades(book)
#' @export
lob_orders <- function(book) {
  check_book(book)
  data.frame(
    id = book$o_id,
    side = ifelse(book$o_side == 1L, "bid", "ask"),
    price = book$o_tick * book$tick_size,
    qty = book$o_qty,
    agent = book$o_agent,
    time = book$o_time,
    stringsAsFactors = FALSE
  )
}

#' @rdname lob_orders
#' @export
lob_trades <- function(book) {
  check_book(book)
  n <- seq_len(book$n_tr)
  data.frame(
    event = book$tr_event[n],
    time = book$tr_time[n],
    price = book$tr_tick[n] * book$tick_size,
    qty = book$tr_qty[n],
    side = ifelse(book$tr_side[n] == 1L, "buy", "sell"),
    buyer = book$tr_buyer[n],
    seller = book$tr_seller[n],
    passive_id = book$tr_passive[n],
    stringsAsFactors = FALSE
  )
}

#' @export
print.lob <- function(x, ...) {
  q <- lob_quotes(x)
  cat("<lob> tick size ", format(x$tick_size), "\n", sep = "")
  cat("  resting orders: ", length(x$o_id), "   trades: ", x$n_tr, "\n", sep = "")
  cat("  best bid: ", fmt_num(q$bid), " (", q$bid_size, ")   best ask: ",
      fmt_num(q$ask), " (", q$ask_size, ")\n", sep = "")
  invisible(x)
}

# ---- Internal engine --------------------------------------------------------
# These functions work in ticks and skip argument checks; the simulator calls
# them once per event.

book_submit <- function(b, side, qty, limit, agent) {
  n0 <- b$n_tr
  rem <- book_match(b, side, qty, limit, agent)
  id <- NA_integer_
  if (rem > 0 && !is.na(limit)) {
    id <- book_add(b, side, limit, rem, agent)
  }
  if (b$n_tr > n0) {
    b$last_tick <- b$tr_tick[b$n_tr]
  }
  list(id = id, filled = qty - rem, resting = if (is.na(id)) 0 else rem)
}

book_match <- function(b, side, qty, limit, agent) {
  opp <- -side
  rem <- qty
  while (rem > 0) {
    idx <- which(b$o_side == opp)
    if (!length(idx)) break
    tk <- b$o_tick[idx]
    best <- if (side > 0L) min(tk) else max(tk)
    if (!is.na(limit) && (if (side > 0L) best > limit else best < limit)) break
    # Positions at the best price are already in time priority.
    for (i in idx[tk == best]) {
      f <- min(rem, b$o_qty[i])
      b$o_qty[i] <- b$o_qty[i] - f
      if (side > 0L) {
        book_record(b, best, f, side, agent, b$o_agent[i], b$o_id[i])
      } else {
        book_record(b, best, f, side, b$o_agent[i], agent, b$o_id[i])
      }
      rem <- rem - f
      if (rem <= 0) break
    }
    book_compact(b)
  }
  rem
}

book_add <- function(b, side, tick, qty, agent) {
  id <- b$next_id
  b$next_id <- id + 1L
  n <- length(b$o_id) + 1L
  b$o_id[n] <- id
  b$o_side[n] <- side
  b$o_tick[n] <- tick
  b$o_qty[n] <- qty
  b$o_agent[n] <- agent
  b$o_time[n] <- b$time
  id
}

book_remove <- function(b, i) {
  if (!length(i)) return(invisible())
  b$o_id <- b$o_id[-i]
  b$o_side <- b$o_side[-i]
  b$o_tick <- b$o_tick[-i]
  b$o_qty <- b$o_qty[-i]
  b$o_agent <- b$o_agent[-i]
  b$o_time <- b$o_time[-i]
  invisible()
}

book_compact <- function(b) {
  book_remove(b, which(b$o_qty <= 0))
}

book_record <- function(b, tick, qty, side, buyer, seller, passive) {
  n <- b$n_tr + 1L
  if (n > length(b$tr_event)) {
    m <- 2L * length(b$tr_event)
    for (v in c("tr_event", "tr_time", "tr_tick", "tr_qty", "tr_side",
                "tr_buyer", "tr_seller", "tr_passive")) {
      x <- b[[v]]
      length(x) <- m
      b[[v]] <- x
    }
  }
  b$tr_event[n] <- b$event
  b$tr_time[n] <- b$time
  b$tr_tick[n] <- tick
  b$tr_qty[n] <- qty
  b$tr_side[n] <- side
  b$tr_buyer[n] <- buyer
  b$tr_seller[n] <- seller
  b$tr_passive[n] <- passive
  b$n_tr <- n
}

book_best <- function(b) {
  s <- b$o_side
  bi <- which(s == 1L)
  ai <- which(s == -1L)
  if (length(bi)) {
    tb <- b$o_tick[bi]
    bid <- max(tb)
    bid_size <- sum(b$o_qty[bi][tb == bid])
  } else {
    bid <- NA_real_
    bid_size <- 0
  }
  if (length(ai)) {
    ta <- b$o_tick[ai]
    ask <- min(ta)
    ask_size <- sum(b$o_qty[ai][ta == ask])
  } else {
    ask <- NA_real_
    ask_size <- 0
  }
  c(bid = bid, ask = ask, bid_size = bid_size, ask_size = ask_size)
}

# ---- Checks and helpers -----------------------------------------------------

check_book <- function(book) {
  if (!inherits(book, "lob")) {
    stop("`book` must be a limit order book created by lob_new().", call. = FALSE)
  }
}

parse_side <- function(side) {
  if (!is.character(side) || length(side) != 1L || !side %in% c("buy", "sell")) {
    stop('`side` must be "buy" or "sell".', call. = FALSE)
  }
  if (side == "buy") 1L else -1L
}

check_qty <- function(qty) {
  if (!is.numeric(qty) || length(qty) != 1L || !is.finite(qty) || qty <= 0 ||
      qty != round(qty)) {
    stop("`qty` must be a single positive whole number.", call. = FALSE)
  }
}

price_to_tick <- function(book, price) {
  if (!is.numeric(price) || length(price) != 1L || !is.finite(price) || price <= 0) {
    stop("`price` must be a single positive number or NULL.", call. = FALSE)
  }
  tk <- price / book$tick_size
  if (abs(tk - round(tk)) > 1e-6) {
    stop("`price` (", price, ") is not a multiple of the tick size (",
         book$tick_size, ").", call. = FALSE)
  }
  round(tk)
}

fmt_num <- function(x) {
  if (is.na(x)) "NA" else format(x, nsmall = 2)
}

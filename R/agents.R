#' Trading agents
#'
#' Agent constructors describe a type of trader and its parameters. Pass a
#' list of agents to [simulate_lob()]. Each agent acts at random times: its
#' events arrive as a Poisson process with the stated rate (events per unit of
#' simulated time), and the simulator picks which agent acts next in
#' proportion to the agents' current rates.
#'
#' All prices and distances are measured in ticks.
#'
#' * `agent_zi()` is *zero-intelligence* order flow, as in Smith et al.
#'   (2003): limit orders arrive at `limit_rate` and are placed a random
#'   `1:max_offset` ticks away from the opposite best quote (so they never
#'   cross the spread, but can improve it); market orders arrive at
#'   `market_rate`; each of the agent's resting orders is cancelled at
#'   `cancel_rate`. Order sides are buy or sell with equal probability.
#' * `agent_noise()` sends market orders of random side and a size drawn
#'   uniformly from `1:size_max`.
#' * `agent_market_maker()` cancels its quotes and requotes `half_spread`
#'   ticks either side of the mid price, shifted by `skew` ticks per unit of
#'   inventory to lean against its position. It stops quoting the side that
#'   would push its absolute inventory beyond `inventory_limit`.
#' * `agent_momentum()` buys with a market order when the mid price has
#'   risen by more than `threshold` ticks over the last `lookback` events, and
#'   sells when it has fallen by more than `threshold`.
#' * `agent_informed()` observes a latent fundamental value that follows a
#'   random walk with volatility `sigma` ticks per square root of time unit.
#'   It buys when the best ask is more than `threshold` ticks below the value
#'   and sells when the best bid is more than `threshold` ticks above it.
#'
#' @param limit_rate,market_rate Arrival rates of limit and market orders.
#' @param cancel_rate Cancellation rate per resting order.
#' @param rate Arrival rate of the agent's decisions.
#' @param size Order size (a positive whole number).
#' @param size_max Largest order size for noise traders.
#' @param max_offset Largest distance, in ticks, of a zero-intelligence limit
#'   order from the opposite best quote.
#' @param half_spread Half of the market maker's quoted spread, in ticks.
#' @param inventory_limit Largest absolute inventory the market maker will
#'   carry.
#' @param skew Ticks the market maker shifts its quotes per unit of
#'   inventory.
#' @param lookback Number of events the momentum trader looks back.
#' @param threshold Signal, in ticks, needed before the agent trades.
#' @param sigma Volatility of the informed trader's fundamental value.
#' @param name Label used in results. Duplicate names are made unique.
#' @return An object of class `lob_agent`.
#' @references Smith, E., Farmer, J. D., Gillemot, L. and Krishnamurthy, S.
#'   (2003). Statistical theory of the continuous double auction.
#'   *Quantitative Finance*, 3(6), 481--514. \doi{10.1088/1469-7688/3/6/307}
#' @examples
#' agents <- list(
#'   agent_zi(limit_rate = 2, market_rate = 0.5, cancel_rate = 0.05),
#'   agent_market_maker(rate = 1, half_spread = 2),
#'   agent_noise(rate = 0.2)
#' )
#' agents[[2]]
#' @name agents
NULL

#' @rdname agents
#' @export
agent_zi <- function(limit_rate = 1, market_rate = 0.2, cancel_rate = 0.02,
                     size = 1, max_offset = 10, name = "zi") {
  check_rate(limit_rate, "limit_rate")
  check_rate(market_rate, "market_rate")
  check_rate(cancel_rate, "cancel_rate")
  check_count(size, "size")
  check_count(max_offset, "max_offset")
  build <- function(id) {
    list(
      rate = function(b, ctx) {
        limit_rate + market_rate + cancel_rate * sum(b$o_agent == id)
      },
      act = function(b, ctx) {
        own <- which(b$o_agent == id)
        what <- sample.int(3L, 1L, prob = c(limit_rate, market_rate,
                                             cancel_rate * length(own)))
        side <- if (stats::runif(1) < 0.5) 1L else -1L
        if (what == 1L) {
          q <- book_best(b)
          k <- sample.int(max_offset, 1L)
          if (side == 1L) {
            anchor <- if (is.na(q[["ask"]])) floor(ctx$ref) + 1 else q[["ask"]]
            tick <- anchor - k
          } else {
            anchor <- if (is.na(q[["bid"]])) ceiling(ctx$ref) - 1 else q[["bid"]]
            tick <- anchor + k
          }
          book_submit(b, side, size, max(1, tick), id)
        } else if (what == 2L) {
          book_submit(b, side, size, NA_real_, id)
        } else {
          book_remove(b, own[sample.int(length(own), 1L)])
        }
      }
    )
  }
  new_agent("zero_intelligence", name,
            list(limit_rate = limit_rate, market_rate = market_rate,
                 cancel_rate = cancel_rate, size = size, max_offset = max_offset),
            build)
}

#' @rdname agents
#' @export
agent_noise <- function(rate = 0.5, size_max = 5, name = "noise") {
  check_rate(rate, "rate")
  check_count(size_max, "size_max")
  build <- function(id) {
    list(
      rate = function(b, ctx) rate,
      act = function(b, ctx) {
        side <- if (stats::runif(1) < 0.5) 1L else -1L
        book_submit(b, side, sample.int(size_max, 1L), NA_real_, id)
      }
    )
  }
  new_agent("noise", name, list(rate = rate, size_max = size_max), build)
}

#' @rdname agents
#' @export
agent_market_maker <- function(rate = 1, half_spread = 1, size = 5,
                               inventory_limit = 50, skew = 0.05,
                               name = "market_maker") {
  check_rate(rate, "rate")
  if (!is.numeric(half_spread) || length(half_spread) != 1L || half_spread < 0.5) {
    stop("`half_spread` must be a single number of at least 0.5 ticks.", call. = FALSE)
  }
  check_count(size, "size")
  check_rate(inventory_limit, "inventory_limit")
  check_rate(skew, "skew")
  build <- function(id) {
    list(
      rate = function(b, ctx) rate,
      act = function(b, ctx) {
        book_remove(b, which(b$o_agent == id))
        q <- book_best(b)
        mid <- if (is.na(q[["bid"]]) || is.na(q[["ask"]])) ctx$ref else
          (q[["bid"]] + q[["ask"]]) / 2
        inv <- ctx$inv[id + 2L]
        centre <- mid - skew * inv
        bid <- floor(centre - half_spread)
        ask <- ceiling(centre + half_spread)
        if (ask <= bid) ask <- bid + 1
        if (inv + size <= inventory_limit && bid >= 1) {
          book_submit(b, 1L, size, bid, id)
        }
        if (inv - size >= -inventory_limit) {
          book_submit(b, -1L, size, ask, id)
        }
      }
    )
  }
  new_agent("market_maker", name,
            list(rate = rate, half_spread = half_spread, size = size,
                 inventory_limit = inventory_limit, skew = skew),
            build)
}

#' @rdname agents
#' @export
agent_momentum <- function(rate = 0.2, lookback = 50, threshold = 2, size = 2,
                           name = "momentum") {
  check_rate(rate, "rate")
  check_count(lookback, "lookback")
  check_rate(threshold, "threshold")
  check_count(size, "size")
  build <- function(id) {
    list(
      rate = function(b, ctx) rate,
      act = function(b, ctx) {
        e <- ctx$event
        if (e <= lookback) return(invisible())
        signal <- ctx$ref - ctx$mid_hist[e - lookback]
        if (signal > threshold) {
          book_submit(b, 1L, size, NA_real_, id)
        } else if (signal < -threshold) {
          book_submit(b, -1L, size, NA_real_, id)
        }
      }
    )
  }
  new_agent("momentum", name,
            list(rate = rate, lookback = lookback, threshold = threshold, size = size),
            build)
}

#' @rdname agents
#' @export
agent_informed <- function(rate = 0.2, sigma = 1, threshold = 2, size = 2,
                           name = "informed") {
  check_rate(rate, "rate")
  check_rate(sigma, "sigma")
  check_rate(threshold, "threshold")
  check_count(size, "size")
  build <- function(id) {
    st <- new.env(parent = emptyenv())
    st$v <- NA_real_
    st$t <- 0
    st$path_t <- numeric()
    st$path_v <- numeric()
    list(
      rate = function(b, ctx) rate,
      act = function(b, ctx) {
        if (is.na(st$v)) st$v <- ctx$ref
        st$v <- st$v + sigma * sqrt(b$time - st$t) * stats::rnorm(1)
        st$t <- b$time
        st$path_t[length(st$path_t) + 1L] <- b$time
        st$path_v[length(st$path_v) + 1L] <- st$v
        q <- book_best(b)
        if (!is.na(q[["ask"]]) && q[["ask"]] < st$v - threshold) {
          book_submit(b, 1L, size, NA_real_, id)
        } else if (!is.na(q[["bid"]]) && q[["bid"]] > st$v + threshold) {
          book_submit(b, -1L, size, NA_real_, id)
        }
      },
      finish = function(tick_size) {
        data.frame(time = st$path_t, value = st$path_v * tick_size)
      }
    )
  }
  new_agent("informed", name,
            list(rate = rate, sigma = sigma, threshold = threshold, size = size),
            build)
}

#' @export
print.lob_agent <- function(x, ...) {
  cat("<lob_agent> ", x$name, " (", x$type, ")\n", sep = "")
  p <- vapply(x$params, format, character(1))
  cat(paste0("  ", names(p), " = ", p, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

new_agent <- function(type, name, params, build) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }
  structure(list(type = type, name = name, params = params, build = build),
            class = "lob_agent")
}

check_rate <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    stop("`", arg, "` must be a single non-negative number.", call. = FALSE)
  }
}

check_count <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 1 || x != round(x)) {
    stop("`", arg, "` must be a single positive whole number.", call. = FALSE)
  }
}

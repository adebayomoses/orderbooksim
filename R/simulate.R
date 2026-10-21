#' Simulate a limit order book driven by trading agents
#'
#' Runs an event-driven simulation in continuous time. At each event the
#' simulator draws the waiting time from an exponential distribution with the
#' agents' total rate, picks one agent in proportion to its current rate, and
#' lets it act on the book. Best quotes are recorded after every event.
#'
#' The book starts with `init_levels` price levels on each side of
#' `init_price`, each holding `init_size` units owned by the `"initial"`
#' liquidity provider (agent id 0). Without it, the first orders would have no
#' prices to anchor to.
#'
#' @param agents A `lob_agent` or a list of them; see [agents].
#' @param n_events Number of events to simulate.
#' @param init_price Starting mid price.
#' @param tick_size Minimum price increment.
#' @param init_levels,init_size Number of initial price levels on each side and
#'   the quantity at each. Use `init_levels = 0` to start with an empty book.
#' @param shocks Optional data frame of exogenous market orders with columns
#'   `event` (the event at which the shock replaces the agents' action),
#'   `side` (`"buy"` or `"sell"`) and `qty`. Shocks are recorded as agent
#'   `"shock"` (id -1).
#' @param seed Optional random seed. The global random number generator state
#'   is restored afterwards.
#' @return An object of class `lob_sim`, a list with:
#'   * `quotes`: one row per event with `event`, `time`, `actor`, `bid`,
#'     `ask`, `mid`, `spread`, `spread_ticks`, `bid_size`, `ask_size`.
#'   * `trades`: the trade log, with buyer and seller names.
#'   * `agents`: one row per agent with trades, volume bought and sold,
#'     final inventory, cash and profit marked to the final mid price.
#'   * `fundamental`: the informed traders' value paths, if any.
#'   * `book`: the final `lob`.
#'   * `shocks`, `params`.
#' @seealso [summary.lob_sim()], [plot.lob_sim()], [price_impact()],
#'   [resilience()], [stylized_facts()]
#' @examples
#' sim <- simulate_lob(
#'   list(agent_zi(limit_rate = 2, market_rate = 0.4, cancel_rate = 0.05),
#'        agent_market_maker(half_spread = 2)),
#'   n_events = 2000, seed = 1
#' )
#' sim
#' summary(sim)
#' @export
simulate_lob <- function(agents, n_events = 5000, init_price = 100,
                         tick_size = 0.01, init_levels = 5, init_size = 5,
                         shocks = NULL, seed = NULL) {
  if (inherits(agents, "lob_agent")) agents <- list(agents)
  if (!is.list(agents) || !length(agents) ||
      !all(vapply(agents, inherits, logical(1), "lob_agent"))) {
    stop("`agents` must be a lob_agent or a non-empty list of them.", call. = FALSE)
  }
  check_count(n_events, "n_events")
  if (!is.numeric(init_price) || length(init_price) != 1L || init_price <= 0) {
    stop("`init_price` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(init_levels) || length(init_levels) != 1L || init_levels < 0 ||
      init_levels != round(init_levels)) {
    stop("`init_levels` must be a single non-negative whole number.", call. = FALSE)
  }
  check_count(init_size, "init_size")
  shocks <- check_shocks(shocks, n_events)

  if (is.null(seed)) {
    run_simulation(agents, n_events, init_price, tick_size, init_levels,
                   init_size, shocks)
  } else {
    withr::with_seed(seed, run_simulation(agents, n_events, init_price,
                                          tick_size, init_levels, init_size,
                                          shocks))
  }
}

run_simulation <- function(agents, n_events, init_price, tick_size,
                           init_levels, init_size, shocks) {
  b <- lob_new(tick_size)
  init_tick <- price_to_tick(b, round(init_price / tick_size) * tick_size)
  for (k in seq_len(init_levels)) {
    if (init_tick - k >= 1) book_add(b, 1L, init_tick - k, init_size, 0L)
    book_add(b, -1L, init_tick + k, init_size, 0L)
  }

  n_ag <- length(agents)
  names_ag <- make.unique(vapply(agents, `[[`, character(1), "name"), sep = "_")
  acts <- lapply(seq_len(n_ag), function(i) agents[[i]]$build(i))

  ctx <- new.env(parent = emptyenv())
  ctx$ref <- init_tick
  ctx$event <- 0L
  ctx$mid_hist <- numeric(n_events)
  # Inventory and cash by agent id, stored at position id + 2 (ids -1, 0, 1, ...).
  ctx$inv <- numeric(n_ag + 2L)
  ctx$cash <- numeric(n_ag + 2L)

  q_time <- numeric(n_events)
  q_actor <- integer(n_events)
  q_bid <- q_ask <- q_bsz <- q_asz <- numeric(n_events)
  shock_at <- if (is.null(shocks)) integer() else shocks$event
  seen <- 0L

  for (e in seq_len(n_events)) {
    rates <- vapply(acts, function(a) a$rate(b, ctx), numeric(1))
    total <- sum(rates)
    if (!is.finite(total) || total <= 0) {
      stop("All agents have zero activity at event ", e,
           ". Give at least one agent a positive rate.", call. = FALSE)
    }
    b$time <- b$time + stats::rexp(1L, total)
    b$event <- e
    ctx$event <- e

    s <- which(shock_at == e)
    if (length(s)) {
      for (j in s) {
        book_submit(b, parse_side(shocks$side[j]), shocks$qty[j], NA_real_, -1L)
      }
      q_actor[e] <- -1L
    } else {
      k <- if (n_ag == 1L) 1L else sample.int(n_ag, 1L, prob = rates)
      acts[[k]]$act(b, ctx)
      q_actor[e] <- k
    }

    if (b$n_tr > seen) {
      for (t in (seen + 1L):b$n_tr) {
        val <- b$tr_tick[t] * b$tr_qty[t]
        bi <- b$tr_buyer[t] + 2L
        si <- b$tr_seller[t] + 2L
        ctx$inv[bi] <- ctx$inv[bi] + b$tr_qty[t]
        ctx$inv[si] <- ctx$inv[si] - b$tr_qty[t]
        ctx$cash[bi] <- ctx$cash[bi] - val
        ctx$cash[si] <- ctx$cash[si] + val
      }
      seen <- b$n_tr
    }

    q <- book_best(b)
    if (!is.na(q[["bid"]]) && !is.na(q[["ask"]])) {
      ctx$ref <- (q[["bid"]] + q[["ask"]]) / 2
    } else if (!is.na(b$last_tick)) {
      ctx$ref <- b$last_tick
    }
    ctx$mid_hist[e] <- ctx$ref
    q_time[e] <- b$time
    q_bid[e] <- q[["bid"]]
    q_ask[e] <- q[["ask"]]
    q_bsz[e] <- q[["bid_size"]]
    q_asz[e] <- q[["ask_size"]]
  }

  ids <- c(-1L, 0L, seq_len(n_ag))
  labels <- c("shock", "initial", names_ag)
  label_of <- function(id) labels[match(id, ids)]

  quotes <- data.frame(
    event = seq_len(n_events),
    time = q_time,
    actor = label_of(q_actor),
    bid = q_bid * tick_size,
    ask = q_ask * tick_size,
    mid = (q_bid + q_ask) / 2 * tick_size,
    spread = (q_ask - q_bid) * tick_size,
    spread_ticks = q_ask - q_bid,
    bid_size = q_bsz,
    ask_size = q_asz,
    stringsAsFactors = FALSE
  )

  trades <- lob_trades(b)
  trades$buyer <- label_of(trades$buyer)
  trades$seller <- label_of(trades$seller)

  final_tick <- ctx$ref
  keep <- if (is.null(shocks)) ids >= 0L else rep(TRUE, length(ids))
  agent_tab <- data.frame(
    id = ids,
    name = labels,
    type = c("shock", "initial", vapply(agents, `[[`, character(1), "type")),
    trades = vapply(labels, function(l) sum(trades$buyer == l | trades$seller == l),
                    numeric(1)),
    bought = vapply(labels, function(l) sum(trades$qty[trades$buyer == l]), numeric(1)),
    sold = vapply(labels, function(l) sum(trades$qty[trades$seller == l]), numeric(1)),
    inventory = ctx$inv,
    cash = ctx$cash * tick_size,
    stringsAsFactors = FALSE
  )
  agent_tab$pnl <- agent_tab$cash + agent_tab$inventory * final_tick * tick_size
  agent_tab <- agent_tab[keep, , drop = FALSE]
  rownames(agent_tab) <- NULL

  fundamental <- NULL
  for (i in seq_len(n_ag)) {
    if (!is.null(acts[[i]]$finish)) {
      f <- acts[[i]]$finish(tick_size)
      if (nrow(f)) {
        f$agent <- names_ag[i]
        fundamental <- rbind(fundamental, f)
      }
    }
  }

  structure(
    list(
      quotes = quotes,
      trades = trades,
      agents = agent_tab,
      fundamental = fundamental,
      book = b,
      shocks = shocks,
      params = list(n_events = n_events, init_price = init_price,
                    tick_size = tick_size, init_levels = init_levels,
                    init_size = init_size,
                    agents = stats::setNames(lapply(agents, `[[`, "params"), names_ag))
    ),
    class = "lob_sim"
  )
}

check_shocks <- function(shocks, n_events) {
  if (is.null(shocks)) return(NULL)
  if (!is.data.frame(shocks) || !all(c("event", "side", "qty") %in% names(shocks))) {
    stop("`shocks` must be a data frame with columns `event`, `side` and `qty`.",
         call. = FALSE)
  }
  if (!nrow(shocks)) return(NULL)
  shocks <- shocks[order(shocks$event), c("event", "side", "qty"), drop = FALSE]
  shocks$side <- as.character(shocks$side)
  if (any(!shocks$side %in% c("buy", "sell"))) {
    stop('Shock `side` must be "buy" or "sell".', call. = FALSE)
  }
  if (any(shocks$event < 1 | shocks$event > n_events | shocks$event != round(shocks$event))) {
    stop("Shock `event` values must be whole numbers between 1 and `n_events`.",
         call. = FALSE)
  }
  if (any(shocks$qty <= 0 | shocks$qty != round(shocks$qty))) {
    stop("Shock `qty` values must be positive whole numbers.", call. = FALSE)
  }
  rownames(shocks) <- NULL
  shocks
}

#' @export
print.lob_sim <- function(x, ...) {
  q <- x$quotes
  cat("<lob_sim> ", nrow(q), " events over ", format(utils::tail(q$time, 1), digits = 4),
      " time units, ", nrow(x$trades), " trades\n", sep = "")
  ag <- x$agents[x$agents$type != "initial", c("name", "type"), drop = FALSE]
  cat("  agents: ", paste0(ag$name, " (", ag$type, ")", collapse = ", "), "\n", sep = "")
  last <- utils::tail(q, 1)
  cat("  final bid/ask: ", fmt_num(last$bid), " / ", fmt_num(last$ask), "\n", sep = "")
  if (!is.null(x$shocks)) cat("  shocks: ", nrow(x$shocks), "\n", sep = "")
  invisible(x)
}

#' Summarise liquidity in a simulated market
#'
#' Spreads and best-quote sizes are *time-weighted*: each quote counts for the
#' time until the next event, which is how liquidity is experienced by a
#' trader arriving at a random moment.
#'
#' @param object A `lob_sim` from [simulate_lob()].
#' @param ... Unused.
#' @return An object of class `summary.lob_sim`: a list with the number of
#'   events, duration, trades and volume, the time-weighted mean spread (in
#'   price units and ticks), the median spread in ticks, time-weighted mean
#'   best bid and ask sizes, the share of time with a one-sided book, realised
#'   volatility of the mid price per square-root time unit, and the first and
#'   last mid prices.
#' @examples
#' sim <- simulate_lob(agent_zi(limit_rate = 2, market_rate = 0.5,
#'                              cancel_rate = 0.05),
#'                     n_events = 1000, seed = 42)
#' summary(sim)
#' @export
summary.lob_sim <- function(object, ...) {
  q <- object$quotes
  w <- time_weights(q$time)
  two <- !is.na(q$spread)
  mid <- fill_mid(object)
  r <- diff(log(mid))
  duration <- utils::tail(q$time, 1)
  out <- list(
    n_events = nrow(q),
    duration = duration,
    n_trades = nrow(object$trades),
    volume = sum(object$trades$qty),
    mean_spread = wmean(q$spread[two], w[two]),
    mean_spread_ticks = wmean(q$spread_ticks[two], w[two]),
    median_spread_ticks = stats::median(q$spread_ticks[two]),
    mean_bid_size = wmean(q$bid_size, w),
    mean_ask_size = wmean(q$ask_size, w),
    one_sided_share = sum(w[!two]) / sum(w),
    realised_vol = sqrt(sum(r^2) / duration),
    first_mid = mid[1],
    last_mid = utils::tail(mid, 1)
  )
  structure(out, class = "summary.lob_sim")
}

#' @export
print.summary.lob_sim <- function(x, ...) {
  cat("Limit order book simulation\n")
  cat(sprintf("  %-28s %s\n", "events / duration",
              paste(x$n_events, "/", format(x$duration, digits = 4))))
  cat(sprintf("  %-28s %s\n", "trades / volume", paste(x$n_trades, "/", x$volume)))
  cat(sprintf("  %-28s %.4f (%.2f ticks)\n", "mean spread (time-weighted)",
              x$mean_spread, x$mean_spread_ticks))
  cat(sprintf("  %-28s %.1f ticks\n", "median spread", x$median_spread_ticks))
  cat(sprintf("  %-28s %.2f / %.2f\n", "mean best bid / ask size",
              x$mean_bid_size, x$mean_ask_size))
  cat(sprintf("  %-28s %.1f%%\n", "time with one-sided book", 100 * x$one_sided_share))
  cat(sprintf("  %-28s %.5f\n", "realised volatility", x$realised_vol))
  cat(sprintf("  %-28s %.2f -> %.2f\n", "mid price", x$first_mid, x$last_mid))
  invisible(x)
}

#' Compare liquidity across agent mixes
#'
#' Runs [simulate_lob()] once per scenario with the same settings and seed, and
#' collects the key numbers from [summary.lob_sim()] into one table. This is a
#' quick way to see, for example, how adding a market maker changes spreads.
#'
#' @param scenarios A named list. Each element is a list of agents (or a
#'   single agent).
#' @param ... Further arguments passed to [simulate_lob()], such as
#'   `n_events` and `seed`.
#' @return A data frame with one row per scenario.
#' @examples
#' zi <- agent_zi(limit_rate = 1, market_rate = 0.4, cancel_rate = 0.05)
#' compare_scenarios(
#'   list(zi_only = list(zi),
#'        with_mm = list(zi, agent_market_maker(half_spread = 1))),
#'   n_events = 1000, seed = 1
#' )
#' @export
compare_scenarios <- function(scenarios, ...) {
  if (!is.list(scenarios) || is.null(names(scenarios)) || any(!nzchar(names(scenarios)))) {
    stop("`scenarios` must be a named list of agent lists.", call. = FALSE)
  }
  rows <- lapply(names(scenarios), function(nm) {
    s <- summary(simulate_lob(scenarios[[nm]], ...))
    data.frame(
      scenario = nm,
      trades = s$n_trades,
      volume = s$volume,
      mean_spread_ticks = s$mean_spread_ticks,
      mean_bid_size = s$mean_bid_size,
      mean_ask_size = s$mean_ask_size,
      one_sided_share = s$one_sided_share,
      realised_vol = s$realised_vol,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Price impact of trades
#'
#' Measures how far the mid price moves in the direction of aggressive order
#' flow. For each event in which an incoming order traded, the trade sign is
#' +1 for buys and -1 for sells, and impact at lag `L` is the average of
#' `sign * (mid[e + L] - mid[e - 1])` in ticks, where `mid[e - 1]` is the mid
#' price just before the event. Lag 0 is the immediate impact.
#'
#' With `by = "size"`, immediate impact is averaged within quantile bins of
#' the traded quantity instead, which shows the concave shape of impact in
#' size.
#'
#' Mid prices are carried forward through moments when one side of the book
#' is empty.
#'
#' @param sim A `lob_sim`.
#' @param lags Event lags at which to measure impact (for `by = "lag"`).
#' @param by `"lag"` or `"size"`.
#' @param bins Number of size bins (for `by = "size"`).
#' @return A data frame of class `lob_impact` with the impact in ticks and the
#'   number of events behind each estimate.
#' @examples
#' sim <- simulate_lob(list(agent_zi(limit_rate = 2, market_rate = 0.4,
#'                                   cancel_rate = 0.05),
#'                          agent_noise(rate = 0.3, size_max = 8)),
#'                     n_events = 3000, seed = 3)
#' price_impact(sim, lags = c(0, 10, 50))
#' price_impact(sim, by = "size", bins = 4)
#' @export
price_impact <- function(sim, lags = 0:50, by = c("lag", "size"), bins = 5) {
  check_sim(sim)
  by <- match.arg(by)
  mid <- fill_mid(sim)
  n <- length(mid)
  ts <- sim$params$tick_size
  pre <- c(mid[1], mid[-n])  # mid just before event e
  tr <- sim$trades
  if (!nrow(tr)) {
    stop("The simulation has no trades.", call. = FALSE)
  }
  signed <- tapply(tr$qty * ifelse(tr$side == "buy", 1, -1), tr$event, sum)
  ev <- as.integer(names(signed))
  sgn <- sign(as.numeric(signed))
  size <- abs(as.numeric(signed))
  keep <- sgn != 0
  ev <- ev[keep]
  sgn <- sgn[keep]
  size <- size[keep]

  if (by == "lag") {
    if (!is.numeric(lags) || any(lags < 0) || any(lags != round(lags))) {
      stop("`lags` must be non-negative whole numbers.", call. = FALSE)
    }
    out <- lapply(lags, function(L) {
      ok <- ev + L <= n
      d <- sgn[ok] * (mid[ev[ok] + L] - pre[ev[ok]]) / ts
      data.frame(lag = L, impact_ticks = mean(d), n = sum(ok))
    })
    out <- do.call(rbind, out)
  } else {
    check_count(bins, "bins")
    d <- sgn * (mid[ev] - pre[ev]) / ts
    br <- unique(stats::quantile(size, seq(0, 1, length.out = bins + 1L), names = FALSE))
    if (length(br) < 2L) br <- c(min(size) - 0.5, max(size) + 0.5)
    g <- cut(size, br, include.lowest = TRUE)
    out <- data.frame(
      size_bin = levels(g),
      mean_size = as.numeric(tapply(size, g, mean)),
      impact_ticks = as.numeric(tapply(d, g, mean)),
      n = as.integer(table(g)),
      stringsAsFactors = FALSE
    )
    out <- out[out$n > 0, , drop = FALSE]
  }
  rownames(out) <- NULL
  structure(out, class = c("lob_impact", "data.frame"), by = by)
}

#' Resilience of the book after liquidity shocks
#'
#' For each shock injected through the `shocks` argument of [simulate_lob()],
#' compares the spread before and after the shock and counts how long the
#' book takes to recover.
#'
#' The pre-shock spread is the average spread over the `window` events before
#' the shock. The book has *recovered* at the first event at or after the
#' shock whose spread is at most `tolerance` times the pre-shock spread (a
#' one-sided book never counts as recovered).
#'
#' @param sim A `lob_sim` with shocks.
#' @param window Number of events used before and after each shock.
#' @param tolerance Multiple of the pre-shock spread that counts as
#'   recovered.
#' @return A data frame with one row per shock: `event`, `side`, `qty`,
#'   `filled`, `pre_spread_ticks`, `peak_spread_ticks` (`Inf` if a side was
#'   emptied), `events_to_recover`, `time_to_recover` (`NA` if the book had
#'   not recovered by the end of the simulation) and `mid_change_ticks`, the
#'   change in mid price from just before the shock to `window` events after.
#' @examples
#' sim <- simulate_lob(
#'   list(agent_zi(limit_rate = 3, market_rate = 0.3, cancel_rate = 0.05),
#'        agent_market_maker(half_spread = 2)),
#'   n_events = 3000, seed = 7,
#'   shocks = data.frame(event = 1500, side = "sell", qty = 30)
#' )
#' resilience(sim)
#' @export
resilience <- function(sim, window = 200, tolerance = 1) {
  check_sim(sim)
  if (is.null(sim$shocks)) {
    stop("The simulation has no shocks. Use the `shocks` argument of simulate_lob().",
         call. = FALSE)
  }
  check_count(window, "window")
  q <- sim$quotes
  n <- nrow(q)
  sp <- q$spread_ticks
  sp_inf <- ifelse(is.na(sp), Inf, sp)
  mid <- fill_mid(sim)
  ts <- sim$params$tick_size
  tr <- sim$trades
  rows <- lapply(seq_len(nrow(sim$shocks)), function(j) {
    e <- sim$shocks$event[j]
    before <- seq.int(max(1L, e - window), e - 1L)
    before <- before[before >= 1L & before < e]
    pre <- if (length(before)) mean(sp[before], na.rm = TRUE) else NA_real_
    after <- e:min(n, e + window)
    rec <- which(!is.na(sp[e:n]) & sp[e:n] <= tolerance * pre)
    k <- if (length(rec)) e + rec[1] - 1L else NA_integer_
    pre_mid <- if (e > 1L) mid[e - 1L] else mid[1]
    data.frame(
      event = e,
      side = sim$shocks$side[j],
      qty = sim$shocks$qty[j],
      filled = sum(tr$qty[tr$event == e & (tr$buyer == "shock" | tr$seller == "shock")]),
      pre_spread_ticks = pre,
      peak_spread_ticks = max(sp_inf[after]),
      events_to_recover = k - e,
      time_to_recover = if (is.na(k)) NA_real_ else q$time[k] - q$time[e],
      mid_change_ticks = (mid[min(n, e + window)] - pre_mid) / ts,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Stylised facts of simulated returns
#'
#' Samples the mid price on a regular time grid and computes the statistics
#' usually compared with real markets: the excess kurtosis of returns (fat
#' tails give values above 0), the autocorrelation of returns (close to zero
#' in efficient markets, often negative at lag 1 because of bid-ask bounce)
#' and the autocorrelation of absolute returns (slowly decaying positive values
#' indicate volatility clustering).
#'
#' @param sim A `lob_sim`.
#' @param dt Sampling interval in simulated time. Defaults to a grid of about
#'   `n_events / 10` points.
#' @param max_lag Largest autocorrelation lag.
#' @return An object of class `lob_facts`: a list with `returns`, `dt`,
#'   `excess_kurtosis`, and data frames `acf_returns` and `acf_abs_returns`.
#' @examples
#' sim <- simulate_lob(list(agent_zi(limit_rate = 2, market_rate = 0.5,
#'                                   cancel_rate = 0.05),
#'                          agent_momentum(rate = 0.3)),
#'                     n_events = 4000, seed = 11)
#' stylized_facts(sim)
#' @export
stylized_facts <- function(sim, dt = NULL, max_lag = 20) {
  check_sim(sim)
  q <- sim$quotes
  mid <- fill_mid(sim)
  duration <- utils::tail(q$time, 1)
  if (is.null(dt)) dt <- duration / max(50, nrow(q) %/% 10)
  if (!is.numeric(dt) || length(dt) != 1L || dt <= 0 || dt >= duration) {
    stop("`dt` must be a positive number smaller than the simulated duration.",
         call. = FALSE)
  }
  grid <- seq(0, duration, by = dt)
  idx <- findInterval(grid, q$time)
  p <- c(mid[1], mid)[idx + 1L]  # before the first event, use the first mid
  r <- diff(log(p))
  m2 <- mean((r - mean(r))^2)
  kurt <- if (m2 > 0) mean((r - mean(r))^4) / m2^2 - 3 else NA_real_
  max_lag <- min(max_lag, length(r) - 1L)
  a1 <- acf_values(r, max_lag)
  a2 <- acf_values(abs(r), max_lag)
  structure(
    list(
      returns = r,
      dt = dt,
      excess_kurtosis = kurt,
      acf_returns = data.frame(lag = seq_len(max_lag), acf = a1),
      acf_abs_returns = data.frame(lag = seq_len(max_lag), acf = a2)
    ),
    class = "lob_facts"
  )
}

#' @export
print.lob_facts <- function(x, ...) {
  cat("Stylised facts of mid-price returns (", length(x$returns),
      " returns, dt = ", format(x$dt, digits = 3), ")\n", sep = "")
  cat(sprintf("  %-34s %.3f\n", "excess kurtosis", x$excess_kurtosis))
  cat(sprintf("  %-34s %.3f\n", "acf of returns, lag 1", x$acf_returns$acf[1]))
  cat(sprintf("  %-34s %.3f\n", "acf of |returns|, lag 1", x$acf_abs_returns$acf[1]))
  k <- min(10L, nrow(x$acf_abs_returns))
  cat(sprintf("  %-34s %.3f\n", paste0("acf of |returns|, mean lags 1-", k),
              mean(x$acf_abs_returns$acf[seq_len(k)])))
  invisible(x)
}

# ---- Helpers ----------------------------------------------------------------

check_sim <- function(sim) {
  if (!inherits(sim, "lob_sim")) {
    stop("`sim` must be a simulation from simulate_lob().", call. = FALSE)
  }
}

# Mid price with gaps (one-sided book) filled by the last known value.
fill_mid <- function(sim) {
  m <- sim$quotes$mid
  if (all(is.na(m))) {
    stop("The book was never two-sided, so no mid price is available.", call. = FALSE)
  }
  first <- which(!is.na(m))[1]
  m[seq_len(first - 1L)] <- m[first]
  ok <- !is.na(m)
  m[ok][cumsum(ok)]
}

time_weights <- function(time) {
  w <- diff(time)
  c(w, if (length(w)) stats::median(w) else 1)
}

wmean <- function(x, w) {
  if (!length(x) || sum(w) <= 0) return(NA_real_)
  sum(x * w) / sum(w)
}

acf_values <- function(x, max_lag) {
  if (max_lag < 1L || stats::var(x) == 0) return(rep(NA_real_, max(0L, max_lag)))
  as.numeric(stats::acf(x, lag.max = max_lag, plot = FALSE)$acf)[-1L]
}

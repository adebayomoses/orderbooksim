#' Plot a simulated market
#'
#' @param x A `lob_sim`.
#' @param type What to draw:
#'   * `"price"`: best bid and ask over time, with trades as points and shocks
#'     as vertical lines.
#'   * `"spread"`: the bid-ask spread in ticks over time.
#'   * `"depth"`: the final order book (see [plot.lob()]).
#'   * `"inventory"`: each agent's inventory over time.
#'   * `"fundamental"`: the informed traders' fundamental value against the
#'     mid price.
#' @param ... Further arguments passed to the underlying base graphics call.
#' @return `x`, invisibly.
#' @examples
#' sim <- simulate_lob(
#'   list(agent_zi(limit_rate = 2, market_rate = 0.4, cancel_rate = 0.05),
#'        agent_market_maker(half_spread = 2)),
#'   n_events = 1500, seed = 2,
#'   shocks = data.frame(event = 800, side = "buy", qty = 25)
#' )
#' plot(sim)
#' plot(sim, type = "spread")
#' plot(sim, type = "inventory")
#' @export
plot.lob_sim <- function(x, type = c("price", "spread", "depth", "inventory",
                                     "fundamental"), ...) {
  type <- match.arg(type)
  q <- x$quotes
  shock_t <- if (is.null(x$shocks)) numeric() else q$time[x$shocks$event]
  switch(
    type,
    price = {
      rng <- headroom(range(c(q$bid, q$ask, x$trades$price), na.rm = TRUE))
      graphics::plot(q$time, q$ask, type = "s", col = pal[["ask"]], ylim = rng,
                     xlab = "Time", ylab = "Price", main = "Best quotes and trades", ...)
      graphics::lines(q$time, q$bid, type = "s", col = pal[["bid"]])
      if (nrow(x$trades)) {
        graphics::points(x$trades$time, x$trades$price, pch = 16, cex = 0.3,
                         col = grDevices::adjustcolor(pal[["trade"]], 0.5))
      }
      graphics::abline(v = shock_t, lty = 2, col = pal[["shock"]])
      graphics::legend("topleft", c("ask", "bid", "trade"), bty = "n", horiz = TRUE,
                       col = c(pal[["ask"]], pal[["bid"]], pal[["trade"]]),
                       lty = c(1, 1, NA), pch = c(NA, NA, 16))
    },
    spread = {
      one <- is.na(q$spread_ticks)
      rng <- range(q$spread_ticks, na.rm = TRUE)
      if (!all(is.finite(rng))) rng <- c(0, 1)
      if (any(one)) rng <- headroom(rng)
      graphics::plot(q$time, q$spread_ticks, type = "s", col = pal[["trade"]], ylim = rng,
                     xlab = "Time", ylab = "Spread (ticks)", main = "Bid-ask spread", ...)
      if (any(one)) {
        # Shade spells when one side of the book is empty (no spread exists).
        usr <- graphics::par("usr")
        r <- rle(one)
        ends <- cumsum(r$lengths)
        starts <- ends - r$lengths + 1L
        end_t <- c(q$time[-1], utils::tail(q$time, 1))
        for (k in which(r$values)) {
          graphics::rect(q$time[starts[k]], usr[3], end_t[ends[k]], usr[4], border = NA,
                         col = grDevices::adjustcolor(pal[["shock"]], 0.25))
        }
        graphics::legend("topleft", "one-sided book", bty = "n", border = NA,
                         fill = grDevices::adjustcolor(pal[["shock"]], 0.25))
      }
      graphics::abline(v = shock_t, lty = 2, col = pal[["shock"]])
    },
    depth = plot.lob(x$book, ...),
    inventory = plot_inventory(x, ...),
    fundamental = {
      if (is.null(x$fundamental)) {
        stop("The simulation has no informed traders.", call. = FALSE)
      }
      f <- x$fundamental
      rng <- headroom(range(c(q$mid, f$value), na.rm = TRUE))
      graphics::plot(q$time, q$mid, type = "s", col = pal[["trade"]], ylim = rng,
                     xlab = "Time", ylab = "Price",
                     main = "Mid price and fundamental value", ...)
      for (a in unique(f$agent)) {
        g <- f[f$agent == a, ]
        graphics::lines(g$time, g$value, col = pal[["shock"]], type = "s")
      }
      graphics::legend("topleft", c("mid", "fundamental"), bty = "n", horiz = TRUE,
                       col = c(pal[["trade"]], pal[["shock"]]), lty = 1)
    }
  )
  invisible(x)
}

#' Plot the depth of a limit order book
#'
#' Draws a horizontal bar chart of resting quantity by price, bids below and
#' asks above the spread.
#'
#' @param x A `lob`.
#' @param levels Number of price levels per side.
#' @param cex.names Size of the price labels; smaller values let every level
#'   be labelled.
#' @param ... Further arguments passed to [graphics::barplot()].
#' @return `x`, invisibly.
#' @examples
#' book <- lob_new()
#' for (k in 1:5) {
#'   lob_submit(book, "buy", qty = 6 - k, price = 100 - k / 100)
#'   lob_submit(book, "sell", qty = k, price = 100 + k / 100)
#' }
#' plot(book)
#' @export
plot.lob <- function(x, levels = 10, cex.names = 0.75, ...) {
  d <- lob_depth(x, levels)
  if (!nrow(d)) {
    graphics::plot.new()
    graphics::title("Empty order book")
    return(invisible(x))
  }
  d <- d[order(d$price), ]
  cols <- ifelse(d$side == "bid", pal[["bid"]], pal[["ask"]])
  graphics::barplot(d$qty, names.arg = format(d$price, nsmall = 2), horiz = TRUE,
                    col = cols, border = NA, las = 1, xlab = "Quantity",
                    cex.names = cex.names,
                    main = "Order book depth", ...)
  graphics::legend("bottomright", c("ask", "bid"), fill = c(pal[["ask"]], pal[["bid"]]),
                   border = NA, bty = "n")
  invisible(x)
}

#' @export
plot.lob_impact <- function(x, ...) {
  if (identical(attr(x, "by"), "size")) {
    graphics::plot(x$mean_size, x$impact_ticks, type = "b", pch = 16,
                   col = pal[["trade"]], xlab = "Signed volume per event (absolute)",
                   ylab = "Immediate impact (ticks)", main = "Price impact by size", ...)
  } else {
    graphics::plot(x$lag, x$impact_ticks, type = "l", col = pal[["trade"]], lwd = 2,
                   xlab = "Events after trade", ylab = "Impact (ticks)",
                   main = "Price impact", ...)
  }
  graphics::abline(h = 0, col = "grey60")
  invisible(x)
}

#' @export
plot.lob_facts <- function(x, ...) {
  op <- graphics::par(mfrow = c(1, 2))
  on.exit(graphics::par(op))
  ci <- 1.96 / sqrt(length(x$returns))
  for (k in c("acf_returns", "acf_abs_returns")) {
    a <- x[[k]]
    graphics::plot(a$lag, a$acf, type = "h", lwd = 3, col = pal[["trade"]],
                   ylim = range(c(a$acf, ci, -ci, 0), na.rm = TRUE),
                   xlab = "Lag", ylab = "Autocorrelation",
                   main = if (k == "acf_returns") "Returns" else "Absolute returns", ...)
    graphics::abline(h = c(-ci, 0, ci), lty = c(2, 1, 2), col = "grey60")
  }
  invisible(x)
}

plot_inventory <- function(x, ...) {
  tr <- x$trades
  ag <- x$agents$name[x$agents$type != "initial"]
  if (!nrow(tr) || !length(ag)) {
    stop("No trades to plot.", call. = FALSE)
  }
  paths <- lapply(ag, function(a) {
    d <- ifelse(tr$buyer == a, tr$qty, 0) - ifelse(tr$seller == a, tr$qty, 0)
    c(0, cumsum(d))
  })
  t <- c(0, tr$time)
  cols <- grDevices::hcl.colors(length(ag), "Dark 3")
  graphics::plot(range(t), range(unlist(paths)), type = "n", xlab = "Time",
                 ylab = "Inventory", main = "Agent inventories", ...)
  graphics::abline(h = 0, col = "grey80")
  for (i in seq_along(ag)) graphics::lines(t, paths[[i]], type = "s", col = cols[i])
  graphics::legend("topleft", ag, col = cols, lty = 1, bty = "n", cex = 0.8)
}

# Bid/ask/shock hues come from a palette validated for colour-vision deficiency.
# Extra space above the data for a legend.
headroom <- function(rng) c(rng[1], rng[2] + 0.15 * diff(rng))

pal <- c(bid = "#2a78d6", ask = "#eb6834", trade = "#52514e", shock = "#6250d6")

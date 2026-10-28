# orderBookSim

<!-- badges: start -->
[![R-CMD-check](https://github.com/adebayomoses/orderBookSim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adebayomoses/orderBookSim/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/adebayomoses/orderBookSim/graph/badge.svg)](https://app.codecov.io/gh/adebayomoses/orderBookSim)
<!-- badges: end -->

Agent-based limit order book simulation for teaching and researching market
microstructure in R.

`orderBookSim` gives you:

- **A matching engine.** A continuous double auction with price-time priority,
  limit, market and cancel orders (`lob_new()`, `lob_submit()`, `lob_cancel()`).
- **Trading agents.** Zero-intelligence order flow (Smith et al. 2003), noise
  traders, inventory-constrained market makers, momentum traders and informed
  traders (`agent_*()`).
- **An event-driven simulator.** Continuous-time Poisson arrivals, reproducible
  seeds and injectable liquidity shocks (`simulate_lob()`).
- **Liquidity measures.** Time-weighted spreads and depth, price impact by lag
  and size, resilience after shocks, and stylised facts of returns.
- **An interactive app.** A 'shiny' explorer for the classroom (`run_lob_app()`).

## Installation

```r
# install.packages("remotes")
remotes::install_github("adebayomoses/orderBookSim")
```

## Example

```r
library(orderBookSim)

agents <- list(
  agent_zi(limit_rate = 2, market_rate = 0.4, cancel_rate = 0.05),
  agent_market_maker(half_spread = 2),
  agent_informed(rate = 0.2)
)

sim <- simulate_lob(agents, n_events = 5000, seed = 1,
                    shocks = data.frame(event = 2500, side = "sell", qty = 40))

summary(sim)          # spreads, depth, volatility
plot(sim)             # quotes and trades
price_impact(sim)     # impact curve
resilience(sim)       # recovery after the shock
stylized_facts(sim)   # fat tails, volatility clustering
```

See `vignette("orderBookSim")` for a guided tour.

## Performance

The engine is written in plain R and handles about 10,000 events per second
on a laptop, which is enough for teaching and exploratory research. The
engine's internals are kept in a few small functions (`book_*()`) so that
they can be moved to C++ later without changing the user-facing API.

## Reference

Smith, E., Farmer, J. D., Gillemot, L. and Krishnamurthy, S. (2003).
Statistical theory of the continuous double auction. *Quantitative Finance*,
3(6), 481–514. <https://doi.org/10.1088/1469-7688/3/6/307>

mixed_agents <- function() {
  list(
    agent_zi(limit_rate = 2, market_rate = 0.4, cancel_rate = 0.05),
    agent_market_maker(half_spread = 2),
    agent_noise(rate = 0.2),
    agent_momentum(rate = 0.2, lookback = 20),
    agent_informed(rate = 0.2)
  )
}

test_that("simulations are reproducible and leave the global seed alone", {
  set.seed(99)
  before <- .Random.seed
  s1 <- simulate_lob(mixed_agents(), n_events = 800, seed = 5)
  expect_identical(.Random.seed, before)
  s2 <- simulate_lob(mixed_agents(), n_events = 800, seed = 5)
  expect_identical(s1$quotes, s2$quotes)
  expect_identical(s1$trades, s2$trades)
})

test_that("inventory and cash are conserved across agents", {
  s <- simulate_lob(mixed_agents(), n_events = 1500, seed = 1,
                    shocks = data.frame(event = 700, side = "sell", qty = 30))
  expect_equal(sum(s$agents$inventory), 0)
  expect_equal(sum(s$agents$cash), 0, tolerance = 1e-8)
  expect_equal(s$agents$bought - s$agents$sold, s$agents$inventory)
  expect_equal(sum(s$agents$bought), sum(s$trades$qty))
})

test_that("output has one quote row per event and valid books", {
  s <- simulate_lob(mixed_agents(), n_events = 1000, seed = 2)
  expect_s3_class(s, "lob_sim")
  expect_equal(nrow(s$quotes), 1000)
  expect_true(all(diff(s$quotes$time) > 0))
  two <- !is.na(s$quotes$spread_ticks)
  expect_true(all(s$quotes$spread_ticks[two] >= 1))   # never crossed or locked
  expect_true(all(s$trades$buyer %in% s$agents$name))
  expect_true(all(s$trades$seller %in% s$agents$name))
  expect_false(is.null(s$fundamental))
  expect_output(print(s), "lob_sim")
})

test_that("shocks are executed by the shock agent at the chosen event", {
  s <- simulate_lob(mixed_agents(), n_events = 600, seed = 3,
                    shocks = data.frame(event = 300, side = "buy", qty = 10))
  expect_equal(s$quotes$actor[300], "shock")
  shock_tr <- s$trades[s$trades$buyer == "shock", ]
  expect_true(all(shock_tr$event == 300))
  expect_true(sum(shock_tr$qty) > 0)
})

test_that("a single agent and an empty starting book both work", {
  s <- simulate_lob(agent_zi(limit_rate = 1, market_rate = 0.3), n_events = 300,
                    seed = 4, init_levels = 0)
  expect_equal(nrow(s$quotes), 300)
})

test_that("duplicate agent names are made unique", {
  s <- simulate_lob(list(agent_zi(), agent_zi()), n_events = 200, seed = 1)
  expect_equal(s$agents$name, c("initial", "zi", "zi_1"))
})

test_that("market makers respect their inventory limit", {
  s <- simulate_lob(
    list(agent_noise(rate = 1, size_max = 3),
         agent_market_maker(rate = 2, size = 2, inventory_limit = 6, skew = 0)),
    n_events = 2000, seed = 8
  )
  mm <- s$trades$buyer == "market_maker" | s$trades$seller == "market_maker"
  d <- ifelse(s$trades$buyer == "market_maker", s$trades$qty, 0) -
    ifelse(s$trades$seller == "market_maker", s$trades$qty, 0)
  expect_true(any(mm))
  expect_true(max(abs(cumsum(d))) <= 6)
})

test_that("bad arguments are rejected", {
  expect_error(simulate_lob(list(), 10), "lob_agent")
  expect_error(simulate_lob(agent_zi(), n_events = 0), "n_events")
  expect_error(simulate_lob(agent_zi(), 10, shocks = data.frame(event = 20, side = "buy", qty = 1)),
               "between 1")
  expect_error(simulate_lob(agent_zi(), 10, shocks = data.frame(event = 2, side = "x", qty = 1)),
               "buy")
  expect_error(simulate_lob(agent_zi(limit_rate = 0, market_rate = 0, cancel_rate = 0), 10,
                            init_levels = 0), "zero activity")
  expect_error(agent_zi(limit_rate = -1), "non-negative")
  expect_error(agent_market_maker(half_spread = 0), "half_spread")
})

test_that("agents print their parameters", {
  expect_output(print(agent_market_maker()), "half_spread")
})

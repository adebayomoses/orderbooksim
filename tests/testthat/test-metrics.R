sim_fixture <- function(...) {
  simulate_lob(
    list(agent_zi(limit_rate = 2, market_rate = 0.4, cancel_rate = 0.05),
         agent_market_maker(half_spread = 2),
         agent_noise(rate = 0.3, size_max = 6)),
    n_events = 2000, seed = 10, ...
  )
}

test_that("summary reports sensible liquidity numbers", {
  s <- summary(sim_fixture())
  expect_s3_class(s, "summary.lob_sim")
  expect_true(s$mean_spread_ticks >= 1)
  expect_equal(s$mean_spread, s$mean_spread_ticks * 0.01, tolerance = 1e-8)
  expect_true(s$one_sided_share >= 0 && s$one_sided_share <= 1)
  expect_true(s$realised_vol > 0)
  expect_output(print(s), "mean spread")
})

test_that("a market maker narrows the spread", {
  zi <- agent_zi(limit_rate = 0.5, market_rate = 0.3, cancel_rate = 0.05)
  cmp <- compare_scenarios(
    list(zi_only = list(zi), with_mm = list(zi, agent_market_maker(rate = 2, half_spread = 1))),
    n_events = 3000, seed = 1
  )
  expect_equal(cmp$scenario, c("zi_only", "with_mm"))
  expect_lt(cmp$mean_spread_ticks[2], cmp$mean_spread_ticks[1])
  expect_error(compare_scenarios(list(agent_zi())), "named list")
})

test_that("price impact is positive on average and by size", {
  s <- sim_fixture()
  imp <- price_impact(s, lags = c(0, 5, 20))
  expect_s3_class(imp, "lob_impact")
  expect_equal(imp$lag, c(0, 5, 20))
  expect_gt(imp$impact_ticks[1], 0)
  sz <- price_impact(s, by = "size", bins = 3)
  expect_true(all(c("size_bin", "mean_size", "impact_ticks", "n") %in% names(sz)))
  expect_equal(sum(sz$n), imp$n[1])
  expect_error(price_impact(s, lags = -1), "non-negative")
})

test_that("resilience describes each shock", {
  s <- sim_fixture(shocks = data.frame(event = c(600, 1400), side = c("sell", "buy"),
                                       qty = c(30, 30)))
  r <- resilience(s, window = 100)
  expect_equal(nrow(r), 2)
  expect_equal(r$event, c(600, 1400))
  expect_true(all(r$filled > 0 & r$filled <= r$qty))
  expect_true(all(r$peak_spread_ticks >= r$pre_spread_ticks))
  expect_true(all(is.na(r$events_to_recover) | r$events_to_recover >= 0))
  expect_lt(r$mid_change_ticks[1], 0)       # selling pressure moves price down
  expect_error(resilience(sim_fixture()), "no shocks")
})

test_that("stylised facts are computed on a regular grid", {
  f <- stylized_facts(sim_fixture(), max_lag = 10)
  expect_s3_class(f, "lob_facts")
  expect_equal(nrow(f$acf_returns), 10)
  expect_true(is.finite(f$excess_kurtosis))
  expect_true(all(abs(f$acf_abs_returns$acf) <= 1))
  expect_output(print(f), "excess kurtosis")
  expect_error(stylized_facts(sim_fixture(), dt = -1), "dt")
})

test_that("plots run without error", {
  s <- simulate_lob(
    list(agent_zi(limit_rate = 2, market_rate = 0.4, cancel_rate = 0.05),
         agent_informed(rate = 0.2)),
    n_events = 600, seed = 1,
    shocks = data.frame(event = 300, side = "sell", qty = 10)
  )
  path <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (type in c("price", "spread", "depth", "inventory", "fundamental")) {
    expect_invisible(plot(s, type = type))
  }
  expect_invisible(plot(price_impact(s)))
  expect_invisible(plot(price_impact(s, by = "size")))
  expect_invisible(plot(stylized_facts(s)))
  expect_invisible(plot(lob_new()))
  expect_error(plot(simulate_lob(agent_zi(), 100, seed = 1), "fundamental"), "informed")
})

test_that("the shiny app reports a missing dependency clearly", {
  skip_if(requireNamespace("shiny", quietly = TRUE))
  expect_error(run_lob_app(), "shiny")
})

test_that("the shiny app object can be built", {
  skip_if_not_installed("shiny")
  expect_s3_class(lob_app(), "shiny.appobj")
})

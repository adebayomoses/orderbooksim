test_that("market orders follow price-time priority and trade at passive prices", {
  b <- lob_new()
  lob_submit(b, "sell", 5, 100.01, agent = 1)
  lob_submit(b, "sell", 5, 100.02, agent = 2)
  lob_submit(b, "sell", 3, 100.01, agent = 3)
  res <- lob_submit(b, "buy", 7, agent = 9)

  expect_equal(res$filled, 7)
  expect_true(is.na(res$id))
  tr <- lob_trades(b)
  expect_equal(tr$seller, c(1L, 3L))         # older order at the best price first
  expect_equal(tr$qty, c(5, 2))
  expect_equal(tr$price, c(100.01, 100.01))
  expect_equal(tr$side, c("buy", "buy"))

  d <- lob_depth(b)
  expect_equal(d$price, c(100.01, 100.02))
  expect_equal(d$qty, c(1, 5))
})

test_that("market orders larger than the book are partly filled", {
  b <- lob_new()
  lob_submit(b, "buy", 4, 99.99)
  res <- lob_submit(b, "sell", 10)
  expect_equal(res$filled, 4)
  expect_equal(res$resting, 0)
  expect_equal(nrow(lob_orders(b)), 0)
})

test_that("market orders on an empty side do nothing", {
  b <- lob_new()
  res <- lob_submit(b, "buy", 3)
  expect_equal(res$filled, 0)
  expect_equal(nrow(lob_trades(b)), 0)
})

test_that("crossing limit orders execute, then rest the remainder", {
  b <- lob_new()
  lob_submit(b, "sell", 2, 100.00)
  lob_submit(b, "sell", 2, 100.05)
  res <- lob_submit(b, "buy", 5, 100.02)
  expect_equal(res$filled, 2)
  expect_equal(res$resting, 3)
  expect_equal(lob_trades(b)$price, 100.00)
  q <- lob_quotes(b)
  expect_equal(q$bid, 100.02)
  expect_equal(q$ask, 100.05)
  expect_equal(q$bid_size, 3)
  expect_equal(q$spread, 0.03, tolerance = 1e-9)
})

test_that("cancel removes only existing orders", {
  b <- lob_new()
  o <- lob_submit(b, "buy", 3, 99.99)
  expect_true(lob_cancel(b, o$id))
  expect_false(lob_cancel(b, o$id))
  expect_false(lob_cancel(b, 12345L))
  expect_true(is.na(lob_quotes(b)$bid))
})

test_that("cancelling a partly filled order removes the remainder", {
  b <- lob_new()
  o <- lob_submit(b, "sell", 5, 100.01)
  lob_submit(b, "buy", 2)
  expect_equal(lob_orders(b)$qty, 3)
  expect_true(lob_cancel(b, o$id))
  expect_equal(nrow(lob_orders(b)), 0)
})

test_that("input validation catches bad orders", {
  b <- lob_new(0.05)
  expect_error(lob_submit(b, "hold", 1, 100), "buy")
  expect_error(lob_submit(b, "buy", 0, 100), "positive whole")
  expect_error(lob_submit(b, "buy", 1.5, 100), "positive whole")
  expect_error(lob_submit(b, "buy", 1, 100.01), "tick size")
  expect_error(lob_new(-1), "tick_size")
  expect_error(lob_quotes(list()), "lob_new")
})

test_that("depth and print work on an empty book", {
  b <- lob_new()
  expect_equal(nrow(lob_depth(b)), 0)
  expect_output(print(b), "resting orders: 0")
})

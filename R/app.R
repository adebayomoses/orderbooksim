#' Explore order book dynamics interactively
#'
#' Launches a 'shiny' application in which you choose the mix of agents,
#' optionally inject a liquidity shock, and see the resulting quotes, spread,
#' book depth, price impact and liquidity summary. It is designed for
#' teaching: change one slider at a time and compare.
#'
#' Requires the 'shiny' package.
#'
#' @param ... Passed to `shiny::runApp()`, for example `port` or
#'   `launch.browser`.
#' @return Called for its side effect of running the app.
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   run_lob_app()
#' }
#' @export
run_lob_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("run_lob_app() needs the 'shiny' package. Install it with ",
         'install.packages("shiny").', call. = FALSE)
  }
  shiny::runApp(lob_app(), ...)
}

lob_app <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("orderBookSim: limit order book explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Zero-intelligence flow"),
        shiny::sliderInput("zi_limit", "Limit order rate", 0, 5, 2, step = 0.1),
        shiny::sliderInput("zi_market", "Market order rate", 0, 2, 0.4, step = 0.05),
        shiny::sliderInput("zi_cancel", "Cancel rate per order", 0, 0.2, 0.05, step = 0.01),
        shiny::h4("Other agents"),
        shiny::checkboxInput("mm_on", "Market maker", TRUE),
        shiny::sliderInput("mm_half", "Market maker half-spread (ticks)", 0.5, 5, 1.5,
                           step = 0.5),
        shiny::sliderInput("noise", "Noise trader rate", 0, 2, 0.2, step = 0.05),
        shiny::sliderInput("momentum", "Momentum trader rate", 0, 2, 0, step = 0.05),
        shiny::sliderInput("informed", "Informed trader rate", 0, 2, 0, step = 0.05),
        shiny::h4("Liquidity shock"),
        shiny::sliderInput("shock_qty", "Shock size (0 = none)", 0, 100, 0, step = 5),
        shiny::radioButtons("shock_side", NULL, c("sell", "buy"), inline = TRUE),
        shiny::h4("Run"),
        shiny::numericInput("n_events", "Events", 3000, min = 200, max = 50000, step = 500),
        shiny::numericInput("seed", "Seed", 1, min = 1, step = 1),
        shiny::actionButton("go", "Simulate", class = "btn-primary")
      ),
      shiny::mainPanel(
        width = 9,
        shiny::fluidRow(
          shiny::column(6, shiny::plotOutput("price", height = 300)),
          shiny::column(6, shiny::plotOutput("spread", height = 300))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::plotOutput("depth", height = 300)),
          shiny::column(6, shiny::plotOutput("impact", height = 300))
        ),
        shiny::fluidRow(
          shiny::column(5, shiny::h4("Liquidity"), shiny::tableOutput("summary")),
          shiny::column(7, shiny::h4("Agents"), shiny::tableOutput("agents"))
        ),
        shiny::uiOutput("shock_info")
      )
    )
  )

  server <- function(input, output, session) {
    sim <- shiny::eventReactive(input$go, {
      ag <- list(agent_zi(input$zi_limit, input$zi_market, input$zi_cancel))
      if (isTRUE(input$mm_on)) ag <- c(ag, list(agent_market_maker(half_spread = input$mm_half)))
      if (input$noise > 0) ag <- c(ag, list(agent_noise(rate = input$noise)))
      if (input$momentum > 0) ag <- c(ag, list(agent_momentum(rate = input$momentum)))
      if (input$informed > 0) ag <- c(ag, list(agent_informed(rate = input$informed)))
      n <- as.integer(input$n_events)
      shocks <- if (input$shock_qty > 0) {
        data.frame(event = n %/% 2L, side = input$shock_side, qty = input$shock_qty)
      }
      simulate_lob(ag, n_events = n, shocks = shocks, seed = input$seed)
    }, ignoreNULL = FALSE)

    output$price <- shiny::renderPlot(plot(sim(), "price"))
    output$spread <- shiny::renderPlot(plot(sim(), "spread"))
    output$depth <- shiny::renderPlot(plot(sim(), "depth"))
    output$impact <- shiny::renderPlot(plot(price_impact(sim(), lags = 0:50)))
    output$summary <- shiny::renderTable({
      s <- summary(sim())
      data.frame(
        measure = c("Trades", "Mean spread (ticks)", "Mean best bid size",
                    "Mean best ask size", "Time one-sided (%)", "Realised volatility"),
        value = vapply(list(s$n_trades, s$mean_spread_ticks, s$mean_bid_size,
                            s$mean_ask_size, 100 * s$one_sided_share, s$realised_vol),
                       format, character(1), digits = 3)
      )
    })
    output$agents <- shiny::renderTable({
      a <- sim()$agents
      a <- a[a$type != "initial", c("name", "trades", "inventory", "pnl")]
      a$trades <- as.integer(a$trades)
      a$inventory <- as.integer(a$inventory)
      a
    }, digits = 2)
    output$shock_info <- shiny::renderUI({
      s <- sim()
      if (is.null(s$shocks)) return(NULL)
      r <- resilience(s)
      peak <- if (is.finite(r$peak_spread_ticks)) {
        sprintf("peak %.1f ticks", r$peak_spread_ticks)
      } else {
        "one side of the book emptied"
      }
      recovery <- if (is.na(r$events_to_recover)) {
        "did not recover before the run ended"
      } else {
        sprintf("recovered after %d events", r$events_to_recover)
      }
      shiny::p(sprintf(
        "Shock: %s %d at event %d (%d filled). Spread %.1f ticks -> %s; %s.",
        r$side, as.integer(r$qty), r$event, as.integer(r$filled),
        r$pre_spread_ticks, peak, recovery
      ))
    })
  }

  shiny::shinyApp(ui, server)
}

#Project: InvestmentPortfolio
#Version: 1.13

#--------------- Library Management ---------------
packages_needed <- c("renv", "shiny", "shinyjs", "shinydashboard", "quantmod", "xts", "zoo","ROI","DT","dplyr","PerformanceAnalytics", "TTR", "PortfolioAnalytics","plotly", "lubridate", "ggplot2", "reshape2", "shinythemes")
new_packages <- packages_needed[!packages_needed %in% installed.packages()[, "Package"]]
if (length(new_packages)) install.packages(new_packages)
lapply(packages_needed, library, character.only = TRUE)

#--------------- Utility Functions ---------------
isNewQuarter <- function(date, previousDate) {
  month(date) %in% c(1, 4, 7, 10) && month(date) != month(previousDate)
}

getNormalizedWeights <- function(symbol_weights) {
  # Convert input to numeric vector if it's not already
  if(is.list(symbol_weights)) {
    symbol_weights <- unlist(symbol_weights)
  }
  symbol_weights <- as.numeric(symbol_weights)
  
  total_weight <- sum(symbol_weights, na.rm = TRUE)
  if (total_weight > 0) {
    return(symbol_weights / total_weight)
  } else {
    return(rep(1 / length(symbol_weights), length(symbol_weights)))
  }
}

simulatePortfolio <- function(initial_weights, combined_returns) {
  if(is.null(initial_weights) || length(initial_weights) == 0 || sum(is.na(initial_weights)) > 0 || sum(initial_weights) == 0) {
    return(NULL)  # Return NULL or another safe value indicating an error or incomplete state
  }
  
  num_assets <- length(initial_weights)
  portfolio_values <- rep(NA, nrow(combined_returns))
  weights <- matrix(NA, nrow = nrow(combined_returns), ncol = num_assets, dimnames = list(NULL, names(initial_weights)))
  
  # Ensure initial_weights is not empty or null
  if(is.null(initial_weights) || length(initial_weights) == 0) {
    stop("initial_weights is empty or not defined.")
  }
  
  portfolio_values[1] <- 100
  weights[1, ] <- initial_weights
  
  if (nrow(combined_returns) > 1) { # Check if there are enough rows to perform the operation
    for (i in 2:nrow(combined_returns)) {
      asset_values <- portfolio_values[i - 1] * weights[i - 1, ] * (1 + combined_returns[i, ])
      portfolio_values[i] <- sum(asset_values)
      weights[i, ] <- asset_values / portfolio_values[i]
      
      if (isNewQuarter(index(combined_returns)[i], index(combined_returns)[i - 1])) {
        weights[i, ] <- initial_weights
      }
    }
  } else {
    warning("combined_returns does not have enough data.")
  }
  
  return(list(values_xts = xts(portfolio_values, order.by=index(combined_returns)), weights_xts = xts(weights, order.by=index(combined_returns))))
}

#--------------- CPPI Strategy Implementation ---------------
calculateCPPI <- function(risky_returns, riskfree_returns, initial_value, floor_percentage, multiplier) {
  floor_value <- initial_value * floor_percentage
  portfolio_value <- initial_value
  cushion <- portfolio_value - floor_value
  allocation_to_risky <- cushion * multiplier
  
  # Ensure allocation does not exceed total portfolio value
  allocation_to_risky <- min(allocation_to_risky, portfolio_value)
  allocation_to_riskfree <- portfolio_value - allocation_to_risky
  
  # Initial allocations
  risky_value <- allocation_to_risky
  riskfree_value <- allocation_to_riskfree
  
  # Arrays to store portfolio values over time
  portfolio_values <- numeric(length(risky_returns))
  portfolio_values[1] <- portfolio_value
  
  # Simulation over time
  for (i in 2:length(risky_returns)) {
    risky_value <- risky_value * (1 + risky_returns[i])
    riskfree_value <- riskfree_value * (1 + riskfree_returns[i])
    
    portfolio_value <- risky_value + riskfree_value
    cushion <- max(0, portfolio_value - floor_value)
    allocation_to_risky <- min(cushion * multiplier, portfolio_value)
    allocation_to_riskfree <- portfolio_value - allocation_to_risky
    
    risky_value <- allocation_to_risky
    riskfree_value <- allocation_to_riskfree
    
    portfolio_values[i] <- portfolio_value
  }
  
  return(portfolio_values)
}

#--------------- CPPI Tab UI Function Definition ---------------
cppiTabUI <- function() {
  tabItem(tabName = "cppi",
          fluidRow(
            box(plotOutput("cppiPerformancePlot"), title = "CPPI vs Benchmark Performance", width = 12)
          ),
          fluidRow(
            column(4,
                   numericInput("multiplier", "Multiplier", value = 3, min = 0, max = 50),
                   numericInput("floorPercentage", "Floor Percentage", value = 80, min = 0, max = 100)
            ),
            column(8,
                   box(DT::dataTableOutput("cppiPerformanceMetricsTable"), title = "Performance Metrics", width = 12)
            )
          )
  )
}

#--------------- UI Definition ---------------
# Define the JavaScript code
jsCode <- "shinyjs.runAnalysis = function() { $('#runAnalysis').click(); }"

ui <- dashboardPage(
  dashboardHeader(title = "Investment Portfolio Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("dashboard")),
      menuItem("Allocation", tabName = "allocation", icon = icon("area-chart")),
      menuItem("Efficient Frontier", tabName = "efficientFrontier", icon = icon("line-chart")),
      menuItem("CPPI Info", tabName = "cppiInfo", icon = icon("info-circle")),
      menuItem("CPPI", tabName = "cppi", icon = icon("rocket"))
    ),
    actionButton("runAnalysis", "Run Analysis", class = "btn-primary"),
    dateInput("startDate", "Start Date", value = "2023-12-31"),
    dateInput("endDate", "End Date", value = Sys.Date()),
    selectInput("symbols", "Symbols", choices = c("SPY", "AGG", "QQQ", "IWM", "EFA", "EEM", "TLT", "IEF", "LQD", "GLD"), multiple = TRUE, selected = c("SPY", "AGG")),
    uiOutput("benchmarkWeightsUI"),
    uiOutput("fundWeightsUI")
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("runAnalysis")),
    tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-AMS_HTML")),
    tabItems(
      # Tab for Analysis
      tabItem(tabName = "analysis",
              conditionalPanel(
                condition = "output.loading === true",
                tags$div(class = "loading-message", "Loading data... Please wait.")
              ),
              fluidRow(
                box(plotOutput("performanceComparisonPlot"), title = "Performance Comparison", width = 12)
              ),
              fluidRow(
                box(DT::dataTableOutput("performanceMetricsTable"), title = "Performance Metrics", width = 12)
              )
      ),
      # Tab for Allocation
      tabItem(tabName = "allocation",
              fluidRow(
                box(plotOutput("benchmarkAllocationPlot"), title = "Benchmark Allocation", width = 6),
                box(plotOutput("fundAllocationPlot"), title = "Fund Allocation", width = 6)
              )
      ),
      # Tab for Efficient Frontier
      tabItem(tabName = "efficientFrontier",
              fluidRow(
                box(plotlyOutput("efficientFrontierPlot"), title = "Efficient Frontier", width = 12)
              )
      ),
      tabItem(tabName = "cppiInfo",
              h2("Constant Proportion Portfolio Insurance (CPPI)"),
              p("The Constant Proportion Portfolio Insurance (CPPI) is a dynamic investment strategy that adjusts the exposure between a risky asset and a safe asset to protect against significant losses while allowing for upside potential. It's designed to prevent the portfolio value from dropping below a specified floor, effectively combining the growth opportunities of risky investments with the security of risk-free assets."),
              h4("Performance in Different Market Environments:"),
              p("CPPI performs well in trending markets, whether upwards or downwards, due to its mechanism of increasing exposure to risky assets as their value rises and decreasing it as they fall. However, it may underperform in highly volatile markets with no clear trend, as the frequent adjustments between asset allocations can lead to suboptimal outcomes, particularly if transaction costs are significant."),
              h4("Parameters and Formula:"),
              p("Floor Value (F): The minimum portfolio value that the strategy aims to protect."),
              p("Cushion (C): The difference between the current portfolio value (V) and the floor value."),
              p("Multiplier (m): A factor that determines the level of investment in the risky asset, based on the cushion."),
              tags$div(HTML("Investment in Risky Asset = Cushion × Multiplier = (V - F) × m")),
              tags$div(HTML("Investment in Risk-Free Asset = Portfolio Value - Investment in Risky Asset")),
              tags$div(HTML("$$I_{\\text{risky}} = (V - F) \\cdot m$$")),
              tags$div(HTML("$$I_{\\text{safe}} = V - I_{\\text{risky}}$$")),
              h4("References:"),
              p(a("Investopedia CPPI", href="https://www.investopedia.com/terms/c/constant-proportion-portfolio-insurance.asp", target="_blank")),
              p(a("CFA Institute", href="https://www.cfainstitute.org/en/membership/professional-development/refresher-readings/constant-proportion-portfolio-insurance", target="_blank")),
              p(a("Risk Encyclopedia", href="http://riskencyclopedia.com/articles/constant_proportion_portfolio_insurance/", target="_blank"))
      ),
      cppiTabUI()
    ),
    # Author label and version number
    absolutePanel(
      bottom = 10, right = 10, 
      style = "background: transparent; color: #555; font-size: 9px;",
      HTML("Author: Prof.Frenzel <br> Version: 1.10")
    ),
    theme = shinytheme("sandstone")
  )
)

#--------------- Server Logic ---------------
server <- function(input, output, session) {
  # Initialize reactive value for loading state
  loading_state <- reactiveVal(TRUE)
  
  # Output for conditionalPanel to listen to for showing/hiding loading message
  output$loading <- reactive({ loading_state() })
  outputOptions(output, "loading", suspendWhenHidden = FALSE)
  
  # Set default values and trigger data loading immediately
  observe({
    updateSelectInput(session, "symbols", selected = c("SPY", "AGG"))
    
    # Ensure default weights are set for both benchmark and fund weights
    lapply(c("SPY", "AGG"), function(sym) {
      updateNumericInput(session, paste0("benchmarkWeight", sym), value = ifelse(sym == "SPY", 0.6, 0.4))
      updateNumericInput(session, paste0("fundWeight", sym), value = ifelse(sym == "SPY", 1, 0))
    })
    
    # Set loading state to TRUE right before fetching data
    loading_state(TRUE)
    
    # Programmatic click to trigger analysis
    shinyjs::runjs('$("#runAnalysis").click();')
  })
  
  # UI for setting benchmark weights
  output$benchmarkWeightsUI <- renderUI({
    req(input$symbols)
    lapply(input$symbols, function(sym) {
      numericInput(inputId = paste0("benchmarkWeight", sym),
                   label = paste("Benchmark Weight for", sym),
                   value = ifelse(sym == "SPY", 0.6, ifelse(sym == "AGG", 0.4, 1 / length(input$symbols))),
                   min = 0, max = 1, step = 0.01)
    })
  })
  
  # UI for setting fund weights
  output$fundWeightsUI <- renderUI({
    req(input$symbols)
    lapply(input$symbols, function(sym) {
      numericInput(inputId = paste0("fundWeight", sym),
                   label = paste("Fund Weight for", sym),
                   value = ifelse(sym == "SPY", 1, 0),  # Default to 100% SPY, 0% everything else
                   min = 0, max = 1, step = 0.01)
    })
  })
  
  # Reactive expression to fetch symbol data based on user inputs
  symbolData <- eventReactive(input$runAnalysis, {
    req(input$symbols, input$startDate, input$endDate)
    
    # Attempt to fetch data, with error handling
    data <- tryCatch({
      lapply(input$symbols, function(sym) {
        getSymbols(sym, src = "yahoo", from = input$startDate, to = input$endDate, auto.assign = FALSE)
      })
    }, error = function(e) {
      # On error, log and return NULL to prevent downstream errors
      print(paste("Error fetching symbol data:", e$message))
      NULL
    })
    
    # Data fetching complete, set loading state to FALSE
    loading_state(FALSE)
    
    # Return fetched data
    data
  }, ignoreNULL = FALSE)
  
  combinedReturns <- eventReactive(input$runAnalysis, {
    req(symbolData())
    daily_returns <- lapply(symbolData(), function(data) dailyReturn(Cl(data)))
    do.call(merge.xts, daily_returns)
  }, ignoreNULL = FALSE)
  
  portfolioSimulation <- eventReactive(input$runAnalysis, {
  # Use req to ensure that all required inputs are available before proceeding
  req(input$symbols, input$startDate, input$endDate)
  req(sapply(input$symbols, function(sym) input[[paste0("benchmarkWeight", sym)]]))
  req(sapply(input$symbols, function(sym) input[[paste0("fundWeight", sym)]]))

  benchmark_weights <- sapply(input$symbols, function(sym) as.numeric(input[[paste0("benchmarkWeight", sym)]]))
  fund_weights <- sapply(input$symbols, function(sym) as.numeric(input[[paste0("fundWeight", sym)]]))

  benchmark_weights <- getNormalizedWeights(benchmark_weights)
  fund_weights <- getNormalizedWeights(fund_weights)

  benchmark_portfolio <- simulatePortfolio(benchmark_weights, combinedReturns())
  fund_portfolio <- simulatePortfolio(fund_weights, combinedReturns())

  list(benchmark = benchmark_portfolio, fund = fund_portfolio)
}, ignoreNULL = FALSE)
  
  output$benchmarkAllocationPlot <- renderPlot({
    req(portfolioSimulation())
    weights_xts <- portfolioSimulation()$benchmark$weights_xts
    weights_df <- as.data.frame(weights_xts)
    weights_df$Date <- index(weights_xts)
    # Ensure symbols are properly named in the melted data
    colnames(weights_df)[-length(colnames(weights_df))] <- input$symbols
    weights_long <- reshape2::melt(weights_df, id.vars = "Date", variable.name = "Symbol", value.name = "Weight")
    
    ggplot(weights_long, aes(x = Date, y = Weight, fill = Symbol)) +
      geom_area(position = 'stack') +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      labs(x = "", y = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  })
  
  output$fundAllocationPlot <- renderPlot({
    req(portfolioSimulation())
    weights_xts <- portfolioSimulation()$fund$weights_xts
    weights_df <- as.data.frame(weights_xts)
    weights_df$Date <- index(weights_xts)
    # Ensure symbols are properly named in the melted data
    colnames(weights_df)[-length(colnames(weights_df))] <- input$symbols
    weights_long <- reshape2::melt(weights_df, id.vars = "Date", variable.name = "Symbol", value.name = "Weight")
    
    ggplot(weights_long, aes(x = Date, y = Weight, fill = Symbol)) +
      geom_area(position = 'stack') +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      labs(x = "", y = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  })
  
  output$performanceComparisonPlot <- renderPlot({
    req(portfolioSimulation())
    benchmark_values <- as.numeric(portfolioSimulation()$benchmark$values_xts)
    fund_values <- as.numeric(portfolioSimulation()$fund$values_xts)
    comparison_df <- data.frame(Date = index(portfolioSimulation()$benchmark$values_xts), Benchmark = benchmark_values, Fund = fund_values)
    comparison_long <- reshape2::melt(comparison_df, id.vars = 'Date')
    
    ggplot(comparison_long, aes(x = Date, y = value, color = variable)) +
      geom_line() +
      theme_minimal() +
      labs(x = "Date", y = "Portfolio Value") +
      scale_color_manual(values = c("Benchmark" = "#2C3E50", "Fund" = "#E74C3C")) +
      theme(legend.title = element_blank(), legend.position = "top")
  })
  
  #--------------- CPPI Performance Plot ---------------
  output$cppiPerformancePlot <- renderPlot({
    req(input$multiplier, input$floorPercentage)  # Ensure inputs are available
    
    # Debugging: Inspect combinedReturns structure
    print(str(combinedReturns()))
    
    # Check if combinedReturns has any data and the required columns
    if (is.null(combinedReturns()) || ncol(combinedReturns()) == 0 || 
        !("SPY" %in% colnames(combinedReturns())) || !("AGG" %in% colnames(combinedReturns()))) {
      print("SPY or AGG data is missing or combinedReturns is empty.")
      return()
    }
    
    combined_returns <- combinedReturns()
    
    risky_returns <- combined_returns[,"SPY"]
    riskfree_returns <- combined_returns[,"AGG"]
    
    floor_percentage <- input$floorPercentage / 100
    
    cppi_values <- calculateCPPI(as.numeric(risky_returns), as.numeric(riskfree_returns), 100, floor_percentage, input$multiplier)
    
    cppi_ts <- xts(cppi_values, order.by = index(risky_returns))
    
    plot(cppi_ts, main = "CPPI Strategy Performance", ylab = "Portfolio Value", col = "blue", type = 'l')
    lines(portfolioSimulation()$benchmark$values_xts, col = "red")
    
    legend("topright", legend = c("CPPI Portfolio", "Benchmark"), col = c("blue", "red"), lty = 1)
  })
  
  #--------------- CPPI Performance Metrics Table ---------------
  output$cppiPerformanceMetricsTable <- DT::renderDataTable({
    req(input$multiplier, input$floorPercentage)  # Ensure inputs are available
    
    # Debugging: Inspect combinedReturns structure
    print(str(combinedReturns()))
    
    if (is.null(combinedReturns()) || ncol(combinedReturns()) == 0 || 
        !("SPY" %in% colnames(combinedReturns())) || !("AGG" %in% colnames(combinedReturns()))) {
      print("SPY or AGG data is missing or combinedReturns is empty.")
      return()
    }
    
    combined_returns <- combinedReturns()
    
    risky_returns <- combined_returns[,"SPY"]
    riskfree_returns <- combined_returns[,"AGG"]
    
    floor_percentage <- input$floorPercentage / 100
    
    cppi_values <- calculateCPPI(as.numeric(risky_returns), as.numeric(riskfree_returns), 100, floor_percentage, input$multiplier)
    
    cppi_returns <- dailyReturn(xts(cppi_values, order.by = index(risky_returns)))
    
    benchmark_returns <- dailyReturn(portfolioSimulation()$benchmark$values_xts)
    
    cppi_metrics <- calculateAndFormatMetrics(cppi_returns, benchmark_returns)
    benchmark_metrics <- calculateAndFormatMetrics(benchmark_returns)
    
    metrics_data <- rbind(CPPI = cppi_metrics, Benchmark = benchmark_metrics)
    metrics_df <- data.frame(Metric = rownames(metrics_data), metrics_data, stringsAsFactors = FALSE)
    
    DT::datatable(metrics_df, options = list(pageLength = 5, autoWidth = TRUE), rownames = FALSE)
  })
  
  #--------------- Performance Metrics Table Rendering ---------------
  output$performanceMetricsTable <- DT::renderDataTable({
    sim_results <- req(portfolioSimulation())
    req(!is.null(sim_results$benchmark$values_xts) && !is.null(sim_results$fund$values_xts))
    
    benchmark_returns <- dailyReturn(portfolioSimulation()$benchmark$values_xts)
    fund_returns <- dailyReturn(portfolioSimulation()$fund$values_xts)
    risk_free_rate <- 0  # Assuming risk-free rate is 0 for Beta calculation, adjust as necessary
    
    # Helper function to calculate metrics and format them
    calculateAndFormatMetrics <- function(returns, benchmark_returns = NULL) {
      total_return <- Return.cumulative(returns, geometric = TRUE)
      annual_return <- mean(Return.annualized(returns, scale = 252))
      annual_volatility <- sd(returns) * sqrt(252)
      sharpe_ratio <- (annual_return-risk_free_rate)/annual_volatility
      max_drawdown <- -maxDrawdown(returns)
      beta <- if (!is.null(benchmark_returns)) CAPM.beta(returns, benchmark_returns, Rf = risk_free_rate) else NA
      
      # Format all the metrics to round to 4 decimal places
      c(
        Total_Return = round(total_return, 4),
        Annual_Return = round(annual_return, 4),
        Annual_Volatility = round(annual_volatility, 4),
        Sharpe_Ratio = round(sharpe_ratio, 4),
        Max_Drawdown = round(max_drawdown, 4),
        Beta = round(beta, 4)
      )
    }
    
    # Calculate metrics for fund and benchmark
    fund <- calculateAndFormatMetrics(fund_returns, benchmark_returns)
    benchmark <- calculateAndFormatMetrics(benchmark_returns)
    
    # Create a matrix to bind the metrics into a data frame with row names as the first column
    metrics_data <- rbind(fund, benchmark)
    metrics_df <- data.frame(Metric = rownames(metrics_data), metrics_data)
    rownames(metrics_df) <- NULL
    
    # Find the index of the row with the highest Sharpe Ratio
    best_sharpe_index <- which.max(metrics_df$Sharpe_Ratio)
    
    DT::datatable(metrics_df, options = list(
      pageLength = 1,
      searching = FALSE,
      lengthChange = FALSE,
      paging = FALSE, 
      info = FALSE 
    ), rownames = FALSE) %>%
      DT::formatStyle(
        'Metric',
        target = 'row', 
        backgroundColor = styleEqual(metrics_df$Sharpe_Ratio, rep(c('white', '#EBF5FB'), length.out = length(metrics_df$Sharpe_Ratio))[best_sharpe_index]),
        color = styleEqual(metrics_df$Sharpe_Ratio, rep(c('black', 'navy'), length.out = length(metrics_df$Sharpe_Ratio))[best_sharpe_index])
      ) %>%
      DT::formatStyle(
        columns = c('Total_Return', 'Annual_Return', 'Annual_Volatility', 'Sharpe_Ratio', 'Max_Drawdown', 'Beta'),
        fontWeight = styleEqual(best_sharpe_index - 1, 'bold') 
      ) 
  }, server = FALSE)
  
  #--------------- Efficient Frontier ---------------
  output$efficientFrontierPlot <- renderPlotly({
    req(portfolioSimulation())
    req(symbolData())
    
    returns <- do.call(cbind, lapply(symbolData(), function(x) dailyReturn(Cl(x))))
    colnames(returns) <- input$symbols
    
    portf <- portfolio.spec(assets = colnames(returns))
    portf <- add.constraint(portfolio = portf, type = "full_investment")
    portf <- add.constraint(portfolio = portf, type = "box", min = 0, max = 1)
    portf <- add.objective(portfolio = portf, type = "return", name = "mean")
    portf <- add.objective(portfolio = portf, type = "risk", name = "StdDev")
    
    ef <- create.EfficientFrontier(R = returns, portfolio = portf, type = "mean-StdDev", points = 50)
    
    annual_returns <- ef$frontier[, "mean"] * 252  # Assuming 252 trading days in a year
    annual_volatility <- ef$frontier[, "StdDev"] * sqrt(252)
    sharpe_ratios <- annual_returns / annual_volatility
    
    ef_data <- data.frame(
      Risk = annual_volatility,
      Return = annual_returns,
      SharpeRatio = sharpe_ratios
    )
    
    hover_text <- sapply(1:nrow(ef_data), function(idx) {
      portfolio_weights <- ef$frontier[idx, grep("w.", colnames(ef$frontier))]
      info <- paste(input$symbols, sprintf("%.2f%%", portfolio_weights * 100))
      sharpe <- sprintf("Sharpe Ratio: %.2f", ef_data$SharpeRatio[idx])
      paste(c(info, sharpe), collapse = "<br>")
    })
    
    plot_ly(ef_data, x = ~Risk, y = ~Return, type = 'scatter', mode = 'markers',
            hoverinfo = 'text', text = hover_text,
            marker = list(size = 10, color = ~SharpeRatio, colorscale = 'RdYlGn', showscale = FALSE)) %>%
      layout(title = 'Efficient Frontier',
             xaxis = list(title = 'Annual Volatility'),
             yaxis = list(title = 'Annual Return'),
             hovermode = 'closest')
  })
  
  # Once the app is initialized, trigger the data fetch
  observe({
    shinyjs::runjs('$("#runAnalysis").click();')
  })
}

shinyApp(ui = ui, server = server)
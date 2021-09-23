#################################################################
#                        Preface
#################################################################
# Jonathan Mac - Shiny App
# R version 3.5.3 (2019-03-11) -- "Great Truth"
# Packages used: shiny



#################################################################
#                       Build Shiny App
#################################################################


# Set User Interface #
#################################################################

ui <- fluidPage(
  
  #Modify Font Size
  tags$head(
    tags$style(HTML("
                    #confint {
                    color: black;
                    background: white;
                    font-family: 'Times New Roman', Times, serif;
                    font-size: 25px;
                    font-style: italic;
                    }
                    
                    "))
    ),
  
  
  # App title
  titlePanel("Confidence Interval for Sharpe Ratio - Monte Carlo Simulation"),
  
  # Side Bar Layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      #choose number of returns
      numericInput(inputId = "obs", label = "Choose Number of Returns",
                   value = 12, min = 3, max = 3650), #up to 3650 days (10 years)
      
      #choose frequency
      selectInput(inputId = "freq", label = "Choose Frequency of Returns",
                  choices = c("daily", "monthly", "yearly"),
                  selected = "monthly"), #default to monthly
      
      #choose confidence level
      numericInput(inputId = "ci", label = "Choose Confidence Level (Decimal)",
                   value = .9, min = 0.00001, max = .99999, #critical value can't equal exactly 0 or 1
                   step = .05)
      
    ),
    # Main panel for displaying outputs
    mainPanel(
      
      # Output
      textOutput(outputId = "confint"),
      
      # Output
      plotOutput(outputId = "plot1")
      
    )
    
    
  )  
  
    )



# Set Server #
#################################################################

server <- function(input, output) {
  
  
  # 1) Load Library
  library(shiny)
  
  
  # 2) Generate Stock Price data for Company A 
  stock_return_data <- reactive({
    
    # Comments below contain code explaining this object step-by-step
    #ipo.price = 100 # Company A's initial public offering price of $100
    #future_daily_returns = rnorm(n=3650,mean=.0002, sd=.002) # future daily returns for 10 years
    #stock_return_data = ipo.price*(cumprod(1+future_daily_returns)) # future stock prices
    
    100*(cumprod(1+rnorm(n=3650,mean=.0002, sd=.002)))
    
  })
  
  
  # 3) Create helper functions f1, f2, f3 to help execute simulation 
  
  ### f1 function ###
  
  # f1 is a function that takes two arguments (number of returns and frequency of returns) and
  # returns the total iterations, or number of future daily stock prices needed to be simulated
  
  f1 = function(a = input$obs, b = input$freq){ #default argument values to shiny inputs
    
    if (b == "daily") {
      iterations = a*1
      
    } else if(b == "monthly"){
      iterations = a*20 #each month has approximately 20 trading days (weekdays)
      
    } else{ #yearly
      iterations = a*262 #each year has approximately 262 days trading days (weekdays)
    }
    
    return(iterations)
  }
  
  
  
  ### f2 function ###
  
  # f2 is a function that takes the number of simulated returns needed and performs 
  # a monte carlo simulation to return a vector of future stock prices based on r (periodic daily return).
  
  f2 = function(c){ #argument value should come from f1()
    
    # Monte Carlo Simulation step-by-step
    
    # 1) first get stock data
    STOCK <- stock_return_data()
    
    # 2) retrieve daily periodic returns (use closing price)
    STOCK_daily_periodic = STOCK[2:length(STOCK)]/STOCK[1:(length(STOCK)-1)]
    STOCK_daily_periodic = log(STOCK_daily_periodic) #take log bc periodic
    
    # 3) solve for r (periodic daily return)
    
    # r = drift + shock
    drift = mean(STOCK_daily_periodic) - var(STOCK_daily_periodic)/2
    shock = sd(STOCK_daily_periodic)*qnorm(runif(c)) #vector of length = iterations, where qnorm were critical values from a uniform simulation
    
    r = rep(drift,length(shock)) + shock
    
    
    # 4) simulate new future returns
    
    last.close.price = STOCK[length(STOCK)] #start from most recent stock price of Company A
    monte_carlo_prices = cumprod(c(last.close.price,exp(r))) #vector of stock prices based on periodic daily return
    monte_carlo_prices = monte_carlo_prices[-1] #remove original closing price
    
    return(monte_carlo_prices)
    
  }
  
  
  ### f3 function ###
  
  # f3 is a function that takes daily stock prices from monte carlo simulation
  # and returns annualized return and annualized volatility
  
  f3 = function(e = input$obs, f = input$freq, g){ # g default should come from f2() simulation results
    
    # 1) Solve for annualized return = (1 + total return)^period - 1
    
    if (f == "daily") { #as established in f1 function, 262 trading days in a year)
      
      #number of periods in a year
      period = 262/(1*e)
      
      #retrieve daily returns from prices from monte carlo simulation
      len = length(g) #total number of simulated prices
      new = g[2:len]
      old = g[1:(len-1)]
      daily.returns = (new-old)/old #daily returns = (new - old)/old
      
      #get total return
      total.return = prod(1 + daily.returns) #multiply to compound
      
      #annualize return
      annualized.return = (total.return^period) - 1 #annualized return = (1 + total return)^period - 1
      
      
    } else if(f == "monthly"){
      
      period = 12/(1*e)
      
      #retrieve monthly returns
      monthly.returns = rep(NA,e)
      
      for (i in 1:e) { #loop over every month (each 20 days long)
        new = g[i*20]
        old = g[((i-1)*20)+1]
        monthly.returns[i] = (new-old)/old
        
      }
      
      #get total return
      total.return = prod(1 + monthly.returns)
      
      #annualize
      annualized.return = (total.return^period) - 1
      
    } else{ #yearly
      
      period = 1/(1*e)
      
      #retrieve yearly returns
      yearly.returns = rep(NA,e)
      
      for (i in 1:e) { #loop over every year (each 262 days long)
        new = g[(i*262)]
        old = g[(((i-1)*262)+1)]
        yearly.returns[i] = (new-old)/old
        
      }
      
      total.return = prod(1 + yearly.returns)
      
      annualized.return = (total.return^period) - 1
    }
    
    
    # 2) Solve for annualized volatility = sd(returns)*sqrt(# trading days in frequency)
    
    if (f == "daily") { #262 trading days in a year
      
      annualized.volatility = sd(daily.returns)*sqrt(262)
      
    } else if(f == "monthly"){
      
      annualized.volatility = sd(monthly.returns)*sqrt(12)
      
    } else{ #yearly as established in f1 function, 262 trading days in a year)
      
      annualized.volatility = sd(yearly.returns)
      
    }
    
    # 3) return the results
    return(c(annualized.return, annualized.volatility))
  }
  
  
  ### f4 function ###
  
  # f4 is a function that helps perform n monte carlo simulations. For each simulation, f4 uses f2 to simulate
  # and record returns (for graphing purposes later), and uses f3 to help obtain a confidence interval from those returns
  
  f4 = function(n = 100){ #default to 30 simulations
    
    # Accumulate annualized returns & volatilities from monte carlo simulations to get confidence interval later
    annualized_returns = rep(NA,n) #to store annualized returns from n monte carlo simulations
    annualized_volatilities = rep(NA,n) #to store annualized volatilities
    
    # Accumulate simulated stock prices to graph later
    nrow = length( f2(c = f1()) )
    price.matrix = matrix(NA, nrow=nrow, ncol=n)
    
    #Loop over n simulations
    for (i in 1:n) {
      
      monte_carlo_prices = f2(c = f1()) #new simulated stock prices everytime
      results = f3(g = monte_carlo_prices) #return & volatility
      
      #append results
      annualized_returns[i] = results[1]
      annualized_volatilities[i] = results[2]
      
      #append simulations
      price.matrix[,i] = monte_carlo_prices
      
      
    }
    
    # Obtain all observed Sharpe Ratios: SR = Return/(Volatility) assuming no risk free rate
    Sharpe.Ratios = annualized_returns/annualized_volatilities 
    
    # Solve for confidence interval for observed Sharpe Ratios 
    center = mean(Sharpe.Ratios)
    spread = abs(qnorm(((input$ci)/2)+.5)*sd(Sharpe.Ratios)) #divide significance level by 2 bc this is two sided test
    conf.interval = round(c(center-spread,center+spread),3)
    
    
    #Return confidence interval and recorded simulated prices
    return(list(conf.interval,price.matrix))
    
  } 
  
  
  # 4) Execute Monte Carlo Simulation with helper functions, and record results in global space
  
  #create global object iterations
  final.results <- reactive({
    f4()
  })
  
  #create global object iterations
  conf.interval <- reactive({
    final.results()[1]
  })
  
  #create global object monte_carlo_prices, which contains stock prices for one monte carlo simulation
  price.matrix <- reactive({
    data.frame(final.results()[2])
  })
  
  
  
  ### Print Output ###
  output$confint <- renderPrint({
    
    paste("We are ", input$ci*100, "% confident that our sharpe ratio lies between ", 
          conf.interval())
    
  })
  
  
  output$plot1 <- renderPlot({
    
    n = 100
    matplot(price.matrix(), type = c("b"),pch=1,col = 1:n,
            xlab = "Time (over days)", ylab = "Future Daily Stock Price ($)",
            main = "Company A - Monte Carlo Simulation")
    
  })
  
}

shinyApp(ui = ui, server = server)
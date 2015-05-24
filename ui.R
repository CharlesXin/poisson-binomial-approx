shinyUI(fluidPage(
  titlePanel("Poisson Approximation to the Binomial Distribution"),
  
  sidebarLayout(position = "left",
        sidebarPanel( 
                p("The binomial distribution takes two parameters:", 
                        
                        tags$ul(tags$li("n - the number of trials")),
                        tags$ul(tags$li("p - the probability of 'success' for each trial")),
                  
                   "If n is large and p is very small, the Poisson distribution with λ \u003D np can be 
                    used to approximate the binomial distribution. The larger the n and the smaller the p, 
                    the better is the approximation. "),
                
                numericInput(inputId = "n", label = h5("Enter a positive integer for n:"), 
                            value=50, min=0,  step = 1),
                numericInput(inputId = "p", label = h5("Enter p (0 < p < 1):"), 
                             value=0.5, min=0, step = 0.01),
                selectInput(inputId = "upr",
                label = h5("Calculate P(X < a), P(X <= a),
                                       P(X > a) or P(X >= a): "),
                            choices = c("<" = "lt", "<=" = "leq",
                                        ">" = "gt", ">=" = "geq"),
                            selected = "lt"),
                numericInput("a", label = h5("Enter a (a = 0, 1, 2, ...n): "),
                          value = 10, step=1),   
                
                actionButton("run", "Run")
                
        ),
        
        mainPanel(                
                tableOutput("tab"), 
                br(),
                plotOutput("bnplot")
        ))
  
))
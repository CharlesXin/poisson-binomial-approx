library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {  
        process <- reactive({
                if(input$run==0) return(NULL)
                
                ## get input parameters
                n = isolate(input$n)
                p = isolate(input$p)
                a = isolate(input$a)
                ineq.sign = isolate(switch(input$upr, 
                                           "lt" = "<", "leq" = "<=",
                                           "gt" = ">", "geq" = ">="))
                
                ## calculate binomial exact probability
                prob.bin = isolate(switch(input$upr,
                                            "lt"  = pbinom(a-1, size=n, prob=p),
                                            "leq" = pbinom(a, size=n, prob=p), 
                                            "gt"  = pbinom(a, size=n, prob=p, lower.tail = F),
                                            "geq" = pbinom(a-1, size=n, prob=p, lower.tail = F)))
                prob.bin = round(prob.bin, 4)
                
                ## calculate poisson approx. probability
                lambda = n*p
                prob.pois = isolate(switch(input$upr,
                                           "lt"  = ppois(a-1, lambda),
                                           "leq" = ppois(a, lambda), 
                                           "gt"  = ppois(a, lambda, lower.tail=F),                                           
                                           "geq" = ppois(a-1, lambda, lower.tail=F)))
                prob.pois = round(prob.pois, 4)
                
                ## make a matrix
                M = matrix(c(n, p, prob.bin), nrow=3)
                M = cbind(M, c(lambda, NA, prob.pois))
                M = cbind(M, c("λ", "---", "---"))
                
                probName = paste0(paste("P(X", ineq.sign, a), ")")
                rownames(M) = c("n", "p", probName)
                colnames(M) = c("Binomial Exact", "Poisson Approx.", "---")
                
                ## collect above processed data into a list
                out = list()
                out$M = M; out$a = a; out$lambda=lambda; out$n=n; out$p=p
                
                ## return
                out
        })
        
        output$tab <- renderTable({
                if(input$run==0) return(NULL)
                
                ## call process() to get M
                process()$M
        })
        
        output$bnplot = renderPlot({
                if(input$run==0) return(NULL)
                
                ## call process()
                proc = process()
                a = proc$a
                lambda = proc$lambda
                n = proc$n
                p = proc$p
                
                ## plot 
                big.size = element_text(size=13)
                bold.text = element_text(face='bold')
                
                # auto adjust the range of the plot based on lambda: lambda +- 4*sqrt(lambda)
                x=max(floor(lambda-4*sqrt(lambda)), 0):floor(lambda+4*sqrt(lambda))
                
                # set n in stat_function
                if (a>=floor(lambda+4*sqrt(lambda))) {
                        num = length(x)+a-floor(lambda+4*sqrt(lambda))
                } else if(a<floor(lambda-4*sqrt(lambda))) {
                        num = length(x)+floor(lambda-4*sqrt(lambda))-a
                } else {
                        num = length(x) 
                }
                                                                                
                plt = ggplot(transform(data.frame(x=x), y=dbinom(x, size=n, prob=p)), aes(x, y)) + 
                      geom_bar(stat="identity", fill=hcl(h=195,l=65,c=100),color="black") +                       
                      stat_function(geom = "line", fun=dpois, args=list(lambda = lambda), color=hcl(h=15,l=65,c=100), n=num, size=1.2) +
                      theme_bw() + 
                      theme(axis.text.x=big.size, axis.text.y=big.size, 
                            title=bold.text) + 
                      labs(x = "", y = "", title="Binomial Distribution with Possion Density Curve")
                
                ## add vertical line x=a
                plt = plt + geom_vline(xintercept = a, color="darkblue", size=1.2)
                
                ## draw plt
                print(plt)
        })
})
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                tags$head(tags$title("Painel de derivadas do NIPE")),
                #mainPanel("Núcleo Integrado de Pesquisa Econômica \n Painel de derivadas"),
                titlePanel(
                  h1("Núcleo Integrado de Pesquisa Econômica: \n Explicando a derivada", align = "center")
                ),
                
                # Application title
                titlePanel("Domínio"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("inferior", "Limite inferior:", -100, min = -100, max = 0), 
                    sliderInput("superior", "Limite superior:", 100, min = 0.1, max = 100),
                    sliderInput("x", label = "Indique o valor de x", min = -100, max = 100, value = 0),
                    textInput(inputId = "funcao", label = "Informe a função", value = parse(text = "2*x^2"))
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("grafico1"), textOutput("derivada"), textOutput("xvalue"), textOutput("inclinacao")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$funcao, {
    output$grafico1  <- renderPlot({
      fc = expression(input$funcao)
      f = function(x){return(eval(parse(text = fc)))}
      xx = c()
      yy = c()
      
      for(i in input$inferior:input$superior){
        xx[i - input$inferior + 1] = i
        yy[i - input$inferior + 1] = f(i)
        yy[i - input$inferior + 1] = eval(parse(text = yy[i - input$inferior + 1] ), envir=list(x=i))
      }
      deriv = D(parse(text = input$funcao), "x")
      # 
      if(is.numeric(deriv) == FALSE){
        flx = eval(deriv, envir=list(x=input$x))
      }else{
        flx = deriv
      }
      
      fx = eval(parse(text = f(input$x)), envir=list(x=input$x))
      xstar = input$x
      tangente = function(x){fx - flx*xstar + flx*x}
      
      
      plot(x = xx, y = yy, col = 'blue',  type = "l", xlab = "x", ylab = "f(x)", lwd = 3.5, main = "Representação da derivada")
      points(x = input$x, y = tangente(input$x), col = "black")
      #    par(new = TRUE)
      if(input$x %in% -1:-5){
        lines(x = (input$x - 4*input$x):(input$x+4*input$x), y = tangente((input$x - 4*input$x):(input$x+4*input$x)), type = "l", lwd = 3.5, col = "red")
      }else if(input$x %in% -5:-10){
        lines(x = (input$x - 1.75*input$x):(input$x+1.75*input$x), y = tangente((input$x - 1.75*input$x):(input$x+1.75*input$x)), type = "l", lwd = 3.5, col = "red")
      }else if(input$x ==0){
        lines(x = -25:25, y = tangente(-25:25), type = "l", lwd = 3.5, col = "red")
      }else if(input$x %in% 1:5){
        lines(x = (input$x - 4*input$x):(input$x+4*input$x), y = tangente((input$x - 4*input$x):(input$x+4*input$x)), type = "l", lwd = 3.5, col = "red")
      }else if(input$x %in% 5:10){
        lines(x = (input$x - 1.75*input$x):(input$x+1.75*input$x), y = tangente((input$x - 1.75*input$x):(input$x+1.75*input$x)), type = "l", lwd = 3.5, col = "red")
      }else if(input$x %in% 10:35){
        lines(x = (input$x - 1.25*input$x):(input$x+1.25*input$x), y = tangente((input$x - 1.25*input$x):(input$x+1.25*input$x)), type = "l", lwd = 3.5, col = "red")
      }else if(input$x > 35){
        lines(x = (input$x - 0.5*input$x):(input$x+0.5*input$x), y = tangente((input$x - 0.5*input$x):(input$x+0.5*input$x)), type = "l", lwd = 3.5, col = "red")
      }else if(input$x >10){
        lines(x = (input$x - 0.5*input$x):(input$x+0.65*input$x), y = tangente((input$x - 0.65*input$x):(input$x+0.65*input$x)), type = "l",lwd = 3.5, col = "yellow")
      }else{
        lines(x = (input$x - input$x):(input$x+input$x), y = tangente((input$x - input$x):(input$x+input$x)), type = "l", lwd = 3.5, col = "red")
      }
      legend("top", legend=c("f(x)", "f'(x)"),
             col=c("blue", "red"), lty=1:2, cex=0.8)
      
    })
  })
  
  
  output$derivada = renderPrint({
    deriv = D(parse(text = input$funcao), "x")
    cat("A derivada da função é: f1(x) = ")
    print(deriv) 
  })
  
  output$xvalue = renderPrint({
    cat(sprintf("Valor de x: %i", input$x))
  })
  
  output$inclinacao = renderPrint({
    deriv = D(parse(text = input$funcao), "x")
    if(is.numeric(deriv) == FALSE){
      flx = eval(deriv, envir=list(x=input$x))
    }else{
      flx = deriv
    }
    if(is.integer(flx) ==TRUE){
      cat(sprintf("Inclinação = %i", eval(deriv, envir=list(x=input$x)) ))
    }else{
      cat(sprintf("Coeficiente de Inclinação = %0.2f", eval(deriv, envir=list(x=input$x))))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(shinythemes)



ui <- fluidPage(
  theme = shinytheme("flatly"),
  fluidPage(
    titlePanel("Obliczenia Ewapotranspiracji"),
    sidebarLayout(
      sidebarPanel(
        HTML("<h4>Zaznacz odpowiednią stację<br></h4>"),
        radioButtons('sep', 'Stacja',
                     c(Gryzyna='Gryzyna',
                       Rozany_Potok='Rozany_Potok')
        ),
        HTML("<br>"),
        fileInput('file1', 'Wskaż plik z danymi meteorologicznymi (w formacie tekstowym)'),
        tags$hr(),
        downloadButton('downloadData', 'Pobierz przetworzone dane')
      ),
      mainPanel(
        #verbatimTextOutput("ncount_2"),
        textOutput("text"),
        #tableOutput('contents'),
        tableOutput('tabelka'),
        tableOutput('tabelka_rok'),
        verbatimTextOutput("plot2"),
        plotOutput('plot3'),
        plotOutput('plot4')
        
      )
      
    )
  )
)

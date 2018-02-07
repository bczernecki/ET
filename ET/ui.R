library(shiny)
library(shinythemes)



ui <- fluidPage(
  theme = shinytheme("flatly"),
  fluidPage(
    titlePanel("Obliczenia Ewapotranspiracji"),
    sidebarLayout(
      sidebarPanel(
        # # HTML("<h4>Zaznacz odpowiednią stację<br></h4>"),
        # # radioButtons('sep', 'Stacja',
        # #              c(Gryzyna='Gryzyna',
        # #                Rozany_Potok='Rozany_Potok')
        # # ),
        # HTML("<br>"),
        HTML("<h4>Zdefiniuj współrzędne geograficzne stacji:</h4>"),
        textInput("lon","Długość geograficzna (E) | GPK 15.28 | Różany 16.94",""),
        textInput("lat","Szerokość geograficzna (N) 
                  | GPK 52.19 | Różany 52.46",""),
        textInput("alt","Wysokość n.p.m. (w metrach): | GPK 68 | Różany 86",""),
        hr(),
        
        fileInput('file1', 'Wskaż plik z danymi meteorologicznymi (w formacie tekstowym)'),
        tags$hr(),
        downloadButton('downloadData', 'Pobierz przetworzone dane')
        
      ),
      mainPanel(
        #verbatimTextOutput("ncount_2"),
        #verbatimTextOutput("lat"),
        #tableOutput('contents'),
        tableOutput('tabelka'),
        tableOutput('tabelka_rok'),
        #verbatimTextOutput("plot2"),
        plotOutput('plot3'),
        plotOutput('plot4')
        
      )
      
    )
  )
)

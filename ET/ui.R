ui <- fluidPage(
  fluidPage(
    titlePanel("Obliczenia Ewapotranspiracji"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Wskaż plik z danymi w formacie: "nazwapliku.txt"'),
        tags$hr(),
        HTML("<h4>Przed załadowaniem pliku należy zaznaczyć odpowiednią stację!<br></h4>"),
        radioButtons('sep', 'Stacja',
                     c(Gryzyna='Gryzyna',
                       Rozany_Potok='Rozany_Potok')
                     ),
        downloadButton('downloadData', 'Pobierz przetworzone dane')
      ),
      mainPanel(
        verbatimTextOutput("ncount_2"),
        tableOutput('contents'),
        renderPlot('plot2')
      )
      
    )
  )
)
library(shiny)
library(readxl)
library(water)

server <- function(input, output) {
  
  
  
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    


    dane <- read.table(inFile$datapath, skip = 2, na.strings = c("---", "------"))
    colnames(dane) <- c("date","time","t2m","t2max","t2min","rh","dpt","ws","wd","pulsacja","gust","windchill","HI","WindChill","THW","THWI","slp","prec","prec_rate", "rad",
                        "solar_energy", "max_rad","UV","UV1","UVmax","hdd","cdd","in_temp","in_hum","in_dew","in_heat","in_EMC","density","et_davis",
                        "wind_sampling","wind_tx","ISS","ARC")
    dane <- as.data.frame(dane)
    head(dane)
    
    if(input$sep=="Gryzyna") {
      alt <- 68
      lon1 <- 15.28
      lat1 <- 52.19
    } 
    
    if(input$sep=="Rozany_Potok") {
      alt <- 86
      lon1 <- 16.94
      lat1 <- 52.46
    } 

    dane$alt <- alt
    dane$lon <- lon1
    dane$lat <- lat1
    dane$date2 <- as.Date(dane$date, format="%y-%m-%d")
    hh <- as.numeric(unlist(lapply(strsplit(as.character(dane$time), split = ":"), function(x) x[1])))
    minuty <- as.numeric(unlist(lapply(strsplit(as.character(dane$time), split = ":"), function(x) x[2])))
    
    dane$date2 <- as.POSIXct(paste0(dane$date2, dane$time), format="%Y-%m-%d %H:%M")
    
    doy <- as.numeric(strftime(as.Date(dane$date, format="%y-%m-%d"), format="%j"))
    
    WeatherStation1  <- data.frame(wind=dane$ws,RH=dane$rh, temp=dane$t2m,
                                   radiation=dane$rad, height=2, lat=lat1, long=lon1, elev=alt)
    
    dane$ET0_chwilowe <- round(water::hourlyET(WeatherStation1, hours=hh, DOY=doy, long.z=15, ET = "ETo", ET.instantaneous = T)/2,3)
    dane$ET0_na_godz <- round(water::hourlyET(WeatherStation1, hours=hh, DOY=doy, long.z=15, ET = "ETo", ET.instantaneous = T), 3)
    dane$ET0_wys_chwilowe <- round(water::hourlyET(WeatherStation1, hours=hh, DOY=doy, long.z=15, ET = "ETr", ET.instantaneous = T)/2,3)
    dane$ET0_wys_na_godz <- round(water::hourlyET(WeatherStation1, hours=hh, DOY=doy, long.z=15, ET = "ETr", ET.instantaneous = T), 3)
    
    # poprawka dla wartosci w niepelnych godzinach:
    # tj. dla minut roznych od 0 wprowadz hourlyET jako NA
    dane$ET0_na_godz[which(minuty!=0)] <- NA
    dane$ET0_wys_na_godz[which(minuty!=0)] <- NA
    
    
    
    
    return(dane)
    
  })
  
 
  napis <- reactive({
    
    inFile <- input$file1
    if (is.null(input$file1)) {
      c("W tym miejscu pojawi się kontrolnie pierwsze 6-rzędów przetworzonych danych oraz wykres słupkowy...")
    }else{
      return(NULL)
    }
  })
  
  output$text <- renderText(napis())
  
  
  
  output$contents <- renderTable(
    
    head(getData())
    
  )
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "ET.xls", sep = "")
    },
    content = function(file) {
      WriteXLS::WriteXLS(getData(), file)
    })
  
  
  
  
  output$plot2 <- renderText(
    print(str(getData()))
    #plot(dane$ET0_wys_na_godz)
    )
  
  output$plot3 <- renderPlot(
    if (is.null(input$file1)) {
      return(NULL)
    }else{
    plot(getData()[, c("date2","ET0_chwilowe")], type='h', col='blue', xlab='')
    }
    
    #df <- getData()
    #tail(df)
    #plot(df$ET0_chwilowe)
    #plot(dane$ET0_wys_na_godz)
  )
  
    
}
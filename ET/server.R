library(shiny)
library(readxl)
library(water)
library(dplyr)

server <- function(input, output) {
  
  
  
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    


    dane <- read.table(inFile$datapath, skip = 2, na.strings = c("---", "------"), sep="\t")
    colnames(dane) <- c("date","time","t2m","t2max","t2min","rh","dpt","ws","wd","pulsacja","gust","windchill","HI","WindChill","THW","THSW","slp","prec","prec_rate", "rad",
                        "solar_energy", "max_rad","UV","UV1","UVmax","hdd","cdd","in_temp","in_hum","in_dew","in_heat","in_EMC","density","et_davis",
                        "wind_sampling","wind_tx","ISS","ARC")
    dane <- as.data.frame(dane)
    head(dane)
    
    # if(input$sep=="Gryzyna") {
    #   alt <- 68
    #   lon1 <- 15.28
    #   lat1 <- 52.19
    # } 
    # 
    # if(input$sep=="Rozany_Potok") {
    #   alt <- 86
    #   lon1 <- 16.94
    #   lat1 <- 52.46
    # } 
    # 
    alt <- as.numeric(as.character(input$alt))
    lon1 <- as.numeric(as.character(input$lon))
    lat1 <- as.numeric(as.character(input$lat))

    dane$alt <- alt
    dane$lon <- lon1
    dane$lat <- lat1
    dane$dni <- as.Date(dane$date, format="%Y-%m-%d")
    dane$date2 <- as.Date(dane$date, format="%Y-%m-%d")
    dane$miesiac <- format(dane$date2, format="%Y-%m")
    dane$rok <- format(dane$date2, format="%Y")
    
    hh <- as.numeric(unlist(lapply(strsplit(as.character(dane$time), split = ":"), function(x) x[1])))
    minuty <- as.numeric(unlist(lapply(strsplit(as.character(dane$time), split = ":"), function(x) x[2])))
    
    dane$date2 <- as.POSIXct(paste0(dane$date2, dane$time), format="%Y-%m-%d %H:%M")
    
    doy <- as.numeric(strftime(as.Date(dane$date, format="%Y-%m-%d"), format="%j"))
    
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
    
    # obliczenie thsw
    # e = (dane$rh / 100) * 6.105 * exp (17.27 * dane$t2m / (237.7 + dane$t2m))
    # ATthw = dane$t2m + 0.33 * e - 0.70 * dane$ws - 4.00
    # dane$thw = round(ATthw, 2)
    # 
    # ATthsw = dane$t2m + 0.348 * e - 0.70 * dane$ws + 0.70 * dane$rad / (dane$ws + 10) - 4.25;
    # dane$thsw = round(ATthsw, 1);
    
    dane$prec <- ifelse(dane$prec==0.0, NA, dane$prec)
    
    
    
    return(dane)
    
  })
  
 
  napis <- reactive({
    
    inFile <- input$file1
    if (is.null(input$file1)) {
      c("Obliczenia pojawią się po wgraniu pliku" ) 
      }else{
      return(NULL)
    }
  })
  
  output$text <- renderText(napis())
  
  
  
  # output$contents <- renderTable(
  #   if (is.null(input$file1)) {
  #     return(NULL)
  #   }else{
  #    getData() %>% dplyr::select(., date:pulsacja, prec, ET0_chwilowe:ET0_wys_na_godz) %>% head()
  #   }
  # 
  # )
  
  output$tabelka <- renderTable(
    if (is.null(input$file1)) {
      return(NULL)
    }else{
      df <- getData()
      
      dni_z_opadem <- df %>% dplyr::group_by(dni) %>% dplyr::summarise(jestopad=sum(prec, na.rm=T)) 
      dni_z_opadem$binarnie <-ifelse(dni_z_opadem$jestopad>0,1,0) 
      dni_z_opadem$miesiac <-format(dni_z_opadem$dni,"%Y-%m")
      dni_z_opadem2 <- dni_z_opadem %>%  dplyr::group_by(miesiac) %>% dplyr::summarise(liczba_dni_z_opadem=sum(binarnie, na.rm=T)) 
      
      wynik <- df %>% dplyr::group_by(miesiac) %>% 
        dplyr::summarise(ewapotranspiracja=sum(ET0_na_godz, na.rm=T), 
                         opad=sum(prec, na.rm = T),
                         temperatura=mean(t2m, na.rm = T),
                         wiatr=mean(ws, na.rm=T)) %>%
        mutate(bilans_parowania=opad-ewapotranspiracja,
               liczba_dni_z_opadem=sum(ifelse(opad>0,1,0))) %>% 
        dplyr::select(miesiac, ewapotranspiracja, opad, bilans_parowania, temperatura, wiatr) %>% as.data.frame()
      
      wynik <- dplyr::left_join(wynik, dni_z_opadem2)
      
      
      return(wynik)
    }
  )
  
  
  output$tabelka_rok <- renderTable(
    if (is.null(input$file1)) {
      return(NULL)
    }else{
      df <- getData()
      df$rok
      wynik <- df %>% 
        dplyr::summarise(ewapotranspiracja=sum(ET0_na_godz, na.rm=T), 
                         opad=sum(prec, na.rm = T),
                         temperatura=mean(t2m, na.rm = T),
                         wiatr=mean(ws, na.rm=T)) %>%
        
        mutate(bilans_parowania=opad-ewapotranspiracja,
        rok=paste(min(df$rok, na.rm=T),max(df$rok, na.rm=T), sep="-")) %>% 
        dplyr::select(rok, ewapotranspiracja, opad, bilans_parowania, temperatura, wiatr) %>% as.data.frame()
      
      return(wynik)
    }
  )
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "ET.xls", sep = "")
    },
    content = function(file) {
      WriteXLS::WriteXLS(getData(), file)
    })
  
  
  
  
  output$plot2 <-  renderText({ input$alt })
  
  output$plot3 <- renderPlot(
    if (is.null(input$file1)) {
      return(NULL)
    }else{
    plot(getData()[, c("date2","ET0_na_godz")], type='h', col='blue', xlab='', lwd=2, main='ewapotranspiracja potencjalna')
    }
  )
    
    output$plot4 <- renderPlot(
      if (is.null(input$file1)) {
        return(NULL)
      }else{
        plot(getData()[, c("date2","prec")], type='h', col='red', xlab='', main='opady', lwd=2)
      }
    )
    
    

    
    
    
    #df <- getData()
    #tail(df)
    #plot(df$ET0_chwilowe)
    #plot(dane$ET0_wys_na_godz)
  #)
  
    
}
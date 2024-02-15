
library(shiny)
pacman::p_load(shinythemes, markdown, dplyr, readxl, leaflet)

#############################################################
#User Interface
ui<-navbarPage("GEO 1012",theme = shinytheme("cosmo"),
               tabPanel("Visualización básica",tags$head(tags$style(".btn{color: black; padding:2px 2px}")),
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('crucero'),
                            
                            selectInput('estacion',label = 'Estaciones',
                                        choices =c('Nehuentué','Queule','Puerto Saavedra'),
                                        selected = 'Nehuentué',selectize = T,multiple = F),
                            selectInput('location',label = 'Unicación de estación',
                                        choices =c('2 millas','5 millas','10 millas'),
                                        selected = '2 millas',selectize = T,multiple = F),
                            h5('Resumen de datos'),br(),
                            verbatimTextOutput('resumen',placeholder = T),#column(2,textOutput('count')),
                            width = 3),
                          mainPanel(
                                    fluidRow(uiOutput("ref1")),
                            column(8,div(plotOutput(outputId = "profPlot"),
                                         style = "height: 75%;width: auto;",class='img-responsive'),offset = 2)
                            
                          )
                        )
               ), #first tab panel
               tabPanel("Visualización de transectas",
                        
                          sidebarPanel(
                            uiOutput('crucero2'),
                            selectInput('estacion2',label = 'Transectas',
                                        choices =c('Nehuentué','Queule','Puerto Saavedra'),
                                        selected = 'Nehuentué',selectize = T,multiple = F),
                            selectInput('variable',label = 'Variable a visualizar',
                                        choices =c('Temperatura','Salinidad','Densidad'),
                                        selected = 'Temperatura',selectize = T,multiple = F)
                        ),
                        mainPanel(
                          fluidRow(uiOutput("ref2")),
                          (plotOutput(outputId = "transPlot"))
                        )
               ),#second tab panel
               #last panel
               #Description
               tabPanel('Acerca de la página',
                        sidebarLayout(sidebarPanel(  fluidRow(#Credits section
                          h4(strong("Generado en base a:")),
                          h5("Instituto Fomento Pesquero,", a("IFOP.",href="https://www.ifop.cl")), 
                          h5('Paquete ',a(href="https://CRAN.R-project.org/package=oce", "oce (v1.2-0)"), 
                             "oce: Analysis of Oceanographic Data"),hr()),
                          h5(p('Desarrollo: MSc. José A. Lastra')),h5(p('Contacto: jose.lastra@pucv.cl')),
                          h5(p('Colaboración: Elizabeth Daille - Montsserrat Truyol')),br(),HTML("<img style='width: 100%; height: 75%;' src='logo.png' class='img-responsive' />"),
                          h5(strong("Sitio",a("LabGRS",href="http://www.pucv.cl/uuaa/site/edic/base/port/labgrs.html"))),fluid=F,width = 2),
                          mainPanel(h5(strong('Acerca de la aplicación:')),div(style="text-align:justify",p("La",(em("aplicación de visualización de datos oceanográficos")), "permite al estudiante filtrar y visualizar información oceanográfica de la Región de l Araucanía, correspondientes al", strong("Programa de manejo de las Floraciones de algas nocivas y toxinas marinas en el océano Pacífico desde Biobío a Aysén (I Etapa) 2018 de IFOP."))),
                                    h5(strong('Funcionamiento:')),
                                    div(style="text-align:justify",p('La plataforma entrega información de 10 cruceros oceanográficos para tres transectas en los sectores de: Nehuentué, Puerto Saavedra y Queule.'),p('La primera pestaña permite la visualización de cada estación con los respectivos perfiles verticales de temperatura, salinidad y densidad para cada una de las fechas disponibles con un resumen de las estadísticas básicas de la información visualizada. Mientras que la segunda pestaña permite al estudiante seleccionar la transecta, el crucero a visualizar y la variable de interés')),
                                    h5(strong('Uso e interpretación:')),
                                    div(style="text-align:justify",p('Al desplegarse el visualizador el estudiante verá la primera pestaña, correpsondiente a la información por estación, donde podrá realizar filtros por crucero, nombre de estación y posición en la transecta. El panel principal mostrará la ubicación de la estación en un mapa dinámico y los perfiles verticales'),br(),
                                        HTML('<center><img  src="imagen_1.png" style="width: 85%; height: 85%" class="img-responsive"  align="middle" ></center>')),
                                    h6(strong(em('Fig.1 Vista de la pestaña inicial de la aplicación')),align='center'),
                                    br(),
                                    div(style="text-align:justify",p('Al cambiar de pestaña, el estudiante podrá visualizar los filtros disponibles para la fecha de cruceros, la transecta de interés y la variable disponible para visualizar. En el panel principal podrá visualizar la ubicación de las estaciones de la transecta y la variable seleccionada en la parte inferior'),br(),
                                        HTML('<center><img  src="imagen_2.png" style="width: 85%; height: 85%" class="img-responsive"  align="middle" ></center>')),
                                    h6(strong(em('Fig.1 Vista de la pestaña de visualización de transectas')),align='center'),br())
                        )),collapsible = T)


vals <- reactiveValues(count=0)

server<-function(input, output, session) {
  

  #read datasets
  datos<-readRDS(file = "data/crucerosBiobio_2018.rds")
  datos1<-na.omit(datos)
  dates<-as.Date(datos1$fecha)
  datos1$fecha<-dates
  t<-readRDS(file = "data/GMRTv3_7_20200511topo.rds")
  
  ############################################################
  #TAB N°1
  #############################################################
  #render user interface inputs
  seleccion<-head(unique(dates),1)
  output$crucero<-renderUI({
    selectInput(
      "cruc",'Seleccione crucero',
      unique(dates), multiple = F,selected = seleccion )
  })

  #############################################################
  #Filter information for plot
  datos_plot<-function(){

    datos.subset<-datos1 %>% filter(fecha==input$cruc & Transecto==input$location & Estaciones==input$estacion)
    #as ctd data
    dlat<- unique(datos.subset$`Latitud (S)`)
    dlong<- unique(datos.subset$`Longitud (W)`)
    
    dato<-as.ctd(salinity = datos.subset$`Salinidad (UPS)`,temperature = datos.subset$`Temperatura (°C)`,pressure =        datos.subset$`Profundidad (m)`,latitude = dlat,longitude = dlong)
#return 
    dato
    }
  
#plot creation

    output$profPlot <- renderPlot({
      req(input$cruc)
      par(mar=c(1,1,1,1))
      plot(datos_plot(),which=c(10,9,11,1), colCoastline="lightgray",type='l',ytype='depth')
    },height = 900,res=120)
  
 
  #summary statistics
  output$resumen <- renderPrint({
    req(input$cruc)
    datos.subset2<-datos1 %>% filter(fecha==input$cruc & Transecto==input$location & Estaciones==input$estacion)
    summary(datos.subset2[,c(10,12,13)])
  })
  
  
  #reference map
  output$ref1<-renderUI({
    leafletOutput("map1") #creates leaflet output for data display
  })
  
  #Basic map
  map=leaflet()  %>%addProviderTiles("Esri.WorldImagery") %>% addProviderTiles("CartoDB.PositronOnlyLabels")
  
  #Proxy leaflet maps for reactive data
  #Render basic map
  output$map1<-renderLeaflet({map})#renders map for display in shiny
  
  observe({
    req(input$cruc)
    datos.subset<-datos1 %>% filter(fecha==input$cruc & Transecto==input$location & Estaciones==input$estacion)
    #as ctd data
    dlat<- unique(datos.subset$`Latitud (S)`)
    dlong<- unique(datos.subset$`Longitud (W)`)
    dt<-as.data.frame(cbind(dlat,dlong))
    
    leafletProxy('map1')%>% setView(lng = dt$dlong,lat = dt$dlat,zoom = 12) %>% clearMarkers()%>%addMarkers(lng = dt$dlong,lat = dt$dlat)
  })
  ##########################################################################
  #Tab panel 2
  seleccion<-head(unique(dates),1)
  output$crucero2<-renderUI({
    selectInput(
      "cruc2",'Seleccione crucero',
      unique(dates), multiple = F,selected = seleccion )
  })
  
  trans_plot<-function(){
    
    datos.subset<-datos1 %>% filter(fecha == input$cruc2 & Estaciones == input$estacion2)
    #as ctd data
    dlat<- unique(datos.subset$`Latitud (S)`)
    dlong<- unique(datos.subset$`Longitud (W)`)
    
    seccion<-as.section(datos.subset$`Salinidad (UPS)`,datos.subset$`Temperatura (°C)`,
                        datos.subset$`Profundidad (m)`,datos.subset$`Longitud (W)`,
                        datos.subset$`Latitud (S)`,datos.subset$Código)
    #return 
    seccion
  }
  
  #section plot creation
  
  output$transPlot <- renderPlot({
    req(input$cruc2)
    req(input$variable)
    par(mar=c(1,1,1,1))
    data <- switch(input$variable,#makes reactive dataset using ui interaction
                   "Temperatura" = "temperature",
                   "Salinidad" = "salinity",
                   "Densidad" = "sigmaTheta")
    plot(sectionGrid(trans_plot(),trim = T,p=standardDepths(5)), ylim=c(0,70),ztype="image",which=data,
         showBottom=t,xtype='longitude',ytpye='depth',showStations=T,showStart=T)
  },width = 'auto',height = 600,res = 120)
  
  
  #reference map
  output$ref2<-renderUI({
    leafletOutput("map2") #creates leaflet output for data display
  })
  
  #Basic map
  map.1=leaflet()  %>%addProviderTiles("Esri.WorldImagery") %>% addProviderTiles("CartoDB.PositronOnlyLabels")
  
  #Proxy leaflet maps for reactive data
  #Render basic map
  output$map2<-renderLeaflet({map.1})#renders map for display in shiny
  
  observe({
    req(input$cruc2)
    datos.subset<-datos1 %>% filter(fecha == input$cruc2 & Estaciones == input$estacion2)
    #as ctd data
    dlat<- unique(datos.subset$`Latitud (S)`)
    dlong<- unique(datos.subset$`Longitud (W)`)
    dt<-as.data.frame(cbind(dlat,dlong))
    
    leafletProxy('map2')%>% setView(lng = dt$dlong[2],lat = dt$dlat[2],zoom = 12) %>% clearMarkers()%>%addMarkers(lng = dt$dlong,lat = dt$dlat)
  })
  
  ###########################################################################
  #count visits
  
  #      would run, leading to an infinite loop.
  isolate(vals$count <- vals$count + 1)
  
  # When a session ends, decrement the counter.
  session$onSessionEnded(function(){
    # We use isolate() here for the same reasons as above.
    isolate(vals$count <- vals$count - 1)
  })
  
  # Reactively update the client.
  output$count <- renderText({
    vals$count
  })
}

#########
#Deploy app
shinyApp(ui, server)

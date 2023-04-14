#load packages
pacman::p_load(shiny, leaflet, tidyverse, sf, tmap, DT, tidyverse, sfdep, readxl, plyr, Kendall, plotly, dplyr,stringdist, spdep)

#read data - Combined rds files
IPC_combined_districtlevel <- read_rds("data/rds/IPC_combined_India_districtlevel.rds")
IPC_combined_India_districtlevel <- st_as_sf(IPC_combined_districtlevel)
IPC_combined_India_districtlevel <- st_make_valid(IPC_combined_India_districtlevel)
IPC_combined_India_districtlevel <- select(IPC_combined_India_districtlevel, -c(statename, State, District))

SLL_combined_districtlevel <- read_rds("data/rds/SLL_combined_India_districtlevel.rds")
SLL_combined_India_districtlevel <- st_as_sf(SLL_combined_districtlevel)
SLL_combined_India_districtlevel <- st_make_valid(SLL_combined_India_districtlevel)
SLL_combined_India_districtlevel <- select(SLL_combined_India_districtlevel, -c(statename, State, District))


ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Overview",
             mainPanel(
               h2("Overview"),
               p(style="text-align:justify;", "India is a land of rich culture and breathtaking natural beauty, with a history dating back thousands of years. However, alongside this diversity lies a wide range of crime rates observed across different states and districts. From crimes against women to property and violent crimes, India's crime statistics provide a complex picture of this beautiful country.
                 Our research would focus on conducting a geospatial analysis of crime patterns in India by identifying the crime rates in the different districts of India. It will help us understand the spatial distribution and trends of crime in different regions of the country. By analysing the types of crimes on a district level, we can identify areas where crime rates are high and low, and explore factors that may contribute to these patterns.
                "),
               
               h2("Dataset"),
               tabsetPanel(
                 id = 'dataset',
                 tabPanel("Indian Penal Code (IPC)", DT::dataTableOutput("IPC")),
                 tabPanel("Special and Local Laws (SLL)", DT::dataTableOutput("SLL"))
               )
             )
             ),
    
    
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel(
                 #select files
                 selectInput(inputId = "filename",
                             label = "Select crimes under",
                             choices = list("Indian Penal Code" = "IPC",
                                            "State and Local Laws" = "SLL"),
                             selected = "IPC"),
                 #choose crimes 
                 selectInput(inputId = "crimes_chosen",
                             label = "Select crimes",
                             choices = NULL),
                 #classification method
                 selectInput(inputId = "classification",
                             label = "Classification method:",
                             choices = list("fixed" = "fixed", 
                                            "sd" = "sd", 
                                            "equal" = "equal", 
                                            "pretty" = "pretty", 
                                            "quantile" = "quantile", 
                                            "kmeans" = "kmeans", 
                                            "hclust" = "hclust", 
                                            "bclust" = "bclust", 
                                            "fisher" = "fisher", 
                                            "jenks" = "jenks"),
                             selected = "pretty"),
                 #number of classes
                 sliderInput(inputId = "classes", 
                             label = "Number of classes",
                             min = 6,
                             max = 12,
                             value = c(8)),
                 #colour scheme
                 selectInput(inputId = "colour",
                             label = "Colour scheme:",
                             choices = list("blues" = "Blues", 
                                            "reds" = "Reds", 
                                            "greens" = "Greens",
                                            "Yellow-Orange-Red" = "YlOrRd",
                                            "Yellow-Orange-Brown" = "YlOrBr",
                                            "Yellow-Green" = "YlGn",
                                            "Orange-Red" = "OrRd"),
                             selected = "YlOrRd")
               ),
               mainPanel(
                 tmapOutput("mapPlot",
                            width = "100%",
                            height = 400)
               )
             )
             ),
    
    tabPanel("Global Spatial Autocorrelation",
             sidebarLayout(
               sidebarPanel(
                 #select files
                 selectInput(inputId = "filename1",
                             label = "Select crimes under",
                             choices = list("Indian Penal Code" = "IPC",
                                            "State and Local Laws" = "SLL"),
                             selected = "IPC"),
                 #choose crimes 
                 selectInput(inputId = "crimes_chosen1",
                             label = "Select crimes",
                             choices = NULL),
                 #classification method
                 selectInput(inputId = "autocorrelation",
                             label = "Spatial Autocorrelation Test:",
                             choices = list("Moran's L Test" = "moran", 
                                            "Geary's C Test" = "geary"),
                             selected = "Moran's L Test"),
               ),
               mainPanel(
                 plotOutput("spatialmaps"),
                 plotOutput("correlogram")
               )
             )
             
             ),
    tabPanel("Moran's & LISA Cluster Maps",
             sidebarLayout(
               sidebarPanel(
                 #select files
                 selectInput(inputId = "filename2",
                             label = "Select crimes under",
                             choices = list("Indian Penal Code" = "IPC",
                                            "State and Local Laws" = "SLL"),
                             selected = "IPC"),
                 #choose crimes 
                 selectInput(inputId = "crimes_chosen2",
                             label = "Select crimes",
                             choices = NULL),
                 #classification method
                 selectInput(inputId = "maps",
                             label = "Visualising Maps:",
                             choices = list("Local Moran's values" = "moran", 
                                            "Lisa Cluster Map" = "lisa"),
                             selected = "Local Moran's values"),
                 conditionalPanel(
                   condition = "input.maps == 'moran'",
                   selectInput(inputId = "moran_type", 
                               label = "Select Variable", 
                               choices = list("Local Moran's L Statistics" = "li",
                                 "Expectation" = "E.li",
                                 "Variance" = "Var.li",
                                 "Standard Deviation" = "Z.li",
                                 "P-value" = "Pr()"))
                 )
               ),
               mainPanel(
                 plotOutput("moran_lisa"),
               )
             )
             
             ),
    
    tabPanel("Hot&Cold Spot Analysis")
     
  ),
  
  
)


server <- function(input, output, session) {
  
  ###################################################################################
  # TabPanel1 - OVERVIEW
  # Display Dataset
  output$IPC= DT::renderDataTable({IPC_combined_districtlevel})
  output$SLL= DT::renderDataTable({SLL_combined_districtlevel})
  
  
  ###################################################################################
  # TabPanel2 - EDA
  filename <- reactive({
    filename = input$filename
  })

  observeEvent(filename(), {
    crimes_chosen <- colnames(eval(parse(text = paste0(input$filename,"_combined_India_districtlevel"))))
    updateSelectInput(inputId = "crimes_chosen", choices = crimes_chosen)
  })
  
  crimes_chosen <- reactive({
    req(input$crimes_chosen)
  })
  
  output$mapPlot <- renderTmap({
    sf_use_s2(FALSE)
    tmap_options(check.and.fix = TRUE) + 
    tm_shape(eval(parse(text = paste0(input$filename,"_combined_India_districtlevel")))) + 
      tm_fill(col = input$crimes_chosen,
              n = input$classes,
              style = input$classification,
              palette = input$colour) + 
      tm_borders(lwd = 0.1, alpha = 1) #+ 
      #tm_view(set.zoom.limits = c(11,16))
  })
  
  ###################################################################################
  # TabPanel3 - Global Spatial Autocorrelation
  filename1 <- reactive({
    filename1 = input$filename1
  })
  
  observeEvent(filename1(), {
    crimes_chosen1 <- colnames(eval(parse(text = paste0(input$filename1,"_combined_India_districtlevel"))))
    updateSelectInput(inputId = "crimes_chosen1", choices = crimes_chosen1)
  })
  
  crimes_chosen1 <- reactive({
    req(input$crimes_chosen1)
  })
  
  output$spatialmaps <- renderPlot({
    file_name <- paste0(input$filename1, "_combined_India_districtlevel.rds")
    folder_path <- paste0("data/rds/", file_name)
    autocorr_filename <- read_rds(folder_path)
    autocorr_filename <- st_as_sf(autocorr_filename)
    autocorr_filename <- st_cast(autocorr_filename, "MULTIPOLYGON")
    autocorr_filename <- st_make_valid(autocorr_filename)
   
    data_values = autocorr_filename[[input$crimes_chosen1]]
    
    #Computing Contiguity Spatial Weights
    wm <- poly2nb(autocorr_filename, queen=TRUE)
    set.ZeroPolicyOption(TRUE)
    rswm <- nb2listw(wm, style="B", zero.policy = TRUE)
    
    if(input$autocorrelation == "moran"){
      # Moran's L Test
      moran.test(data_values,listw=rswm,zero.policy = TRUE,na.action=na.omit)
      set.seed(1234)
      bperm = moran.mc(data_values,listw=rswm, nsim=999, zero.policy = TRUE, na.action=na.omit)
      
      hist(bperm$res, 
           freq=TRUE, 
           breaks=20, 
           xlab="Simulated Moran's I")
      abline(v=0, 
             col="red") 
      
      output$correlogram <- renderPlot({
        MI_corr <- sp.correlogram(wm, 
                                  data_values, 
                                  order=6, 
                                  method="I", 
                                  style="W",
                                  zero.policy = TRUE)
        plot(MI_corr)
      })
      
    } else {
      # Geary's C Test
      geary.test(data_values, listw=rswm)
      set.seed(1234)
      bperm=geary.mc(data_values, listw=rswm, nsim=999)
      hist(bperm$res, freq=TRUE, breaks=20, xlab="Simulated Geary c")
      abline(v=1, col="red") 
      
      output$correlogram <- renderPlot({
        GC_corr <- sp.correlogram(wm, 
                                  data_values, 
                                  order=6, 
                                  method="C", 
                                  style="W",
                                  zero.policy = TRUE)
        plot(GC_corr)
      })
    }
  })
  
  ###################################################################################
  # TabPanel4 - Global Spatial Autocorrelation
  filename2 <- reactive({
    filename2 = input$filename2
  })
  
  observeEvent(filename2(), {
    crimes_chosen2 <- colnames(eval(parse(text = paste0(input$filename2,"_combined_India_districtlevel"))))
    updateSelectInput(inputId = "crimes_chosen2", choices = crimes_chosen2)
  })
  
  crimes_chosen2 <- reactive({
    req(input$crimes_chosen2)
  })
  
  output$moran_lisa <- renderPlot({
    file_name <- paste0(input$filename2, "_combined_India_districtlevel.rds")
    folder_path <- paste0("data/rds/", file_name)
    autocorr_filename <- read_rds(folder_path)
    autocorr_filename <- st_as_sf(autocorr_filename)
    autocorr_filename <- st_cast(autocorr_filename, "MULTIPOLYGON")
    autocorr_filename <- st_make_valid(autocorr_filename)
    
    data_values = autocorr_filename[[input$crimes_chosen2]]
    
    #Computing Contiguity Spatial Weights
    wm <- poly2nb(autocorr_filename, queen=TRUE)
    set.ZeroPolicyOption(TRUE)
    rswm <- nb2listw(wm, style="B", zero.policy = TRUE)
    
    if(input$moran_type == "li"){
      graph_name = "Local Moran's L Statistics"
    } else if (input$moran_type == "E.li") { 
      graph_name = "Expectation of Local Moran Statistic uner Randomisation Hypothesis"
    } else if (input$moran_type == "Var.li") { 
      graph_name = "Variance of Local Moran Statistic uner Randomisation Hypothesis"
    } else if (input$moran_type == "Z.li") { 
      graph_name = "Standard Deviate of Local Moran Statistic"
    } else {
      graph_name = "P-value of Local Moran Statistc"
    }
    
    if(input$maps == "moran"){
      fips <- order(autocorr_filename$District)
      localMI <- localmoran(data_values, rswm)
      data.localMI <- cbind(autocorr_filename,localMI) %>% rename(Pr.Ii = Pr.z....E.Ii..)
      localMI.map <- tm_shape(data.localMI) +
        tm_fill(col = input$moran_type, 
                style = "pretty", 
                title = graph_name) +
        tm_borders(alpha = 0.5) +
        tmap_options(check.and.fix = TRUE)
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
  
  
  
  
  
  
  


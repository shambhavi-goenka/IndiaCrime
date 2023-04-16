#load packages
pacman::p_load(shiny, tidyverse, sf, tmap, DT, tidyverse, sfdep, readxl, plyr, Kendall, plotly, dplyr,stringdist, spdep)

#read data - Combined rds files
IPC_combined_districtlevel <- read_rds("data/rds/IPC_combined_India_districtlevel.rds")
IPC_combined_India_districtlevel <- st_as_sf(IPC_combined_districtlevel)
IPC_districtlevel <-  st_cast(IPC_combined_India_districtlevel, "MULTIPOLYGON")
IPC_districtlevel <- st_make_valid(IPC_districtlevel)
IPC_combined_India_districtlevel <- st_make_valid(IPC_combined_India_districtlevel)
IPC_combined_India_districtlevel <- select(IPC_combined_India_districtlevel, -c(statename, State, District))

SLL_combined_districtlevel <- read_rds("data/rds/SLL_combined_India_districtlevel.rds")
SLL_combined_India_districtlevel <- st_as_sf(SLL_combined_districtlevel)
SLL_districtlevel <-  st_cast(SLL_combined_India_districtlevel, "MULTIPOLYGON")
SLL_districtlevel <- st_make_valid(SLL_districtlevel)
SLL_combined_India_districtlevel <- st_make_valid(SLL_combined_India_districtlevel)
SLL_combined_India_districtlevel <- select(SLL_combined_India_districtlevel, -c(statename, State, District))

#Computing Contiguity Spatial Weights
set.ZeroPolicyOption(TRUE)
wm_IPC <- poly2nb(IPC_districtlevel, queen=TRUE)
rswm_IPC <- nb2listw(wm_IPC, style="B", zero.policy = TRUE)
wm_SLL <- poly2nb(SLL_districtlevel, queen=TRUE)
rswm_SLL <- nb2listw(wm_SLL, style="B", zero.policy = TRUE)





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
                            height = 400),
                 br(),
                 h3("Overview of Choropleth Analysis in India"),
                 br(),
                 p(style="text-align:justify;", "Choropleth analysis is a spatial data visualization technique that uses color shading or patterns to represent a variable of interest on a map, such as population density or crime rates. The purpose of choropleth analysis is to provide an easy-to-understand visual representation of geographic data and to identify patterns and trends that may not be immediately apparent from raw data."),
                 br(),
                 p(style="text-align:justify;", "Choropleth analysis of crime patterns in India at the district level provides a visual representation of the spatial distribution and trends of crimes in different regions of the country. By analyzing the types of crimes on a district level, we can identify areas where crime rates are high and low, and explore factors that may contribute to these patterns. This analysis can help policymakers and law enforcement agencies to better understand the patterns of crime in different regions and take measures to prevent and reduce crime rates. It can also help citizens to become more aware of the crime rates in their regions and take necessary precautions. Overall, the choropleth analysis of crimes in India at the district level can help in identifying high-risk areas and design targeted interventions to reduce crime rates and improve public safety.")
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
                             selected = "moran"),
               ),
               mainPanel(
                 plotOutput("spatialmaps"),
                 plotOutput("correlogram"),
                 br(),
                 h3("Overview of Global Spatial Autocorrelation Anlaysis of Crimes in India"),
                 br(),
                 p(style="text-align:justify;", "The purpose of global spatial autocorrelation is to identify the presence of spatial patterns in a dataset. It helps to understand whether there is clustering or dispersion of values in space, and whether the observed pattern is the result of chance or some underlying process."),
                 br(),
                 p(style="text-align:justify;", "Global spatial autocorrelation helps in identifying areas where similar values of a variable are clustered together and areas where values are dissimilar. This information can be used to identify spatial trends and patterns in the data and to explore potential underlying factors that may be contributing to these patterns. It can also be used to identify areas where certain variables are over-represented or under-represented, which can be useful for planning and decision-making."),
                 br(""),
                 p(style="text-align:justify;", "In the context of crimes in India at the district level, our analysis can help to identify whether there is clustering or dispersion of crime rates across different districts. By understanding the degree and nature of spatial dependence in the crime data, we can gain insights into the underlying factors that may be driving these patterns. For instance, high levels of spatial autocorrelation may suggest that there are certain regional factors or cultural practices that are contributing to higher crime rates in certain areas. On the other hand, low levels of spatial autocorrelation may indicate that crime rates are more random or sporadic, and may require different strategies for prevention and control. Overall, the analysis of global spatial autocorrelation can provide a useful tool for policymakers and law enforcement agencies to better understand the distribution and dynamics of crime in India, and develop more targeted and effective interventions.")
               )
             )
             
    ),
    tabPanel("Cluster&Outlier Analysis & LISA Cluster Maps",
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
                             selected = "lisa"),
                 conditionalPanel(
                   condition = "input.maps == 'moran'",
                   selectInput(inputId = "moran_type", 
                               label = "Select Variable", 
                               choices = list("Local Moran's L Statistics" = "li",
                                              "Expectation" = "E.li",
                                              "Variance" = "Var.li",
                                              "Standard Deviation" = "Z.li",
                                              "P-value" = "Pr()"))
                 ),
                 conditionalPanel(
                   condition = "input.maps == 'lisa'",
                   selectInput(inputId = "lisa_type", 
                               label = "Select Type of Map", 
                               choices = list("Moran Scatterplot" = "m_scp",
                                              "Moran Scatterplot with Standardized Variable" = "m_scp_sv",
                                              "Lisa Map Classes" = "lisa_map"))
                 )
               ),
               mainPanel(
                 plotOutput("moran_lisa"),
                 br(),
                 h3("Overview of Cluster and Outlier Anlaysis on Crimes in India"),
                 p(style="text-align:justify;", "Cluster and outlier analysis, also known as Local Moran's L, is a spatial statistics technique that is used to identify spatial clusters of high or low values (hotspots and coldspots) of a particular variable of interest. The purpose of cluster and outlier analysis is to identify areas in a geographic region where the values of a particular variable are statistically different from what is expected by chance."),
                 br(),
                 p(style="text-align:justify;", "By conducting cluster and outlier analysis of crimes in India at the district level, we aim to identify spatial clusters or hotspots of high and low crime rates. This analysis helps to identify areas where crime rates are significantly higher or lower than the average rates in neighboring areas. The local Moran's L statistic is used to detect clusters of high or low crime rates that are spatially autocorrelated. By identifying these clusters, policymakers and law enforcement agencies can better target their efforts towards areas with the highest crime rates, improve resource allocation, and develop more effective crime prevention strategies."),
                 br(),
                 br(),
                 h3("Overview of Lisa Cluster Map on Crimes in India"),
                 br(),
                 p(style="text-align:justify;", "A Local Indicators of Spatial Association (LISA) cluster map is a tool used in spatial data analysis to visualize the degree of clustering in a dataset. It helps identify spatial clusters of high or low values of a particular variable of interest and can be useful in identifying patterns and trends in the data. In the context of crime analysis, a LISA cluster map can help identify areas where high or low rates of crime are clustered together, which can provide insights into the underlying factors contributing to crime patterns."),
                 br(),
                 p(style="text-align:justify;", "By creating LISA cluster maps of crimes in India at the district level, we can identify regions with high or low crime rates and detect spatial clusters of these regions. This information can be used to focus resources and interventions in areas with high crime rates or identify factors contributing to lower crime rates in specific areas, which can inform future policy decisions. Additionally, LISA cluster maps can help identify spatial spillovers of crime between neighboring regions, which can inform the design of crime prevention and control strategies.")
               )
             )
             
    ),
    
    tabPanel("Hot&Cold Spot Analysis",
             sidebarLayout(
               sidebarPanel(
                 #select files
                 selectInput(inputId = "filename3",
                             label = "Select crimes under",
                             choices = list("Indian Penal Code" = "IPC",
                                            "State and Local Laws" = "SLL"),
                             selected = "IPC"),
                 #choose crimes 
                 selectInput(inputId = "crimes_chosen3",
                             label = "Select crimes",
                             choices = NULL),
                 #classification method
                 selectInput(inputId = "distance_matrix",
                             label = "Distance Weight Matrix:",
                             choices = list("Fixed Distance Weight Matrix" = "fixed", 
                                            "Adaptive Distance Weight Matrix" = "adaptive"),
                             selected = "fixed")
               ),
               mainPanel(
                 plotOutput("hcmaps"),
                 br(),
                 h3("Overview of Hot and Cold Spot Analysis of Crimes in India"),
                 br(),
                 p(style="text-align:justify;", "The purpose of hot and cold spot analysis is to identify spatial patterns of high and low crime density in a particular area. This analysis helps to identify areas with high crime rates, which can help in the deployment of resources and law enforcement efforts to tackle crime effectively. It can also highlight areas with low crime rates, which can be used as a benchmark for improving public safety in other areas. Our analysis can help identify areas with high or low crime rates, and help policymakers and law enforcement agencies to develop targeted interventions to address crime and improve public safety.")
               )
             )
             )
    
  )
  
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
    if(input$filename1 == "IPC"){
      autocorr_filename <- IPC_districtlevel
      wm <- wm_IPC
      rswm <- rswm_IPC
    } else {
      autocorr_filename <- SLL_districtlevel
      wm <- wm_SLL
      rswm <- rswm_SLL
    }
    data_values = autocorr_filename[[input$crimes_chosen1]]
    
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
    if(input$filename2 == "IPC"){
      autocorr_filename <- IPC_districtlevel
      wm <- wm_IPC
      rswm <- rswm_IPC
    } else {
      autocorr_filename <- SLL_districtlevel
      wm <- wm_SLL
      rswm <- rswm_SLL
    }
    
    data_values = autocorr_filename[[input$crimes_chosen2]]
    fips <- order(autocorr_filename$District)
    localMI <- localmoran(data_values, rswm)
    data.localMI <- cbind(autocorr_filename,localMI) %>% rename(Pr.Ii = Pr.z....E.Ii..)
    
    if(input$maps == "moran"){
      
      # Getting the Graph Name and Type of Graph
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
      localMI.map <- tm_shape(data.localMI) +
        tm_fill(col = input$moran_type, 
                style = "pretty", 
                title = graph_name) +
        tm_borders(alpha = 0.5) +
        tmap_options(check.and.fix = TRUE)
      
    } else {
      if(input$lisa_type == "m_scp"){
        y_axis <- paste("Spatially Lag ", input$crimes_chosen2 ," 2021")
        x_axis <- paste(input$crimes_chosen2, " 2021")
        nci <- moran.plot(data_values, rswm,
                              labels=as.character(autocorr_filename$District), 
                              xlab=x_axis,
                              ylab=y_axis)
      } else if (input$lisa_type == "m_scp_sv") { 
        y_axis <- paste("Spatially Lag z-", input$crimes_chosen2 ," 2021")
        x_axis <- paste("z-", input$crimes_chosen2, " 2021")
        autocorr_filename$Z.input$crimes_chosen <- scale(data_values) %>% 
          as.vector 
        nci2 <- moran.plot(autocorr_filename$Z.input$crimes_chosen, rswm,
                               labels=as.character(autocorr_filename$District),
                               xlab=x_axis, 
                               ylab=y_axis)
      } else { 
        quadrant <- vector(mode="numeric",length=nrow(localMI))
        autocorr_filename$lag <- lag.listw(rswm, data_values)
        DV <- autocorr_filename$lag - mean(autocorr_filename$lag)     
        LM_I <- localMI[,1] - mean(localMI[,1])   
        signif <- 0.05     
        quadrant[DV <0 & LM_I>0] <- 1
        quadrant[DV >0 & LM_I<0] <- 2
        quadrant[DV <0 & LM_I<0] <- 3  
        quadrant[DV >0 & LM_I>0] <- 4  
        quadrant[localMI[,5]>signif] <- 0
        
        variable <- qtm(autocorr_filename, input$crimes_chosen2)
        
        data.localMI$quadrant <- quadrant
        colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
        clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
        
        LISAmap <- tm_shape(data.localMI) +
          tm_fill(col = "quadrant", 
                  style = "cat", 
                  palette = colors[c(sort(unique(quadrant)))+1], 
                  labels = clusters[c(sort(unique(quadrant)))+1],
                  popup.vars = c("")) +
          tm_view(set.zoom.limits = c(11,17)) +
          tm_borders(alpha=0.5)
        
        tmap_arrange(variable, LISAmap, 
                     asp=1, ncol=2)
      }
    }
    
  })
  
  
  
  ###################################################################################
  # TabPanel5 - Hot and Cold Spot Analysis
  filename3 <- reactive({
    filename3 = input$filename3
  })
  
  observeEvent(filename3(), {
    crimes_chosen3 <- colnames(eval(parse(text = paste0(input$filename3,"_combined_India_districtlevel"))))
    updateSelectInput(inputId = "crimes_chosen3", choices = crimes_chosen3)
  })
  
  crimes_chosen3 <- reactive({
    req(input$crimes_chosen3)
  })
  
  output$hcmaps <- renderPlot({
    if(input$filename3 == "IPC"){
      autocorr_filename <- IPC_districtlevel
      #wm <- wm_IPC
      #rswm <- rswm_IPC
    } else {
      autocorr_filename <- SLL_districtlevel
      #wm <- wm_SLL
      #rswm <- rswm_SLL
    }
    
    data_values = autocorr_filename[[input$crimes_chosen3]]
    
    longitude <- map_dbl(autocorr_filename$geometry, ~st_centroid(.x)[[1]])
    latitude <- map_dbl(autocorr_filename$geometry, ~st_centroid(.x)[[2]])
    coords <- cbind(longitude, latitude)
    
    k1 <- knn2nb(knearneigh(coords))
    k1dists <- unlist(nbdists(k1, coords, longlat = TRUE))
    
    if(input$distance_matrix == "fixed"){
      wm_d62 <- dnearneigh(coords, 0, 62, longlat = TRUE)
      wm62_lw <- nb2listw(wm_d62, style = 'B')
      fips <- order(autocorr_filename$District)
      gi.fixed <- localG(data_values, wm62_lw)
      autocorr_filename.gi <- cbind(autocorr_filename, as.matrix(gi.fixed)) %>%
        rename(gstat_fixed = as.matrix.gi.fixed.)
      variable1 <- qtm(autocorr_filename, input$crimes_chosen3)
      
      Gimap <-tm_shape(autocorr_filename.gi) +
        tm_fill(col = "gstat_fixed", 
                style = "pretty",
                palette="-RdBu",
                title = "local Gi") +
        tm_borders(alpha = 0.5)
      
      tmap_arrange(variable1, Gimap, asp=1, ncol=2)
    } else {
      knn <- knn2nb(knearneigh(coords, k=6))
      knn_lw <- nb2listw(knn, style = 'B')
      fips <- order(data_values)
      gi.adaptive <- localG(data_values, knn_lw)
      autocorr_filename.gi1 <- cbind(autocorr_filename, as.matrix(gi.adaptive)) %>%
        rename(gstat_adaptive = as.matrix.gi.adaptive.)
      variable3 <- qtm(autocorr_filename, input$crimes_chosen3)
      
      Gimap <- tm_shape(autocorr_filename.gi1) + 
        tm_fill(col = "gstat_adaptive", 
                style = "pretty", 
                palette="-RdBu", 
                title = "local Gi") + 
        tm_borders(alpha = 0.5)
      
      tmap_arrange(variable3, 
                   Gimap, 
                   asp=1, 
                   ncol=2)
    }
    
    
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
  
  
  
  
  
  
  


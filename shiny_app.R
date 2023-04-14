#load packages
pacman::p_load(shiny, tidyverse, sf, tmap, DT)

#read data - Kelly's combined rds files
IPC_combined_India_districtlevel <- read_rds("data/rds/IPC_combined_India_districtlevel.rds")
IPC_combined_India_districtlevel <- st_as_sf(IPC_combined_India_districtlevel)
IPC_combined_India_districtlevel <- st_make_valid(IPC_combined_India_districtlevel)
IPC_combined_India_districtlevel <- select(IPC_combined_India_districtlevel, -c(statename, State, District))

SLL_combined_India_districtlevel <- read_rds("data/rds/SLL_combined_India_districtlevel.rds")
SLL_combined_India_districtlevel <- st_as_sf(SLL_combined_India_districtlevel)
SLL_combined_India_districtlevel <- st_make_valid(SLL_combined_India_districtlevel)
SLL_combined_India_districtlevel <- select(SLL_combined_India_districtlevel, -c(statename, State, District))


ui <- fluidPage(
  
  titlePanel("Choropleth Mapping"),
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
)


server <- function(input, output, session) {
  
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
  
}


# Run the application 
shinyApp(ui = ui, server = server)
  
  
  
  
  
  
  


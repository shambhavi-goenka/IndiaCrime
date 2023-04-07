#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
pacman::p_load(sf, tmap, kableExtra, tidyverse, sfdep, readxl, plyr, Kendall, plotly, dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Load the data files
  IPC <- read_xlsx("data/1 Districtwise IPC Crimes_2021.xlsx")
  SLL <- read_xlsx("data/2 Districtwise SLL Crimes_2021.xlsx")
  
  # We remove the first 3 rows as they are just headings and have NA values. We will not be using them anyways.
  IPC <- IPC %>% slice(5:n())
  SLL <- SLL %>% slice(5:n())
  
  # Convert to numeric 
  df <- IPC %>% select(-c("...1", "...2")) %>% mutate_if(is.character,as.numeric)
  df <- SLL %>% select(-c("...1", "...2")) %>% mutate_if(is.character,as.numeric)
  
  # Group data to reduce cardinality
  temp_df <- df |> rowwise() |>
    summarise(murders = sum(`Murder (Sec.302 IPC)`,`Dowry Deaths (Sec.304-B IPC)`),
              attempted_muders = sum(`Culpable Homicide not amounting to Murder (Sec.304 IPC)`,`Abetment of Suicide (Sec.305/306 IPC)`, `Attempt to Commit Murder (Sec.307 IPC)`, `Attempt to commit Culpable Homicide (Sec.308 IPC)`, `Attempt to Commit Suicide (Sec.309 IPC)`, `Miscarriage, Infanticide, Foeticide and Abandonment (Sec.313 to 318 IPC) \r\n`),
              death_negligence = sum(`Causing Death by Negligence`,`...6`,`...7`,`...8`,`...9`,`...10`,`...11`,`...12`),
              hurt = sum(`Hurt`,`Wrongful Restraint/Confinement (Sec.341 to 348 IPC)`),
              women_assault = `Assault on Women with Intent to Outrage her Modesty`,
              kidnapping_abduction = `Kidnapping and Abduction`,
              human_exploitation = sum(`Human Trafficking (U/S 370)`, `Exploitation of Trafficked Person 370A IPC)`, `Selling of Minors for Prostitution (Sec.372 IPC)`, `Buying of Minors for Prostitution (Sec.373 IPC)`, `Rape (Sec.376 IPC)`, `Attempt to Commit Rape (Sec.376/511 IPC)`, `Unnatural Offences (Sec.377 IPC)`),
              state_offence = `...64`,
              public_tranquility = `Offences against Public Tranquillity (Total)`,
              movable_property = `Offences against Property (Total)`,
              document_fraud = `Offences Relating to Documents & Property Marks (Total)`,
              miscellaneous = sum(`Miscellaneous IPC Crimes(Total)`, `...143`))
  
  #join into df
  df <- cbind(IPC$"...1", IPC$"...2", temp_df) %>% rename("State" = "IPC$...1", "District" = "IPC$...2")
  
  # Group data to reduce cardinality
  temp_df <- df |> rowwise() |>
    summarise(women = `...7`,
              children = `...14`,
              st_sc = `...19`,
              state = `...23`,
              arms_related = `...31`,
              information_infringement = `...35`,
              trade_finance = `...42`,
              narcotics = `...48`,
              environment = `...56`,
              foreigner = `...61`,
              railway = `...64`,
              media = `...69`,
              food = `...73`,
              others = sum(`...91`, `Other SLL Crimes`))
  
  #Join into df
  df <- cbind(SLL$"...1", SLL$"...2", temp_df) %>% rename("State" = "SLL$...1","District" = "SLL$...2")
}

# Run the application 
shinyApp(ui = ui, server = server)

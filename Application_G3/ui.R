#    https://shiny.posit.co/

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Carte des AOP Françaises"), 
  
  tabsetPanel(
    tabPanel("Accueil",
             titlePanel("Accueil"),
             
             
             imageOutput("isara_logo"),
             
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
              ) # fermeture sidebarLayout
    ),# fermeture tabPanel1
    
    tabPanel("Carte AOP",
             titlePanel("Carte Intéractive des AOP"),
             leafletOutput("map")
    ), 
    
    tabPanel("About",
             titlePanel("L'Equipe"),
             fluidRow(column(width = 4),
                      column(width = 2, offset = 3)),
             fluidRow(column(width = 12)),
             checkboxGroupInput("variable", "Variables to show:",
                                c("Cylinders" = "cyl",
                                  "Transmission" = "am",
                                  "Gears" = "gear"))
    )
  )# fermeture tabsetPanel

)# fermeture Fluidpage
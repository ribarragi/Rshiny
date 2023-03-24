#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readr)
library(ggplot2)
library(dplyr)
library(shiny)
library(lubridate)
library(treemapify)
library(shinyWidgets)
library(DT)

data <- read_csv("CrimesChicago_2022.csv")
data$Date <- parse_date_time(data$Date, "%m/%d/%y %I:%M:%S %p")
data$month <- format(as.Date(data$Date), "%m")
data$Month <- month.abb[as.integer(data$month)]
data$Hour <- hour(data$Date)
data$Wday <- wday(data$Date, label = TRUE)

data$`Location Description` <- dplyr::case_when(data$`Location Description` %in% c("AIRCRAFT", "BOAT / WATERCRAFT", "COIN OPERATED MACHINE", "OTHER (SPECIFY)", "VESTIBULE", "FARM") ~ "OTHER",
                                                data$`Location Description` %in% c("AIRPORT BUILDING NON-TERMINAL - NON-SECURE AREA", "AIRPORT BUILDING NON-TERMINAL - SECURE AREA", "AIRPORT EXTERIOR - NON-SECURE AREA", 
                                                                                   "AIRPORT EXTERIOR - SECURE AREA", "AIRPORT PARKING LOT", "AIRPORT TERMINAL LOWER LEVEL - NON-SECURE AREA", 
                                                                                   "AIRPORT TERMINAL LOWER LEVEL - SECURE AREA", "AIRPORT TERMINAL MEZZANINE - NON-SECURE AREA", "AIRPORT TERMINAL UPPER LEVEL - NON-SECURE AREA", 
                                                                                   "AIRPORT TERMINAL UPPER LEVEL - SECURE AREA", "AIRPORT TRANSPORTATION SYSTEM (ATS)", "AIRPORT VENDING ESTABLISHMENT", 
                                                                                   "AIRPORT/AIRCRAFT", "FEDERAL BUILDING", "FIRE STATION", "POLICE FACILITY / VEHICLE PARKING LOT", "GOVERNMENT BUILDING / PROPERTY", 
                                                                                   "POLICE FACILITY", "JAIL / LOCK-UP FACILITY", "LIBRARY") ~ 'GOVERNMENT FACILITY',
                                                data$`Location Description` %in% c("ALLEY", "BRIDGE", "CEMETARY", "PARK PROPERTY", "FOREST PRESERVE", "LAKEFRONT / WATERFRONT / RIVERBANK", 
                                                                                   "RIVER BANK", "SIDEWALK", "STAIRWELL", "STREET", "HIGHWAY / EXPRESSWAY") ~ 'OPEN PUBLIC SPACE',
                                                data$`Location Description` %in% c("CHA PARKING LOT", "CHA PARKING LOT / GROUNDS", "DRIVEWAY", "DRIVEWAY - RESIDENTIAL", "PARKING LOT", 
                                                                                   "PARKING LOT / GARAGE (NON RESIDENTIAL)", "GARAGE", "PORCH") ~ 'OPEN PRIVATE SPACE',
                                                data$`Location Description` %in% c("ANIMAL HOSPITAL", "HOSPITAL BUILDING / GROUNDS", "MEDICAL / DENTAL OFFICE", "NURSING / RETIREMENT HOME") ~ "MEDICAL FACILITY", 
                                                data$`Location Description` %in% c("ABANDONED BUILDING", "CONSTRUCTION SITE", "VACANT LOT", "VACANT LOT / LAND") ~ "VACANT LOT/SPACE", 
                                                data$`Location Description` %in% c("APARTMENT", "CHA APARTMENT", "CHA HALLWAY / STAIRWELL / ELEVATOR", "CHA LOBBY", "RESIDENCE", "HOUSE", "RESIDENCE - GARAGE", 
                                                                                   "RESIDENCE - PORCH / HALLWAY", "RESIDENCE - YARD (FRONT / BACK)", "HALLWAY" ) ~ "RESIDENCE", 
                                                data$`Location Description` %in% c("ATM (AUTOMATIC TELLER MACHINE)", "BANK", "CREDIT UNION", "CURRENCY EXCHANGE", "SAVINGS AND LOAN") ~ "BANK",
                                                data$`Location Description` %in% c("AUTO", "TAXICAB", "TRUCK", "VEHICLE - COMMERCIAL", "VEHICLE - COMMERCIAL: ENTERTAINMENT / PARTY BUS", 
                                                                                   "VEHICLE - COMMERCIAL: TROLLEY BUS", "VEHICLE - DELIVERY TRUCK", "VEHICLE - OTHER RIDE SHARE SERVICE (LYFT, UBER, ETC.)", 
                                                                                   "VEHICLE NON-COMMERCIAL") ~ "VEHICLE",
                                                data$`Location Description` %in% c("COLLEGE / UNIVERSITY - GROUNDS", "COLLEGE / UNIVERSITY - RESIDENCE HALL", "DAY CARE CENTER", "PUBLIC GRAMMAR SCHOOL", 
                                                                                   "SCHOOL - PRIVATE BUILDING", "SCHOOL - PRIVATE GROUNDS", "SCHOOL - PUBLIC BUILDING", "SCHOOL - PUBLIC GROUNDS") ~ "SCHOOL",
                                                data$`Location Description` %in% c("CHURCH / SYNAGOGUE / PLACE OF WORSHIP") ~ "CHURCH",
                                                data$`Location Description` %in% c("FACTORY / MANUFACTURING BUILDING", "WAREHOUSE", "YARD") ~ "INDUSTRIAL SPACE",
                                                data$`Location Description` %in% c("SPORTS ARENA / STADIUM", "MOVIE HOUSE / THEATER") ~ "VENUE", 
                                                data$`Location Description` %in% c('CTA "L" TRAIN', "CTA BUS", "CTA BUS STOP", "CTA PARKING LOT / GARAGE / OTHER PROPERTY", "CTA PLATFORM", "CTA PROPERTY ", "CTA STATION", 
                                                                                   "CTA TRACKS - RIGHT OF WAY", "CTA TRAIN", "OTHER RAILROAD PROPERTY / TRAIN DEPOT", "RAILROAD PROPERTY", "OTHER COMMERCIAL TRANSPORTATION") ~ "PUBLIC TRANSIT", 
                                                data$`Location Description` %in% c("APPLIANCE STORE", "ATHLETIC CLUB", "AUTO / BOAT / RV DEALERSHIP" , "BANQUET HALL", "BAR OR TAVERN", "BARBER SHOP/BEAUTY SALON", 
                                                                                   "BARBERSHOP", "BOWLING ALLEY", "CAR WASH", "CLEANING STORE", "COMMERCIAL / BUSINESS OFFICE", "CONVENIENCE STORE", 
                                                                                   "DEPARTMENT STORE", "DRUG STORE", "GAS STATION", "GROCERY FOOD STORE", "PAWN SHOP", "POOL ROOM", "LIQUOR STORE", "HOTEL", 
                                                                                   "HOTEL / MOTEL", "RESTAURANT", "RETAIL STORE", "TAVERN", "TAVERN / LIQUOR STORE", "SMALL RETAIL STORE") ~ "COMMERCIAL PROPERTY")

data$IsDomestic <- ifelse(data$Domestic == TRUE, "Domestic", "Non-domestic")
vars_show_table = c("ID", "Date", "Primary Type", "Location Description", "Description", "Domestic", "Arrest")

# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Application title -----------------------------------------------
  titlePanel("Crimes in Chicago, 2022"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select domestic / non domestic crime
      checkboxGroupInput(inputId = "domestic_check",
                         label = "Include Domestic Violence Crimes",
                         choices = c("Domestic", "Non-domestic"),
                         selected = "Non-domestic"),
      
      # Pick crime location
      pickerInput(inputId = "location_picker",
                  label = "Location", 
                  choices=c("COMMERCIAL PROPERTY", "OPEN PUBLIC SPACE", "RESIDENCE", "OPEN PRIVATE SPACE", "SCHOOL",
                            "PUBLIC TRANSIT", "VEHICLE", "MEDICAL FACILITY","GOVERNMENT FACILITY", "BANK",
                            "VACANT LOT/SPACE", "CHURCH", "VENUE", "INDUSTRIAL SPACE" , "OTHER"), 
                  options = list(`actions-box` = TRUE), 
                  multiple = T, 
                  selected = c("MEDICAL FACILITY", "BANK")),
      
      # Show data table
      checkboxInput(inputId = "show_table",
                    label = "Show data table",
                    value = TRUE),
      
      # download button
      downloadLink("downloadData",
                   "Download")
      
    ),
    
    # Output --------------------------------------------------------
    mainPanel(
      # Create tabs to display the three graphs, one per tab
      tabsetPanel(
        tabPanel(title = "Bar Chart", plotOutput(outputId = "bar_chart")),
        tabPanel(title = "Tree Map", plotOutput(outputId = "tree_map")),
        tabPanel(title = "Heat Map", plotOutput(outputId = "heat_map"))
      ),
      
      #Show tableat bottom of the tabsPanel
      DT::dataTableOutput(outputId = "crimes_table")
    )
  )
)

# Define server function required to create the bar chart ---------
server <- function(input, output, session) {
  
  #observe({
  #  print(input$location_picker)
  #})
  
  # Create subset of the data --
  data_subset_domestic <- reactive({
    req(input$domestic_check, input$location_picker)
    filter(data, IsDomestic %in% input$domestic_check & `Location Description` %in% input$location_picker)
  })
  
  # Create graph
  output$bar_chart <- renderPlot({
    ggplot(data = data_subset_domestic(), aes(x = reorder(Month, as.integer(month)))) +
      geom_bar(fill = "#66194d") + xlab("Month") + ylab("Count of crimes") +
      ggtitle("CRIMES PER MONTH") +
      theme(axis.text = element_text(size = 14), 
            axis.title = element_text(size = 16, face = "bold"), 
            panel.background = element_rect(fill = "white"), 
            axis.ticks = element_blank(), 
            plot.title = element_text(size=18, face="bold"))
    
  })
  
  # Create a second graph
  output$tree_map <- renderPlot({
    ggplot(count(data_subset_domestic(), `Primary Type`), aes(area = n, label = `Primary Type` , fill = n)) +
      geom_treemap() +
      geom_treemap_text(fontface = "bold", colour = "white", place = "center", 
                        reflow = TRUE, min.size = 3) +
      scale_fill_gradient(low = "#f5d6eb", high = "#66194d", name = "Crime count") + 
      ggtitle("TYPES OF CRIMES") +
      theme(plot.title = element_text(size=18, face="bold"), 
            legend.key.height = unit(1.2, 'cm'), 
            legend.key.width = unit(1.2, 'cm'), 
            legend.title = element_text(size = 14), 
            legend.text = element_text(size = 14))
  })
  
  
  # Create third graph: heatmap
  output$heat_map <- renderPlot({
    ggplot(count(data_subset_domestic(), Hour, Wday), aes(Hour, Wday)) +
      geom_tile(aes(fill = n), colour = "white", na.rm = TRUE)  +
      ggtitle("CRIMES BY DAY AND TIME") +
      scale_fill_gradient(low = "#f5d6eb", high = "#66194d", name = "Crime count") + 
      scale_x_continuous(breaks=seq(0,23,1)) +
      theme(axis.text = element_text(size = 14), 
            axis.text.y=element_text(margin=margin(r = -35)),
            axis.title = element_text(size = 16, face = "bold"), 
            panel.background = element_rect(fill = "white"), 
            axis.ticks = element_blank(), 
            plot.title = element_text(size=18, face="bold"), 
            legend.key.height = unit(1.2, 'cm'), 
            legend.key.width = unit(1.2, 'cm'), 
            legend.title = element_text(size = 14), 
            legend.text = element_text(size = 14))
  })
  
  
  # Print data table if checked -------------------------------------
  output$crimes_table <- DT::renderDataTable(
    if(input$show_table){
      DT::datatable(data = data_subset_domestic()[, vars_show_table], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_subset_domestic", Sys.Date(), ".csv", sep="")
    },
    content = function(con) {
      write.csv(data, con)
    })
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

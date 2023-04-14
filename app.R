library(readxl)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

crashes <- read_excel("C:/Users/Will Wood/Downloads/crashes.xlsx")
crashes<- crashes %>% 
    drop_na()
crashes <- crashes[complete.cases(crashes), ]
crashes$prty_type <- factor(crashes$prty_type)
crashes$milt_time <- as.POSIXct(crashes$milt_time, format = "%Y-%m-%d %H:%M:%S")

#Cleaning data for page
crashes$crsh_type_group <- factor(
    case_when(
        crashes$crsh_type_cd == 1 ~ "Single Motor Vehicle",
        crashes$crsh_type_cd %in% c(2, 3) ~ "Head-On",
        crashes$crsh_type_cd == 4 ~ "Angle",
        crashes$crsh_type_cd %in% c(5, 6, 7) ~ "Rear-End",
        crashes$crsh_type_cd %in% c(8, 9) ~ "Sideswipe",
        crashes$crsh_type_cd == 10 ~ "Other/Unknown"
    )
)
crashes$hospitalization <- factor(
    ifelse(crashes$injy_svty_cd %in% c(1, 2, 3), "Hospitalized", "Not hospitalized")
)
wthr_levels <- c("Clear", "Cloudy", "Fog/Smoke", "Rain", "Snow/Blowing Snow", "Severe Wind", "Sleet/Hail", "Other/Unknown")

# Convert wthr_cd to a factor with new levels
crashes$wthr_cd <- factor(crashes$wthr_cd, levels = 1:8, labels = wthr_levels)

# Create new column with string values
crashes <- crashes %>% 
    mutate(lighting = case_when(
        lit_cd == 1 ~ "Daylight",
        lit_cd == 2 ~ "Dawn",
        lit_cd == 3 ~ "Dusk",
        lit_cd == 4 ~ "Dark-Lighted",
        lit_cd == 5 ~ "Dark-Unlighted",
        lit_cd == 6 ~ "Other Unknown",
        is.na(lit_cd) ~ "Not Entered",
        TRUE ~ "Unknown"
    ))



crashes <- crashes %>% 
    mutate(road_condition = case_when(
        rd_cond_cd == 1 ~ "Dry",
        rd_cond_cd == 2 ~ "Wet",
        rd_cond_cd == 3 ~ "Icy",
        rd_cond_cd == 4 ~ "Snowy",
        rd_cond_cd == 5 ~ "Muddy",
        rd_cond_cd == 6 ~ "Slushy",
        rd_cond_cd == 7 ~ "Debris",
        rd_cond_cd == 8 ~ "Other/Unknown",
        TRUE ~ "Unknown"
    ))



crashes <- crashes %>% 
    mutate(mdot_region = case_when(
        mdot_regn_cd == 0 ~ "Statewide Multi-Region",
        mdot_regn_cd == 1 ~ "Superior",
        mdot_regn_cd == 2 ~ "North",
        mdot_regn_cd == 3 ~ "Grand",
        mdot_regn_cd == 4 ~ "Bay",
        mdot_regn_cd == 5 ~ "Southwest",
        mdot_regn_cd == 6 ~ "University",
        mdot_regn_cd == 7 ~ "Metro",
        TRUE ~ "Unknown"
    ))





crashes <- crashes %>% 
    mutate(vehc_defect = case_when(
        vehc_dfct_cd == 1 ~ "Brakes",
        vehc_dfct_cd == 2 ~ "Lights/reflectors",
        vehc_dfct_cd == 3 ~ "Steering",
        vehc_dfct_cd == 4 ~ "Tires/Wheels",
        vehc_dfct_cd == 5 ~ "Windows",
        vehc_dfct_cd == 6 ~ "Other",
        is.na(vehc_dfct_cd) ~ "Not Entered",
        TRUE ~ "Unknown"
    ))





crashes <- crashes %>% 
    mutate(drug_susp = ifelse(drug_susp_ind == 1, "Yes", "No"),
           alch_susp = ifelse(alch_susp_ind == 1, "Yes", "No"))







# Create a new variable for crash type group
crashes$crsh_type_group <- factor(
    case_when(
        crashes$crsh_type_cd == 1 ~ "Single Motor Vehicle",
        crashes$crsh_type_cd %in% c(2, 3) ~ "Head-On",
        crashes$crsh_type_cd == 4 ~ "Angle",
        crashes$crsh_type_cd %in% c(5, 6, 7) ~ "Rear-End",
        crashes$crsh_type_cd %in% c(8, 9) ~ "Sideswipe",
        crashes$crsh_type_cd == 10 ~ "Other/Unknown"
    )
)

# Define the UI
ui <- fluidPage(
    titlePanel("Crash Data Explorer"),
    sidebarLayout(
        sidebarPanel(
            selectInput("roadCond", "Road Condition:", choices = c("Dry", "Wet", "Icy", "Snowy", "Muddy", "Slushy", "Debris", "Other/Unknown")),
            sliderInput("ageRange", "Age Range:", min = 0, max = 100, value = c(0, 100)),
            checkboxInput("alcohol", "Alcohol Involved?"),
            checkboxInput("drugs", "Drugs Involved?"),
            sliderInput("speedLimit", "Speed Limit:", min = 0, max = 100, value = c(0, 100))
        ),
        mainPanel(
            plotOutput("genderPlot"),
            plotOutput("yearPlot"),
            plotOutput("crashTypePlot")
        )
    )
)

# Define the server
server <- function(input, output) {
    
    # Filter the data based on user inputs
    filteredData <- reactive({
        crashes %>%
            filter(road_condition == input$roadCond,
                   prty_age >= input$ageRange[1] & prty_age <= input$ageRange[2],
                   alch_susp_ind == input$alcohol,
                   drug_susp_ind == input$drugs,
                   spd_limt >= input$speedLimit[1] & spd_limt <= input$speedLimit[2])
    })
    
    # Create a plot of the number of crashes by gender
    output$genderPlot <- renderPlot({
        ggplot(filteredData(), aes(x = gndr_cd)) +
            geom_bar() +
            labs(title = "Number of Crashes by Gender", x = "Gender", y = "Count")
    })
    
    # Create a plot of the number of crashes by year
    output$yearPlot <- renderPlot({
        ggplot(filteredData(), aes(x = year)) +
            geom_bar() +
            labs(title = "Number of Crashes by Year", x = "Year", y = "Count")
    })
    
    # Create a plot of the number of crashes by crash type
    output$crashTypePlot <- renderPlot({
        ggplot(filteredData(), aes(x = crsh_type_group)) +
            geom_bar() +
            labs(title = "Number of Crashes by Crash Type", x = "Crash Type", y = "Count")
    })
    
}

# Run the app
shinyApp(ui = ui, server = server)

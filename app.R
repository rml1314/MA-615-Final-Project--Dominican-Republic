library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(maps)
library(WDI)
library(ggplot2)
library(plotly)
library(fresh)
library(rsconnect)

# Color theme for my shiny application
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#005bb5",  # Darker blue for header and navbar
    aqua = "#003d80",        # Update "info" color
    blue = "#0073e6"         # Update "primary" color
  ),
  adminlte_sidebar(
    dark_bg = "#003d80",     # Deep blue for sidebar
    dark_hover_bg = "#0073e6",  # Lighter blue for hover effect
    dark_color = "#f0f8ff"   # Text color for sidebar items
  ),
  adminlte_global(
    content_bg = "#f0f8ff"   # Light bluish gray for body
  )
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dominican Republic"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General Description", tabName = "general", icon = icon("globe")),
      menuItem("Key Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Comparative Analysis", tabName = "comparative", icon = icon("chart-line")),
      menuItem("SWOT Analysis", tabName = "swot", icon = icon("th")),
      menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    tabItems(
      # General Description Tab
      tabItem(
        tabName = "general",
        fluidPage(
          tabsetPanel(
            # Map of the Dominican Republic
            tabPanel(
              title = "Map of Dominican Republic",
              fluidRow(
                column(12,
                       box(
                         title = "Map of Dominican Republic", status = "primary", solidHeader = TRUE,
                         width = NULL,
                         leafletOutput("mapDominican", height = "500px")  # Interactive map using Leaflet
                       )),
                column(12,
                       box(
                         title = "Facts and Values", status = "info", solidHeader = TRUE,
                         width = NULL,
                         DT::dataTableOutput("factsTable")  # DataTable for facts
                       ))
              )
            ),
            # Global Location
            tabPanel(
              title = "Global Location",
              fluidRow(
                column(12,
                       box(
                         title = "Global Location", status = "primary", solidHeader = TRUE,
                         width = NULL,
                         plotOutput("globalMap", height = "500px"),  # Placeholder for global map
                         p("The Dominican Republic is located in the Caribbean, occupying the eastern two-thirds of the island of Hispaniola, shared with Haiti.")
                       ))
              )
            ),
            # Key Facts
            tabPanel(
              title = "Key Facts",
              fluidPage(
                h2("Key Facts About the Dominican Republic"),
                selectInput("fact_category", "Select a Category:",
                            choices = c("Government", "Economy", "People", "Natural Environment", "History")),
                
                # Output for the text description
                textOutput("fact_description"),
                
                # Output for the image
                uiOutput("fact_image")  # Use uiOutput to render an image
              )
            ),
            # Narrative Description
            tabPanel(
              title = "Narrative Description",
              fluidRow(
                column(12,
                       box(
                         title = "Narrative Description", status = "info", solidHeader = TRUE,
                         width = NULL,
                         p("The Dominican Republic is a captivating nation known for its rich history, breathtaking landscapes, and vibrant culture."),
                         p("With a storied past as the site of the first permanent European settlement in the Americas, the Dominican Republic offers a unique blend of colonial heritage and modern-day attractions."),
                         p("Its economy is fueled by tourism, agriculture, and industry, making it one of the Caribbean's most dynamic nations."),
                         p("Whether exploring the bustling streets of Santo Domingo or the serene beaches of Punta Cana, the Dominican Republic is a destination that leaves an indelible mark on its visitors.")
                       ))
              )
            )
          )
        )
      ),
      
      # Key Demographics Tab
      tabItem(
        tabName = "demographics",
        fluidPage(
          fluidRow(
            column(4,
                   selectInput("x_var", "Select X-axis Variable:", 
                               choices = c("Population" = "population", 
                                           "GDP" = "GDP", 
                                           "Tourism" = "tourism", 
                                           "Year" = "year"),
                               selected = "population")
            ),
            column(4,
                   selectInput("y_var", "Select Y-axis Variable:", 
                               choices = c("Population" = "population", 
                                           "GDP" = "GDP", 
                                           "Tourism" = "tourism", 
                                           "Year" = "year"),
                               selected = "tourism")
            )
          ),
          fluidRow(
            column(12,
                   box(
                     title = "Dynamic Plot", status = "primary", solidHeader = TRUE,
                     width = NULL,
                     plotOutput("dynamicPlot")  # Placeholder for dynamic plot
                   ))
          )
        )
      ),
      
      # Comparative Analysis Tab
      tabItem(
        tabName = "comparative",
        fluidPage(
          h2("Comparative Analysis of Dominican Republic with Other Caribbean Islands"),
          fluidRow(
            column(12,
                   box(
                     title = "Dynamic Comparative Plot", status = "primary", solidHeader = TRUE,
                     width = NULL,
                     fluidRow(
                       column(6,
                              selectInput(
                                inputId = "xAxisVar",
                                label = "Select X-axis Variable:",
                                choices = c("Year" = "year", 
                                            "Population (in Millions)" = "population", 
                                            "GDP (in Billions USD)" = "gdp", 
                                            "Tourism (in Millions)" = "tourists"),
                                selected = "year"
                              )
                       ),
                       column(6,
                              selectInput(
                                inputId = "yAxisVar",
                                label = "Select Y-axis Variable:",
                                choices = c("Population (in Millions)" = "population", 
                                            "GDP (in Billions USD)" = "gdp", 
                                            "Tourism (in Millions)" = "tourists"),
                                selected = "population"
                              )
                       )
                     ),
                     plotOutput("DynamicPlot")  # Dynamic Comparative Plot
                   )
            )
          )
        )
      ),
      
      # SWOT Analysis Tab
      tabItem(
        tabName = "swot",
        fluidPage(
          h2("SWOT Analysis of the Dominican Republic"),
          fluidRow(
            column(6,
                   box(
                     title = "Strengths", status = "success", solidHeader = TRUE,
                     width = NULL,
                     p("The Dominican Republic has a robust tourism sector, a diverse natural landscape, and is a leading exporter of products like tobacco, coffee, and sugar."),
                     p("Its strategic location in the Caribbean facilitates trade, and it benefits from strong foreign investment.")
                   )
            ),
            column(6,
                   box(
                     title = "Weaknesses", status = "warning", solidHeader = TRUE,
                     width = NULL,
                     p("The country faces challenges with income inequality, high unemployment rates, and corruption."),
                     p("Additionally, there is a dependency on remittances and susceptibility to natural disasters such as hurricanes.")
                   )
            )
          ),
          fluidRow(
            column(6,
                   box(
                     title = "Opportunities", status = "info", solidHeader = TRUE,
                     width = NULL,
                     p("Growing opportunities exist in renewable energy development, expansion of the technology sector, and strengthening regional trade agreements."),
                     p("Improving infrastructure and education could also enhance economic potential.")
                   )
            ),
            column(6,
                   box(
                     title = "Threats", status = "danger", solidHeader = TRUE,
                     width = NULL,
                     p("The Dominican Republic is vulnerable to climate change, including rising sea levels and severe weather conditions."),
                     p("Economic dependency on tourism and exports leaves it exposed to global economic fluctuations.")
                   )
            )
          )
        )
      ),
      
      # References Tab
      tabItem(
        tabName = "references",
        fluidPage(
          h2("References"),
          fluidRow(
            column(12,
                   box(
                     title = "Key Facts Image Reference", status = "info", solidHeader = TRUE,
                     width = NULL,
                     tags$ul(
                       tags$li(tags$a("Image source for Key Facts: Government", href = "https://encrypted-tbn2.gstatic.com/images?q=tbn:ANd9GcTL6sBDS3B_FC6oclaGT-2kV1iA0leET1q2XAhFgzvDKwhFhLie", target = "_blank")),
                       tags$li(tags$a("Image source for Key Facts: Economy", href = "https://www.google.com/imgres?q=dominican%20republic%20economy&imgurl=https%3A%2F%2Fupload.wikimedia.org%2Fwikipedia%2Fcommons%2Fe%2Fe3%2FSantoDomingoedit.JPG&imgrefurl=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FEconomy_of_the_Dominican_Republic&docid=yciL9Oq5THjl_M&tbnid=dlBOFVcPm2D1KM&vet=12ahUKEwjW5sH_r6GKAxV7MlkFHaIjJYIQM3oECF0QAA..i&w=2359&h=1325&hcb=2&ved=2ahUKEwjW5sH_r6GKAxV7MlkFHaIjJYIQM3oECF0QAA", target = "_blank")),
                       tags$li(tags$a("Image source for Key Facts: People", href = "https://www.google.com/imgres?q=dominican%20republic%20people&imgurl=https%3A%2F%2Fupload.wikimedia.org%2Fwikipedia%2Fcommons%2Fthumb%2F0%2F06%2FDominican_Republic_People.JPG%2F300px-Dominican_Republic_People.JPG&imgrefurl=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FDominicans&docid=phd7j0kaRavQKM&tbnid=svCT_YZkKYqP4M&vet=12ahUKEwiav4KmsKGKAxVbMVkFHUsWNGoQM3oECB0QAA..i&w=300&h=225&hcb=2&ved=2ahUKEwiav4KmsKGKAxVbMVkFHUsWNGoQM3oECB0QAA", target = "_blank")),
                       tags$li(tags$a("Image source for Key Facts: Natural Environment", href = "https://www.google.com/imgres?q=dominican%20republic%20natural%20environment&imgurl=https%3A%2F%2Fsimplydominican.com%2Fwp-content%2Fuploads%2F2024%2F07%2FDiscover-the-national-parks-and-natural-wonders-of-the-Dominican-Republic.-275966484.jpg&imgrefurl=https%3A%2F%2Fsimplydominican.com%2Fdiscover-national-parks-natural-wonders-dominican-republic%2F&docid=Y447fw3Vvfvp3M&tbnid=BwE702EIkgnO0M&vet=12ahUKEwiz__2xsKGKAxUxFFkFHfi4Ei0QM3oECEkQAA..i&w=1344&h=768&hcb=2&ved=2ahUKEwiz__2xsKGKAxUxFFkFHfi4Ei0QM3oECEkQAA", target = "_blank")),
                       tags$li(tags$a("Image source for Key Facts: History", href = "https://www.google.com/imgres?q=dominican%20republic%20natural%20history&imgurl=https%3A%2F%2Fupload.wikimedia.org%2Fwikipedia%2Fcommons%2F4%2F43%2FDesangles_Colon_engrillado.jpg&imgrefurl=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FHistory_of_the_Dominican_Republic&docid=27II5zZ--50ftM&tbnid=uAFvRHC7mBo-5M&vet=12ahUKEwjUj9jCsKGKAxVRFFkFHc-WA-AQM3oECHkQAA..i&w=708&h=532&hcb=2&ved=2ahUKEwjUj9jCsKGKAxVRFFkFHc-WA-AQM3oECHkQAA", target = "_blank"))
                     )
                   )
            ),
            column(12,
                   box(
                     title = "SWOT Analysis References", status = "info", solidHeader = TRUE,
                     width = NULL,
                     p("SWOT Analysis references:"),
                     tags$ul(
                       tags$li(tags$a("Strengths", href = "https://www.worldbank.org/en/country/dominicanrepublic/overview", target = "_blank")),
                       tags$li(tags$a("Weaknesses", href = "https://www.undrr.org/", target = "_blank")),
                       tags$li(tags$a("Opportunities", href = "https://www.irena.org/", target = "_blank")),
                       tags$li(tags$a("Threats", href = "https://unfccc.int/", target = "_blank"))
                     )
                   )
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
# General Description Tab    
    # Map of Dominican Republic
    output$mapDominican <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -70.1627, lat = 18.7357, zoom = 7) %>%
        addMarkers(lng = -70.1627, lat = 18.7357, popup = "Dominican Republic")
    })
    
    # Facts and Values Table
    output$factsTable <- DT::renderDataTable({
      facts_data <- data.frame(
        Fact = c("Capital", "Population (2024)", "GDP (2024)", "Main Language"),
        Value = c("Santo Domingo", "10.8 million", "293.37 billion USD", "Spanish")
      )
      DT::datatable(facts_data, options = list(dom = 't', paging = FALSE, searching = FALSE))
    })
    
    # Global Location Map
    output$globalMap <- renderPlot({
      world <- map_data("world")
      ggplot(world) +
        geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
        geom_point(aes(x = -70.1627, y = 18.7357), color = "blue", size = 3) +
        labs(title = "Global Location of Dominican Republic") +
        theme_minimal(base_size = 15)
    })
    
    # Reactive expression to get description and image based on selected category
    fact_info <- reactive({
      switch(input$fact_category,
             "Government" = list(
               description = "The Dominican Republic is a representative democracy with a president serving as head of state and government.",
               image = "government.jpg"),
             "Economy" = list(
               description = "The economy is diverse, with key sectors including tourism, agriculture, mining, and manufacturing.",
               image = "economy.jpg"),
             "People" = list(
               description = "The Dominican Republic is home to approximately 11 million people, known for their vibrant culture, music, and sports.",
               image = "people.jpg"),
             "Natural Environment" = list(
               description = "It features tropical rainforests, mountains, and beautiful beaches, with a climate characterized as tropical marine.",
               image = "natural_environment.jpg"),
             "History" = list(
               description = "The island was the site of Christopher Columbus's first landing in the New World in 1492 and has a rich colonial history.",
               image = "history.jpg")
      )
    })
    
    # Render the text description
    output$fact_description <- renderText({
      fact_info()$description
    })
    
    # Render the image
    output$fact_image <- renderUI({
      img(src = fact_info()$image, height = "300px", width = "auto")  # Adjust dimensions as necessary
    })

# Key Demographics Tab
  # With Projection
    generate_projection_plot <- function(indicator, title, y_label, conversion_factor = 1, y_conversion_label = "") {
      # Fetch data
      data <- WDI(country = "DOM", indicator = indicator, start = 1990, end = 2024)
      
      # Rename indicator column for consistency
      colnames(data)[which(colnames(data) == indicator)] <- "value"
      
      # Fit a linear regression model
      model <- lm(value ~ year, data = data)
      
      # Create a new data frame with future years (2024-2030)
      future_years <- data.frame(year = 2024:2030)
      
      # Predict future values using the linear model
      future_values <- predict(model, newdata = future_years)
      
      # Combine actual and projected data
      projected_data <- data.frame(
        year = c(data$year, future_years$year),
        value = c(data$value, future_values),
        type = c(rep("Actual", length(data$year)), rep("Projected", length(future_years$year)))
      )
      
      # Apply conversion factor
      projected_data$value <- projected_data$value / conversion_factor
      
      # Create and return the plot
      renderPlot({
        ggplot(projected_data, aes(x = year, y = value, color = type)) +
          geom_line() +
          geom_point() +
          labs(
            title = title,
            x = "Year", 
            y = paste0(y_label, ifelse(y_conversion_label != "", paste0(" (", y_conversion_label, ")"), ""))
          ) +
          scale_x_continuous(breaks = seq(min(projected_data$year), max(projected_data$year), by = 2)) +
          scale_color_manual(values = c("Actual" = "blue", "Projected" = "red")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    }
    
    # Population plot
    output$populationPlot <- generate_projection_plot(
      indicator = "SP.POP.TOTL",
      title = "Actual Population (1990-2024) vs. Projected Population (2024-2030) of the Dominican Republic",
      y_label = "Population",
      conversion_factor = 1e6,
      y_conversion_label = "Millions"
    )
    
    # GDP plot
    output$GDPPlot <- generate_projection_plot(
      indicator = "NY.GDP.MKTP.CD",
      title = "Actual GDP (1990-2024) vs. Projected GDP (2024-2030) of the Dominican Republic",
      y_label = "GDP",
      conversion_factor = 1e9,
      y_conversion_label = "Billion current USD"
    )
    
    # Tourism plot
    output$TourismPlot <- generate_projection_plot(
      indicator = "ST.INT.ARVL",
      title = "Actual Tourism (1990-2024) vs. Projected Tourism (2024-2030) of the Dominican Republic",
      y_label = "Tourism",
      conversion_factor = 1e6,
      y_conversion_label = "Millions"
    )
  
  # Without Projection but with dynamic plot  
    # Load data for all indicators
    data_population <- WDI(country = "DOM", indicator = "SP.POP.TOTL", start = 1990, end = 2024)
    data_gdp <- WDI(country = "DOM", indicator = "NY.GDP.MKTP.CD", start = 1990, end = 2024)
    data_tourism <- WDI(country = "DOM", indicator = "ST.INT.ARVL", start = 1990, end = 2024)
    
    # Combine data into a single data frame
    combined_data <- data.frame(
      year = data_population$year,
      population = data_population$SP.POP.TOTL / 1e6,  # Convert to millions
      GDP = data_gdp$NY.GDP.MKTP.CD / 1e9,            # Convert to billions
      tourism = data_tourism$ST.INT.ARVL / 1e6        # Convert to millions
    )
    
    # Reactive plot based on user inputs
    output$dynamicPlot <- renderPlot({
      x_var <- input$x_var
      y_var <- input$y_var
      
      ggplot(combined_data, aes_string(x = x_var, y = y_var)) +
        geom_line() +
        geom_point() +
        labs(
          title = paste("Relationship Between", x_var, "and", y_var),
          x = x_var,
          y = y_var
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

# Comparative Analysis Tab    
    # Define the list of Caribbean countries with ISO-2 codes
    countries_iso2 <- c("DO", "CU", "HT", "JM", "PR", "TT", "BB")
    
    # Set the indicators we want to pull from the World Bank
    indicators <- c("NY.GDP.MKTP.CD",   # GDP (current USD)
                    "SP.POP.TOTL",      # Population total
                    "ST.INT.ARVL")      # International tourism (arrivals)
    
    # Fetch the data for the selected countries from 2010 to 2024
    caribbean_data <- WDI(country = countries_iso2, indicator = indicators, start = 2010, end = 2024)
    
    # Rename columns for better readability
    caribbean_data <- caribbean_data %>%
      rename(
        country = country,
        year = year,
        gdp = NY.GDP.MKTP.CD,
        population = SP.POP.TOTL,
        tourists = ST.INT.ARVL
      ) %>%
      mutate(
        gdp = gdp / 1e9,             # Convert GDP to billions
        population = population / 1e6, # Convert population to millions
        tourists = tourists / 1e6    # Convert tourists to millions
      )
    
    # Render Dynamic Comparative Plot
    output$DynamicPlot <- renderPlot({
      ggplot(caribbean_data, aes(x = !!sym(input$xAxisVar), y = !!sym(input$yAxisVar), color = country)) +
        geom_line() +
        labs(
          title = paste(
            "Relationship Between",
            names(which(c("year", "population", "gdp", "tourists") == input$xAxisVar)),
            "and",
            names(which(c("population", "gdp", "tourists") == input$yAxisVar))
          ),
          x = input$xAxisVar,
          y = input$yAxisVar
        ) +
        scale_color_manual(values = c(
          "Dominican Republic" = "blue", 
          "Haiti" = "green", 
          "Cuba" = "red", 
          "Jamaica" = "orange",
          "Puerto Rico" = "purple",
          "Trinidad and Tobago" = "cyan",
          "Barbados" = "magenta"
        )) +
        theme_minimal() +
        theme(
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
#rsconnect::setAccountInfo(name='rml1314520', token='8AC5FB56F0D7F0D2331E7F5AF324A970', secret='fQT4H7fhjIJhSaDuGFxoHhx3v0u4fB4wKw+5Uf3f')
#rsconnect::deployApp(appDir = "/Users/freezing2night/Desktop/MA 615/Dominican_Republic_Shiny")






setwd("C:/Users/Andrew/Documents/legiscan/heatmap/")
#save.image("data.RData")
load("data.RData")

library(foreach)
library(shinyjs)
library(plotly)
library(shinyWidgets)
library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(tibble)
library(ggplot2)

heatmap_data <- heatmap_data %>% filter(pct!= 1 & pct != 0)

ui <- fluidPage(
  uiOutput("dynamicTitle"),
  div(class="filter-row",style="display:flex; justify-content: space-between;margin-top:0.5vh; margin-bottom: 0px;padding-bottom:0px;margin-left:10vw;margin-right:10vw;",
      div(class = "filter-party", selectInput("party", "Select Party:", choices = c("D", "R"))),
      div(class = "filter-role", selectInput("role", "Select Chamber:", choices = c("House" = "Rep", "Senate" = "Sen"))),
      div(class = "filter-vote-type", 
          selectInput("vote_type", "Select Vote Type:", 
                      choices = c("All", "With Party", "Independent Vote", "Maverick Votes"), 
                      selected = "All")),
      
#div(class = "filter-term",selectInput("term", "Select Term:",choices = c("2013-2014","2015-2016","2017-2018","2019-2020","2021-2022", "2023-2024"),selected = "2023-2024")),
      div(class = "filter-final", 
          selectInput("final", "Final (Third Reading) Vote?", 
                      choices = c("Y","N","All"), 
                      selected = "Y"))
  ),
  plotlyOutput("heatmapPlot", height = "200vh") # Adjust plot height as needed
)



server <- function(input, output) {
  output$dynamicTitle <- renderUI({
    
    #year <- input$term
    
    partytext <- if(input$party == "D") "Democrats" else if(input$party == "R") "Republicans" else "All Parties"
    roleTitle <- if(input$role == "Rep") "Florida House" else if(input$role == "Sen") "Florida Senate" else "Florida Legislature"
    fullTitle <- paste0(roleTitle, " Voting Patterns: ", partytext)
    partytext2 <- if(input$party == "D") "Republicans" else if(input$party == "R") "Democrats" else "All Parties"
    color1 <- if(input$party == "D") "Red" else if(input$party == "R") "Blue"
    color1hex <-if(input$party == "D") "#d73027" else if(input$party == "R") "#4575b4"
    color2 <-if(input$party == "D") "Blue" else if(input$party == "R") "Red"
    color2hex <-if(input$party == "D") "#4575b4" else if(input$party == "R") "#d73027"
    HTML(paste0("<h2 style='text-align: center;'>", fullTitle, "</h2>
                <span style='font-size: 14px;line-height:0.5;'>\n<b style ='font-size:1.75rem;color: ",color1hex,";'>",color1," votes</b>: Legislator aligned <i>against</i> most ",partytext," and <i>with</i> most ",partytext2,".</span>
                <span style='font-size: 14px;line-height:0.5;'>\n<b style='font-size:1.75rem;color:",color2hex,";'>",color2," votes</b>: Legislator aligned <i>with</i> most ",partytext, ".</span>","<span style='font-size: 14px;line-height:0.5;'>\n<b style='color: #6DA832;font-size:1.75rem;'>Green votes</b>: Legislator aligned <i>against</i> both parties in bipartisan decisions.<br/><br/>\nBlank spaces indicate the legislators did not vote, either because they weren't assigned to those committees or they missed those votes.\n</span><span style='font-size: 14px;'>Displayed votes exclude ones where all members of a party voted unanimously. The table includes both amendment and bill votes.</span>"))
  })
  
  filteredData <- reactive({
    data <- heatmap_data 
    # Apply filters based on input
#    if (input$term != "All") {data <- data %>% filter(two_year_period == input$term)}
    if (input$final != "All") {
      data <- data %>% filter(final == input$final)
    }
    
    if (input$party != "All") {
      data <- data %>% dplyr::filter(party == input$party)
      if (input$party == "D") {
        data <- data %>% dplyr::filter(roll_call_id %in% d_votes$roll_call_id)
      } else if (input$party == "R") {
        data <- data %>% dplyr::filter(roll_call_id %in% r_votes$roll_call_id)
      }
    }
    if (input$vote_type != "All") {
      data <- data %>% dplyr::filter(as.character(partisan_metric2) == input$vote_type)
    }
    if (input$role != "All") {
      data <- data %>% dplyr::filter(role == input$role)
    }
    return(data)
  })
  
  output$heatmapPlot <- renderPlotly({
    data <- filteredData()
    # Determine colors based on party
    low_color <- if(input$party == "D") "#4575b4" else if(input$party == "R") "#d73027" else "#4575b4"
    mid_color <- "#6DA832"
    high_color <- if(input$party == "D") "#d73027" else if(input$party == "R") "#4575b4" else "#d73027"
    #levels_partisan_metric <- unique(data$partisan_metric2)
    
    # Define dynamic color mapping based on the party input
    #color_mapping <- #if(input$party == "D") {
    #c(#"With Party" = "#4575b4", 
    #  "Independent Vote" = "green", "Maverick Votes" = "#d73027")
    #} else if(input$party == "R") {
    #  c("With Party" = "#d73027", "Independent Vote" = "green", "Maverick Votes" = "#4575b4")
    # } else {
    #  c("With Party" = "#4575b4", "Independent Vote" = "green", "Maverick Votes" = "#d73027") # Default or other parties
    # }
    
    # Apply dynamic color mapping based on the party selection and the vote type
    #dynamic_colors <- sapply(data$partisan_metric2, function(metric) color_mapping(input$party)[metric])
    
    # Generate the plot
    p <- ggplot(data, aes(x = name, y = as.factor(roll_call_id), fill = partisan_metric2, text = hover_text)) +
      geom_tile(color = "white", linewidth = 0.1) +
      scale_fill_gradient2(low = low_color, high = high_color, mid = "#6DA832", midpoint = 1,
      ) +
      theme_minimal() +
      #scale_fill_manual(values = color_mapping) + # Apply the dynamic color mapping
      scale_y_discrete(labels = y_labels) +
      scale_x_discrete(position = "top") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 10),
            legend.position = "none",
            plot.title = element_blank(),
            plot.subtitle = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(autosize=TRUE,xaxis=list(side="top"),
             font = list(family = "Archivo"),
             margin = list(t=85), #list(l = 0, r = 0, t = 60, b = 10),  # Adjust margins to ensure the full title and subtitle are visible,
             annotations = list(
               x = 1.0, 
               y = 1.075,  # Position the annotation at the top center
               xref = 'paper', 
               yref = 'paper',
               text = 'Source: Florida Legislature Voting Records via LegiScan. Analysis by Andrew Pantazi',
               showarrow = FALSE,
               xanchor = 'right',
               yanchor = 'bottom',
               font = list(size = 11,family="Archivo")  # Adjust font size as needed
             ) ,
             plot_bgcolor = "rgba(255,255,255,0.85)",  # Transparent plot background
             paper_bgcolor = "rgba(255,255,255,0.85)"
      ) %>% 
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)

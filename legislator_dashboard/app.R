load("data.RData")
#load("C:/Users/Andrew/Documents/legiscan/legislators/data5.RData")
#save.image("C:/Users/Andrew/Documents/legiscan/legislators/data5.RData")
library(foreach)
library(shinyjs)
library(profvis)
library(DT)
library(data.table)
library(plotly)
library(shinyWidgets)
library(shiny)
library(jsonlite)
library(lubridate)
library(forcats)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(tibble)
library(ggplot2)
library(tidyverse)

ui <- fluidPage(
  useShinyjs(), # Enable shinyjs for dynamic UI effects
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=Archivo:ital,wght@0,500;0,600;1,500;1,600&display=swap", rel="stylesheet"),
    tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Archivo:ital,wght@0,500;0,600;1,500;1,600&display=swap');
        body { font-family: 'Georgia', serif; background-color: #fbfdfb; display: flex; flex-wrap: wrap; }
        h1, h2, h3, h4, h5, h6 { font-family: 'Archivo', sans-serif; color: #064875; text-transform: uppercase; }
        h2{ display: flex;align-items: center;justify-content: center;align-content: center;flex-wrap: wrap;}
        .filter-group { display: flex; flex-wrap: wrap; justify-content: space-evenly; align-items: center; width: 100%;flex-direction: row;align-content: center }
        .filter-section { margin: 5px;flex-basis: calc(33.333% - 10px);box-sizing: border-box;display: flex;flex-direction: column;flex-wrap: wrap;align-content: center;justify-content: center;align-items: center;}
        .btn-filter { margin: 5px; padding: 5px 10px; border: none; border-radius: 4px; cursor: pointer; width: 30%; min-width: 80px; box-sizing: border-box; background-color: #098677; color: white; font-family: 'Archivo', sans-serif; 
        }
        .bill-container, .legislator-profile { border-radius: 7px; margin: 1.5vw; border: 1px solid #8dd4df; background-color: #fbfdfb; }
        .bill-container{padding-inline:2.5vw;padding-block: 0.5vw; display:flex;flex-wrap:nowrap;flex-direction:column;justify-content:space-evenly;}
        .btn-party.R { background-color: #B22234; color: white; }
        .btn-party.D { background-color: #0047AB; color: white; }
        .btn-role.Rep { background-color: #f99a10; color: white; }
        .btn-role.Sen { background-color: #064875; color: white; }
        .btn-final.Y { background-color: #750648; color: white; }
        .btn-final.N { background-color: #355604; color: white; }
        .btn-filter.selected { filter: brightness(75%); }
        .content { display: flex; justify-content: space-evenly; width: 100%; }
        .votes-section { width: 65%; }
        .legislator-profile { border: 1px solid #ddd; padding: 20px; border-radius: 15px; width: 35%; margin-inline: 4%; }
        .vote-link { cursor: pointer; text-decoration: underline; color: blue; }
        .votes {border-color: #8dd4df;  padding: 2px; border-style: solid; border-radius: 7px; margin-block: 5px; }
        .independent-vote {  border-color: #ffc41d; }
        .maverick-vote { border-color: #098677; }
        .regular-vote { border-color: #064875; }
        .profile-filter-section{margin-block: 5%;line-height: 1.5;}
        label#voteType-label {line-height: .95;}
        .vote-details-button { background-color: #064875; color: #fbfdfb; display:flex;justify-content:center;align-items:center;  font-size: 1.5rem; width: auto;}
        .navigation{background-color:#fbf7ed;display: flex;flex-direction: row;flex-wrap: wrap;align-content: center;justify-content: space-evenly;align-items: center;margin-top: 10px;}
        label #items_per_page-label{font-size:1.2rem;}
        .items{font-size:1.2rem;}
        .btn-page:hover, .vote-link:hover, .info-button:hover {
    transform: scale(1.05);
    box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        } 
  div#filterFeedback {
      display: flex;
      align-items: center;
      justify-content: center;
      align-content: center;
      flex-wrap: wrap;
      flex-direction: column;
      padding-inline:5px;
  }
  button#info{font-size:1rem;display:contents;} 
  button#filterinfo{font-size:1rem;display:contents;}
        div .filter-section span{ font-family: 'Archivo', sans-serif; color: #064875; text-transform: uppercase; font-size: 1.7rem;font-weight: bold;}
              div .filter-section label{ font-family: 'Archivo', sans-serif; color: #064875; text-transform: uppercase; font-size: 1.7rem;font-weight: bold;}
              .btn-filter:hover, .btn-page:hover {
    transform: scale(1.05);
    box-shadow: 0 4px 8px rgba(0,0,0,0.2);
  }
              @media (max-width: 768px) {
      .content {
      flex-direction: column-reverse;}
      .legislator-profile{    display: flex;
      flex-direction: column;
      flex-wrap: wrap;
      align-content: center;
      justify-content: center;
      width: 100%;
      margin: 1vw;
      padding: 0;}
    .votes-section{    display: flex;
      flex-direction: column;
      flex-wrap: wrap;
      align-content: center;
      justify-content: center;
      width: 100%;
      margin: 1vw;
      padding: 0;}
              }
  button#view_liberal_conservative {
    margin-bottom: 10px;
    width:100%;
    font-size: 14px;
    text-transform: uppercase;
    font-family: 'Archivo', sans-serif;
      white-space: normal; /* Ensure that whitespace and breaks work as normal */
  text-align: center; /* Center the text */
  background-color:#23707C;
  }
div#votefilterinfo {
    padding-inline: 5px;
}
div#filter-info {
    display: flex;
    align-items: baseline;
    /* margin-bottom: 20px; */
    flex-wrap: wrap;
    flex-direction: row;
    justify-content: center;
}
      "))
  ),
  titlePanel("Legislator Voting Patterns Dashboard"),
  div(class="filter-group",
      div(class="filter-section", span("Party:"), actionButton("btn_party_R", "R", class="btn-filter btn-party R"), actionButton("btn_party_D", "D", class="btn-filter btn-party D")),
      div(class="filter-section", span("Chamber:"), actionButton("btn_role_Rep", "House", class="btn-filter btn-role Rep"), actionButton("btn_role_Sen", "Senate", class="btn-filter btn-role Sen")),
      div(class="filter-section", span("Final Vote:"), actionButton("btn_final_Y", "Yes", class="btn-filter btn-final Y"), actionButton("btn_final_N", "No", class="btn-filter btn-final N"))
  ),
  div(class="filter-group",
      div(class="filter-section", textInput("searchText", "Search Bills:", placeholder="Type to search in titles or descriptions...")),
      div(class="filter-section",selectizeInput("legislator", "Select Legislator:", choices = NULL,options = list('create' = TRUE, 'persist' = FALSE, 'placeholder' = 'Type to search...', 'onInitialize' = I("function() { this.on('dropdown_open', function() { $('.selectize-dropdown-content').perfectScrollbar(); }); }"))))
      ),
  div(id="filter-info", style="display: flex; align-items: center; margin-bottom: 20px;",
      textOutput("filterFeedback"),
      uiOutput("votefilterinfo") # This will dynamically generate additional info or tooltip content
  ),  div(class="content",
      div(class="votes-section", uiOutput("votesDisplay")),
      div(class="legislator-profile",
          actionButton("view_liberal_conservative", HTML("Liberal/Conservative<br>Rankings"),
                                                    class = "btn btn-primary", onclick = "window.location.href='https://data.jaxtrib.org/legislator_heatmap';"),      div(class="filter-section",selectInput("session_year","Select Session Year:",choices = NULL,selected = NULL,multiple = TRUE,width = '100%')),
          uiOutput("legislatorProfile"),
          div(class="profile-filter-section", checkboxGroupInput("voteType", label=HTML('Key Vote Type: <button id="info" type="button" class="btn btn-default action-button shiny-bound-input" onclick="Shiny.setInputValue(\'info_clicked\', true, {priority: \'event\'});"><i class="fas fa-info-circle"></i></button>'), choices=list("Voted Against Both Parties"="Independent", "Voted With Other Party"="Maverick", "Voted With Own Party"="Normal")))
      )
  ),
  div(class="navigation",
      actionButton("first_page", "First", class="btn-page"),
      actionButton("prev_page", "Previous", class="btn-page"),
      textOutput("page_info"),
      actionButton("next_page", "Next", class="btn-page"),
      actionButton("last_page", "Last", class="btn-page"),
      selectInput("items_per_page", "Items per Page:",choices = c(20, 50, 100), selected = 20)),
  tags$script(HTML('
      $(document).on("click", "#info", function() {
      // Trigger a Shiny event when the info button is clicked
      Shiny.setInputValue("info_clicked", true, {priority: "event"});
    });
        $(document).ready(function() {
          // Existing functionality: Toggle vote details on click
          $(document).on("click", ".vote-link", function() {
            var detailsId = $(this).attr("id").replace("vote-details-link-", "vote-details-");
            $("#" + detailsId).toggle();
          });
          
          // Detect Enter keypress in the selectize input (legislator search)
          $("#legislator").next(".selectize-control").find(".selectize-input input").on("keypress", function(e) {
            if(e.which == 13) { // Enter key pressed
              // Trigger an event in Shiny to update the dropdown based on current input
              Shiny.setInputValue("legislator_enter_pressed", $("#legislator").val(), {priority: "event"});
            }
          });
          
        });
         $(document).on("focus", "#legislator-selectize-input", function() {
                     Shiny.setInputValue("legislator_focused", true);
                     });
                $(document).on("blur", "#legislator-selectize-input", function() {
                  Shiny.setInputValue("legislator_focused", false);
                });
      '))
)


server <- function(input, output, session) {
  # Initialize reactive values to store filter states
  values <- reactiveValues(party = character(0), role = character(0), final = character(0))
  
  all_legislators <- unique(heatmap_data$name)
  all_legislators_with_all <- c("All" = "All", all_legislators) #not working, also below I changed the filter away from the >0 length to != "all"
  
  updateSelectizeInput(session, "legislator", choices = all_legislators_with_all, server = TRUE)
  observe({
    updateSelectInput(session, "session_year", 
                      choices = sort(unique(heatmap_data$session_year)),
                      selected = sort(unique(heatmap_data$session_year)))
  })
  
  output$votefilterinfo <- renderUI({
    div(
      actionButton("filterinfo", icon("info-circle"))
    )
  })
  
  observeEvent(input$filterinfo, {
    showModal(modalDialog(
      title = "Legislator Dashboard",HTML("
        <p>This dashboard allows you to see every vote a lawmaker has taken, highlighting votes where they voted against their own party or against both parties. The data includes every committee, amendment and final vote on a bill. Use the filters to limit legislators by party or chamber. You can also filter the votes so you only see final roll call votes or just those votes against their own party.</p>
      ")
    ))
  })
  
  
  observeEvent(input$btn_party_R, {
    if ("R" %in% values$party) {
      values$party <- setdiff(values$party, "R") # Remove R if selected
    } else {
      values$party <- c(values$party, "R") # Add R if not selected
    }
  })
  observeEvent(input$btn_party_D, {
    if ("D" %in% values$party) {
      values$party <- setdiff(values$party, "D")
    } else {
      values$party <- c(values$party, "D")
    }
  })
  # Chamber: House and Senate
  observeEvent(input$btn_role_Rep, {
    if ("Rep" %in% values$role) {
      values$role <- setdiff(values$role, "Rep")
    } else {
      values$role <- c(values$role, "Rep")
    }
  })
  
  observeEvent(input$btn_role_Sen, {
    if ("Sen" %in% values$role) {
      values$role <- setdiff(values$role, "Sen")
    } else {
      values$role <- c(values$role, "Sen")
    }
  })
  
  # Final Vote: Yes and No
  observeEvent(input$btn_final_Y, {
    if ("Y" %in% values$final){
      values$final <- setdiff(values$final,"Y")
    } else{values$final <- c(values$final,"Y")
    }
  })
  
  observeEvent(input$btn_final_N, {
    if ("N" %in% values$final){
      values$final <- setdiff(values$final,"N")
    } else{values$final <- c(values$final,"N")
    }
  })
  
  filtered_data <- reactive({
    data <- heatmap_data
    if (length(values$party) > 0) {
      data <- data %>% filter(party %in% values$party)
    }
    if (length(values$role) > 0) {
      data <- data %>% filter(role %in% values$role)
    }
    if (length(values$final) > 0) {
      data <- data %>% filter(final %in% values$final)
    }
    if ("Independent" %in% input$voteType) {
      data <- data %>% filter(vote_with_neither == 1)
    }
    if ("Maverick" %in% input$voteType) {
      data <- data %>% filter(maverick_votes == 1)
    }
    if ("Normal" %in% input$voteType) {
      data <- data %>% filter(maverick_votes == 0 & vote_with_neither == 0)
    }
    if (!is.null(input$searchText) && input$searchText != "") {
      data <- data %>% filter(grepl(input$searchText, title, fixed = TRUE) | grepl(input$searchText, description, fixed = TRUE))
    }
    # Apply legislator filter based on dropdown selection
    if (!is.null(input$legislator) && input$legislator != "All") {
      data <- data %>% filter(grepl(input$legislator, name, fixed = TRUE))
    }
    if (!is.null(input$session_year) && length(input$session_year) > 0) {
      data <- data %>% filter(session_year %in% input$session_year)
    }
    data
  })
  
  
  filtered_legdata <- reactive({
    data <- heatmap_data
    if (length(values$party) > 0) {
      data <- data %>% filter(party %in% values$party)
    }
    if (length(values$role) > 0) {
      data <- data %>% filter(role %in% values$role)
    }
    if (length(values$final) > 0) {
      data <- data %>% filter(final %in% values$final)
    }
    if ("Independent" %in% input$voteType) {
      data <- data %>% filter(vote_with_neither == 1)
    }
    if ("Maverick" %in% input$voteType) {
      data <- data %>% filter(maverick_votes == 1)
    }
    if ("Normal" %in% input$voteType) {
      data <- data %>% filter(maverick_votes == 0 & vote_with_neither == 0)
    }
    # Remember the current selection to possibly reapply it later
    data
  })
  
  
  observe({
    # Assuming filtered_legdata() is your reactive dataset that changes based on filters
    current_selection <- input$legislator
    filtered_legislators <- unique(filtered_legdata()$name)
    
    if (!is.null(input$legislator_enter_pressed)) {
      data <- data %>% filter(grepl(input$legislator_enter_pressed, title, fixed = TRUE) | grepl(input$legislator_enter_pressed, description, fixed = TRUE))}
    else{
      # Update dropdown choices. This does not inherently cause recursion.
      updateSelectizeInput(session, "legislator", choices = filtered_legislators,selected = current_selection)
    }}
  )
  
  count_independent_votes <- reactive({
    sum(filtered_data()$vote_with_neither == 1)
  })
  count_maverick_votes <- reactive({
    sum(filtered_data()$maverick_votes == 1)
  })
  count_normal_votes <- reactive({
    sum(filtered_data()$maverick_votes == 0 & filtered_data()$vote_with_neither == 0)
  })
  pct_independent_votes <- reactive({
    round(count_independent_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
  })
  pct_maverick_votes <- reactive({
    round(count_maverick_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
  })
  pct_normal_votes <- reactive({
    round(count_normal_votes()/sum(!is.na(filtered_data()$vote_with_neither)),3)*100
  })
  
  
  
  # Observe and update checkbox labels dynamically
  observe({
    current_selections <- input$voteType
    choices_vector <- c("Independent", "Maverick", "Normal")
    names(choices_vector) <- c(
      paste0("Voted Against Both Parties (", count_independent_votes()," - ",pct_independent_votes(), "%)"),
      paste0("Voted With Other Party (", count_maverick_votes()," - ",pct_maverick_votes(), "%)"),
      paste0("Voted With Their Party (", count_normal_votes()," - ",pct_normal_votes(), "%)")
    )
    
    updateCheckboxGroupInput(session, "voteType",
                             #label = "Key Vote Type:",
                             choices = choices_vector,selected = current_selections)
  })
  
  # After selecting a legislator, display their profile
  output$legislatorProfile <- renderUI({
    # Assuming 'heatmap_data' contains all the necessary legislator information
    selected_legislator <- input$legislator
    if (!is.null(selected_legislator) && selected_legislator != "") {
      legislator_info <- heatmap_data %>%
        filter(name == selected_legislator) %>% distinct(name, district, party, role, ballotpedia2) %>% slice(1)
      div(
        h3(a(href = legislator_info$ballotpedia2, target = "_blank", selected_legislator)),
        p("District: ", legislator_info$district),
        p("Party: ", legislator_info$party),
        p("Chamber: ", legislator_info$role)
      )
    }
  })
  
  # Reactive value for tracking the current page
  current_page <- reactiveVal(1)
  # Define how many items (bills) you want per page
  items_per_page <- reactive({ as.numeric(input$items_per_page) })
  
  # Calculate paginated data based on current page and items per page
  paginated_data <- reactive({
    if (nrow(filtered_data()) == 0) return(data.frame())
    start_index <- (current_page() - 1) * items_per_page() + 1
    end_index <- min(nrow(filtered_data()), start_index + items_per_page() - 1)
    filtered_data()[start_index:end_index, ]
  })
  
  # Render the paginated bills along with details
  output$votesDisplay <- renderUI({
    data <- paginated_data()
    if (is.null(input$legislator) || input$legislator == "" || nrow(data) == 0) {
      return(div(class = "no-data", "No bills available for display."))
    }
    
    # Dynamically create UI elements for each bill
    ui_elements <- lapply(unique(data$number), function(bill) {
      bill_data <- data[data$number == bill, ]
      bill_title <- unique(bill_data$title)[1]
      descriptions <- unique(bill_data$description)
      html_string <- paste0("<h4>", bill_title, " - ", "<a href='", bill_data$url[1], "' target='_blank'>", bill_data$number[1],"</a></h4>","<h5>",bill_data$session_name[1],"</h5><p>",bill_data$description[1],"</p>")
      
      div(class = 'bill-container',
          html_content <- HTML(html_string),
          actionButton(inputId = sprintf('vote-details-link-%s', gsub("[^A-Za-z0-9]", "", bill)), 
                       label = "Vote Details Info", 
                       class = "btn btn-info vote-details-button"),
          div(
            id = sprintf('vote-details-%s', gsub("[^A-Za-z0-9]", "", bill)),
            class = 'vote-details',
            style = 'display:none;',
            lapply(unique(bill_data$roll_call_id), function(roll_call) {
              roll_call_data <- bill_data[bill_data$roll_call_id == roll_call, ]
              
              pct_yes <- paste0(round(roll_call_data$pct[1] * 100, 2), "%")
              date_format <- format(as.Date(roll_call_data$date),"%b %d, %Y")
              special_vote_class <- ifelse(roll_call_data$vote_with_neither[1] == 1, "independent-vote",
                                           ifelse(roll_call_data$maverick_votes[1] == 1, "maverick-vote", "regular-vote"))
              
              special_vote_text <- ifelse(roll_call_data$vote_with_neither[1] == 1,
                                          "This legislator voted <b><i>against</i></b> the majorities of both parties.",
                                          ifelse(roll_call_data$maverick_votes[1] == 1,
                                                 sprintf("This legislator voted <b><i>against</i></b> the majority of their party (%s) and <b><i>with</i></b> the majority of the other party.", roll_call_data$party[1]),
                                                 "This legislator voted with their party majority."))
              
              legislator_vote <- paste(roll_call_data$name[1], "voted", roll_call_data$vote_text[1])
              
              div(class = paste("votes",special_vote_class), 
                  HTML(paste0(
                    "<p>",roll_call_data$desc," - <b>", date_format,"</b></p><p>", pct_yes, " of legislators voted Yea. </p><p>", special_vote_text, "</p><p>", legislator_vote,"</p><p class='disclaimer'><i>This vote wasn't necessarily a vote of the bill, and it could have been a vote on an amendment. For more details, examine the bill's vote information on the <a href='",roll_call_data$state_link,"' target='_blank'>Legislature's website.</a></i></p>"))
              )
            }) #close lapply
          ) #close vote detail div
      ) #close bill container div
    }) #close other lapply
    do.call(tagList, ui_elements)
  }) #close renderui
  
  #change button colors when activated
  observe({
    if("R" %in% values$party) {
      runjs('$("#btn_party_R").addClass("selected");')
    } else {
      runjs('$("#btn_party_R").removeClass("selected");')
    }
  })
  observe({
    if("D" %in% values$party) {
      runjs('$("#btn_party_D").addClass("selected");')
    } else {
      runjs('$("#btn_party_D").removeClass("selected");')
    }
    # Repeat for other buttons
  })
  observe({
    if("Rep" %in% values$role) {
      runjs('$("#btn_role_Rep").addClass("selected");')
    } else {
      runjs('$("#btn_role_Rep").removeClass("selected");')
    }
    # Repeat for other buttons
  })
  observe({
    if("Sen" %in% values$role) {
      runjs('$("#btn_role_Sen").addClass("selected");')
    } else {
      runjs('$("#btn_role_Sen").removeClass("selected");')
    }
    # Repeat for other buttons
  })
  observe({
    if("N" %in% values$final) {
      runjs('$("#btn_final_N").addClass("selected");')
    } else {
      runjs('$("#btn_final_N").removeClass("selected");')
    }
    # Repeat for other buttons
  })
  observe({
    if("Y" %in% values$final) {
      runjs('$("#btn_final_Y").addClass("selected");')
    } else {
      runjs('$("#btn_final_Y").removeClass("selected");')
    }
  })
  
  # Logic to handle 'Previous' and 'Next' pagination buttons
  observeEvent(input$prev_page, { if (current_page() > 1) current_page(current_page() - 1) })
  
  observeEvent(input$next_page, {
    total_items <- nrow(filtered_data())
    if ((current_page() * items_per_page) < total_items) current_page(current_page() + 1)
  })
  observeEvent(input$first_page, {
    current_page(1)
  })
  
  observeEvent(input$info, {
    showModal(modalDialog(
      title = "Vote Types",HTML("
        <p>There are three main ways legislators vote:</p>
        <p>1. They vote with the majority of their own party, even when it means voting with a majority of the opposing party on bipartisan or unanimous votes.</p>
        <p>2. They vote against the majority of legislators in both parties.</p>
        <p>3. Or they vote against the majority of the legislators in their own party AND with the majority of the opposing party.</p>
      ")
    ))
  })
  
  observeEvent(input$last_page, {
    total_pages <- ceiling(nrow(filtered_data()) / items_per_page)
    current_page(total_pages)
    shinyjs::toggleState("first_page", condition = current_page() > 1)
    shinyjs::toggleState("prev_page", condition = current_page() > 1)
    shinyjs::toggleState("next_page", condition = current_page() < total_pages)
    shinyjs::toggleState("last_page", condition = current_page() < total_pages)
  })
  
  output$page_info <- renderText({
    total_items <- nrow(filtered_data())
    start_item <- (current_page() - 1) * items_per_page() + 1
    end_item <- min(start_item + items_per_page() - 1, total_items)
    paste("Showing items", start_item, "to", end_item, "of", total_items)
  })
  output$filterFeedback <- renderText({
    filtered_count <- nrow(filtered_data())
    paste("Showing", filtered_count, "results based on current filters.")
  })
  
  shinyjs::runjs('
    $(document).on("click", ".vote-details-button", function() {
      var detailsId = $(this).attr("id").replace("vote-details-link-", "vote-details-");
      $("#" + detailsId).toggle();
    });
  ')
}

shinyApp(ui, server)

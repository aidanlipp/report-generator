library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(shinycssloaders)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Baseball Performance Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Performance Report", tabName = "report", icon = icon("chart-bar")),
      menuItem("Coach Input", tabName = "coach_input", icon = icon("edit"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .metric-table {
          border-collapse: collapse;
          width: 100%;
          margin: 20px 0;
        }
        .metric-table th, .metric-table td {
          border: 1px solid #ddd;
          padding: 12px;
          text-align: center;
        }
        .metric-table th {
          background-color: #5cb85c;
          color: white;
          font-weight: bold;
        }
        .above-average {
          background-color: #d4edda !important;
          color: #155724;
        }
        .below-average {
          background-color: #f8d7da !important;
          color: #721c24;
        }
        .assessment-section {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 5px;
          margin: 10px 0;
        }
        .recommendation-box {
          background-color: #e8f5e8;
          border-left: 4px solid #5cb85c;
          padding: 15px;
          margin: 10px 0;
        }
      "))
    ),
    
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
        fluidRow(
          box(
            title = "Upload CSV Data", status = "primary", solidHeader = TRUE, width = 12,
            fileInput("file", "Choose CSV File",
                     accept = c(".csv")),
            checkboxInput("header", "Header", TRUE),
            checkboxInput("stringsAsFactors", "Strings as factors", FALSE),
            br(),
            withSpinner(DT::dataTableOutput("contents"))
          )
        )
      ),
      
      # Coach Input Tab
      tabItem(tabName = "coach_input",
        fluidRow(
          box(
            title = "K-Motion Analysis", status = "info", solidHeader = TRUE, width = 12,
            textAreaInput("kmotion_input", "K-Motion Analysis Text:", 
                         value = "1. The data suggests that there are torso deceleration issues. The torso accelerates and then hits a point where it stalls and then speeds back up. This is likely due to oblique or erector strength or control issues.\n\n2. The data suggests that the upper arm is firing early in the swing. It begins to accelerate and then the body tries to catch up. Ideally the arm is moving as a byproduct of the torso as opposed to trying to get a head start.",
                         height = "150px", width = "100%"),
            br(),
            actionButton("save_kmotion", "Save K-Motion Analysis", class = "btn-info")
          )
        ),
        
        fluidRow(
          box(
            title = "Swing Recommendations", status = "warning", solidHeader = TRUE, width = 12,
            textAreaInput("swing_recommendations_input", "Swing Recommendations:", 
                         value = "1. The lead arm extension is causing the torso to have to decelerate slowly. The torso has to over decelerate slowly to allow the arms to get around the lead arm extension.\n\n2. By eliminating some of the early lead arm extension the torso can stabilize sooner which will allow the arms to take more speed and release out through the ball as opposed to slightly across.\n\n3. Focus on maintaining proper torso rotation sequence to improve overall swing efficiency.",
                         height = "150px", width = "100%"),
            br(),
            actionButton("save_swing_rec", "Save Swing Recommendations", class = "btn-warning")
          )
        ),
        
        fluidRow(
          box(
            title = "Movement Screen Assessment", status = "success", solidHeader = TRUE, width = 12,
            div(id = "movement_screens",
              fluidRow(
                column(6, textInput("screen1_name", "Screen 1 Name:", value = "Pelvic Tilt")),
                column(6, textAreaInput("screen1_findings", "Findings:", 
                                      value = "Able to create anterior tilt, but limited in creating posterior tilt. Noted bulging disc issue in lower back.",
                                      height = "80px"))
              ),
              fluidRow(
                column(6, textInput("screen2_name", "Screen 2 Name:", value = "Pelvic Dissociation")),
                column(6, textAreaInput("screen2_findings", "Findings:", 
                                      value = "Showed sway to the left in hips during screen, as opposed to rotation. This will affect unloading of body in swing.",
                                      height = "80px"))
              ),
              fluidRow(
                column(6, textInput("screen3_name", "Screen 3 Name:", value = "Overhead Squat")),
                column(6, textAreaInput("screen3_findings", "Findings:", 
                                      value = "Limited in initial screen but passed secondary screen. Likely due to thoracic extension limitations.",
                                      height = "80px"))
              ),
              fluidRow(
                column(6, textInput("screen4_name", "Screen 4 Name:", value = "Glute Bridge")),
                column(6, textAreaInput("screen4_findings", "Findings:", 
                                      value = "Showed vibration during screen. This is likely due to core instability issues.",
                                      height = "80px"))
              )
            ),
            br(),
            fluidRow(
              column(6, actionButton("add_screen", "Add Another Screen", class = "btn-success")),
              column(6, actionButton("save_movement", "Save Movement Screens", class = "btn-success"))
            )
          )
        )
      ),
      
      # Performance Report Tab
      tabItem(tabName = "report",
        fluidRow(
          box(
            title = "Player Selection & Chart Options", status = "info", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, selectInput("selected_player", "Select Player:", choices = NULL)),
              column(4, selectInput("chart_type", "Distribution Chart Type:", 
                                  choices = list("Bat Speed" = "bat_speed",
                                               "Exit Velocity" = "exit_velo",
                                               "Rotational Acceleration" = "rot_acc",
                                               "Attack Angle" = "attack_angle"))),
              column(4, actionButton("generate_report", "Generate Report", 
                                   class = "btn-success", style = "margin-top: 25px;"))
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.show_report",
          fluidRow(
            box(
              title = textOutput("report_title"), status = "success", solidHeader = TRUE, width = 12,
              
              # Distribution Charts Row
              fluidRow(
                column(6, withSpinner(plotlyOutput("dist_chart1", height = "300px"))),
                column(6, withSpinner(plotlyOutput("dist_chart2", height = "300px")))
              ),
              
              br(),
              
              # Performance Metrics Table
              fluidRow(
                column(8, 
                  h4("Performance Metrics", style = "color: #5cb85c; font-weight: bold;"),
                  withSpinner(htmlOutput("metrics_table"))
                ),
                column(4,
                  div(class = "recommendation-box",
                    h4("K-Motion", style = "background-color: #5cb85c; color: white; padding: 8px; margin: -15px -15px 15px -15px; text-align: center;"),
                    htmlOutput("kmotion_analysis")
                  )
                )
              ),
              
              br(),
              
              # Swing Recommendations
              fluidRow(
                column(12,
                  div(class = "recommendation-box",
                    h4("Swing Recommendations", style = "background-color: #5cb85c; color: white; padding: 8px; margin: -15px -15px 15px -15px; text-align: center;"),
                    htmlOutput("swing_recommendations")
                  )
                )
              ),
              
              br(),
              
              # Movement Screen Assessment
              fluidRow(
                column(12,
                  h4("Movement Screen Assessment", style = "background-color: #5cb85c; color: white; padding: 10px; text-align: center;"),
                  div(class = "assessment-section",
                    withSpinner(htmlOutput("movement_assessment"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store coach inputs
  coach_inputs <- reactiveValues(
    kmotion_text = "1. The data suggests that there are torso deceleration issues. The torso accelerates and then hits a point where it stalls and then speeds back up. This is likely due to oblique or erector strength or control issues.\n\n2. The data suggests that the upper arm is firing early in the swing. It begins to accelerate and then the body tries to catch up. Ideally the arm is moving as a byproduct of the torso as opposed to trying to get a head start.",
    swing_recommendations = "1. The lead arm extension is causing the torso to have to decelerate slowly. The torso has to over decelerate slowly to allow the arms to get around the lead arm extension.\n\n2. By eliminating some of the early lead arm extension the torso can stabilize sooner which will allow the arms to take more speed and release out through the ball as opposed to slightly across.\n\n3. Focus on maintaining proper torso rotation sequence to improve overall swing efficiency.",
    movement_screens = list(
      list(name = "Pelvic Tilt", findings = "Able to create anterior tilt, but limited in creating posterior tilt. Noted bulging disc issue in lower back."),
      list(name = "Pelvic Dissociation", findings = "Showed sway to the left in hips during screen, as opposed to rotation. This will affect unloading of body in swing."),
      list(name = "Overhead Squat", findings = "Limited in initial screen but passed secondary screen. Likely due to thoracic extension limitations."),
      list(name = "Glute Bridge", findings = "Showed vibration during screen. This is likely due to core instability issues.")
    )
  )
  
  # Counter for dynamic movement screens
  screen_counter <- reactiveVal(4)
  
  # Reactive value to store uploaded data
  data <- reactiveVal()
  
  # Save coach inputs
  observeEvent(input$save_kmotion, {
    coach_inputs$kmotion_text <- input$kmotion_input
    showNotification("K-Motion analysis saved!", type = "message")
  })
  
  observeEvent(input$save_swing_rec, {
    coach_inputs$swing_recommendations <- input$swing_recommendations_input
    showNotification("Swing recommendations saved!", type = "message")
  })
  
  observeEvent(input$save_movement, {
    screens <- list()
    current_count <- screen_counter()
    
    for(i in 1:current_count) {
      screen_name_id <- paste0("screen", i, "_name")
      screen_findings_id <- paste0("screen", i, "_findings")
      
      if(!is.null(input[[screen_name_id]]) && !is.null(input[[screen_findings_id]])) {
        if(input[[screen_name_id]] != "" || input[[screen_findings_id]] != "") {
          screens[[length(screens) + 1]] <- list(
            name = input[[screen_name_id]],
            findings = input[[screen_findings_id]]
          )
        }
      }
    }
    
    coach_inputs$movement_screens <- screens
    showNotification("Movement screens saved!", type = "message")
  })
  
  # Add new movement screen
  observeEvent(input$add_screen, {
    current_count <- screen_counter()
    new_count <- current_count + 1
    screen_counter(new_count)
    
    insertUI(
      selector = "#movement_screens",
      where = "beforeEnd",
      ui = fluidRow(
        column(6, textInput(paste0("screen", new_count, "_name"), paste("Screen", new_count, "Name:"), value = "")),
        column(6, textAreaInput(paste0("screen", new_count, "_findings"), "Findings:", 
                               value = "", height = "80px"))
      )
    )
  })
  
  # File upload handling
  observeEvent(input$file, {
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   stringsAsFactors = input$stringsAsFactors)
    
    data(df)
    
    # Update player choices
    player_names <- paste(df$`First.Name`, df$`Last.Name`)
    updateSelectInput(session, "selected_player", choices = player_names)
  })
  
  # Display uploaded data
  output$contents <- DT::renderDataTable({
    req(data())
    DT::datatable(data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Process data for analysis
  processed_data <- reactive({
    req(data())
    df <- data()
    
    # Calculate averages for each player across 5 swings
    player_stats <- df %>%
      rowwise() %>%
      mutate(
        avg_exit_velo = mean(c(`Exit.Velo.1`, `Exit.Velo.2`, `Exit.Velo.3`, `Exit.Velo.4`, `Exit.Velo.5`), na.rm = TRUE),
        avg_bat_speed = mean(c(`Bat.Speed.1`, `Bat.Speed.2`, `Bat.Speed.3`, `Bat.Speed.4`, `Bat.Speed.5`), na.rm = TRUE),
        avg_rot_acc = mean(c(`Rot..Acc..1`, `Rot..Acc..2`, `Rot..Acc..3`, `Rot..Acc..4`, `Rot..Acc..5`), na.rm = TRUE),
        avg_vba = mean(c(`VBA.1`, `VBA.2`, `VBA.3`, `VBA.4`, `VBA.5`), na.rm = TRUE),
        avg_attack_angle = mean(c(`Attack.Angle.1`, `Attack.Angle.2`, `Attack.Angle.3`, `Attack.Angle.4`, `Attack.Angle.5`), na.rm = TRUE),
        avg_on_plane_eff = mean(c(`On.Plane.Efficiency.1`, `On.Plane.Efficiency.2`, `On.Plane.Efficiency.3`, `On.Plane.Efficiency.4`, `On.Plane.Efficiency.5`), na.rm = TRUE),
        player_name = paste(`First.Name`, `Last.Name`)
      )
    
    # Calculate group averages
    group_avg_exit_velo <- mean(player_stats$avg_exit_velo, na.rm = TRUE)
    group_avg_bat_speed <- mean(player_stats$avg_bat_speed, na.rm = TRUE)
    group_avg_rot_acc <- mean(player_stats$avg_rot_acc, na.rm = TRUE)
    group_avg_vba <- mean(player_stats$avg_vba, na.rm = TRUE)
    group_avg_attack_angle <- mean(player_stats$avg_attack_angle, na.rm = TRUE)
    group_avg_on_plane_eff <- mean(player_stats$avg_on_plane_eff, na.rm = TRUE)
    
    list(
      player_stats = player_stats,
      group_averages = list(
        exit_velo = group_avg_exit_velo,
        bat_speed = group_avg_bat_speed,
        rot_acc = group_avg_rot_acc,
        vba = group_avg_vba,
        attack_angle = group_avg_attack_angle,
        on_plane_eff = group_avg_on_plane_eff
      )
    )
  })
  
  # Show report logic
  output$show_report <- reactive({
    !is.null(input$generate_report) && input$generate_report > 0 && !is.null(data())
  })
  outputOptions(output, "show_report", suspendWhenHidden = FALSE)
  
  # Report title
  output$report_title <- renderText({
    req(input$selected_player, input$generate_report > 0)
    paste(input$selected_player, "- Performance Analysis & Recommendations")
  })
  
  # Distribution charts
  output$dist_chart1 <- renderPlotly({
    req(input$generate_report > 0, processed_data(), input$selected_player, input$chart_type)
    
    player_data <- processed_data()$player_stats %>%
      filter(player_name == input$selected_player)
    
    if(input$chart_type == "bat_speed") {
      all_values <- c(player_data$`Bat.Speed.1`, player_data$`Bat.Speed.2`, 
                     player_data$`Bat.Speed.3`, player_data$`Bat.Speed.4`, player_data$`Bat.Speed.5`)
      title <- "Bat Speed Distribution"
      player_avg <- player_data$avg_bat_speed
      group_avg <- processed_data()$group_averages$bat_speed
    } else if(input$chart_type == "exit_velo") {
      all_values <- c(player_data$`Exit.Velo.1`, player_data$`Exit.Velo.2`, 
                     player_data$`Exit.Velo.3`, player_data$`Exit.Velo.4`, player_data$`Exit.Velo.5`)
      title <- "Exit Velocity Distribution"
      player_avg <- player_data$avg_exit_velo
      group_avg <- processed_data()$group_averages$exit_velo
    } else if(input$chart_type == "rot_acc") {
      all_values <- c(player_data$`Rot..Acc..1`, player_data$`Rot..Acc..2`, 
                     player_data$`Rot..Acc..3`, player_data$`Rot..Acc..4`, player_data$`Rot..Acc..5`)
      title <- "Rotational Acceleration Distribution"
      player_avg <- player_data$avg_rot_acc
      group_avg <- processed_data()$group_averages$rot_acc
    } else {
      all_values <- c(player_data$`Attack.Angle.1`, player_data$`Attack.Angle.2`, 
                     player_data$`Attack.Angle.3`, player_data$`Attack.Angle.4`, player_data$`Attack.Angle.5`)
      title <- "Attack Angle Distribution"
      player_avg <- player_data$avg_attack_angle
      group_avg <- processed_data()$group_averages$attack_angle
    }
    
    all_values <- all_values[!is.na(all_values)]
    
    p <- ggplot(data.frame(values = all_values), aes(x = values)) +
      geom_density(fill = "lightblue", alpha = 0.7, color = "blue") +
      geom_vline(xintercept = player_avg, color = "blue", linetype = "dashed", size = 1) +
      geom_vline(xintercept = group_avg, color = "orange", linetype = "dashed", size = 1) +
      labs(title = title, x = "", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$dist_chart2 <- renderPlotly({
    req(input$generate_report > 0, processed_data(), input$selected_player)
    
    player_data <- processed_data()$player_stats %>%
      filter(player_name == input$selected_player)
    
    # Always show rotational acceleration as second chart
    all_values <- c(player_data$`Rot..Acc..1`, player_data$`Rot..Acc..2`, 
                   player_data$`Rot..Acc..3`, player_data$`Rot..Acc..4`, player_data$`Rot..Acc..5`)
    all_values <- all_values[!is.na(all_values)]
    
    p <- ggplot(data.frame(values = all_values), aes(x = values)) +
      geom_density(fill = "lightcoral", alpha = 0.7, color = "red") +
      geom_vline(xintercept = player_data$avg_rot_acc, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = processed_data()$group_averages$rot_acc, color = "orange", linetype = "dashed", size = 1) +
      labs(title = "Rotational Acceleration Distribution", x = "Rotational Acceleration (deg/s)", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Performance metrics table
  output$metrics_table <- renderUI({
    req(input$generate_report > 0, processed_data(), input$selected_player)
    
    player_data <- processed_data()$player_stats %>%
      filter(player_name == input$selected_player)
    group_avg <- processed_data()$group_averages
    
    metrics <- data.frame(
      Metric = c("Exit Velocity", "Bat Speed", "Rotational Acceleration", "VBA", "Attack Angle", "On Plane Efficiency"),
      Player_Avg = c(
        round(player_data$avg_exit_velo, 1),
        round(player_data$avg_bat_speed, 1),
        round(player_data$avg_rot_acc, 1),
        round(player_data$avg_vba, 1),
        round(player_data$avg_attack_angle, 1),
        paste0(round(player_data$avg_on_plane_eff, 1), "%")
      ),
      Group_Avg = c(
        round(group_avg$exit_velo, 1),
        round(group_avg$bat_speed, 1),
        round(group_avg$rot_acc, 1),
        round(group_avg$vba, 1),
        round(group_avg$attack_angle, 1),
        paste0(round(group_avg$on_plane_eff, 1), "%")
      )
    )
    
    # Determine if above or below average
    above_below <- c(
      player_data$avg_exit_velo > group_avg$exit_velo,
      player_data$avg_bat_speed > group_avg$bat_speed,
      player_data$avg_rot_acc < group_avg$rot_acc, # Lower is better for rot acc issues
      player_data$avg_vba > group_avg$vba,
      player_data$avg_attack_angle > group_avg$attack_angle,
      player_data$avg_on_plane_eff > group_avg$on_plane_eff
    )
    
    table_html <- "<table class='metric-table'>"
    table_html <- paste0(table_html, "<tr><th>Metric</th><th>Player Avg</th><th>Group Avg</th></tr>")
    
    for(i in 1:nrow(metrics)) {
      class_name <- if(above_below[i]) "above-average" else "below-average"
      table_html <- paste0(table_html, "<tr class='", class_name, "'>",
                          "<td><strong>", metrics$Metric[i], "</strong></td>",
                          "<td>", metrics$Player_Avg[i], "</td>",
                          "<td>", metrics$Group_Avg[i], "</td>",
                          "</tr>")
    }
    
    table_html <- paste0(table_html, "</table>")
    HTML(table_html)
  })
  
  # K-Motion Analysis
  output$kmotion_analysis <- renderUI({
    text_lines <- strsplit(coach_inputs$kmotion_text, "\n")[[1]]
    formatted_text <- paste(text_lines, collapse = "<br>")
    HTML(formatted_text)
  })
  
  # Swing Recommendations
  output$swing_recommendations <- renderUI({
    text_lines <- strsplit(coach_inputs$swing_recommendations, "\n")[[1]]
    formatted_text <- paste(text_lines, collapse = "<br>")
    HTML(formatted_text)
  })
  
  # Movement Assessment
  output$movement_assessment <- renderUI({
    screens <- coach_inputs$movement_screens
    
    if(length(screens) == 0) {
      return(HTML("<p>No movement screens added yet. Go to Coach Input tab to add screens.</p>"))
    }
    
    html_content <- ""
    for(i in 1:length(screens)) {
      screen <- screens[[i]]
      if(!is.null(screen$name) && !is.null(screen$findings) && 
         screen$name != "" && screen$findings != "") {
        html_content <- paste0(html_content, 
          "<div style='background-color: #5cb85c; color: white; padding: 8px; margin: 10px 0 5px 0; font-weight: bold;'>",
          screen$name, "</div>",
          "<p style='margin: 5px 0 15px 0; padding: 10px; background-color: white; border: 1px solid #ddd;'>",
          gsub("\n", "<br>", screen$findings), "</p>"
        )
      }
    }
    
    if(html_content == "") {
      html_content <- "<p>No movement screens with content found. Please add screens in the Coach Input tab.</p>"
    }
    
    HTML(html_content)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

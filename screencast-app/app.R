library(shiny)
library(bslib)
library(tidyverse)

# Load Data ----

full_results_shiny <- 
  read_rds("data/full_results.rds")

# User Interface ----
ui <- 
  page_sidebar(
    title = "First Study Results for Screencast",
    sidebar = 
      sidebar(
        helpText(
          "Create graphs with the results information from the study!"
        ),
        selectInput(
          "event",
          label = 
            "Choose an event to display",
          choices = 
            unique(full_results_shiny$event)
        )
      ),
    plotOutput(outputId = "plot")
  )

# Server logic ---

server <- 
  function(input, output) {
    output$plot <- renderPlot({
      # filter data based off selection of input
      data <- 
        full_results_shiny %>% 
        filter(event == input$event)
      
      # when we choose an event, make plot based for the event
      if (input$event == "Number of Attacks Treated") {
        data %>% 
          ggplot(aes(x = time, y = value, group = group_title, color = group_title)) + 
          # geom_bar(stat = "identity", position = position_dodge()) + 
          geom_line(linewidth = 1) + 
          geom_point(size = 3) + 
          theme_bw() + 
          labs(
            x = "Time",
            y = "Number of Attacks",
            title = "Comparison of Total Attacks and Pain-Free Attacks at 15 and 30 minutes",
            color = "Cohort"
          )
      } else if (input$event == "Disability score") {
        data %>% 
          ggplot(aes(x = time, y = value, group = group_title, fill = group_title)) + 
          geom_bar(stat = "identity", position = position_dodge()) +
          # geom_line(linewidth = 1) + 
          # geom_point(size = 3) + 
          theme_bw() + 
          expand_limits(y = c(0,5)) +
          labs(
            x = "Time",
            y = "Disability Score",
            title = "Change in Disability Score from Baseline to 2 Weeks After Baseline",
            subtitle = "Scores: 1 = minor, 2 = minor/moderate, 3 = moderate, 4 = moderate/severe, 5 = severe",
            fill = "Cohort"
          )
      } else if (input$event == "VAS scale") {
        data %>% 
          ggplot(aes(x = time, y = value, group = group_title, color = group_title)) + 
          # geom_bar(stat = "identity", position = position_dodge()) + 
          geom_line(linewidth = 1) + 
          geom_point(size = 3) + 
          theme_bw() + 
          expand_limits(y = c(0,100)) +
          labs(
            x = "Time",
            y = "VAS scale",
            title = "Mean VAS Scale Change From Baseline to After 2 Weeks",
            subtitle = "VAS scale (Overall health) from 0-100 mm where higher score is better (100) than lower score (0).",
            color = "Cohort"
          )
      } else if (input$event == "Patients Who Used Any Type of Rescue Medication") {
        data %>% 
          ggplot(aes(x = group_id, y = value, group = group_title, fill = group_title)) + 
          geom_bar(stat = "identity", position = position_dodge()) +
          theme_bw() + 
          expand_limits(y = c(0,50)) +
          labs(
            x = "Group",
            y = "Number of Patients",
            title = "Total Patients Who Used Any Type of Rescue Medication",
            fill = "Cohort"
          )
      } else {
        # Default: Mobility - Anxiety
        data %>% 
          ggplot(aes(x = time, y = value, group = group_title, color = group_title)) + 
          # geom_bar(stat = "identity", position = position_dodge()) + 
          geom_line(linewidth = 1, alpha = 0.7) + 
          geom_point(size = 3) + 
          facet_wrap(~event) + 
          theme_bw() + 
          expand_limits(y = c(0,3)) +
          labs(
            x = "Time",
            y = "Value",
            title = "Mean Change of Questionnaire EQ-5D-3L From Baseline to After 2 Weeks Treatment",
            subtitle = "Each dimension has 3 levels: 1 = no problems, 2 = moderate problems, 3=extreme problems",
            color = "Cohort"
          )
        
      }
    })
  }

# Run app ---

shinyApp(ui, server)
# Load packages ---
library(shiny)
library(bslib)
library(tidyverse)

# Load data ---
first_study_outcome_measures <- 
  read_csv("data/first_study_outcome_measures.csv")

# User interface ---
ui <- 
  page_sidebar(
    title = "First Study Results",
    sidebar = 
      sidebar(
        helpText("Create graphs with the results information from the study."),
        selectInput(
          "event",
          label = "Choose an event to display:",
          choices = c(
            "Number of attacks treated",
            "Disability score",
            "Mobility",
            "Selfcare",
            "Activity",
            "Pain",
            "Anxiety",
            "VAS scale",
            "Patients Who Used Any Type of Rescue Medication"
          )
        )
      ),
    # Output: Graphs ---
    plotOutput(outputId = "plot", height = "600px")
  )

# Server logic ---
server <- function(input, output) {
  
  output$plot <- renderPlot({
    # Filter data based on selection
    data <- first_study_outcome_measures %>%
      filter(event == input$event)
    
    # Choose plot type based on event
    if (input$event == "Number of attacks treated") {
      ggplot(data, aes(x = time, y = value, color = group_title, group = group_title)) +
        geom_line(linewidth = 1, alpha = 0.7) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_brewer("Group", palette = "Set1") +
        labs(
          x = "Time",
          y = "Number of Attacks",
          title = "Comparison of Total Attacks and Pain-Free Attacks at 15 and 30 Minutes"
        ) +
        theme_bw()
      
    } else if (input$event == "Disability score") {
      ggplot(data, aes(x = time, y = value, fill = group_title)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_brewer("Group", palette = "Set1") +
        labs(
          x = "Time",
          y = "Disability Score",
          title = "Mean Change of Disability Scores from Baseline to 2 Weeks"
        ) +
        expand_limits(y = c(0, 5)) +
        theme_bw()
      
    } else if (input$event == "Patients Who Used Any Type of Rescue Medication") {
      ggplot(data, aes(x = group_title, y = value, fill = group_title)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer("Group", palette = "Set1") +
        labs(
          x = "Group",
          y = "Number of Patients",
          title = "Patients Who Used Any Type of Rescue Medication"
        ) +
        theme_bw()
      
    } else if (input$event == "VAS scale") {
      ggplot(data, aes(x = group_title, y = value, fill = group_title)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer("Group", palette = "Set1") +
        labs(
          x = "Group",
          y = "VAS Scale",
          title = "VAS scale (Overall health) from 0-100 mm where higher score is better (100) than lower score (0)."
        ) +
        theme_bw()
      
    } else {
      # Default: EQ-5D type events (Mobility, Selfcare, Activity, Pain, Anxiety)
      ggplot(data, aes(x = time, y = value, group = group_title, color = group_title)) +
        geom_line(linewidth = 1, alpha = 0.7) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_brewer("Group", palette = "Set1") +
        labs(
          x = "Time",
          y = "Value",
          title = paste("Mean Change for", input$event),
          subtitle = "EQ-5D-3L: 1 = no problems, 2 = moderate, 3 = extreme"
        ) +
        expand_limits(y = c(0, 3)) +
        theme_bw()
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

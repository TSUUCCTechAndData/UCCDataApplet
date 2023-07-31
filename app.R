# Packages

library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(shinythemes)

# Data

load("/Users/dane_winterboer/Desktop/UCC Data/S23 - Services Applet/Data Cleaning/Data Cleaning/UCCClean.RData")

# Generates List of Majors
majors_list = unique(c(levels(clean_data$Major1st), levels(clean_data$Major2nd)))

# Get max/min date range for UI input
max_date <- max(clean_data$Date)
min_date <- min(clean_data$Date)

# UI

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("paper"),
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Output Controls
        h3("Output Controls:"),
        
        # Select graph/report style
        selectInput(inputId = "ReportStyle", 
                    label = "Report Style:",
                    choices = c(
                      "Counts (Bar)"   = "style_bar", 
                      "Percentages (Pie/Bar)Groups" = "style_percentage", 
                      "Date/Time (Line Chart)"  = "style_time"), 
                    selected = "critics_score"
        ),
        
        # Select non time variable for report
        conditionalPanel(
          condition = "input.ReportStyle != 'style_time'",
          
          selectInput(inputId = "reporting_var", 
                      label = "Reporting Variable:",
                      choices = c("1st Major"                 = "Major1st",
                                  "2nd Major"                 = "Major2nd",
                                  "Class Level"               = "ClassLevel", 
                                  "Career Center Service"     = "CareerCenterService", 
                                  "Meeting Format"            = "MeetingFormat", 
                                  "Appointment Length"        = "AppointmentLength",
                                  "Counselor"                 = "Counselor",
                                  "Appointment Cancellations" = "AppointmentCancelled",
                                  "Meeting Type"              = "WalkInAppointment"
                      ), 
                      selected = "Major1st"
          )
        ),
        
        # Select time/date variable for report
        conditionalPanel(
          condition = "input.ReportStyle == 'style_time'",
          
          selectInput(inputId = "reporting_timedate", 
                      label = "Reporting Variable:",
                      choices = c("Time (24:00)" = "time_time",
                                  "Day"          = "time_day",
                                  "Day of Week"  = "time_week",
                                  "Month"        = "time_month", 
                                  "Year"         = "time_year"
                      ), 
                      selected = "time_day"
          )
        ),
        
        # Select variable for grouping
        selectInput(inputId = "grouping_var", 
                    label = "Group by:",
                    choices = c("(None Selected)"           = "NoGroup",
                                "1st Major"                 = "Major1st",
                                "2nd Major"                 = "Major2nd", 
                                "Class Level"               = "ClassLevel", 
                                "Career Center Service"     = "CareerCenterService", 
                                "Meeting Format"            = "MeetingFormat", 
                                "Appointment Length"        = "AppointmentLength",
                                "Counselor"                 = "Counselor",
                                "Appointment Cancellations" = "AppointmentCancelled",
                                "Meeting Type"              = "WalkInAppointment"
                    ), 
                    selected = "NoGroup"
        ),
        
        # Select graph style for time seris graph
        conditionalPanel(
          condition = "input.ReportStyle == 'style_time'",
          
          sliderTextInput(inputId = "graph_timedate", 
                      label = "Select Graph Style: ",
                      choices = c("Line Plot", "Bar Graph"
                      ), 
                      selected = "Line Plot"
                    )
        ),
      
      
      # Filters
      dropdown(
        div(
          id = "div_filters",
          
          h3("Filters:"),
          
          # Date Range
          dateRangeInput(
            inputId = "DateRange",
            label = "Select Date Range: ",
            start = min_date, end = max_date,
            min = min_date, max = max_date,
            startview = "year"
          ),
          
          # # Time Range
          # sliderInput(
          #   inputId = "TimeRange",
          #   label = "Select Time Range: ",
          #   min = as.POSIXct("00:00:00"),
          #   max = as.POSIXct("24:00:00")
          # ),
          
          # Majors
          pickerInput(
            inputId = "SelectMajors",
            label = "Select Desired Majors: ",
            choices = majors_list,
            selected = majors_list,
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = TRUE
          ),
          
          # Class Level
          pickerInput(
            inputId = "SelectClassLevel",
            label = "Select Desired Class Levels: ",
            choices = levels(clean_data$ClassLevel),
            selected = levels(clean_data$ClassLevel),
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = TRUE
          ),
          
          # UCC Service
          pickerInput(
            inputId = "SelectService",
            label = "Select Desired UCC Services: ",
            choices = levels(clean_data$CareerCenterService),
            selected = levels(clean_data$CareerCenterService),
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = TRUE
          ),
          
          # UCC Counselor
          pickerInput(
            inputId = "SelectCounselor",
            label = "Select UCC Counselor/Staff: ",
            choices = levels(clean_data$Counselor),
            selected = levels(clean_data$Counselor),
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = TRUE
          ),
          
          # Appointment Length
          pickerInput(
            inputId = "SelectLength",
            label = "Select Appointment Length (mins): ",
            choices = levels(clean_data$AppointmentLength),
            selected = levels(clean_data$AppointmentLength),
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = TRUE
          ),
          
          # Reset Button
          actionButton(
            inputId = "reset_filters",
            label = "Reset Filters",
          )
        ),
        
         circle = FALSE,
         label = " Filters ",
         width = "500px",
         tooltip = "Click to see filters."
       ),
      
      # Break
      br(),
      
      # Additional Options
      dropdown(
        div(
          id = "div_options",
          
          h3("Additional Options:"),
          
          radioGroupButtons(
            inputId = "SelectMeetingFormat",
            label = "Meeting Format",
            choices = c("In-Person", "Virtual", "Both"),
            individual = TRUE,
            selected = "Both"
          ),
          
          radioGroupButtons(
            inputId = "SelectWalkIn",
            label = "Meeting Type",
            choices = c("Appointment", "Walk-In", "Both"),
            individual = TRUE,
            selected = "Both"
          ),
          
          radioGroupButtons(
            inputId = "SelectCancellation",
            label = "Cancellations",
            choices = c("Include", "Don't Include", "Only Show"),
            individual = TRUE,
            selected = "Don't Include"
          )
        ),
        
         circle = FALSE,
         label = " Additional Options ",
         width = "500px"
       ),
    ),
    
    
    # Output: Show plots
    mainPanel(
      plotlyOutput(outputId = "boxplot"),
      dataTableOutput(outputId = "nicetable")
    )
  )
)

# Define Server

server <- function(input, output, session) {
  
  # Reactives
  
  #Filters and Options:
  filtered_data <- reactive({
    
    # Filtering by filters
    filtering <- clean_data %>%
      filter(
        Date >= input$DateRange[1] & Date <= input$DateRange[2],
        Major1st %in% input$SelectMajors,
        ClassLevel %in% input$SelectClassLevel,
        CareerCenterService %in% input$SelectService,
        Counselor %in% input$SelectCounselor,
        AppointmentLength %in% input$SelectLength
      )
    
      # Filter by additional options:
    
      # Meeting Format Selection
    if (input$SelectMeetingFormat == "In-Person") {
      filtering <- filtering %>%
        filter(MeetingFormat == "InPerson")
    }
    
    if (input$SelectMeetingFormat == "Virtual") {
      filtering <- filtering %>%
        filter(MeetingFormat == "Virtual")
    }
    
    # Meeting Walkin Selection
    if (input$SelectWalkIn == "Appointment") {
      filtering <- filtering %>%
        filter(WalkInAppointment == "Appointment")
    }
    
    if (input$SelectWalkIn == "Walk-In") {
      filtering <- filtering %>%
        filter(WalkInAppointment == "Walk-In")
    }
    
    # Meeting canceled selection:
    if (input$SelectCancellation == "Only Show") {
      filtering <- filtering %>%
        filter(AppointmentCancelled != "No")
    }
    
    if (input$SelectCancellation == "Don't Include") {
      filtering <- filtering %>%
        filter(AppointmentCancelled == "No")
    }
    
    # Declares finished filtered set
    filtering
  })
  
  # Table Output
  main_table <- reactive({
    
    # Creates new base data frame for tables
    if(input$grouping_var == "NoGroup" & input$ReportStyle != "style_time"){
      baseDF <- as.data.frame(filtered_data()[,c(input$reporting_var)])
      names(baseDF)[1] = toString(input$reporting_var)
    }
    else if (input$ReportStyle != "style_time"){
      baseDF <- filtered_data()[,c(input$reporting_var, input$grouping_var)]
    }
    else if (input$grouping_var == "NoGroup" & input$ReportStyle == "style_time"){
      baseDF <- filtered_data()[,c("DateTime")]
    }
    else if (input$ReportStyle == "style_time"){
      baseDF <- filtered_data()[,c("DateTime", input$grouping_var)]
    }
    
    # DF that will be displayed
    
    # Table for boxplot (no groups)
    if(input$ReportStyle == "style_bar" & input$grouping_var == "NoGroup"){
      
      final_table <- baseDF %>%
        group_by_(input$reporting_var) %>%
        summarize(Total = n()) %>%
        arrange(-Total)
    }
    
    # Table for boxplot (groups)
    if(input$ReportStyle == "style_bar" & input$grouping_var != "NoGroup"){
      
      totals1 <- baseDF %>%
        group_by_(input$reporting_var) %>%
        summarize(Total = n()) %>%
        arrange(-Total)
      
      totals2 <- baseDF %>%
        group_by_(input$reporting_var, input$grouping_var) %>%
        summarise(Total = n()) %>%
        arrange(-Total) %>%
        spread(key = input$grouping_var, value = Total) %>%
        mutate_all(funs(replace_na(.,0)))
      
      final_table <- left_join(totals2, totals1, by = input$reporting_var) %>%
        arrange(-Total)
    }
    
    # Table for percentages (no groups)
    if(input$ReportStyle == "style_percentage" & input$grouping_var == "NoGroup"){
      
      final_table <- baseDF %>%
        group_by_(input$reporting_var) %>%
        summarize(Total = n()) %>%
        mutate(Total = round((Total/sum(Total)) * 100, 2)) %>%
        arrange(-Total)
    }
    
    # Table for percentages (groups)
    if(input$ReportStyle == "style_percentage" & input$grouping_var != "NoGroup"){
      
      totals1 <- baseDF %>%
        group_by_(input$reporting_var) %>%
        summarize(Total = n()) %>%
        mutate(Total = round((Total/sum(Total)) * 100, 2)) %>%
        arrange(-Total)
      
      totals2 <- baseDF %>%
        group_by_(input$reporting_var, input$grouping_var) %>%
        summarise(Total = n()) %>%
        mutate(Total = round((Total/sum(Total)) * 100, 2)) %>%
        arrange(-Total) %>%
        spread(key = input$grouping_var, value = Total) %>%
        mutate_all(funs(replace_na(.,0)))
      
      final_table <- left_join(totals2, totals1, by = input$reporting_var) %>%
        arrange(-Total)
    }
    
    # Table for Time/Date (Time)
    if (input$ReportStyle == "style_time" & input$reporting_timedate == "time_time"){
      
      if (input$grouping_var == "NoGroup"){
        final_table <- filtered_data() %>%
          mutate(DateTime = round_date(DateTime, "15 minutes")) %>%
          mutate(DateTime = format(DateTime, "%H:%M")) %>%
          group_by(DateTime) %>%
          summarise(Total = n())
      }
      else {
        final_table <- filtered_data() %>%
          mutate(DateTime = round_date(DateTime, "15 minutes")) %>%
          mutate(DateTime = format(DateTime, "%H:%M")) %>%
          group_by_("DateTime", input$grouping_var) %>%
          summarise(Total = n())
      }
    }
    
    # Table for Time/Date (Day)
    if (input$ReportStyle == "style_time" & input$reporting_timedate == "time_day"){
      
      if (input$grouping_var == "NoGroup"){
        final_table <- filtered_data() %>%
          mutate(DateTime = format(DateTime, "%m-%d")) %>%
          group_by(DateTime) %>%
          summarise(Total = n())
      }
      else {
        final_table <- filtered_data() %>%
          mutate(DateTime = format(DateTime, "%m-%d")) %>%
          group_by_("DateTime", input$grouping_var) %>%
          summarise(Total = n())
      }
    }
    
    # Table for Time/Date (Day of Week)
    if (input$ReportStyle == "style_time" & input$reporting_timedate == "time_week"){
      
      if (input$grouping_var == "NoGroup"){
        final_table <- filtered_data() %>%
          mutate(DateTime = wday(DateTime, label = TRUE)) %>%
          group_by(DateTime) %>%
          summarise(Total = n())
      }
      else {
        final_table <- filtered_data() %>%
          mutate(DateTime = wday(DateTime, label = TRUE)) %>%
          group_by_("DateTime", input$grouping_var) %>%
          summarise(Total = n())
      }
    }
    
    # Table for Time/Date (Month)
    if (input$ReportStyle == "style_time" & input$reporting_timedate == "time_month"){
      
      if (input$grouping_var == "NoGroup"){
        final_table <- filtered_data() %>%
          mutate(DateTime = format(DateTime, "%Y-%m")) %>%
          group_by(DateTime) %>%
          summarise(Total = n())
      }
      else {
        final_table <- filtered_data() %>%
          mutate(DateTime = format(DateTime, "%Y-%m")) %>%
          group_by_("DateTime", input$grouping_var) %>%
          summarise(Total = n())
      }
    }
    
    # Table for Time/Date (Year)
    if (input$ReportStyle == "style_time" & input$reporting_timedate == "time_year"){
      
      if (input$grouping_var == "NoGroup"){
        final_table <- filtered_data() %>%
          mutate(DateTime = format(DateTime, "%Y")) %>%
          group_by(DateTime) %>%
          summarise(Total = n())
      }
      else {
        final_table <- filtered_data() %>%
          mutate(DateTime = format(DateTime, "%Y")) %>%
          group_by_("DateTime", input$grouping_var) %>%
          summarise(Total = n())
      }
    }
    
    
    # Declaring final table
    final_table
  })
  
  # Plot Output
  main_plot <- reactive({
    
    # Counts - Box Plot
    if (input$ReportStyle == "style_bar"){
      if (input$grouping_var == "NoGroup"){
        Mplot <- filtered_data() %>%
          ggplot(aes_string(x = input$reporting_var, fill = input$reporting_var)) +
          geom_bar() +
          coord_flip() +
          theme(legend.position = "none")
      }
      else {
        Mplot <- filtered_data() %>%
          ggplot(aes_string(x = input$reporting_var, fill = input$grouping_var)) +
          geom_bar() +
          coord_flip() +
          theme_minimal()
      }
    }
    
    # Percentages - Proportions Bar Plot
    if (input$ReportStyle == "style_percentage"){
      if (input$grouping_var == "NoGroup"){
        Mplot <- main_table() %>%
          ggplot(aes(x = "", y = Total)) +
          geom_bar(aes_string(fill = input$reporting_var), stat = "identity") +
          coord_flip()
      }
      else {
        Mplot <- filtered_data() %>%
          ggplot(aes_string(x = input$reporting_var, fill = input$grouping_var)) +
          geom_bar(position = "fill") +
          coord_flip() +
          theme_minimal()
      }
    }
    
    # Time/Date - Graphs
    if (input$ReportStyle == "style_time" & input$graph_timedate == "Line Plot"){
      #Line Plots
      if (input$grouping_var == "NoGroup"){
        Mplot <- main_table() %>%
          ggplot(aes(x = DateTime, y = Total, group = 1)) +
          geom_line() +
          geom_point()
      }
      else {
        Mplot <- main_table() %>%
          ggplot(aes_string(x = "DateTime", y = "Total", group = 1, color = input$grouping_var)) +
          geom_line() +
          geom_point()
      }
    }
    else if (input$ReportStyle == "style_time" & input$graph_timedate == "Bar Graph"){
      # Bar Charts
      if (input$grouping_var == "NoGroup"){
        Mplot <- main_table() %>%
          ggplot(aes(x = DateTime, y = Total)) +
          geom_bar(stat = "identity")
      }
      else {
        Mplot <- filtered_data() %>%
          mutate(DateTime = case_when(input$reporting_timedate == "time_time" ~ round_date(DateTime, "15 minutes"),
                                      input$reporting_timedate == "time_day" ~ DateTime,
                                      input$reporting_timedate == "time_week" ~  DateTime,
                                      input$reporting_timedate == "time_month" ~ DateTime,
                                      input$reporting_timedate == "time_year" ~ DateTime
                                      )
                 ) %>%
          mutate(DateTime = case_when(input$reporting_timedate == "time_time" ~ format(DateTime, "%H:%M"),
                                      input$reporting_timedate == "time_day" ~ format(DateTime, "%m-%d"),
                                      input$reporting_timedate == "time_week" ~  wday(DateTime, label = TRUE),
                                      input$reporting_timedate == "time_month" ~ format(DateTime, "%Y-%m"),
                                      input$reporting_timedate == "time_year" ~ format(DateTime, "%Y")
                                      )
                ) %>%
        ggplot(aes_string(x = "DateTime", fill = input$grouping_var)) +
          geom_bar(stat = "count")
        
      }
    }
    
    # Time/Date - Line + Point Plots
    # if (input$ReportStyle == "style_time"){
    #   if (input$reporting_timedate == "time_time"){
    #     if (input$grouping_var == "NoGroup"){
    #       
    #     }
    #     else{
    #       
    #     }
    #   }
    #   else if (input$reporting_timedate == "time_day"){
    #     if (input$grouping_var == "NoGroup"){
    #       
    #     }
    #     else{
    #       
    #     }
    #   }
    #   else if (input$reporting_timedate == "time_week"){
    #     if (input$grouping_var == "NoGroup"){
    #       
    #     }
    #     else{
    #       
    #     }
    #   }
    #   else if (input$reporting_timedate == "time_month"){
    #     if (input$grouping_var == "NoGroup"){
    #       
    #     }
    #     else{
    #       
    #     }
    #   }
    #   else if (input$reporting_timedate == "time_year"){
    #     if (input$grouping_var == "NoGroup"){
    #       
    #     }
    #     else{
    #       
    #     }
    #   }
    # }
  
      
    # Declares Final Plot
    Mplot  
  })
  
  # Observables
  
  # Reset Filters
  observeEvent(input$reset_filters, {
    reset("div_filters")
  })
  
  # Output Definitions
  
  # Main plot 
  output$boxplot <- renderPlotly({
    ggplotly(main_plot())
    })
  
  output$nicetable <- renderDataTable( server = FALSE,
    main_table(), 
    extensions = "Buttons",
    options = list(
      dom = 'Bfrtip', buttons = 
        list(list(extend = 'collection', 
             buttons = c('csv', 'excel', 'pdf', 'copy', 'print'),
             text = 'Download')
        )
      )
    )
    
  
}

# Shiny App Object

shinyApp(ui = ui, server = server)
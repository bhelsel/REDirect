ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "MOVEIDD EMA Dashboard"),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        "Compliance Overview",
        tabName = "compliance",
        icon = shiny::icon("chart-bar")
      ),
      shinydashboard::menuItem(
        "Upcoming Prompts",
        tabName = "prompts",
        icon = shiny::icon("clock")
      ),
      shinydashboard::menuItem(
        "Participant Details",
        tabName = "details",
        icon = shiny::icon("user")
      )
    )
  ),

  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      # Compliance tab
      shinydashboard::tabItem(
        tabName = "compliance",
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("total_participants"),
          shinydashboard::valueBoxOutput("overall_compliance"),
          shinydashboard::valueBoxOutput("active_participants")
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Compliance by Participant and Phase",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            shiny::plotOutput("compliance_plot", height = "500px")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Compliance Summary Table",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            DT::DTOutput("compliance_table")
          )
        )
      ),

      # Upcoming prompts tab
      shinydashboard::tabItem(
        tabName = "prompts",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Filter Options",
            width = 12,
            status = "warning",
            shiny::selectInput(
              "participant_filter",
              "Select Participant:",
              choices = NULL,
              selected = NULL
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Upcoming Prompts Schedule",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DT::DTOutput("upcoming_prompts_table")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Prompt Timeline (7 Days Past - 3 Days Future)",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            shiny::plotOutput("prompt_timeline", height = "400px")
          )
        )
      ),

      # Participant details tab
      shinydashboard::tabItem(
        tabName = "details",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Select Participant",
            width = 12,
            status = "warning",
            shiny::selectInput(
              "participant_detail",
              "Participant ID:",
              choices = NULL
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Participant Progress",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            shiny::plotOutput("participant_progress", height = "400px")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Response History",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            DT::DTOutput("response_history")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Load and process data
  data <- shiny::reactive({
    # Replace this path with your CSV file path
    df <- REDirect::export_reports("moveid", 98993)
    # Filter out intake events and keep only EMA data
    df <- df |>
      dplyr::filter(
        redcap_repeat_instrument == "ema" &
          redcap_event_name != "familiarization_arm_1"
      ) |>
      dplyr::mutate(
        prompt_time = lubridate::ymd_hm(prompt_time, tz = "America/Chicago"),
        survey_start_time = lubridate::ymd_hm(
          survey_start_time,
          tz = "America/Chicago"
        ),
        responded = ifelse(!is.na(survey_start_time) & ema_complete == 2, 1, 0),
        phase = dplyr::case_when(
          grepl("familiarization", redcap_event_name) ~ "Familiarization",
          grepl("burst_1", redcap_event_name) ~ "Burst 1",
          grepl("burst_2", redcap_event_name) ~ "Burst 2",
          TRUE ~ "Other"
        )
      )
    return(df)
  })
  # # Update participant filters
  shiny::observe({
    participants <- unique(data()$record_id)
    shiny::updateSelectInput(
      session,
      "participant_filter",
      choices = c("All", participants),
      selected = "All"
    )
    shiny::updateSelectInput(
      session,
      "participant_detail",
      choices = participants,
      selected = participants[1]
    )
  })
  # # Value boxes
  output$total_participants <- shinydashboard::renderValueBox({
    n_participants <- length(unique(data()$record_id))
    shinydashboard::valueBox(
      n_participants,
      "Total Participants",
      icon = shiny::icon("users"),
      color = "blue"
    )
  })

  compliance <- shiny::reactive({
    comp <- data() |>
      dplyr::filter(prompt_time < Sys.time()) |>
      dplyr::group_by(record_id) |>
      dplyr::summarise(
        rate = mean(responded, na.rm = TRUE) * 100,
        .groups = "drop"
      )
    mean(comp$rate)
  })

  output$overall_compliance <- shinydashboard::renderValueBox({
    shiny::req(compliance())
    shinydashboard::valueBox(
      paste0(round(compliance(), 1), "%"),
      "Overall Compliance",
      icon = shiny::icon("check-circle"),
      color = if (compliance() >= 80) {
        "green"
      } else if (compliance() >= 60) {
        "yellow"
      } else {
        "red"
      }
    )
  })

  output$active_participants <- shinydashboard::renderValueBox({
    current_time <- Sys.time()
    active <- data() |>
      dplyr::filter(prompt_time >= current_time - lubridate::days(7)) |>
      dplyr::summarise(n = dplyr::n_distinct(record_id)) |>
      dplyr::pull(n)
    shinydashboard::valueBox(
      active,
      "Active (Last 7 Days)",
      icon = shiny::icon("user-clock"),
      color = "purple"
    )
  })
  # Compliance plot
  output$compliance_plot <- shiny::renderPlot({
    compliance_data <- data() |>
      dplyr::filter(prompt_time < Sys.time()) |>
      dplyr::group_by(record_id, phase) |>
      dplyr::summarise(
        total_prompts = dplyr::n(),
        completed = sum(responded, na.rm = TRUE),
        compliance_rate = (completed / total_prompts) * 100,
        .groups = "drop"
      ) |>
      dplyr::mutate(
        phase = factor(
          phase,
          levels = c("Familiarization", "Burst 1", "Burst 2")
        )
      )
    ggplot2::ggplot(
      compliance_data,
      ggplot2::aes(x = as.factor(record_id), y = compliance_rate, fill = phase)
    ) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::geom_hline(
        yintercept = compliance(),
        linetype = "dashed",
        color = "red",
        size = 1
      ) +
      ggplot2::labs(
        x = "Participant ID",
        y = "Compliance Rate (%)",
        fill = "Phase"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.text = ggplot2::element_text(size = 14)
      ) +
      ggplot2::scale_fill_manual(values = MetBrewer::met.brewer("Java", 2)) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, 100, 10),
        limits = c(0, 100)
      )
  })
  # Compliance table
  output$compliance_table <- DT::renderDT({
    compliance_summary <- data() |>
      dplyr::mutate(Scheduled = prompt_time > Sys.time()) |>
      dplyr::group_by(record_id) |>
      dplyr::summarise(
        Prompts = dplyr::n(),
        Scheduled = sum(Scheduled, na.rm = TRUE),
        Completed = sum(responded, na.rm = TRUE),
        Missed = sum(responded == 0 & prompt_time < Sys.time(), na.rm = TRUE),
        Compliance = Completed / (Prompts - Scheduled),
        `Last Response` = format(
          max(survey_start_time, na.rm = TRUE),
          "%m/%d/%Y %H:%M:%S"
        ),
        .groups = "drop"
      ) |>
      dplyr::filter(`Last Response` != -Inf) |>
      dplyr::arrange(dplyr::desc(Completed))
    DT::datatable(
      compliance_summary,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    ) |>
      DT::formatStyle(
        "Compliance",
        backgroundColor = DT::styleInterval(
          c(0.6, 0.8),
          c('#f8d7da', '#fff3cd', '#d4edda')
        )
      ) |>
      DT::formatPercentage("Compliance", digits = 1)
  })
  # Upcoming prompts table
  output$upcoming_prompts_table <- DT::renderDT({
    current_time <- Sys.time()
    upcoming <- data() |>
      #dplyr::filter(prompt_time >= current_time) |>
      dplyr::arrange(prompt_time)

    if (input$participant_filter != "All") {
      upcoming <- upcoming |>
        dplyr::filter(record_id == input$participant_filter)
    }
    upcoming_display <- upcoming |>
      dplyr::select(
        record_id,
        phase,
        redcap_repeat_instance,
        days,
        prompts,
        prompt_time,
        responded
      ) |>
      dplyr::mutate(
        Status = ifelse(responded == 1, "Completed", "Pending"),
        Time_Until = as.numeric(
          difftime(
            prompt_time,
            current_time,
            units = "hours"
          )
        ),
        Status = ifelse(
          Status == "Pending" & Time_Until < -1,
          "Late",
          Status
        ),
        prompt_time = format(prompt_time, "%m/%d/%Y %H:%M:%S")
      ) |>
      dplyr::filter(Time_Until > -3) |>
      dplyr::select(
        `Participant ID` = record_id,
        Phase = phase,
        `Prompt # in Phase` = redcap_repeat_instance,
        Day = days,
        `Prompt # in Day` = prompts,
        `Scheduled Time` = prompt_time,
        `Hours Until` = Time_Until,
        Status
      )
    DT::datatable(
      upcoming_display,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    ) |>
      DT::formatRound('Hours Until', 1) |>
      DT::formatStyle(
        'Status',
        backgroundColor = DT::styleEqual(
          c('Completed', 'Pending', "Late"),
          c('#d4edda', '#fff3cd', "#f8d7da")
        )
      )
  })
  # Prompt timeline
  output$prompt_timeline <- shiny::renderPlot({
    current_time <- Sys.time()
    timeline_data <- data() |>
      dplyr::filter(
        prompt_time >= current_time - lubridate::days(3) &
          prompt_time <= current_time + lubridate::days(3)
      )
    if (input$participant_filter != "All") {
      timeline_data <- timeline_data |>
        dplyr::filter(record_id == input$participant_filter)
    }
    ggplot2::ggplot(
      timeline_data,
      ggplot2::aes(
        x = prompt_time,
        y = as.factor(record_id),
        color = factor(responded)
      )
    ) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_vline(
        xintercept = current_time,
        linetype = "dashed",
        color = "blue",
        size = 1
      ) +
      ggplot2::annotate(
        "text",
        x = current_time,
        y = 0.5,
        label = "Now",
        color = "blue",
        vjust = -1
      ) +
      ggplot2::labs(
        x = "Date/Time",
        y = "Participant ID",
        color = "Status"
      ) +
      ggplot2::scale_color_manual(
        values = c("0" = "#e74c3c", "1" = "#2ecc71"),
        labels = c("0" = "Missed", "1" = "Completed")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15)),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.text = ggplot2::element_text(size = 14)
      )
  })
  # Participant progress
  output$participant_progress <- shiny::renderPlot({
    participant_data <- data() |>
      dplyr::filter(
        record_id == input$participant_detail & prompt_time < Sys.time()
      ) |>
      dplyr::arrange(prompt_time) |>
      dplyr::mutate(
        cumulative_compliance = cumsum(responded) / dplyr::row_number() * 100
      )

    ggplot2::ggplot(
      participant_data,
      ggplot2::aes(x = prompt_time, y = cumulative_compliance)
    ) +
      ggplot2::geom_line(color = "#3498db", size = 1.5) +
      ggplot2::geom_point(ggplot2::aes(color = factor(responded)), size = 3) +
      ggplot2::geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
      ggplot2::labs(
        title = paste(
          "Cumulative Compliance for Participant",
          input$participant_detail
        ),
        x = "Date",
        y = "Cumulative Compliance Rate (%)",
        color = "Response"
      ) +
      ggplot2::scale_color_manual(
        values = c("0" = "#e74c3c", "1" = "#2ecc71"),
        labels = c("0" = "Missed", "1" = "Completed")
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, 100, 10),
        limits = c(0, 100)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15)),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.text = ggplot2::element_text(size = 14)
      )
  })
  # Response history
  output$response_history <- DT::renderDT({
    history <- data() |>
      dplyr::filter(record_id == input$participant_detail) |>
      dplyr::arrange(prompt_time) |>
      dplyr::mutate(
        response_delay = as.numeric(
          difftime(
            survey_start_time,
            prompt_time,
            units = "mins"
          )
        ),
        responded = dplyr::case_when(
          responded == 1 & response_delay <= 60 ~ "Completed",
          responded == 1 & response_delay > 60 ~ "Missed",
          responded == 0 & prompt_time > Sys.time() ~ "Pending",
          TRUE ~ "Missed"
        ),
        prompt_time = format(prompt_time, "%m/%d/%Y %H:%M"),
        survey_start_time = format(survey_start_time, "%m/%d/%Y %H:%M")
      ) |>
      dplyr::select(
        Phase = phase,
        Day = days,
        `Prompt #` = prompts,
        `Scheduled` = prompt_time,
        `Responded` = survey_start_time,
        `Delay (min)` = response_delay,
        Status = responded
      )

    DT::datatable(
      history,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    ) |>
      DT::formatRound('Delay (min)', 0) |>
      DT::formatStyle(
        'Status',
        backgroundColor = DT::styleEqual(
          c('Completed', 'Pending', 'Missed'),
          c('#d4edda', '#fff3cd', "#f8d7da")
        )
      )
  })
}

# Run the app
shiny::shinyApp(ui = ui, server = server)

# Task 4: Gender and POSt

genderUI <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(ns("display"), "Show", c("Percentage", "Count")),
    selectInput(ns("palette"), "Bar colours", c("Set2", "Dark2")),
    numericInput(
      ns("y_max"), "Y-axis maximum (0 = automatic, 0–100 for percentages)",
      value = 0, min = 0, step = 1
    ),
    plotOutput(ns("plot"), height = "400px"),
    textOutput(ns("interpretation"), container = tags$p)
  )
}

# Server

genderServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # Use the same demographic filters as the other graphs.
    plot_data <- reactive({
      df <- filtered_data()
      df <- df[
        !is.na(df$gender_identity) & !is.na(df$post_group),
        , drop = FALSE
      ]

      validate(need(
        nrow(df) > 0, "No responses match these filters."
      ))
      df
    })

    output$plot <- renderPlot({
      df <- plot_data()
      upper <- req(input$y_max)
      validate(need(
        is.finite(upper) && upper >= 0,
        "Enter 0 or a positive number for the y-axis maximum."
      ))

      # Percentages use the total within each gender after filtering.
      if (input$display == "Percentage") {
        position <- "fill"
        y_label <- "Percentage within gender"
        y_scale <- scale_y_continuous(labels = scales::label_percent())
        validate(need(
          upper <= 100, "Enter a percentage from 0 to 100."
        ))
        upper <- upper / 100
      } else {
        position <- "stack"
        y_label <- "Number of respondents"
        y_scale <- scale_y_continuous()
      }

      p <- ggplot(
        df, aes(x = gender_identity, fill = post_group)
      ) +
        geom_bar(position = position) +
        scale_fill_brewer(
          palette = input$palette, limits = POST_GROUP_LEVELS
        ) +
        y_scale +
        labs(x = "Gender identity", y = y_label, fill = "POSt category") +
        theme_minimal() +
        theme(legend.position = "bottom")

      if (upper > 0) {
        p <- p + coord_cartesian(ylim = c(0, upper), expand = FALSE)
      }
      p
    })

    # Start with the full axis whenever the unit changes.
    observeEvent(input$display, {
      updateNumericInput(session, "y_max", value = 0)
    }, ignoreInit = TRUE)

    output$interpretation <- renderText({
      df <- plot_data()
      totals <- table(df$gender_identity)
      group_sizes <- paste(names(totals), totals, sep = ": ", collapse = ", ")

      paste0(
        "Responses shown by gender (", group_sizes, "). ",
        "In percentage mode, each bar shows how POSt choices are split within that gender group.",
        if (any(totals < 5)) " A single response can have a big effect in a small group."
      )
    })
  })
}

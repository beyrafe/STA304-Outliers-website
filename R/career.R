# ============================================================
# STA304 Shiny Module - Task 1: Career & Salary Influence
# File: R/career.R
# ============================================================

library(shiny)
library(ggplot2)

# Single-colour options: every view displays one box per POSt category.
plot_colours <- c(
  "Blue" = "#377EB8",
  "Teal" = "#009E73",
  "Purple" = "#984EA3",
  "Orange" = "#E69F00",
  "Red" = "#D55E00",
  "Grey" = "#777777"
)

# ----------------------------- UI MODULE -----------------------------

careerUI <- function(id) {

  ns <- NS(id)

  tagList(

    sidebarLayout(

      sidebarPanel(
        h4("Career & Salary plot controls"),

        selectInput(
          inputId = ns("view"),
          label = "Choose what to display:",
          choices = c(
            "Career Influence" = "career",
            "Salary Influence" = "salary",
            "Combined Career & Salary Mean" = "combined"
          ),
          selected = "combined"
        ),

        selectInput(
          inputId = ns("plot_colour"),
          label = "Choose plot colour:",
          choices = plot_colours,
          selected = "#377EB8"
        ),

        sliderInput(
          inputId = ns("y_range"),
          label = "Adjust rating-axis limits:",
          min = 1,
          max = 10,
          value = c(1, 10),
          step = 1,
          sep = ""
        ),

        helpText(
          "Ratings range from 1 (least influential) to 10 ",
          "(most influential). In the combined view, each student's ",
          "career and salary ratings are averaged to produce one rating."
        )
      ),

      mainPanel(
        h3("Figure 1: Career & Salary Influence"),

        plotOutput(
          outputId = ns("influence_plot"),
          height = "600px"
        ),

        hr(),

        h4("Summary statistics"),
        tableOutput(ns("summary_table")),

        hr(),

        h4("Interpretation"),
        textOutput(ns("interpretation")),

        hr(),

        p(
          "Note: These results describe the students who responded ",
          "to the survey. They should not automatically be treated ",
          "as representative of all students."
        )
      )
    )
  )
}

# --------------------------- SERVER MODULE ---------------------------

careerServer <- function(id, filtered_data) {

  moduleServer(id, function(input, output, session) {

    # Prepare one rating per student for the selected view.
    selected_data <- reactive({

      # The main application supplies data after its demographic filters.
      data <- filtered_data()

      if (input$view == "career") {

        result <- data.frame(
          post_group = data$post_group,
          Rating = data$career_influence
        )

        factor_name <- "Career Influence"

      } else if (input$view == "salary") {

        result <- data.frame(
          post_group = data$post_group,
          Rating = data$salary_influence
        )

        factor_name <- "Salary Influence"

      } else {

        # Average each student's career and salary ratings.
        # This creates ONE combined rating per student.
        result <- data.frame(
          post_group = data$post_group,
          Rating = rowMeans(
            data.frame(
              career = data$career_influence,
              salary = data$salary_influence
            ),
            na.rm = FALSE
          )
        )

        factor_name <- "Combined Mean Influence"
      }

      result <- result[
        !is.na(result$Rating) &
          is.finite(result$Rating) &
          result$Rating >= 1 &
          result$Rating <= 10 &
          !is.na(result$post_group),
      ]

      # Preserve the post_group levels/order supplied by the main app.
      result$post_group <- droplevels(factor(result$post_group))
      result$Factor <- rep(factor_name, nrow(result))

      result
    })

    # One boxplot per POSt group in every view.
    output$influence_plot <- renderPlot({

      plot_data <- selected_data()

      validate(
        need(nrow(plot_data) > 0, "No valid responses are available.")
      )

      validate(
        need(
          input$y_range[1] < input$y_range[2],
          "The upper rating limit must be greater than the lower limit."
        )
      )

      plot_title <- if (input$view == "career") {
        "Career Influence by POSt Category"
      } else if (input$view == "salary") {
        "Salary Influence by POSt Category"
      } else {
        "Combined Career and Salary Influence by POSt Category"
      }

      x_title <- if (input$view == "combined") {
        "Combined Mean Influence Rating (1-10)"
      } else {
        "Influence Rating (1-10)"
      }

      ggplot(
        plot_data,
        aes(
          x = Rating,
          y = post_group
        )
      ) +
        geom_boxplot(
          fill = input$plot_colour,
          colour = "black",
          alpha = 0.8,
          width = 0.65,
          outlier.size = 2
        ) +
        scale_x_continuous(
          breaks = 1:10
        ) +
        coord_cartesian(
          xlim = input$y_range
        ) +
        labs(
          title = plot_title,
          x = x_title,
          y = "POSt Category"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(
            face = "bold",
            size = 16
          ),
          panel.grid.minor = element_blank()
        )
    })

    # Concise interpretation of the selected rating and POSt comparison.
    output$interpretation <- renderText({

      plot_data <- selected_data()

      validate(
        need(nrow(plot_data) > 0, "There are no valid responses to interpret.")
      )

      overall <- plot_data$Rating
      overall_mean <- mean(overall)
      overall_median <- median(overall)

      category_groups <- split(
        plot_data$Rating,
        plot_data$post_group,
        drop = TRUE
      )

      category_medians <- vapply(
        category_groups,
        median,
        numeric(1),
        na.rm = TRUE
      )

      highest_category <- names(category_medians)[which.max(category_medians)]
      lowest_category <- names(category_medians)[which.min(category_medians)]
      highest_median <- max(category_medians)
      lowest_median <- min(category_medians)

      paste0(
        unique(plot_data$Factor), " received an overall mean rating of ",
        round(overall_mean, 2), " and a median rating of ",
        round(overall_median, 2), " out of 10 across ",
        length(overall), " responses. ",
        highest_category, " had the highest median rating (",
        round(highest_median, 2), "), while ",
        lowest_category, " had the lowest (",
        round(lowest_median, 2), ")."
      )
    })

    # Summary statistics for the currently selected rating.
    output$summary_table <- renderTable({

      plot_data <- selected_data()

      validate(
        need(nrow(plot_data) > 0, "No summary statistics are available.")
      )

      category_groups <- split(
        plot_data$Rating,
        plot_data$post_group,
        drop = TRUE
      )

      do.call(
        rbind,
        lapply(names(category_groups), function(category) {

          x <- category_groups[[category]]

          data.frame(
            `POSt Category` = category,
            `Number of responses` = length(x),
            Mean = round(mean(x), 2),
            Median = round(median(x), 2),
            `Standard deviation` = round(sd(x), 2),
            `First quartile` = round(
              as.numeric(quantile(x, 0.25, names = FALSE)), 2
            ),
            `Third quartile` = round(
              as.numeric(quantile(x, 0.75, names = FALSE)), 2
            ),
            IQR = round(IQR(x), 2),
            check.names = FALSE
          )
        })
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s")

  })
}

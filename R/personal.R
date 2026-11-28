personalUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("variable"), "Influence to display:",
      choices = c(
        "Combined influence" = "personal_family_friend_mean",
        "Personal interest" = "personal_interest",
        "Family influence" = "family_influence",
        "Friend influence" = "friend_influence"
      )
    ),
    shiny::selectInput(
      ns("colour"), "Boxplot colour:",
      choices = c(
        "Original grey" = "#eceff1",
        "Blue" = "#b3d9f2",
        "Green" = "#b9e3cc",
        "Orange" = "#f6cf9f"
      )
    ),
    shiny::sliderInput(
      ns("rating_range"), "Displayed rating range:",
      min = 1, max = 10, value = c(1, 10), step = 1
    ),
    shiny::plotOutput(ns("plot"), height = "480px"),
    shiny::textOutput(ns("interpretation")),
    shiny::tags$div(
      class = "personal-summary-section",
      shiny::tags$style(shiny::HTML("
        .personal-summary-section { margin-top: 24px; }
        .personal-summary-section h3 { margin-bottom: 12px; }
        .personal-summary-section .table {
          width: 100%;
          border-collapse: collapse;
          color: #303846;
          margin-bottom: 0;
        }
        .personal-summary-section .table > thead > tr > th,
        .personal-summary-section .table > tbody > tr > td {
          padding: 12px 16px;
          border: 0;
          box-shadow: none;
          background-color: #f9f8f4;
          vertical-align: middle;
        }
        .personal-summary-section .table > thead > tr > th {
          font-weight: 700;
          border-bottom: 1px solid #303846;
          white-space: nowrap;
        }
        .personal-summary-section .table > tbody > tr:nth-child(even) > td {
          background-color: #ebeeeb;
        }
        .personal-summary-section .table > tbody > tr > td:first-child {
          white-space: nowrap;
        }
        .personal-summary-section .table-wrap { overflow-x: auto; }
      ")),
      shiny::tags$h3("Summary statistics"),
      shiny::tags$div(
        class = "table-wrap",
        shiny::tableOutput(ns("summary_table"))
      )
    )
  )
}

personalServer <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_data <- shiny::reactive({
      df <- filtered_data()
      shiny::req(is.data.frame(df))
      required <- c("post_group", "personal_interest", "family_influence", "friend_influence")
      missing_cols <- setdiff(required, names(df))
      shiny::validate(shiny::need(length(missing_cols) == 0,
                                  paste("Dataset is missing:", paste(missing_cols, collapse = ", "))))

      if (!"personal_family_friend_mean" %in% names(df)) {
        df$personal_family_friend_mean <- (
          df$personal_interest + df$family_influence + df$friend_influence
        ) / 3
      }
      chosen <- input$variable
      shiny::req(chosen)
      df$display_rating <- suppressWarnings(as.numeric(as.character(df[[chosen]])))
      df <- df[!is.na(df$post_group) & is.finite(df$display_rating), , drop = FALSE]
      shiny::validate(shiny::need(nrow(df) > 0, "No valid responses match these filters."))
      df
    })

    output$plot <- shiny::renderPlot({
      df <- plot_data()
      name <- switch(input$variable,
                     personal_family_friend_mean = "Combined Personal, Family, and Friend",
                     personal_interest = "Personal Interest",
                     family_influence = "Family",
                     friend_influence = "Friend"
      )

      combined_influence_plot <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = display_rating, y = post_group)
      ) +
        ggplot2::geom_boxplot(
          width = 0.55,
          fill = input$colour,
          colour = "#4a4a4a",
          linewidth = 0.6,
          outlier.colour = "#555555",
          outlier.size = 1.7
        ) +
        ggplot2::scale_x_continuous(breaks = 1:10) +
        ggplot2::coord_cartesian(xlim = input$rating_range) +
        ggplot2::labs(
          title = paste(name, "Influence by POSt Category"),
          x = "Influence Rating (1–10)",
          y = "POSt Category"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 14, face = "bold", hjust = 0.5,
            margin = ggplot2::margin(b = 12)
          ),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          axis.title = ggplot2::element_text(size = 11)
        )

      combined_influence_plot
    })

    # Calculate ANOVA using the selected variable and filtered data.
    anova_result <- shiny::reactive({
      df <- plot_data()
      df$post_group <- droplevels(as.factor(df$post_group))
      group_counts <- table(df$post_group)

      if (length(group_counts) < 2 || any(group_counts < 2)) {
        return(NULL)
      }

      model <- stats::aov(display_rating ~ post_group, data = df)
      result <- summary(model)[[1]]
      f_value <- result[["F value"]][1]
      p_value <- result[["Pr(>F)"]][1]

      if (!is.finite(f_value) || !is.finite(p_value)) {
        return(NULL)
      }
      result
    })

    # Display descriptive statistics for the selected influence by POSt.
    output$summary_table <- shiny::renderTable({
      df <- plot_data()
      groups <- as.character(df$post_group)
      preferred_order <- c("Statistics", "Computer Science", "Mathematics", "Other")
      present <- unique(groups)
      group_order <- c(intersect(preferred_order, present),
                       setdiff(present, preferred_order))

      fmt <- function(x) {
        if (is.finite(x)) sprintf("%.2f", x) else ""
      }

      rows <- lapply(group_order, function(g) {
        ratings <- df$display_rating[groups == g]
        quartiles <- stats::quantile(ratings, probs = c(0.25, 0.75),
                                     names = FALSE, type = 7)
        data.frame(
          "POSt Category" = g,
          "Number of responses" = as.character(length(ratings)),
          "Mean" = fmt(mean(ratings)),
          "Median" = fmt(stats::median(ratings)),
          "Standard deviation" = fmt(stats::sd(ratings)),
          "First quartile" = fmt(quartiles[1]),
          "Third quartile" = fmt(quartiles[2]),
          "IQR" = fmt(stats::IQR(ratings, type = 7)),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })

      do.call(rbind, rows)
    },
    striped = TRUE,
    bordered = FALSE,
    hover = FALSE,
    rownames = FALSE,
    align = "lrrrrrrr",
    na = "",
    width = "100%")

    output$interpretation <- shiny::renderText({
      df <- plot_data()

      group_means <- aggregate(
        display_rating ~ post_group,
        data = df,
        FUN = mean
      )

      top <- group_means[
        which.max(group_means$display_rating), ,
        drop = FALSE
      ]

      top_group <- as.character(top$post_group[1])

      ratings <- df$display_rating[
        as.character(df$post_group) == top_group
      ]

      # Calculate mean, median, and mode
      mean_value <- mean(ratings)
      median_value <- median(ratings)

      frequencies <- table(ratings)
      modes <- as.numeric(names(frequencies)[
        frequencies == max(frequencies)
      ])
      mode_value <- paste(sort(modes), collapse = ", ")

      variable_label <- switch(input$variable,
                               personal_family_friend_mean = "combined",
                               personal_interest = "personal interest",
                               family_influence = "family",
                               friend_influence = "friend"
      )

      # Add the ANOVA statistic, p-value, and significance conclusion.
      result <- anova_result()

      if (is.null(result)) {
        significance_text <- paste(
          "ANOVA is unavailable because group sizes are insufficient",
          "or the F-statistic cannot be estimated."
        )
      } else {
        f_value <- result[["F value"]][1]
        p_value <- result[["Pr(>F)"]][1]
        df_between <- result[["Df"]][1]
        df_residual <- result[["Df"]][2]

        p_text <- if (p_value < 0.001) {
          "p < 0.001"
        } else {
          paste0("p = ", format.pval(p_value, digits = 4, eps = 0.001))
        }

        test_statistics <- paste0(
          "F(", df_between, ", ", df_residual, ") = ",
          round(f_value, 3), ", ", p_text
        )

        if (p_value < 0.05) {
          significance_text <- paste0(
            "The one-way ANOVA found a statistically significant difference ",
            "in mean influence ratings across POSt categories (",
            test_statistics, "). At the 0.05 significance level, ",
            "we reject the null hypothesis. There is evidence that at least ",
            "one population group mean differs."
          )
        } else {
          significance_text <- paste0(
            "The one-way ANOVA did not find a statistically significant difference ",
            "in mean influence ratings across POSt categories (",
            test_statistics, "). At the 0.05 significance level, ",
            "we fail to reject the null hypothesis. There is insufficient evidence ",
            "to conclude that the population group means differ."
          )
        }
      }

      paste0(
        "There are ", nrow(df), " responses shown. ",
        top_group, " has the highest average ",
        variable_label, " influence score. ",
        "Mean: ", round(mean_value, 2), "/10; ",
        "Median: ", round(median_value, 2), "/10; ",
        if (length(modes) > 1) "Modes (tied): " else "Mode: ",
        mode_value, "/10. ",
        significance_text,
        " ANOVA results are exploratory and p-values are unadjusted."
      )
    })

  })
}

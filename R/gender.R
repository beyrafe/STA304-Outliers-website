# Task 4: Gender and POSt

genderUI <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(ns("display"), "Show", c("Percentage", "Count")),
    selectInput(ns("palette"), "Bar colours", c("Deep", "Light")),
    numericInput(
      ns("y_max"), "Y-axis maximum (0 = automatic, 0–100 for percentages)",
      value = 0, min = 0, step = 1
    ),
    plotOutput(ns("plot"), height = "400px"),
    hr(),
    h4("Interpretation"),
    textOutput(ns("interpretation"), container = tags$p),
    hr(),
    h4("Summary statistics"),
    tableOutput(ns("summary_table"))
  )
}

# Server

genderServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # Use the five POSt categories defined in the report's RQ4 analysis.
    post_levels <- c("Statistics", "Computer Science", "Mathematics", "Economics", "Other")
    # Use the same demographic filters as the other graphs.
    plot_data <- reactive({
      df <- filtered_data()
      if ("post_category" %in% names(df)) {
        post <- as.character(df$post_category)
        post[post == "Computer Science & Statistics" & !is.na(post)] <- "Computer Science"
        post[post == "Mathematics & Statistics" & !is.na(post)] <- "Mathematics"
        post[!is.na(post) & !post %in% post_levels] <- "Other"
        df$post_group <- post
      }
      df$post_group <- factor(df$post_group, levels = post_levels)
      df <- df[
        !is.na(df$gender_identity) & !is.na(df$post_group),
        , drop = FALSE
      ]

      validate(need(
        nrow(df) > 0, "No responses match these filters."
      ))
      df
    })

    counts <- reactive({
      df <- plot_data()
      table(droplevels(factor(df$gender_identity)), df$post_group)
    })

    association_test <- reactive({
      observed <- counts()
      observed <- observed[rowSums(observed) > 0, colSums(observed) > 0,
                           drop = FALSE]
      if (any(dim(observed) < 2)) {
        return("Fisher's exact test requires at least two gender groups and two POSt categories.")
      }

      result <- tryCatch(stats::fisher.test(observed, workspace = 2000000),
                         error = function(error) NULL)
      if (is.null(result)) {
        return("Fisher's exact test could not be computed for this selection.")
      }

      conclusion <- if (result$p.value < 0.05) {
        "The selected responses provide evidence of an association at the 5% significance level."
      } else {
        "The selected responses provide insufficient evidence of an association at the 5% significance level."
      }
      sprintf("Fisher's exact test of association for the responses shown (p = %.6f). %s",
              result$p.value, conclusion)
    })

    output$plot <- renderPlot({
      upper <- req(input$y_max)
      validate(need(
        is.finite(upper) && upper >= 0,
        "Enter 0 or a positive number for the y-axis maximum."
      ))

      segments <- as.data.frame(
        counts(),
        responseName = "count"
      )
      names(segments)[1:2] <- c("gender_identity", "post_group")
      segments <- segments[segments$count > 0, , drop = FALSE]

      # Use the same values for bar heights and their labels.
      if (input$display == "Percentage") {
        gender_totals <- ave(segments$count, segments$gender_identity, FUN = sum)
        segments$height <- segments$count / gender_totals
        segments$label <- scales::label_percent(accuracy = 0.1)(segments$height)
        y_label <- "Percentage within gender"
        y_scale <- scale_y_continuous(labels = scales::label_percent())
        validate(need(
          upper <= 100, "Enter a percentage from 0 to 100."
        ))
        upper <- upper / 100
      } else {
        segments$height <- segments$count
        segments$label <- as.character(segments$count)
        y_label <- "Number of respondents"
        y_scale <- scale_y_continuous()
      }

      palette <- if (req(input$palette) == "Light") {
        c("#A9E9F2", "#FFD88A", "#DFCCFF", "#CFEDA3", "#F9BDDA")
      } else {
        RColorBrewer::brewer.pal(length(post_levels), "Set2")[c(1:3, 5, 4)]
      }

      p <- ggplot(segments, aes(x = gender_identity, y = height, fill = post_group)) +
        geom_col() +
        geom_text(
          aes(label = label), position = position_stack(vjust = 0.5),
          colour = "#17212B", size = 3, show.legend = FALSE
        ) +
        scale_fill_manual(values = palette, limits = post_levels) +
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
      totals <- rowSums(counts())
      group_sizes <- paste(names(totals), totals, sep = ": ", collapse = ", ")

      paste0(
        "Responses shown by gender (", group_sizes, "). ",
        "In percentage mode, each bar shows how POSt choices are split within that gender group.",
        if (any(totals < 5)) " A single response can have a big effect in a small group.",
        " ", association_test()
      )
    })

    output$summary_table <- renderTable({
      observed <- counts()
      with_totals <- addmargins(observed)
      row_percentages <- 100 * sweep(with_totals, 1, with_totals[, ncol(with_totals)], "/")
      values <- matrix(
        sprintf("%d (%.1f%%)", with_totals, row_percentages),
        nrow = nrow(with_totals), dimnames = dimnames(with_totals)
      )
      rownames(values)[nrow(values)] <- "Total"
      colnames(values)[ncol(values)] <- "Total"
      values[nrow(values), ncol(values)] <- as.character(sum(observed))

      data.frame(
        `Gender identity` = rownames(values),
        values,
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 0)
  })
}

admissionUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
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
      min = 1, max = 10,
      value = c(1, 10), step = 1
    ),
    
    shiny::plotOutput(ns("plot"), height = "480px"),
    shiny::tags$hr(),
    shiny::tags$h4("Summary statistics"),
    shiny::tableOutput(ns("summary_table")),
    shiny::tags$hr(),
    shiny::tags$h4("Interpretation"),
    shiny::textOutput(ns("interpretation"))
  )
}


admissionServer <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    plot_data <- shiny::reactive({
      
      df <- filtered_data()
      
      shiny::req(is.data.frame(df))
      
      required <- c(
        "admitted_first_choice",
        "post_req_influence"
      )
      
      missing_cols <- setdiff(required, names(df))
      
      shiny::validate(
        shiny::need(
          length(missing_cols) == 0,
          paste(
            "Dataset is missing:",
            paste(missing_cols, collapse = ", ")
          )
        )
      )
      
      df$admitted_first_choice <- factor(
        trimws(as.character(df$admitted_first_choice)),
        levels = c("No", "Yes")
      )
      
      df$post_req_influence <- suppressWarnings(
        as.numeric(as.character(df$post_req_influence))
      )
      
      df <- df[
        !is.na(df$admitted_first_choice) &
          is.finite(df$post_req_influence) &
          df$post_req_influence >= 1 &
          df$post_req_influence <= 10,
        , drop = FALSE
      ]
      
      shiny::validate(
        shiny::need(
          nrow(df) > 0,
          "No valid responses match these filters."
        )
      )
      
      df
    })
    
    
    output$plot <- shiny::renderPlot({
      
      df <- plot_data()
      
      ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = admitted_first_choice,
          y = post_req_influence
        )
      ) +
        
        ggplot2::geom_boxplot(
          width = 0.55,
          fill = input$colour,
          colour = "#4a4a4a",
          linewidth = 0.6,
          outlier.size = 1.7
        ) +
        
        ggplot2::scale_y_continuous(
          breaks = 1:10
        ) +
        
        ggplot2::coord_cartesian(
          ylim = input$rating_range
        ) +
        
        ggplot2::labs(
          title = paste(
            "POSt Admission Requirements",
            "by First-Choice Admission Status"
          ),
          x = "Admitted to First-Choice POSt",
          y = "POSt Requirements Influence Rating (1–10)"
        ) +
        
        ggplot2::theme_minimal(base_size = 11) +
        
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 14,
            face = "bold",
            hjust = 0.5,
            margin = ggplot2::margin(b = 12)
          ),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.title = ggplot2::element_text(size = 11)
        )
    })
    
    # Welch's two-sided t-test using the currently filtered data.
    test_result <- shiny::reactive({
      df <- plot_data()
      no <- df$post_req_influence[df$admitted_first_choice == "No"]
      yes <- df$post_req_influence[df$admitted_first_choice == "Yes"]

      if (length(no) < 2 || length(yes) < 2) {
        return(NULL)
      }

      result <- tryCatch(
        stats::t.test(no, yes, alternative = "two.sided", var.equal = FALSE),
        error = function(e) NULL
      )

      if (is.null(result) ||
          !all(is.finite(c(result$statistic, result$parameter,
                           result$p.value, result$conf.int)))) {
        return(NULL)
      }

      result
    })

    # Same eight-column descriptive table as the other influence pages.
    output$summary_table <- shiny::renderTable({
      df <- plot_data()
      groups <- split(df$post_req_influence, df$admitted_first_choice, drop = TRUE)

      do.call(rbind, lapply(names(groups), function(group) {
        x <- groups[[group]]
        data.frame(
          `First-Choice Admission` = group,
          `Number of responses` = length(x),
          Mean = round(mean(x), 2),
          Median = round(median(x), 2),
          `Standard deviation` = round(stats::sd(x), 2),
          `First quartile` = round(as.numeric(stats::quantile(x, 0.25, names = FALSE)), 2),
          `Third quartile` = round(as.numeric(stats::quantile(x, 0.75, names = FALSE)), 2),
          IQR = round(stats::IQR(x), 2),
          check.names = FALSE
        )
      }))
    }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")
    
    output$interpretation <- shiny::renderText({
      
      df <- plot_data()
      
      no <- df$post_req_influence[
        df$admitted_first_choice == "No"
      ]
      
      yes <- df$post_req_influence[
        df$admitted_first_choice == "Yes"
      ]
      
      if (length(no) == 0 || length(yes) == 0) {
        return(paste0(
          "There are ", nrow(df),
          " valid responses. Both admission groups ",
          "are required for a comparison."
        ))
      }
      
      difference <- mean(no) - mean(yes)
      
      comparison <- if (difference > 0) {
        "Students who were not admitted reported a higher average influence rating."
      } else if (difference < 0) {
        "Students who were admitted reported a higher average influence rating."
      } else {
        "Both groups reported the same average influence rating."
      }

      result <- test_result()
      if (is.null(result)) {
        significance_text <- paste(
          "A valid Welch's t-test could not be calculated for these responses."
        )
      } else {
        p_value <- result$p.value
        test_text <- paste0(
          "Welch's two-sample t-test produced t(",
          round(unname(result$parameter), 2), ") = ",
          round(unname(result$statistic), 3), ", p = ",
          formatC(p_value, digits = 4, format = "g"),
          ", with a 95% confidence interval for the mean difference (No - Yes) of [",
          round(result$conf.int[1], 2), ", ",
          round(result$conf.int[2], 2), "]. "
        )

        conclusion <- if (p_value < 0.05) {
          direction <- if (difference > 0) "not admitted" else "admitted"
          paste0(
            "The result is statistically significant at the 0.05 level. ",
            "We reject the null hypothesis of equal population means. ",
            "The results provide evidence of a higher mean influence rating ",
            "among students who were ", direction, " to their first-choice POSt."
          )
        } else {
          paste(
            "The result is not statistically significant at the 0.05 level.",
            "We fail to reject the null hypothesis.",
            "There is insufficient evidence of a difference in population mean influence ratings."
          )
        }
        significance_text <- paste0(test_text, conclusion,
          " P-values are unadjusted; filtered comparisons are exploratory.")
      }
      
      paste0(
        "There are ", nrow(df), " valid responses. ",
        "Not admitted (n = ", length(no), "): ",
        "Mean = ", round(mean(no), 2),
        "/10; Median = ", median(no), "/10. ",
        "Admitted (n = ", length(yes), "): ",
        "Mean = ", round(mean(yes), 2),
        "/10; Median = ", median(yes), "/10. ",
        "Mean difference (No - Yes): ",
        round(difference, 2), " points. ",
        comparison, " ", significance_text
      )
    })
    
  })
}

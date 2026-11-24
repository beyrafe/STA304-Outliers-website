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
    shiny::textOutput(ns("interpretation"))
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
      
      paste0(
        "There are ", nrow(df), " responses shown, ",
        top_group, " has the highest average ",
        variable_label, " influence score. ",
        "Mean: ", round(mean_value, 2), "/10; ",
        "Median: ", round(median_value, 2), "/10; ",
        if (length(modes) > 1) "Modes (tied): " else "Mode: ",
        mode_value, "/10. "
        
      )
    })
    
  })
}

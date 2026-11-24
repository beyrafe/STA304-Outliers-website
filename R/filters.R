# ==============================================================================
# R/filters.R  (Task 5)
#
# Shared demographic filters. filtersServer() returns one reactive data frame
# that every graph module receives as `filtered_data`.
# ==============================================================================


# Keep these groups and their order consistent with the data analysis report.
POST_GROUP_LEVELS <- c(
  "Statistics",
  "Computer Science",
  "Mathematics",
  "Other"
)


# Convert the form's detailed POSt responses to the four analysis groups.
# Joint programs are assigned using the program listed first.
derive_post_group <- function(post_category) {
  category <- trimws(as.character(post_category))
  first_major <- trimws(sub("\\s*&.*$", "", category))

  group <- rep(NA_character_, length(first_major))
  group[first_major == "Statistics"] <- "Statistics"
  group[first_major == "Computer Science"] <- "Computer Science"
  group[first_major == "Mathematics"] <- "Mathematics"

  known <- first_major %in% c(
    "Statistics",
    "Computer Science",
    "Mathematics"
  )
  group[!is.na(first_major) & nzchar(first_major) & !known] <- "Other"

  factor(group, levels = POST_GROUP_LEVELS)
}


# Prepare only the columns owned by the demographic filter. Graph-specific
# scores and variables remain the responsibility of their graph modules.
prepare_filter_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!"gender_identity" %in% names(data)) {
    stop("Dataset is missing: gender_identity", call. = FALSE)
  }

  data$gender_identity <- trimws(as.character(data$gender_identity))
  data$gender_identity[data$gender_identity == ""] <- NA_character_

  if ("post_category" %in% names(data)) {
    data$post_group <- derive_post_group(data$post_category)
  } else if ("post_group" %in% names(data)) {
    group <- trimws(as.character(data$post_group))
    group[!group %in% POST_GROUP_LEVELS] <- NA_character_
    data$post_group <- factor(group, levels = POST_GROUP_LEVELS)
  } else {
    stop("Dataset is missing: post_category or post_group", call. = FALSE)
  }

  data
}


# Pure filtering helper used by filtersServer() and easy to check independently.
filter_demographics <- function(data, gender = character(0),
                                post_group = character(0)) {
  data <- prepare_filter_data(data)

  if (length(gender) > 0) {
    data <- data[
      !is.na(data$gender_identity) & data$gender_identity %in% gender,
      ,
      drop = FALSE
    ]
  }

  if (length(post_group) > 0) {
    valid_groups <- intersect(as.character(post_group), POST_GROUP_LEVELS)
    data <- data[
      !is.na(data$post_group) & data$post_group %in% valid_groups,
      ,
      drop = FALSE
    ]
  }

  data$post_group <- droplevels(data$post_group)
  rownames(data) <- NULL
  data
}


filtersUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectizeInput(
      ns("gender"),
      "Gender identity",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "All gender identities")
    ),
    shiny::selectizeInput(
      ns("post_group"),
      "Program of study (POSt)",
      choices = POST_GROUP_LEVELS,
      multiple = TRUE,
      options = list(placeholder = "All POSt categories")
    ),
    shiny::actionButton(
      ns("reset"),
      "Clear filters",
      class = "btn btn-outline-secondary btn-sm"
    )
  )
}


filtersServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    data_source <- if (is.function(data)) data else shiny::reactive(data)

    prepared_data <- shiny::reactive({
      raw <- data_source()
      shiny::validate(
        shiny::need(is.data.frame(raw), "Survey data is unavailable.")
      )

      tryCatch(
        prepare_filter_data(raw),
        error = function(error) {
          shiny::validate(shiny::need(FALSE, conditionMessage(error)))
        }
      )
    })

    # Populate gender choices from the data so the control stays correct if
    # the survey categories change. Empty selections mean "include everyone."
    shiny::observe({
      df <- prepared_data()
      genders <- unique(df$gender_identity[!is.na(df$gender_identity)])

      current <- as.character(shiny::isolate(input$gender))
      current <- intersect(current, genders)

      shiny::updateSelectizeInput(
        session,
        "gender",
        choices = genders,
        selected = current,
        server = TRUE
      )
    })

    shiny::observeEvent(input$reset, {
      shiny::updateSelectizeInput(
        session,
        "gender",
        selected = character(0)
      )
      shiny::updateSelectizeInput(
        session,
        "post_group",
        selected = character(0)
      )
    })

    filtered_data <- shiny::reactive({
      filter_demographics(
        prepared_data(),
        gender = input$gender,
        post_group = input$post_group
      )
    })

    filtered_data
  })
}

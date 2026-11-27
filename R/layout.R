# ==============================================================================
# R/layout.R  (Task 6)
#
# Shared page layout: theme, masthead, the "which graph?" picker, the slot for
# the demographic filters, and the About page.
#
# The graphs themselves are Shiny modules in their own files. A graph appears
# on the site by being listed in graph_registry below. Nothing else in this
# file has to change when a module is added or edited.
# ==============================================================================


# ---- Which graphs exist ------------------------------------------------------
# One entry per graph. `ui` and `server` are the names of the module functions,
# e.g. careerUI(id) and careerServer(id, filtered_data). The module is called
# with `id` as its module id. If a module's functions are not defined yet (its
# branch is not merged), the site still runs and shows a short "not merged yet"
# note in that graph's place.
graph_registry <- list(
  list(
    id = "career",
    figure = "Figure 1",
    title = "Career and salary influence",
    question = paste(
      "How much do career opportunities and salary expectations influence",
      "students' choice of major?"
    ),
    ui = "careerUI",
    server = "careerServer"
  ),
  list(
    id = "personal",
    figure = "Figure 2",
    title = "Personal interest, family and friends",
    question = paste(
      "How much do personal interest, family and friends influence",
      "students' choice of major?"
    ),
    ui = "personalUI",
    server = "personalServer"
  ),
  list(
    id = "admission",
    figure = "Figure 3",
    title = "POSt admission requirements",
    question = paste(
      "Did admission requirements matter more to students who were not",
      "admitted to their first-choice program of study (POSt)?"
    ),
    ui = "admissionUI",
    server = "admissionServer"
  ),
  list(
    id = "gender",
    figure = "Figure 4",
    title = "Gender identity and POSt",
    question = paste(
      "Is a student's gender identity related to the program of study",
      "(POSt) they are in?"
    ),
    ui = "genderUI",
    server = "genderServer"
  )
)

# Choices for the graph drop-downs: label shown to the user -> graph id.
graph_choices <- function() {
  ids <- vapply(graph_registry, function(g) g$id, character(1))
  labels <- vapply(
    graph_registry,
    function(g) paste0(g$figure, ": ", g$title),
    character(1)
  )
  stats::setNames(ids, labels)
}

# Start the server side of every graph module that exists. Each one receives
# the same `filtered_data` reactive, so the demographic filter applies to all
# of them.
start_graph_modules <- function(filtered_data) {
  for (graph in graph_registry) {
    server_fn <- get0(graph$server, mode = "function")
    if (!is.null(server_fn)) {
      server_fn(graph$id, filtered_data)
    }
  }
  invisible(NULL)
}


# ---- Look and feel -----------------------------------------------------------
# Design brief (decided before any UI was written):
#   Idea        An interactive figure page from a journal article, not a
#               business dashboard. The page stays quiet so the ggplot figures,
#               and whatever colours the user picks for them, stand out.
#   Palette     paper #FAF8F4 (page), white (cards and inputs), ink #1B2333
#               (text), deep blue #1F4E79 (links, focus, active controls),
#               burnt amber #A8500F (small figure labels only), rule #E3DED3.
#               Every text/background pair meets WCAG AA (4.5:1 or better).
#   Type        Crimson Pro for headings and research questions, Source
#               Sans 3 for body text and controls (plain numerals, easy to
#               read on a projector).
#   Spacing     Bootstrap's 4/8 px scale; 1.25rem card padding and grid gap.
#   Components  Thin 1px rules, 6px corners, no shadows, underlined nav.
app_theme <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#1B2333",
    primary = "#1F4E79",
    secondary = "#5A6372",
    base_font = bslib::font_collection(
      bslib::font_link(
        "Source Sans 3",
        href = paste0(
          "https://fonts.googleapis.com/css2?family=Source+Sans+3:",
          "ital,wght@0,400;0,600;0,700;1,400&display=swap"
        )
      ),
      "system-ui", "Segoe UI", "Arial", "sans-serif"
    ),
    heading_font = bslib::font_collection(
      bslib::font_link(
        "Crimson Pro",
        href = paste0(
          "https://fonts.googleapis.com/css2?family=Crimson+Pro:",
          "ital,wght@0,500;0,600;0,700;1,500&display=swap"
        )
      ),
      "Georgia", "Times New Roman", "serif"
    ),
    "body-bg" = "#FAF8F4",
    "card-bg" = "#FFFFFF",
    "input-bg" = "#FFFFFF",
    "border-color" = "#E3DED3",
    "border-radius" = "0.375rem",
    "headings-font-weight" = 600
  )
  bslib::bs_add_rules(theme, layout_css)
}

layout_css <- "
:root {
  --paper: #FAF8F4;
  --rule: #E3DED3;
  --accent: #A8500F;
  --muted: #5A6372;
}

/* Masthead */
.navbar {
  border-bottom: 1px solid var(--rule);
  padding-block: 0.75rem;
}
.navbar-brand { white-space: normal; }
.masthead { display: flex; flex-direction: column; gap: 0.15rem; line-height: 1.15; }
.masthead-eyebrow, .figure-label {
  font-family: $font-family-base;
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--accent);
}
.masthead-title {
  font-family: $headings-font-family;
  font-size: clamp(1.3rem, calc(1rem + 1.2vw), 1.75rem);
  font-weight: 600;
  color: $body-color;
}
.navbar .nav-link { font-weight: 700; }

/* Sidebar */
.sidebar h2 { font-size: 1.25rem; margin-bottom: 0.25rem; }
.sidebar hr { border-color: #CFC8B8; opacity: 1; margin-block: 0.5rem; }
.n-showing {
  border-left: 3px solid $primary;
  padding: 0.35rem 0 0.35rem 0.75rem;
  font-weight: 700;
}
.pending-note { color: var(--muted); font-size: 0.9375rem; margin: 0; }

/* Graph area */
.lead-in { max-width: 62rem; font-size: 1.0625rem; margin-bottom: 1.25rem; }
.graph-grid {
  display: grid;
  gap: 1.25rem;
  grid-template-columns: repeat(auto-fit, minmax(#{'min(100%, 30rem)'}, 1fr));
  align-items: start;
}
.graph-grid > div, .graph-grid .graph-card { min-width: 0; }
.graph-card { box-shadow: none; margin-bottom: 0; }
.graph-card > .card-header {
  background: transparent;
  border-bottom: 1px solid var(--rule);
  padding: 1rem 1.25rem 0.85rem;
}
.graph-card > .card-header h2 { font-size: 1.6rem; margin: 0.1rem 0 0; }
.graph-card > .card-body { padding: 1.25rem; container-type: inline-size; }
/* A module's own layout inside a narrow card (two graphs side by side, or a
   phone): its side-by-side columns stack, and a wide table scrolls inside
   the card instead of running past its edge. */
@container (max-width: 42rem) {
  .graph-card .row > [class*='col-'] { width: 100%; }
}
.graph-card table.shiny-table { display: block; max-width: 100%; overflow-x: auto; }
.research-question {
  font-family: $headings-font-family;
  font-style: italic;
  font-weight: 500;
  font-size: 1.25rem;
  line-height: 1.35;
  color: var(--muted);
  max-width: 46rem;
  margin-bottom: 1.25rem;
}
.module-pending {
  border: 1px dashed #B9B2A3;
  border-radius: $border-radius;
  background: var(--paper);
  padding: 1.25rem;
}
.module-pending p { margin: 0; }
.module-pending p + p { margin-top: 0.35rem; color: var(--muted); }

/* About page */
.about { max-width: 46rem; margin: 0 auto; padding: 1.5rem 0 3rem; }
.about h1 { font-size: 2.4rem; margin-bottom: 0.75rem; }
.about h2 { font-size: 1.6rem; margin: 2.25rem 0 0.6rem; padding-top: 1.25rem; border-top: 1px solid var(--rule); }
.about p, .about li { line-height: 1.6; }
.about li + li { margin-top: 0.4rem; }
.about-lead { font-size: 1.15rem; }
.about-facts {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(11rem, 1fr));
  gap: 1rem;
  margin: 1.5rem 0 0;
  padding: 0;
  list-style: none;
}
.about-facts li { margin: 0; border-left: 3px solid var(--accent); padding-left: 0.85rem; }
.about-facts .fact-value {
  display: block;
  font-family: $headings-font-family;
  font-size: 1.7rem;
  font-weight: 600;
  line-height: 1.15;
}
.about-facts .fact-label { color: var(--muted); }
.about-tables {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(16rem, 1fr));
  gap: 1.5rem;
  margin-top: 1rem;
}
.about-table { margin: 0; font-variant-numeric: tabular-nums; }
.about-table caption { caption-side: top; color: $body-color; font-weight: 700; padding-top: 0; }
.about-table th, .about-table td { background: transparent; }
.about-table .num { text-align: right; }
"


# ---- Page --------------------------------------------------------------------
# survey      the cleaned data frame (used for the counts in the text)
# filters_ui  the demographic filter controls from R/filters.R, i.e.
#             filtersUI("filters"). NULL shows a "not merged yet" note.
layoutUI <- function(survey, filters_ui = NULL) {
  bslib::page_navbar(
    id = "page",
    title = shiny::tags$span(
      class = "masthead",
      shiny::tags$span(class = "masthead-eyebrow", "STA304H5 class survey, Fall 2026"),
      shiny::tags$span(class = "masthead-title", "What shapes a student's choice of major?")
    ),
    window_title = "What shapes a student's choice of major? STA304 survey",
    theme = app_theme(),
    lang = "en",
    fillable = FALSE,
    navbar_options = bslib::navbar_options(
      bg = "#FAF8F4",
      theme = "light",
      underline = TRUE
    ),
    bslib::nav_spacer(),
    bslib::nav_panel("Graphs", value = "graphs", graphs_page(survey, filters_ui)),
    bslib::nav_panel("About the survey", value = "about", about_page(survey))
  )
}

# Server side of the layout itself (the graph modules are started separately
# with start_graph_modules()).
layoutServer <- function(input, output, session, survey, filtered_data) {

  # The second drop-down never offers the graph that is already on screen.
  shiny::observeEvent(input$graph, {
    others <- graph_choices()
    others <- others[others != input$graph]
    keep <- if (isTRUE(input$graph2 %in% others)) input$graph2 else others[[1]]
    shiny::updateSelectInput(session, "graph2", choices = others, selected = keep)
  })

  # How many responses the current filters leave in.
  output$n_showing <- shiny::renderText({
    df <- filtered_data()
    shiny::req(is.data.frame(df))
    shown <- nrow(df)
    total <- nrow(survey)
    if (shown == 0) {
      "No responses match these filters."
    } else if (shown == total) {
      paste0("All ", total, " responses are included.")
    } else {
      paste0(
        "Showing ", shown, " of ", total, " responses (",
        round(100 * shown / total), "%)."
      )
    }
  })

  invisible(NULL)
}


# ---- Graphs page -------------------------------------------------------------
graphs_page <- function(survey, filters_ui = NULL) {
  if (is.null(filters_ui)) {
    filters_ui <- shiny::tags$p(
      class = "pending-note",
      "The gender identity and POSt filters will appear here once the",
      "filter module (R/filters.R) is merged."
    )
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      # Always visible. On a narrow screen it sits above the graphs, so the
      # graph picker and filters come before what they control.
      open = list(desktop = "always", mobile = "always-above"),
      bg = "#F3EFE7",
      fg = "#1B2333",

      shiny::tags$h2("Choose a graph"),
      shiny::selectInput(
        "graph", "Graph to display",
        choices = graph_choices()
      ),
      bslib::input_switch("compare", "Show a second graph beside it", value = FALSE),
      shiny::conditionalPanel(
        condition = "input.compare",
        shiny::selectInput(
          "graph2", "Second graph",
          choices = graph_choices()[-1]
        )
      ),

      shiny::tags$hr(),

      shiny::tags$h2("Filter respondents"),
      filters_ui,
      shiny::tags$div(
        class = "n-showing",
        `aria-live` = "polite",
        shiny::textOutput("n_showing", inline = TRUE)
      )
    ),

    shiny::tags$h1(class = "visually-hidden", "Survey graphs"),
    shiny::tags$p(
      class = "lead-in",
      paste0(
        nrow(survey), " students in STA304H5 rated, from 1 (lowest) to 10 ",
        "(highest), how much six different factors influenced their choice ",
        "of major. Choose a graph, narrow down who is included, then use ",
        "the controls on each graph to change what it shows."
      )
    ),
    shiny::tags$div(
      class = "graph-grid",
      lapply(graph_registry, graph_card)
    )
  )
}

# One graph: a card holding the research question and the module's own
# controls, plot and interpretation. The card is only displayed while its
# graph is picked in one of the two drop-downs; Shiny pauses the plots of
# hidden cards, so only what is on screen gets drawn.
graph_card <- function(graph) {
  ui_fn <- get0(graph$ui, mode = "function")
  body <- if (is.null(ui_fn)) module_pending(graph) else ui_fn(graph$id)

  shiny::conditionalPanel(
    condition = sprintf(
      "input.graph === '%1$s' || (input.compare && input.graph2 === '%1$s')",
      graph$id
    ),
    bslib::card(
      class = "graph-card",
      bslib::card_header(
        shiny::tags$span(class = "figure-label", graph$figure),
        shiny::tags$h2(graph$title)
      ),
      bslib::card_body(
        fillable = FALSE,
        shiny::tags$p(class = "research-question", graph$question),
        body
      )
    )
  )
}

# Shown in place of a graph whose module is not merged yet.
module_pending <- function(graph) {
  shiny::tags$div(
    class = "module-pending",
    shiny::tags$p(shiny::tags$strong("This graph is not merged yet.")),
    shiny::tags$p(
      "It will appear here as soon as ",
      shiny::tags$code(paste0(graph$ui, "()")), " and ",
      shiny::tags$code(paste0(graph$server, "()")),
      " are on this branch."
    )
  )
}


# ---- About page --------------------------------------------------------------
about_page <- function(survey) {
  n <- nrow(survey)
  dates <- collection_dates(survey)
  smallest_gender <- smallest_group(survey, "gender_identity")

  shiny::tags$div(
    class = "about",

    shiny::tags$h1("About the survey"),
    shiny::tags$p(
      class = "about-lead",
      "This site shows the results of a class survey run by the Outliers, a",
      "project group in STA304H5 (Surveys, Sampling and Observational Data)",
      "at the University of Toronto Mississauga. We asked our classmates",
      "what influenced their choice of major."
    ),
    shiny::tags$ul(
      class = "about-facts",
      about_fact(n, "survey responses"),
      about_fact(length(graph_registry), "research questions"),
      if (!is.null(dates)) about_fact(dates, "when responses were collected")
    ),

    shiny::tags$h2("What we wanted to find out"),
    shiny::tags$p(
      "At U of T, students enrol in a program of study (called a POSt)",
      "after first year, and some programs have admission requirements.",
      "Our four research questions each have their own graph:"
    ),
    shiny::tags$ol(
      lapply(graph_registry, function(g) {
        shiny::tags$li(g$question, " ", shiny::tags$em(paste0("(", g$figure, ")")))
      })
    ),

    shiny::tags$h2("What we asked"),
    shiny::tags$p(
      "Each student told us their POSt category, whether they were admitted",
      "to their first-choice POSt, and their gender identity. They then",
      "rated six possible influences on their choice of major, from 1",
      "(lowest influence) to 10 (highest influence):"
    ),
    shiny::tags$ul(
      shiny::tags$li("career opportunities and the job market"),
      shiny::tags$li("salary expectations"),
      shiny::tags$li("personal interest"),
      shiny::tags$li("family expectations"),
      shiny::tags$li("friends' program choices"),
      shiny::tags$li("POSt admission requirements")
    ),

    shiny::tags$h2("Who answered"),
    shiny::tags$p(
      paste0(
        "The survey was an online Google Form for students enrolled in ",
        "STA304H5 in Fall 2026. We received ", n, " responses."
      )
    ),
    shiny::tags$div(
      class = "about-tables",
      count_table(survey, "post_group", "POSt group"),
      count_table(survey, "gender_identity", "Gender identity")
    ),

    shiny::tags$h2("How programs are grouped"),
    shiny::tags$p(
      "The form offered twelve program categories. Most of them had only a",
      "handful of responses, so the graphs and the POSt filter use four",
      "groups: Statistics, Computer Science, Mathematics, and Other. Joint",
      "categories are placed by the program listed first, so Mathematics &",
      "Statistics counts as Mathematics and Computer Science & Statistics",
      "counts as Computer Science. Figure 4 is the exception: it shows",
      "Economics as its own group, which the other graphs and the filter",
      "count under Other."
    ),

    shiny::tags$h2("Things to keep in mind"),
    shiny::tags$ul(
      shiny::tags$li(
        shiny::tags$strong("This is not a random sample. "),
        "Students chose whether to respond, and those who did may differ",
        "from those who did not. The graphs describe the students who",
        "answered. They should not be read as true of every STA304 student,",
        "or of U of T students in general."
      ),
      shiny::tags$li(
        shiny::tags$strong("Each student is counted in one program. "),
        "Many students are in double majors, such as Statistics and Computer",
        "Science, and may have reported only one of them. The program groups",
        "overlap more than the graphs suggest."
      ),
      shiny::tags$li(
        shiny::tags$strong("Some groups are very small. "),
        if (!is.null(smallest_gender)) {
          paste0(
            "For example, only ", smallest_gender$n, " students chose \"",
            smallest_gender$name, "\" for gender identity. "
          )
        },
        "With so few people, one or two answers can change a graph",
        "noticeably, especially after filtering."
      )
    ),

    shiny::tags$h2("How the questions were tested"),
    shiny::tags$p(
      "Our written report checks each question with a statistical test, a",
      "way of judging whether a difference between groups is larger than",
      "chance alone would be expected to produce. Questions 1 and 2 use a",
      "one-way ANOVA across the four POSt groups, question 3 uses a Welch",
      "two-sample t-test, and question 4 uses Fisher's exact test."
    ),

    shiny::tags$h2("Using this site"),
    shiny::tags$ul(
      shiny::tags$li(
        "On the Graphs page, pick a graph from the drop-down. Switch on",
        "\"Show a second graph beside it\" to compare two graphs."
      ),
      shiny::tags$li(
        "Use the filters to include only some respondents. Every graph",
        "updates, and the count under the filters shows how many responses",
        "are left."
      ),
      shiny::tags$li(
        "Each graph has its own controls for what is plotted, its colours",
        "and its axis range, with a short interpretation underneath."
      )
    )
  )
}

about_fact <- function(value, label) {
  shiny::tags$li(
    shiny::tags$span(class = "fact-value", value),
    shiny::tags$span(class = "fact-label", label)
  )
}

# "September 26 to October 9, 2026", read from the form's Timestamp column.
# Returns NULL if the column is not there or cannot be read as dates.
collection_dates <- function(survey) {
  if (!"Timestamp" %in% names(survey)) {
    return(NULL)
  }
  days <- as.Date(sub(" .*$", "", as.character(survey$Timestamp)), format = "%m/%d/%Y")
  days <- days[!is.na(days)]
  if (length(days) == 0) {
    return(NULL)
  }
  first <- as.POSIXlt(min(days))
  last <- as.POSIXlt(max(days))
  paste0(
    month.name[first$mon + 1], " ", first$mday, " to ",
    month.name[last$mon + 1], " ", last$mday, ", ", last$year + 1900
  )
}

# Name and size of the smallest category of `column`, or NULL without it.
smallest_group <- function(survey, column) {
  if (!column %in% names(survey)) {
    return(NULL)
  }
  counts <- table(droplevels(factor(survey[[column]])))
  if (length(counts) == 0) {
    return(NULL)
  }
  list(name = names(counts)[which.min(counts)], n = min(counts))
}

# Small "category / responses / share" table for the About page.
count_table <- function(survey, column, heading) {
  if (!column %in% names(survey)) {
    return(NULL)
  }
  counts <- table(droplevels(factor(survey[[column]])))
  shares <- round(100 * counts / sum(counts))

  shiny::tags$table(
    class = "table table-sm about-table",
    shiny::tags$caption(heading),
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th(scope = "col", "Category"),
        shiny::tags$th(scope = "col", class = "num", "Responses"),
        shiny::tags$th(scope = "col", class = "num", "Share")
      )
    ),
    shiny::tags$tbody(
      lapply(seq_along(counts), function(i) {
        shiny::tags$tr(
          shiny::tags$th(scope = "row", names(counts)[i]),
          shiny::tags$td(class = "num", as.integer(counts[i])),
          shiny::tags$td(class = "num", paste0(shares[i], "%"))
        )
      })
    )
  )
}

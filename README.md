# What shapes a student's choice of major?

Survey website for the STA304H5 (Fall 2026) group project by the Outliers, a
project group at the University of Toronto Mississauga. It is an R Shiny app
that shows the results of a class survey on what influenced students' choice
of major.

**Live site:** the Posit Connect Cloud link goes here once the app is deployed.

This repository holds only the website: its code and the survey data it
reads. It is a copy of the website files from the group's project repository,
which is private, and it is updated from there whenever the site changes.

## Run it locally

Open `app.R` in RStudio and click **Run App**, or run `shiny::runApp()` from the
project folder. It needs the packages `shiny`, `bslib` and `ggplot2`.

## How the app is organized

| File | What it holds |
| --- | --- |
| `app.R` | Loads the survey data and starts the app |
| `R/layout.R` | Page layout, the drop-down that picks which graph to display, About page |
| `R/career.R`, `R/personal.R`, `R/admission.R`, `R/gender.R` | One module per graph: `careerUI(id)` and `careerServer(id, filtered_data)`, and so on |
| `R/filters.R` | Gender identity and POSt filter: `filtersUI(id)` and `filtersServer(id, data)`, which returns the filtered data as a reactive and adds the four POSt groups (`post_group`) |
| `R/data_prep.R` | Optional shared cleaning: if it defines `prepare_data(raw)`, `app.R` applies it before filtering |
| `STA304_Outliers_Data_Collection - Form Responses 1.csv` | The survey responses |
| `manifest.json` | The R version and packages Posit Connect Cloud installs |

Shiny loads every file in `R/` automatically, so a graph shows up on the site as
soon as its two module functions exist. Until then its place shows a
"not merged yet" note.

## Deployment

The site is published on [Posit Connect Cloud](https://connect.posit.cloud)
from this repository: branch `main`, primary file `app.R`.

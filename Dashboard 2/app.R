library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(readxl)
library(stringr)
library(purrr)
library(sf)
library(plotly)
library(scales)
library(glue)
library(ggplot2)
library(htmltools)

options(sass.cache = FALSE)

get_app_dir <- function() {
  file_from_args <- grep("^--file=", commandArgs(), value = TRUE)

  if (length(file_from_args) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_from_args[1]), mustWork = FALSE)))
  }

  normalizePath(getwd(), mustWork = TRUE)
}

find_data_dir <- function(app_dir = get_app_dir()) {
  env_dir <- Sys.getenv("TORONTO_DATA_DIR", unset = "")

  required_files <- c(
    "2023 Office of the Mayor.xlsx",
    "2023-WardProfiles-2011-2021-CensusData.xlsx",
    "25-ward-model-december-2018-wgs84-latitude-longitude/WARD_WGS84.shp"
  )

  candidates <- unique(c(
    file.path(app_dir, "data"),
    app_dir,
    env_dir
  ))
  candidates <- candidates[nzchar(candidates)]

  for (d in candidates) {
    if (all(file.exists(file.path(d, required_files)))) {
      return(normalizePath(d, mustWork = TRUE))
    }
  }

  stop(
    paste0(
      "Could not find all required data files. ",
      "Place the input files in the project's data/ folder, or set TORONTO_DATA_DIR ",
      "to a folder containing the mayor workbook, the census workbook, and the ward shapefile folder.\n",
      "Checked:\n- ", paste(candidates, collapse = "\n- ")
    ),
    call. = FALSE
  )
}

stage_file_in_temp <- function(path, subdir = "toronto-dashboard-files") {
  staged_dir <- file.path(tempdir(), subdir)
  dir.create(staged_dir, recursive = TRUE, showWarnings = FALSE)

  staged_path <- file.path(staged_dir, basename(path))
  ok <- file.copy(path, staged_path, overwrite = TRUE)
  if (!ok) {
    stop("Could not stage file for reading: ", path, call. = FALSE)
  }

  staged_path
}

stage_shapefile_in_temp <- function(shapefile_path) {
  shape_dir <- dirname(shapefile_path)
  base_name <- tools::file_path_sans_ext(basename(shapefile_path))
  shape_files <- list.files(
    shape_dir,
    pattern = paste0("^", base_name, "\\."),
    full.names = TRUE,
    ignore.case = TRUE
  )

  staged_dir <- file.path(tempdir(), "toronto-wards-shapefile")
  dir.create(staged_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(shape_files, staged_dir, overwrite = TRUE)

  file.path(staged_dir, basename(shapefile_path))
}

read_sf_compat <- function(shapefile_path) {
  direct_read <- try(sf::st_read(shapefile_path, quiet = TRUE), silent = TRUE)
  if (!inherits(direct_read, "try-error")) {
    return(direct_read)
  }

  message("Direct shapefile read failed. Retrying from a temporary ASCII-only path.")
  staged_path <- stage_shapefile_in_temp(shapefile_path)
  sf::st_read(staged_path, quiet = TRUE)
}

clean_numeric <- function(x) {
  out <- gsub(",", "", as.character(x))
  out <- gsub("%", "", out)
  suppressWarnings(as.numeric(out))
}

clean_candidate_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_replace_all("[‘’]", "'")
}

canonical_candidate_name <- function(x) {
  cleaned_name <- clean_candidate_name(x)

  dplyr::recode(
    cleaned_name,
    "Olivia Chow" = "Chow Olivia",
    "Chow Olivia" = "Chow Olivia",
    "Ana Bail\u00e3o" = "Bail\u00e3o Ana",
    "Bail\u00e3o Ana" = "Bail\u00e3o Ana",
    "Mark Saunders" = "Saunders Mark",
    "Saunders Mark" = "Saunders Mark",
    "Anthony Furey" = "Furey Anthony",
    "Furey Anthony" = "Furey Anthony",
    "Josh Matlow" = "Matlow Josh",
    "Matlow Josh" = "Matlow Josh",
    .default = cleaned_name
  )
}

is_candidate_result_row <- function(candidate, votes) {
  !is.na(candidate) &
    candidate != "" &
    !is.na(votes) &
    !candidate %in% c("Mayor", "Total", "Totals", "TOTAL") &
    !stringr::str_detect(candidate, "^City Ward\\s+\\d+\\s+Totals$")
}

find_optional_input_file <- function(file_name, app_dir = get_app_dir()) {
  env_dir <- Sys.getenv("TORONTO_DATA_DIR", unset = "")
  project_dir <- dirname(app_dir)

  candidate_paths <- unique(c(
    file.path(app_dir, "data", file_name),
    file.path(app_dir, file_name),
    file.path(project_dir, "data", file_name),
    file.path(project_dir, file_name),
    file.path(project_dir, "Dashboard 1", "data", file_name),
    file.path(project_dir, "Dashboard 1", file_name),
    file.path(env_dir, file_name)
  ))
  existing_paths <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_paths) == 0) {
    return(file.path(app_dir, "data", file_name))
  }

  normalizePath(existing_paths[1], mustWork = TRUE)
}

read_mayor_results <- function(path) {
  staged_path <- stage_file_in_temp(path)
  sheets <- readxl::excel_sheets(staged_path)
  ward_sheets <- sheets[stringr::str_detect(sheets, "^Ward\\s+\\d+$")]

  purrr::map_dfr(ward_sheets, function(sh) {
    ward_no <- as.integer(stringr::str_extract(sh, "\\d+"))
    raw <- readxl::read_excel(
      staged_path,
      sheet = sh,
      col_names = FALSE,
      .name_repair = "minimal"
    )

    if (ncol(raw) < 2) {
      return(tibble())
    }

    candidate <- raw[[1]]
    votes <- clean_numeric(raw[[ncol(raw)]])

    tibble(
      ward_no = ward_no,
      candidate = candidate,
      votes = votes
    ) |>
      filter(row_number() >= 4) |>
      mutate(candidate = canonical_candidate_name(candidate)) |>
      filter(is_candidate_result_row(candidate, votes))
  })
}

read_turnout <- function(path) {
  staged_path <- stage_file_in_temp(path)
  vote <- readxl::read_excel(
    staged_path,
    sheet = "2023 Voter Turnout Statisti M",
    .name_repair = "minimal"
  )

  vote |>
    filter(!is.na(Ward)) |>
    filter(grepl("^[0-9]+$", as.character(Ward))) |>
    mutate(Ward = as.integer(Ward)) |>
    group_by(Ward) |>
    summarise(
      total_eligible = sum(`Total Eligible Electors`, na.rm = TRUE),
      number_voted = sum(`Number Voted`, na.rm = TRUE),
      turnout_pct = number_voted / total_eligible * 100,
      .groups = "drop"
    ) |>
    rename(ward_no = Ward)
}

read_turnout_or_empty <- function(path) {
  if (!file.exists(path)) {
    message("Turnout workbook not found. Continuing without turnout metrics.")
    return(tibble(
      ward_no = integer(),
      total_eligible = numeric(),
      number_voted = numeric(),
      turnout_pct = numeric()
    ))
  }

  read_turnout(path)
}

read_census_metrics <- function(path) {
  staged_path <- stage_file_in_temp(path)
  one <- readxl::read_excel(
    staged_path,
    sheet = "2021 One Variable",
    col_names = FALSE,
    .name_repair = "minimal"
  )

  get_row_values <- function(row_index) {
    as.numeric(one[row_index, 3:27] |> unlist())
  }

  tibble(
    ward_no = 1:25,
    median_income = get_row_values(1385),
    bachelors_share = get_row_values(1007) / get_row_values(997) * 100,
    unemployment_rate = get_row_values(1308),
    visible_minority_share = get_row_values(1286) / get_row_values(1285) * 100
  )
}

format_percent_or_na <- function(x, accuracy = 0.1) {
  if (length(x) == 0 || is.na(x[1])) {
    return("N/A")
  }

  percent(x[1] / 100, accuracy = accuracy)
}

make_binned_summary <- function(df, var_name, var_label) {
  x <- df[[var_name]]
  q <- unique(quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE))

  if (length(q) < 3) {
    q <- pretty(range(x, na.rm = TRUE), n = 5)
  }

  df |>
    mutate(bin = cut(.data[[var_name]], breaks = q, include.lowest = TRUE, dig.lab = 8)) |>
    filter(!is.na(bin)) |>
    group_by(candidate, bin) |>
    summarise(avg_vote_share = mean(vote_share, na.rm = TRUE), .groups = "drop") |>
    mutate(demographic = var_label)
}

app_dir <- get_app_dir()
data_dir <- find_data_dir(app_dir)

mayor_file <- file.path(data_dir, "2023 Office of the Mayor.xlsx")
turnout_file <- find_optional_input_file("2023-mayoral-by-election-voter-statistics.xlsx", app_dir)
census_file <- file.path(data_dir, "2023-WardProfiles-2011-2021-CensusData.xlsx")
ward_shp <- file.path(
  data_dir,
  "25-ward-model-december-2018-wgs84-latitude-longitude",
  "WARD_WGS84.shp"
)

stopifnot(
  file.exists(mayor_file),
  file.exists(census_file),
  file.exists(ward_shp)
)

wards_map <- read_sf_compat(ward_shp) |>
  mutate(
    ward_no = as.integer(AREA_S_CD),
    ward_name = as.character(AREA_NAME),
    ward_label = paste0("Ward ", ward_no)
  )

mayor_long <- read_mayor_results(mayor_file)
turnout_df <- read_turnout_or_empty(turnout_file)
census_df <- read_census_metrics(census_file)

ward_totals <- mayor_long |>
  group_by(ward_no) |>
  summarise(total_votes = sum(votes, na.rm = TRUE), .groups = "drop")

results <- mayor_long |>
  left_join(ward_totals, by = "ward_no") |>
  mutate(vote_share = 100 * votes / total_votes)

candidate_totals <- results |>
  group_by(candidate) |>
  summarise(city_votes = sum(votes, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(city_votes))

top_candidates <- candidate_totals |>
  slice_head(n = 3) |>
  pull(candidate)

candidate_colors <- stats::setNames(
  c("#7c3aed", "#84cc16", "#334155"),
  top_candidates
)

overall_total_votes <- sum(candidate_totals$city_votes, na.rm = TRUE)

citywide_top3 <- candidate_totals |>
  filter(candidate %in% top_candidates) |>
  mutate(
    candidate = factor(candidate, levels = top_candidates),
    citywide_share = 100 * city_votes / overall_total_votes,
    fill = unname(candidate_colors[as.character(candidate)])
  ) |>
  arrange(candidate) |>
  mutate(candidate = as.character(candidate))

other_city_votes <- sum(
  candidate_totals$city_votes[!candidate_totals$candidate %in% top_candidates],
  na.rm = TRUE
)

citywide_donut_df <- citywide_top3 |>
  transmute(candidate, city_votes, citywide_share, fill)

if (other_city_votes > 0) {
  citywide_donut_df <- bind_rows(
    citywide_donut_df,
    tibble(
      candidate = "Other candidates",
      city_votes = other_city_votes,
      citywide_share = 100 * other_city_votes / overall_total_votes,
      fill = "#d7d9d6"
    )
  )
}

top_three_combined_share <- sum(citywide_top3$citywide_share, na.rm = TRUE)

candidate_backgrounds <- list(
  "Chow Olivia" = list(
    role = "Former NDP MP",
    support = "Backed by NDP politicians and major labor unions.",
    platform = "Increase renter protections, build 25,000 rent-controlled homes, and expand library hours."
  ),
  "Bail\u00e3o Ana" = list(
    role = "Former Deputy Mayor under John Tory",
    support = "Endorsed by Liberal politicians and former Mayor John Tory",
    platform = "Increase transit funding, build housing on transit routes, and fix 911 wait times."
  ),
  "Saunders Mark" = list(
    role = "Former Chief of Toronto Police and PC candidate for MPP",
    support = "Endorsed by Doug Ford (Premier of Ontario)",
    platform = "Focus on public safety and removing bike lanes on major roads."
  ),
  "Furey Anthony" = list(
    role = "Former Sun columnist and conservative commentator",
    support = "Backed by fiscal conservatives and populist voters",
    platform = "Hire 500 new police officers, phase out the Land Transfer Tax, and clear tent encampments from public parks."
  ),
  "Matlow Josh" = list(
    role = "Long-time City Councillor for Toronto\u2013St. Paul's",
    support = "Backed by urbanists and progressive/centrist advocates",
    platform = "Create 'Public Build Toronto' for housing on city land, freeze the police budget, and restore TTC service levels."
  )
)

get_candidate_background <- function(candidate_name) {
  normalized_name <- enc2utf8(stringr::str_squish(candidate_name))
  canonical_name <- switch(
    normalized_name,
    "Olivia Chow" = "Chow Olivia",
    "Chow Olivia" = "Chow Olivia",
    "Ana Bail\u00e3o" = "Bail\u00e3o Ana",
    "Bail\u00e3o Ana" = "Bail\u00e3o Ana",
    "Mark Saunders" = "Saunders Mark",
    "Saunders Mark" = "Saunders Mark",
    "Anthony Furey" = "Furey Anthony",
    "Furey Anthony" = "Furey Anthony",
    "Josh Matlow" = "Matlow Josh",
    "Matlow Josh" = "Matlow Josh",
    normalized_name
  )

  candidate_background <- candidate_backgrounds[[canonical_name]]

  if (is.null(candidate_background)) {
    return(list(role = "", support = "", platform = ""))
  }

  candidate_background
}

winner_df <- results |>
  filter(candidate %in% top_candidates) |>
  arrange(ward_no, desc(vote_share)) |>
  group_by(ward_no) |>
  summarise(
    winner = first(candidate),
    winner_vote_share = first(vote_share),
    runner_up = nth(candidate, 2),
    runner_up_share = nth(vote_share, 2),
    margin = winner_vote_share - runner_up_share,
    .groups = "drop"
  )

metrics_df <- census_df |>
  left_join(turnout_df, by = "ward_no")

results_full <- results |>
  left_join(metrics_df, by = "ward_no") |>
  left_join(
    wards_map |>
      sf::st_drop_geometry() |>
      select(ward_no, ward_name),
    by = "ward_no"
  )

map_base <- wards_map |>
  left_join(metrics_df, by = "ward_no") |>
  left_join(winner_df, by = "ward_no")

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bg = "#F4F1EA",
    fg = "#22313F",
    primary = "#2E6E9E",
    secondary = "#6B7A8F",
    success = "#4C8C5A",
    warning = "#B8860B"
  ),
  tags$head(
    tags$title("Toronto Mayoral Support Explorer"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    class = "hero-banner",
    div(
      class = "hero-header",
      div(
        class = "hero-copy",
        div(class = "hero-eyebrow", "Toronto 2023 Mayoral Dashboard"),
        h1("Where did mayoral candidates build support across Toronto?"),
        p(
          class = "hero-subtitle",
          "A ward-level explorer linking vote share, coalition geography, and demographic context."
        )
      ),
      div(
        class = "hero-actions",
        actionButton("reset_map", "Reset view", class = "reset-chip")
      )
    ),
    div(
      class = "control-grid",
      div(
        class = "control-card",
        tags$label("Map Mode"),
        radioButtons(
          inputId = "map_mode",
          label = NULL,
          choiceNames = list("Leading candidate", "Support share"),
          choiceValues = c("winner", "support"),
          selected = "winner",
          inline = TRUE
        )
      ),
      div(
        class = "control-card",
        tags$label(`for` = "candidate", "Candidate"),
        selectizeInput(
          inputId = "candidate",
          label = NULL,
          choices = top_candidates,
          selected = top_candidates[1],
          options = list(placeholder = "Choose a candidate")
        )
      ),
      div(
        class = "control-card",
        tags$label(`for` = "demographic", "Compare by demographic"),
        selectInput(
          inputId = "demographic",
          label = NULL,
          choices = c(
            "Median income" = "median_income",
            "Bachelor's degree share" = "bachelors_share",
            "Visible minority share" = "visible_minority_share",
            "Unemployment rate" = "unemployment_rate"
          ),
          selected = "unemployment_rate"
        )
      ),
      div(
        class = "control-card summary-card",
        tags$label("Citywide Vote Share"),
        div(
          class = "summary-card-content",
          div(
            class = "summary-chart-wrap",
            plotlyOutput("citywide_donut", height = 118)
          ),
          div(
            class = "summary-legend",
            lapply(seq_len(nrow(citywide_top3)), function(i) {
              tags$div(
                class = "mini-legend-item",
                tags$span(
                  class = "mini-legend-swatch",
                  style = paste0("background:", citywide_top3$fill[i], ";")
                ),
                tags$span(class = "mini-legend-label", citywide_top3$candidate[i]),
                tags$span(
                  class = "mini-legend-value",
                  paste0(round(citywide_top3$citywide_share[i], 1), "%")
                )
              )
            })
          )
        )
      )
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      class = "viz-card",
      card_header("Ward Support Map"),
      p(
        class = "panel-subtitle",
        "Switch between the ward winner map and a selected candidate's ward-level support share."
      ),
      leafletOutput("ward_map", height = 470)
    ),
    card(
      full_screen = TRUE,
      class = "viz-card",
      card_header("Demographic Comparison"),
      p(
        class = "panel-subtitle",
        "Average vote share by ward quintiles of the selected demographic, with leading citywide candidates shown for context."
      ),
      plotlyOutput("demo_plot", height = 470)
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      class = "viz-card",
      card_header("Ward-Level Support vs. Demographics"),
      plotlyOutput("scatter_plot", height = 430)
    ),
    card(
      full_screen = TRUE,
      class = "viz-card profile-card",
      card_header("Candidate Support Profile"),
      card_body(fill = TRUE, uiOutput("profile_panel"))
    )
  )
)

server <- function(input, output, session) {
  selected_ward <- reactiveVal(NULL)

  observeEvent(input$reset_map, {
    selected_ward(NULL)
  })

  observeEvent(input$ward_map_shape_click, {
    clicked_ward <- as.integer(input$ward_map_shape_click$id)

    if (isTRUE(clicked_ward == selected_ward())) {
      selected_ward(NULL)
    } else {
      selected_ward(clicked_ward)
    }
  }, ignoreNULL = TRUE)

  selected_candidate_color <- reactive({
    candidate_colors[[input$candidate]]
  })

  selected_candidate_results <- reactive({
    results_full |>
      filter(candidate == input$candidate) |>
      select(ward_no, vote_share, votes, total_votes) |>
      rename(
        selected_vote_share = vote_share,
        selected_votes = votes,
        selected_total_votes = total_votes
      )
  })

  map_data <- reactive({
    selected_id <- selected_ward()

    map_base |>
      left_join(selected_candidate_results(), by = "ward_no") |>
      mutate(
        selected_vote_share = dplyr::coalesce(selected_vote_share, 0),
        selected_votes = dplyr::coalesce(selected_votes, 0),
        selected_total_votes = dplyr::coalesce(selected_total_votes, 0),
        is_selected = if (is.null(selected_id)) FALSE else ward_no == selected_id,
        winner_tooltip_html = glue(
          "<strong>{ward_label}: {ward_name}</strong><br/>",
          "Winner: {winner}<br/>",
          "Winning share: {round(winner_vote_share, 1)}%<br/>",
          "Runner-up: {runner_up}<br/>",
          "Margin: {round(margin, 1)} pp"
        ),
        support_tooltip_html = glue(
          "<strong>{ward_label}: {ward_name}</strong><br/>",
          "Candidate: {input$candidate}<br/>",
          "Vote share: {round(selected_vote_share, 1)}%<br/>",
          "Votes: {comma(selected_votes)}<br/>",
          "Total ward votes: {comma(selected_total_votes)}"
        )
      )
  })

  output$ward_map <- renderLeaflet({
    ward_map <- map_data() |> sf::st_transform(4326)

    if (input$map_mode == "winner") {
      winner_palette <- colorFactor(
        palette = candidate_colors[top_candidates],
        domain = top_candidates,
        na.color = "#d7d9d6"
      )
      winner_legend_colors <- candidate_colors[top_candidates]

      leaflet(ward_map) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        addPolygons(
          layerId = ~ward_no,
          fillColor = ~winner_palette(winner),
          fillOpacity = 0.85,
          color = ~ifelse(is_selected, "#111827", "#FFFFFF"),
          weight = ~ifelse(is_selected, 4, 1.4),
          opacity = 1,
          smoothFactor = 0.3,
          label = lapply(ward_map$winner_tooltip_html, HTML),
          labelOptions = labelOptions(
            direction = "auto",
            style = list(
              "font-family" = "Avenir Next, Avenir, Trebuchet MS, sans-serif",
              "font-size" = "12px",
              "padding" = "10px 12px"
            )
          ),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#213547",
            fillOpacity = 0.95,
            bringToFront = TRUE
          )
        ) |>
        addLegend(
          position = "bottomright",
          colors = unname(winner_legend_colors),
          labels = names(winner_legend_colors),
          title = "Leading candidate",
          opacity = 1
        )
    } else {
      max_share <- max(ward_map$selected_vote_share, na.rm = TRUE)
      if (!is.finite(max_share) || max_share <= 0) {
        max_share <- 1
      }

      palette_values <- colorNumeric(
        palette = c("#F7F5EE", selected_candidate_color()),
        domain = c(0, max_share)
      )

      leaflet(ward_map) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        addPolygons(
          layerId = ~ward_no,
          fillColor = ~palette_values(selected_vote_share),
          fillOpacity = 0.88,
          color = ~ifelse(is_selected, "#111827", "#FFFFFF"),
          weight = ~ifelse(is_selected, 4, 1.4),
          opacity = 1,
          smoothFactor = 0.3,
          label = lapply(ward_map$support_tooltip_html, HTML),
          labelOptions = labelOptions(
            direction = "auto",
            style = list(
              "font-family" = "Avenir Next, Avenir, Trebuchet MS, sans-serif",
              "font-size" = "12px",
              "padding" = "10px 12px"
            )
          ),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#213547",
            fillOpacity = 0.95,
            bringToFront = TRUE
          )
        ) |>
        addLegend(
          position = "bottomright",
          pal = palette_values,
          values = ~selected_vote_share,
          title = paste(input$candidate, "vote share (%)"),
          opacity = 1
        )
    }
  })

  output$demo_plot <- renderPlotly({
    label_lookup <- c(
      median_income = "Median income",
      bachelors_share = "Bachelor's degree share",
      visible_minority_share = "Visible minority share",
      unemployment_rate = "Unemployment rate"
    )
    var_name <- input$demographic
    var_label <- label_lookup[[var_name]]

    summary_df <- results_full |>
      filter(candidate %in% top_candidates) |>
      make_binned_summary(var_name, var_label)

    g <- ggplot(summary_df, aes(x = bin, y = avg_vote_share, color = candidate, group = candidate)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.8) +
      scale_color_manual(values = candidate_colors) +
      labs(
        x = var_label,
        y = "Average vote share (%)",
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1)
      )

    ggplotly(g, tooltip = c("x", "y", "colour")) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$citywide_donut <- renderPlotly({
    plot_df <- citywide_donut_df |>
      mutate(
        hover_text = glue(
          "{candidate}<br>",
          "Votes: {comma(city_votes)}<br>",
          "Citywide share: {round(citywide_share, 1)}%"
        )
      )

    plot_ly(
      plot_df,
      labels = ~candidate,
      values = ~city_votes,
      type = "pie",
      hole = 0.62,
      sort = FALSE,
      direction = "clockwise",
      marker = list(
        colors = plot_df$fill,
        line = list(color = "#fffdf9", width = 2)
      ),
      textinfo = "none",
      hoverinfo = "text",
      text = ~hover_text,
      showlegend = FALSE
    ) |>
      layout(
        margin = list(t = 0, r = 0, b = 0, l = 0),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        annotations = list(
          list(
            text = glue(
              "<b>{round(top_three_combined_share, 1)}%</b><br>",
              "<span style='font-size:11px;color:#5b6a78;'>Top 3 total</span>"
            ),
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
      ) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$scatter_plot <- renderPlotly({
    label_lookup <- c(
      median_income = "Median household income ($)",
      bachelors_share = "Bachelor's degree share (%)",
      visible_minority_share = "Visible minority share (%)",
      unemployment_rate = "Unemployment rate (%)"
    )
    var_name <- input$demographic
    x_label <- label_lookup[[var_name]]

    plot_df <- results_full |>
      filter(candidate %in% top_candidates)

    g <- ggplot(plot_df, aes_string(x = var_name, y = "vote_share", color = "candidate")) +
      geom_point(size = 3, alpha = 0.85) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.9) +
      scale_color_manual(values = candidate_colors) +
      labs(
        x = x_label,
        y = "Vote share (%)",
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )

    ggplotly(g, tooltip = c("x", "y", "colour")) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$profile_panel <- renderUI({
    ward_now <- selected_ward()

    if (!is.null(ward_now)) {
      ward_meta <- metrics_df |> filter(ward_no == ward_now)
      ward_name_now <- wards_map |>
        sf::st_drop_geometry() |>
        filter(ward_no == ward_now) |>
        pull(ward_name)
      ward_results <- results_full |>
        filter(ward_no == ward_now, candidate %in% top_candidates) |>
        arrange(desc(vote_share)) |>
        slice_head(n = 3)

      div(
        class = "profile-box",
        h3(glue("Ward {ward_now}: {ward_name_now}")),
        p(class = "small-note", "Click another ward on the map to update this panel."),
        fluidRow(
          column(
            6,
            div(class = "stat-label", "Turnout"),
            div(class = "stat-value", format_percent_or_na(ward_meta$turnout_pct))
          ),
          column(
            6,
            div(class = "stat-label", "Median income"),
            div(class = "stat-value", dollar(ward_meta$median_income))
          )
        ),
        br(),
        tags$strong("Top 3 candidates in this ward"),
        tags$ul(
          lapply(seq_len(nrow(ward_results)), function(i) {
            tags$li(glue("{ward_results$candidate[i]}: {round(ward_results$vote_share[i], 1)}%"))
          })
        ),
        tags$hr(),
        tags$p(glue("Bachelor's degree share: {round(ward_meta$bachelors_share, 1)}%")),
        tags$p(glue("Visible minority share: {round(ward_meta$visible_minority_share, 1)}%")),
        tags$p(glue("Unemployment rate: {round(ward_meta$unemployment_rate, 1)}%"))
      )
    } else {
      candidate_now <- input$candidate
      candidate_background <- get_candidate_background(candidate_now)

      top5 <- results_full |>
        filter(candidate == candidate_now) |>
        arrange(desc(vote_share)) |>
        slice_head(n = 5)

      summary_stats <- top5 |>
        summarise(
          avg_income = mean(median_income, na.rm = TRUE),
          avg_bachelors = mean(bachelors_share, na.rm = TRUE),
          avg_visible_minority = mean(visible_minority_share, na.rm = TRUE),
          avg_unemployment = mean(unemployment_rate, na.rm = TRUE)
        )

      strongest_wards <- paste0("W", paste(top5$ward_no[1:min(3, nrow(top5))], collapse = ", W"))

      div(
        class = "profile-box",
        h3(candidate_now),
        p(
          class = "small-note",
          "Showing the selected candidate profile. Click a ward on the map to switch to ward detail."
        ),
        fluidRow(
          column(
            6,
            div(class = "stat-label", "Strongest wards"),
            div(class = "stat-value", strongest_wards)
          ),
          column(
            6,
            div(class = "stat-label", "Citywide votes"),
            div(
              class = "stat-value",
              comma(candidate_totals$city_votes[candidate_totals$candidate == candidate_now])
            )
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            div(class = "stat-label", "Avg. income (top 5 wards)"),
            div(class = "stat-value", dollar(summary_stats$avg_income))
          ),
          column(
            6,
            div(class = "stat-label", "Bachelor's degree share"),
            div(class = "stat-value", percent(summary_stats$avg_bachelors / 100, accuracy = 0.1))
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            div(class = "stat-label", "Visible minority share"),
            div(class = "stat-value", percent(summary_stats$avg_visible_minority / 100, accuracy = 0.1))
          ),
          column(
            6,
            div(class = "stat-label", "Unemployment rate"),
            div(class = "stat-value", percent(summary_stats$avg_unemployment / 100, accuracy = 0.1))
          )
        ),
        tags$hr(),
        div(
          class = "background-title",
          "Candidate Background"
        ),
        div(
          class = "background-line",
          tags$strong("Role: "),
          candidate_background$role
        ),
        div(
          class = "background-line",
          tags$strong("Support: "),
          candidate_background$support
        ),
        div(
          class = "background-line",
          tags$strong("Platform: "),
          candidate_background$platform
        )
      )
    }
  })
}

shinyApp(ui, server)

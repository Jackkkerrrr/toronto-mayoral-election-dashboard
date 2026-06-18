library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(purrr)
library(scales)
library(sf)
library(tidyr)
library(htmltools)
library(readxl)
library(stringr)

options(sass.cache = FALSE)

`%>%` <- dplyr::`%>%`

# =============================================================================
# Shared Project Helpers
# =============================================================================

get_script_dir <- function() {
  file_from_args <- grep("^--file=", commandArgs(), value = TRUE)

  if (length(file_from_args) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_from_args[1]), mustWork = FALSE)))
  }

  normalizePath(getwd(), mustWork = TRUE)
}

find_project_root <- function() {
  candidates <- unique(c(
    Sys.getenv("STA313_PROJECT_ROOT", unset = ""),
    get_script_dir(),
    getwd()
  ))
  candidates <- candidates[nzchar(candidates)]

  for (candidate in candidates) {
    if (
      dir.exists(file.path(candidate, "Dashboard 1")) &&
      dir.exists(file.path(candidate, "Dashboard 2"))
    ) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }

  stop(
    "Could not locate the STA313 project root containing 'Dashboard 1' and 'Dashboard 2'.",
    call. = FALSE
  )
}

project_root <- find_project_root()
dashboard1_dir <- file.path(project_root, "Dashboard 1")
dashboard2_dir <- file.path(project_root, "Dashboard 2")

load_reference_styles <- function(root_dir) {
  style_path <- file.path(root_dir, "Dashboard 2", "www", "styles.css")
  base_styles <- if (file.exists(style_path)) {
    paste(readLines(style_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    ""
  }

  extra_styles <- paste(
    ".navbar, .navbar-default {",
    "  background: rgba(255, 253, 249, 0.95) !important;",
    "  border-bottom: 1px solid rgba(33, 53, 71, 0.08) !important;",
    "  box-shadow: 0 10px 24px rgba(52, 70, 87, 0.05);",
    "}",
    ".navbar-brand {",
    "  color: #22313f !important;",
    "  font-family: 'Iowan Old Style', 'Palatino Linotype', 'Book Antiqua', serif;",
    "  font-size: 1.15rem;",
    "  font-weight: 700;",
    "  letter-spacing: -0.01em;",
    "}",
    ".navbar-nav > li > a {",
    "  color: #5b6a78 !important;",
    "  font-weight: 700;",
    "}",
    ".navbar-nav > li.active > a, .navbar-nav > li > a:hover {",
    "  background: transparent !important;",
    "  color: #22313f !important;",
    "}",
    ".dashboard-tab {",
    "  padding-top: 1rem;",
    "}",
    ".summary-grid {",
    "  display: grid;",
    "  gap: 0.65rem;",
    "  grid-template-columns: repeat(3, minmax(0, 1fr));",
    "  margin-top: 0.35rem;",
    "}",
    ".summary-stat {",
    "  background: #f7f4ef;",
    "  border-radius: 12px;",
    "  padding: 0.55rem 0.65rem;",
    "}",
    ".summary-stat .choice-label {",
    "  display: block;",
    "  margin-bottom: 0.15rem;",
    "}",
    ".summary-stat .choice-value {",
    "  font-size: 0.98rem;",
    "  margin-top: 0;",
    "}",
    ".detail-shell {",
    "  padding: 0 1rem 1rem;",
    "}",
    ".d1-snapshot-grid {",
    "  display: grid;",
    "  gap: 0.8rem;",
    "  grid-template-columns: repeat(2, minmax(0, 1fr));",
    "  margin-top: 0.15rem;",
    "}",
    ".d1-snapshot-tile {",
    "  background: #f7f4ef;",
    "  border: 1px solid rgba(33, 53, 71, 0.08);",
    "  border-radius: 16px;",
    "  min-height: 92px;",
    "  padding: 0.9rem 1rem;",
    "  display: flex;",
    "  flex-direction: column;",
    "  justify-content: space-between;",
    "  box-shadow: inset 0 1px 0 rgba(255,255,255,0.45);",
    "}",
    ".d1-snapshot-tile .metric-label {",
    "  color: #5b6a78;",
    "  font-size: 0.83rem;",
    "  font-weight: 700;",
    "  letter-spacing: 0.05em;",
    "  margin-bottom: 0.45rem;",
    "  text-transform: uppercase;",
    "}",
    ".d1-snapshot-tile .metric-value {",
    "  color: #22313f;",
    "  font-size: 1.35rem;",
    "  font-weight: 700;",
    "  line-height: 1.1;",
    "}",
    ".ranking-list {",
    "  display: grid;",
    "  gap: 0.45rem;",
    "}",
    ".ranking-row {",
    "  align-items: center;",
    "  display: grid;",
    "  gap: 0.6rem;",
    "  grid-template-columns: 138px 1fr auto;",
    "}",
    ".ranking-label {",
    "  color: #425362;",
    "  font-size: 0.9rem;",
    "  font-weight: 600;",
    "}",
    ".ranking-track {",
    "  background: #ece6db;",
    "  border-radius: 999px;",
    "  height: 8px;",
    "  overflow: hidden;",
    "}",
    ".ranking-fill {",
    "  background: linear-gradient(90deg, #f1b24a 0%, #2e6e9e 100%);",
    "  border-radius: 999px;",
    "  height: 100%;",
    "}",
    ".ranking-badge {",
    "  color: #22313f;",
    "  font-size: 0.85rem;",
    "  font-weight: 700;",
    "  white-space: nowrap;",
    "}",
    ".insight-stack {",
    "  display: grid;",
    "  gap: 0.8rem;",
    "  padding: 0 1rem 1rem;",
    "}",
    ".insight-note {",
    "  background: #f7f4ef;",
    "  border: 1px solid rgba(33, 53, 71, 0.08);",
    "  border-radius: 14px;",
    "  padding: 0.9rem 1rem;",
    "}",
    ".insight-note h4 {",
    "  color: #22313f;",
    "  font-size: 0.92rem;",
    "  font-weight: 700;",
    "  letter-spacing: 0.04em;",
    "  margin: 0 0 0.35rem;",
    "  text-transform: uppercase;",
    "}",
    ".insight-note p {",
    "  color: #425362;",
    "  font-size: 0.95rem;",
    "  line-height: 1.5;",
    "  margin: 0;",
    "}",
    ".empty-note {",
    "  color: #5b6a78;",
    "  font-size: 1rem;",
    "  padding: 1rem 1.2rem 1.2rem;",
    "}",
    ".empty-note strong {",
    "  color: #22313f;",
    "}",
    "@media (max-width: 992px) {",
    "  .summary-grid {",
    "    grid-template-columns: 1fr;",
    "  }",
    "  .d1-snapshot-grid {",
    "    grid-template-columns: 1fr;",
    "  }",
    "  .ranking-row {",
    "    grid-template-columns: 1fr;",
    "  }",
    "}",
    sep = "\n"
  )

  paste(base_styles, extra_styles, sep = "\n")
}

shared_styles <- load_reference_styles(project_root)

plot_theme <- function() {
  theme_minimal(base_size = 12, base_family = "Avenir Next") +
    theme(
      plot.title = element_text(face = "bold", size = 14, colour = "#213547"),
      plot.subtitle = element_text(size = 10.5, colour = "#5A6B7B"),
      axis.title = element_text(face = "bold", colour = "#213547"),
      axis.text = element_text(colour = "#44576A"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10)
    )
}

collapse_with_and <- function(values) {
  values <- values[!is.na(values)]

  if (length(values) <= 1) {
    return(values)
  }

  if (length(values) == 2) {
    return(paste(values, collapse = " and "))
  }

  paste0(
    paste(values[-length(values)], collapse = ", "),
    ", and ",
    values[length(values)]
  )
}

# =============================================================================
# Dashboard 1 Begins Here
# Changes made to Dashboard 1 for consistency with Dashboard 2:
# 1. Replaced the flexdashboard banner with Dashboard 2's hero + control-card layout.
# 2. Reorganized the map, scatter, ward detail, and insight sections into the same
#    two-row card structure used by Dashboard 2.
# 3. Harmonized typography, colors, legends, spacing, and metric tiles while
#    preserving the original turnout-focused content and interaction logic.
# =============================================================================

d1_sub_choices <- list(
  income = c("Under $30k" = "under30k", "$100k+" = "100kplus"),
  education = c(
    "No Diploma / High School" = "nodip",
    "College / Trades" = "college",
    "Bachelor's+" = "bach"
  ),
  age = c(
    "Age 0-14" = "0to14",
    "Age 15-24" = "15to24",
    "Age 25-44" = "25to44",
    "Age 45-64" = "45to64",
    "Age 65+" = "65plus"
  ),
  immigration = c("Immigrants" = "imm", "Non-immigrants" = "nonimm")
)

d1_get_x_info <- function(df, attr, sub) {
  switch(
    paste(attr, sub, sep = "_"),
    income_under30k = list(x = df$pct_income_under_30k, label = "% of households earning under $30k"),
    income_100kplus = list(x = df$pct_income_100k_plus, label = "% of households earning $100k+"),
    education_nodip = list(x = df$pct_no_diploma, label = "% of population with no diploma / high school"),
    education_college = list(x = df$pct_college_trades, label = "% of population with college / trades"),
    education_bach = list(x = df$pct_bachelor_plus, label = "% of population with bachelor's degree or higher"),
    age_0to14 = list(x = df$pct_age_0_to_14, label = "% of population aged 0-14"),
    age_15to24 = list(x = df$pct_age_15_to_24, label = "% of population aged 15-24"),
    age_25to44 = list(x = df$pct_age_25_to_44, label = "% of population aged 25-44"),
    age_45to64 = list(x = df$pct_age_45_to_64, label = "% of population aged 45-64"),
    age_65plus = list(x = df$pct_age_65_plus, label = "% of population aged 65+"),
    immigration_imm = list(x = df$pct_immigrants, label = "% of population who are immigrants"),
    immigration_nonimm = list(x = df$pct_non_immigrants, label = "% of population who are non-immigrants"),
    NULL
  )
}

d1_ward_rank <- function(column, value) {
  rank(-column, ties.method = "min")[which(round(column, 6) == round(value, 6))][1]
}

create_dashboard1_data <- function(data_dir) {
  ward_lookup <- read_excel(file.path(data_dir, "25-wardnames-numbers.xlsx"))
  voter_stats <- read_excel(
    file.path(data_dir, "2023-mayoral-by-election-voter-statistics.xlsx"),
    sheet = "2023 Voter Turnout Statisti M"
  )
  names(voter_stats) <- names(voter_stats) %>%
    gsub("([a-z0-9])([A-Z])", "\\1_\\2", ., perl = TRUE) %>%
    gsub("[^A-Za-z0-9]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .) %>%
    tolower()
  ward_census <- read_excel(
    file.path(data_dir, "2023-WardProfiles-2011-2021-CensusData.xlsx"),
    sheet = "2021 One Variable"
  )
  ward_geo <- read_excel(file.path(data_dir, "2023-WardProfiles-GeographicAreas.xlsx"))

  lookup_clean <- ward_lookup
  names(lookup_clean) <- c("ward", "ward_name")
  lookup_clean <- lookup_clean %>%
    mutate(
      ward = as.numeric(ward),
      ward_name = as.character(ward_name)
    ) %>%
    filter(!is.na(ward))
  lookup_clean <- bind_rows(tibble::tibble(ward = 0, ward_name = "Toronto"), lookup_clean)

  voter_clean <- voter_stats %>%
    mutate(
      ward = trimws(as.character(ward))
    ) %>%
    filter(!is.na(ward), ward != "", grepl("^[0-9]+$", ward)) %>%
    mutate(
      ward = as.numeric(ward),
      total_eligible_electors = as.numeric(gsub(",", "", total_eligible_electors)),
      number_voted = as.numeric(gsub(",", "", number_voted))
    ) %>%
    group_by(ward) %>%
    summarise(
      eligible_voters = sum(total_eligible_electors, na.rm = TRUE),
      number_voted = sum(number_voted, na.rm = TRUE),
      percent_voted = number_voted / eligible_voters * 100,
      .groups = "drop"
    ) %>%
    left_join(lookup_clean, by = "ward") %>%
    select(ward, ward_name, eligible_voters, number_voted, percent_voted)

  toronto_voter <- voter_clean %>%
    summarise(
      ward = 0,
      ward_name = "Toronto",
      eligible_voters = sum(eligible_voters, na.rm = TRUE),
      number_voted = sum(number_voted, na.rm = TRUE),
      percent_voted = number_voted / eligible_voters * 100
    )
  voter_clean <- bind_rows(toronto_voter, voter_clean)

  names(ward_census)[1] <- "indicator"
  names(ward_census)[2:ncol(ward_census)] <- c("toronto", paste0("ward_", 1:(ncol(ward_census) - 2)))

  ward_census$indicator <- ward_census$indicator %>%
    as.character() %>%
    iconv(to = "UTF-8") %>%
    chartr("\u2018\u2019\u201C\u201D", "''\"\"", .)

  ward_census2 <- ward_census %>%
    filter(!if_all(everything(), is.na)) %>%
    filter(!is.na(indicator)) %>%
    filter(
      !indicator %in% c(
        "2021 Census",
        "Source:",
        "Statistics Canada, 2021 Census, Custom Tabulations, 2023.",
        "Wards:",
        "25 Ward model",
        "Prepared by:",
        "Strategic Initiatives, Policy & Analysis",
        "City Planning Division, City of Toronto",
        "December, 2023"
      )
    )

  keep_rows <- c(
    "Total - Age", "0 to 4 years", "5 to 9 years", "10 to 14 years",
    "15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years",
    "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years",
    "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 to 74 years",
    "75 to 79 years", "80 to 84 years", "85 to 89 years", "90 years and over",
    "Median age", "Non-immigrants", "Immigrants",
    "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data",
    "No certificate, diploma or degree",
    "High (secondary) school diploma or equivalency certificate",
    "Postsecondary certificate, diploma or degree",
    "Postsecondary certificate or diploma below bachelor level",
    "Apprenticeship or trades certificate or diploma",
    "Non-apprenticeship trades certificate or diploma", "Apprenticeship certificate",
    "College, CEGEP or other non-university certificate or diploma",
    "University certificate or diploma below bachelor level",
    "Bachelor's degree or higher", "Bachelor's degree",
    "University certificate or diploma above bachelor level",
    "Degree in medicine, dentistry, veterinary medicine or optometry",
    "Master's degree", "Earned doctorate",
    "Total - Population aged 15 years and over by Labour force status - 25% sample data",
    "In the labour force", "Employed", "Unemployed", "Not in the labour force",
    "Unemployment rate",
    "Total - Household total income groups in 2020 for private households - 25% sample data",
    "Under $5,000", "$5,000 to $9,999", "$10,000 to $14,999", "$15,000 to $19,999",
    "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999", "$35,000 to $39,999",
    "$40,000 to $44,999", "$45,000 to $49,999", "$50,000 to $59,999", "$60,000 to $69,999",
    "$70,000 to $79,999", "$80,000 to $89,999", "$90,000 to $99,999", "$100,000 and over",
    "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $199,999", "$200,000 and over"
  )

  census_selected <- ward_census2 %>%
    filter(indicator %in% keep_rows) %>%
    select(indicator, toronto, starts_with("ward_")) %>%
    distinct(indicator, .keep_all = TRUE)

  census_mat <- as.data.frame(census_selected, stringsAsFactors = FALSE)
  rownames(census_mat) <- census_mat$indicator
  census_mat$indicator <- NULL
  census_clean <- as.data.frame(t(census_mat), stringsAsFactors = FALSE)
  census_clean$region <- rownames(census_clean)
  rownames(census_clean) <- NULL
  census_clean <- census_clean %>%
    select(region, everything()) %>%
    mutate(
      ward = case_when(
        region == "toronto" ~ 0,
        TRUE ~ suppressWarnings(as.numeric(str_remove(region, "ward_")))
      )
    ) %>%
    mutate(across(-c(region, ward), ~ as.numeric(gsub(",", "", .)))) %>%
    select(-region) %>%
    left_join(lookup_clean, by = "ward") %>%
    select(ward, ward_name, everything())

  census_clean <- census_clean %>%
    transmute(
      ward,
      ward_name,
      age_0_to_14 = `0 to 4 years` + `5 to 9 years` + `10 to 14 years`,
      age_15_to_24 = `15 to 19 years` + `20 to 24 years`,
      age_25_to_44 = `25 to 29 years` + `30 to 34 years` + `35 to 39 years` + `40 to 44 years`,
      age_45_to_64 = `45 to 49 years` + `50 to 54 years` + `55 to 59 years` + `60 to 64 years`,
      age_65_plus = `65 to 69 years` + `70 to 74 years` + `75 to 79 years` +
        `80 to 84 years` + `85 to 89 years` + `90 years and over`,
      highschool_diploma_or_under =
        `No certificate, diploma or degree` +
        `High (secondary) school diploma or equivalency certificate`,
      edu_college_trades =
        `Postsecondary certificate or diploma below bachelor level` +
        `Apprenticeship or trades certificate or diploma` +
        `Non-apprenticeship trades certificate or diploma` + `Apprenticeship certificate` +
        `College, CEGEP or other non-university certificate or diploma` +
        `University certificate or diploma below bachelor level`,
      edu_bachelor_plus = `Bachelor's degree or higher`,
      immigrants = Immigrants,
      non_immigrants = `Non-immigrants`,
      in_labour_force = `In the labour force`,
      employed = Employed,
      unemployed = Unemployed,
      not_in_labour_force = `Not in the labour force`,
      unemployment_rate = `Unemployment rate`,
      income_under_30k = `Under $5,000` + `$5,000 to $9,999` + `$10,000 to $14,999` +
        `$15,000 to $19,999` + `$20,000 to $24,999` + `$25,000 to $29,999`,
      income_30k_to_59k = `$30,000 to $34,999` + `$35,000 to $39,999` + `$40,000 to $44,999` +
        `$45,000 to $49,999` + `$50,000 to $59,999`,
      income_60k_to_99k = `$60,000 to $69,999` + `$70,000 to $79,999` +
        `$80,000 to $89,999` + `$90,000 to $99,999`,
      income_100k_plus = `$100,000 to $124,999` + `$125,000 to $149,999` +
        `$150,000 to $199,999` + `$200,000 and over`
    )

  geo_clean <- ward_geo[-(1:11), ]
  names(geo_clean) <- c("ward", "area_sq_km")
  geo_clean <- geo_clean %>%
    mutate(
      ward = as.numeric(ward),
      area_sq_km = as.numeric(area_sq_km)
    ) %>%
    filter(!is.na(ward))
  toronto_area <- geo_clean %>%
    summarise(ward = 0, area_sq_km = sum(area_sq_km, na.rm = TRUE))
  geo_clean <- bind_rows(toronto_area, geo_clean) %>%
    left_join(lookup_clean, by = "ward") %>%
    select(ward, ward_name, everything())

  final_data <- voter_clean %>%
    left_join(census_clean, by = c("ward", "ward_name")) %>%
    left_join(geo_clean, by = c("ward", "ward_name"))

  scatter_data <- final_data %>%
    filter(ward != 0) %>%
    mutate(
      total_edu = highschool_diploma_or_under + edu_college_trades + edu_bachelor_plus,
      pct_no_diploma = ifelse(total_edu > 0, highschool_diploma_or_under / total_edu * 100, NA),
      pct_college_trades = ifelse(total_edu > 0, edu_college_trades / total_edu * 100, NA),
      pct_bachelor_plus = ifelse(total_edu > 0, edu_bachelor_plus / total_edu * 100, NA),
      total_income = income_under_30k + income_30k_to_59k + income_60k_to_99k + income_100k_plus,
      pct_income_under_30k = ifelse(total_income > 0, income_under_30k / total_income * 100, NA),
      pct_income_30k_to_59k = ifelse(total_income > 0, income_30k_to_59k / total_income * 100, NA),
      pct_income_60k_to_99k = ifelse(total_income > 0, income_60k_to_99k / total_income * 100, NA),
      pct_income_100k_plus = ifelse(total_income > 0, income_100k_plus / total_income * 100, NA),
      total_age = age_0_to_14 + age_15_to_24 + age_25_to_44 + age_45_to_64 + age_65_plus,
      pct_age_0_to_14 = ifelse(total_age > 0, age_0_to_14 / total_age * 100, NA),
      pct_age_15_to_24 = ifelse(total_age > 0, age_15_to_24 / total_age * 100, NA),
      pct_age_25_to_44 = ifelse(total_age > 0, age_25_to_44 / total_age * 100, NA),
      pct_age_45_to_64 = ifelse(total_age > 0, age_45_to_64 / total_age * 100, NA),
      pct_age_65_plus = ifelse(total_age > 0, age_65_plus / total_age * 100, NA),
      total_imm = immigrants + non_immigrants,
      pct_immigrants = ifelse(total_imm > 0, immigrants / total_imm * 100, NA),
      pct_non_immigrants = ifelse(total_imm > 0, non_immigrants / total_imm * 100, NA)
    )

  toronto_row <- final_data %>% filter(ward == 0)
  city_summary <- list(
    avg_turnout = round(toronto_row$percent_voted, 1),
    total_voted = scales::comma(toronto_row$number_voted),
    total_elig = scales::comma(toronto_row$eligible_voters)
  )

  map_sf <- tryCatch(
    {
      ward_shapes <- sf::st_read(file.path(data_dir, "toronto_wards.geojson"), quiet = TRUE)
      id_candidates <- c("AREA_SHORT_CODE", "AREA_S_CD", "ward")
      name_candidates <- c("AREA_NAME", "ward_name", "NAME")
      id_col <- id_candidates[id_candidates %in% names(ward_shapes)][1]
      name_col <- name_candidates[name_candidates %in% names(ward_shapes)][1]

      ward_shapes %>%
        mutate(
          ward = as.integer(.data[[id_col]]),
          ward_name_shape = if (!is.na(name_col)) as.character(.data[[name_col]]) else NA_character_
        ) %>%
        left_join(scatter_data, by = "ward") %>%
        mutate(ward_name = dplyr::coalesce(ward_name, ward_name_shape)) %>%
        filter(!is.na(percent_voted))
    },
    error = function(e) NULL
  )

  list(
    scatter_data = scatter_data,
    map_sf = map_sf,
    city_summary = city_summary,
    sub_choices = d1_sub_choices
  )
}

dashboard1_data <- create_dashboard1_data(dashboard1_dir)

build_d1_rank_row <- function(label, rank_value, total_wards = 25) {
  bar_width <- round((total_wards - rank_value + 1) / total_wards * 100)

  tags$div(
    class = "ranking-row",
    tags$span(class = "ranking-label", label),
    tags$div(
      class = "ranking-track",
      tags$div(class = "ranking-fill", style = paste0("width:", bar_width, "%;"))
    ),
    tags$span(class = "ranking-badge", paste0("Rank ", rank_value, " of ", total_wards))
  )
}

d1_ui <- function() {
  tagList(
    div(
      class = "dashboard-tab",
      div(
        class = "hero-banner",
        div(class = "hero-eyebrow", "Toronto 2023 Civic Participation Dashboard"),
        h1("Who Votes in Toronto?"),
        p(
          class = "hero-subtitle",
          "A ward-level explorer of turnout, socioeconomic context, and neighbourhood-level participation patterns."
        ),
        div(
          class = "control-grid",
          div(
            class = "control-card",
            tags$label(`for` = "d1_attr_select", "Explore by"),
            selectInput(
              inputId = "d1_attr_select",
              label = NULL,
              choices = c(
                "Income" = "income",
                "Education" = "education",
                "Age" = "age",
                "Immigration" = "immigration"
              ),
              selected = "income"
            )
          ),
          div(
            class = "control-card",
            tags$label(`for` = "d1_sub_select", "Detail within category"),
            selectInput(
              inputId = "d1_sub_select",
              label = NULL,
              choices = dashboard1_data$sub_choices[["income"]],
              selected = unname(dashboard1_data$sub_choices[["income"]][1])
            )
          ),
          div(
            class = "control-card static-choice",
            tags$label("Toronto Summary"),
            div(
              class = "summary-grid",
              div(
                class = "summary-stat",
                tags$span(class = "choice-label", "Citywide turnout"),
                tags$div(class = "choice-value", paste0(dashboard1_data$city_summary$avg_turnout, "%"))
              ),
              div(
                class = "summary-stat",
                tags$span(class = "choice-label", "Votes cast"),
                tags$div(class = "choice-value", dashboard1_data$city_summary$total_voted)
              ),
              div(
                class = "summary-stat",
                tags$span(class = "choice-label", "Eligible voters"),
                tags$div(class = "choice-value", dashboard1_data$city_summary$total_elig)
              )
            ),
            p(
              class = "static-note",
              "Use the map or scatter plot to focus on one ward and compare its turnout profile with the rest of Toronto."
            )
          )
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          class = "viz-card",
          card_header("Ward Turnout Map"),
          p(
            class = "panel-subtitle",
            "Click a ward to inspect its turnout profile and highlight the same ward across the dashboard."
          ),
          leafletOutput("d1_ward_map", height = 470)
        ),
        card(
          full_screen = TRUE,
          class = "viz-card",
          card_header("Turnout vs. Demographic Context"),
          p(
            class = "panel-subtitle",
            "Compare turnout with income, education, age, or immigration composition across Toronto's 25 wards."
          ),
          plotlyOutput("d1_scatter_plot", height = 470)
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          class = "viz-card profile-card",
          card_header("Ward Detail"),
          p(
            class = "panel-subtitle",
            "The selected ward's turnout, demographic snapshot, and rankings among Toronto's 25 wards."
          ),
          uiOutput("d1_ward_profile")
        ),
        card(
          full_screen = TRUE,
          class = "viz-card profile-card",
          card_header("Key Insights"),
          p(
            class = "panel-subtitle",
            "A concise summary of the current turnout relationship, extreme wards, and how the selected ward compares with Toronto overall."
          ),
          uiOutput("d1_insights_panel")
        )
      )
    )
  )
}

# =============================================================================
# Dashboard 2 Begins Here
# The original political-preference dashboard logic is preserved below. For the
# combined app, only input/output prefixes and null-safe selection handling were
# added so both dashboards can coexist in one runnable R file.
# =============================================================================

default_data_paths <- function(base_dir = dashboard2_dir) {
  list(
    mayor_file = file.path(base_dir, "data", "2023 Office of the Mayor.xlsx"),
    census_file = file.path(base_dir, "data", "2023-WardProfiles-2011-2021-CensusData.xlsx"),
    area_file = file.path(base_dir, "data", "2023-WardProfiles-GeographicAreas.xlsx"),
    ward_shapefile = file.path(
      base_dir,
      "data",
      "25-ward-model-december-2018-wgs84-latitude-longitude",
      "WARD_WGS84.shp"
    )
  )
}

resolve_data_paths <- function(base_dir = dashboard2_dir, overrides = list()) {
  utils::modifyList(default_data_paths(base_dir), overrides)
}

validate_input_paths <- function(paths) {
  missing_paths <- names(paths)[!file.exists(unlist(paths, use.names = FALSE))]

  if (length(missing_paths) > 0) {
    stop(
      paste("Missing required data files:", paste(missing_paths, collapse = ", ")),
      call. = FALSE
    )
  }

  invisible(paths)
}

as_numeric_clean <- function(x) {
  as.numeric(gsub(",", "", trimws(as.character(x))))
}

extract_ward_number <- function(x) {
  as.integer(gsub(".*?(\\d+).*", "\\1", x))
}

clean_text <- function(x) {
  enc2utf8(trimws(as.character(x)))
}

extract_ward_name_from_title <- function(title) {
  clean_title <- clean_text(title)
  sub("^City Ward\\s+\\d+\\s+", "", clean_title)
}

find_census_header_row <- function(sheet) {
  header_hits <- which(
    vapply(
      seq_len(nrow(sheet)),
      function(i) {
        row_values <- clean_text(unlist(sheet[i, ], use.names = FALSE))
        any(row_values == "Ward 1", na.rm = TRUE)
      },
      logical(1)
    )
  )

  if (length(header_hits) == 0) {
    stop("Could not locate the ward header row in the census workbook.", call. = FALSE)
  }

  header_hits[1]
}

build_ward_lookup <- function(sheet) {
  header_row <- find_census_header_row(sheet)
  header_values <- clean_text(unlist(sheet[header_row, ], use.names = FALSE))
  ward_cols <- which(grepl("^Ward\\s+\\d+$", header_values))

  tibble::tibble(
    column = ward_cols,
    ward_label = header_values[ward_cols],
    ward_id = extract_ward_number(header_values[ward_cols])
  )
}

get_row_index <- function(sheet, pattern) {
  labels <- clean_text(sheet[[1]])
  row_hits <- grep(pattern, labels, ignore.case = TRUE)

  if (length(row_hits) == 0) {
    stop(
      paste("Could not find a census row matching pattern:", pattern),
      call. = FALSE
    )
  }

  row_hits[1]
}

extract_metric <- function(sheet, lookup, pattern, value_name) {
  row_index <- get_row_index(sheet, pattern)
  values <- as_numeric_clean(unlist(sheet[row_index, lookup$column], use.names = FALSE))

  out <- tibble::tibble(ward_id = lookup$ward_id)
  out[[value_name]] <- values
  out
}

read_mayor_results <- function(mayor_file) {
  ward_sheets <- readxl::excel_sheets(mayor_file)
  ward_sheets <- ward_sheets[grepl("^Ward\\s+\\d+$", ward_sheets)]

  results_long <- purrr::map_dfr(
    ward_sheets,
    function(sheet_name) {
      raw_sheet <- suppressMessages(
        readxl::read_excel(mayor_file, sheet = sheet_name, col_names = FALSE)
      )

      label_column <- clean_text(raw_sheet[[1]])
      mayor_row <- which(label_column == "Mayor")[1]
      totals_row <- which(grepl("Totals$", label_column))[1]

      if (is.na(mayor_row) || is.na(totals_row)) {
        stop(
          paste("Could not parse candidate rows in sheet:", sheet_name),
          call. = FALSE
        )
      }

      ward_id <- extract_ward_number(sheet_name)
      ward_name <- extract_ward_name_from_title(raw_sheet[[1]][1])
      total_votes <- as_numeric_clean(raw_sheet[[ncol(raw_sheet)]][totals_row])
      candidate_rows <- seq.int(mayor_row + 1, totals_row - 1)

      tibble::tibble(
        ward_id = ward_id,
        ward_name = ward_name,
        candidate = clean_text(raw_sheet[[1]][candidate_rows]),
        candidate_votes = as_numeric_clean(raw_sheet[[ncol(raw_sheet)]][candidate_rows]),
        total_votes = total_votes
      ) %>%
        dplyr::filter(!is.na(candidate), candidate != "") %>%
        dplyr::mutate(vote_share = candidate_votes / total_votes)
    }
  )

  total_city_votes <- results_long %>%
    dplyr::distinct(ward_id, total_votes) %>%
    dplyr::summarise(total_votes = sum(total_votes, na.rm = TRUE)) %>%
    dplyr::pull(total_votes)

  citywide_totals <- results_long %>%
    dplyr::group_by(candidate) %>%
    dplyr::summarise(
      citywide_votes = sum(candidate_votes, na.rm = TRUE),
      citywide_vote_share = citywide_votes / total_city_votes,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(citywide_votes)) %>%
    dplyr::mutate(citywide_rank = dplyr::row_number())

  results_long <- results_long %>%
    dplyr::left_join(citywide_totals, by = "candidate")

  ward_summary <- results_long %>%
    dplyr::group_by(ward_id, ward_name) %>%
    dplyr::summarise(
      total_votes = dplyr::first(total_votes),
      winner = candidate[which.max(vote_share)],
      winner_share = max(vote_share, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    results_long = results_long,
    ward_summary = ward_summary,
    citywide_totals = citywide_totals
  )
}

read_census_metrics <- function(census_file) {
  census_sheet <- suppressMessages(
    readxl::read_excel(census_file, sheet = "2021 One Variable", col_names = FALSE)
  )

  ward_lookup <- build_ward_lookup(census_sheet)

  population <- extract_metric(census_sheet, ward_lookup, "^Total - Age$", "population")
  median_age <- extract_metric(census_sheet, ward_lookup, "^Median age$", "median_age")
  median_income <- extract_metric(
    census_sheet,
    ward_lookup,
    "^Median total income of households in 2020 \\(\\$\\)$",
    "median_income"
  )
  education_total <- extract_metric(
    census_sheet,
    ward_lookup,
    "^Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households",
    "education_total"
  )
  bachelor_or_higher <- extract_metric(
    census_sheet,
    ward_lookup,
    "^Bachelor.*degree or higher$",
    "bachelor_or_higher"
  )
  immigrant_total <- extract_metric(
    census_sheet,
    ward_lookup,
    "^Total - Immigrant status and period of immigration for the population in private households",
    "immigrant_total"
  )
  immigrants <- extract_metric(census_sheet, ward_lookup, "^Immigrants$", "immigrants")
  visible_minority_total <- extract_metric(
    census_sheet,
    ward_lookup,
    "^Total - Visible minority for the population in private households",
    "visible_minority_total"
  )
  visible_minority <- extract_metric(
    census_sheet,
    ward_lookup,
    "^Total visible minority population$",
    "visible_minority"
  )

  population %>%
    dplyr::left_join(median_age, by = "ward_id") %>%
    dplyr::left_join(median_income, by = "ward_id") %>%
    dplyr::left_join(education_total, by = "ward_id") %>%
    dplyr::left_join(bachelor_or_higher, by = "ward_id") %>%
    dplyr::left_join(immigrant_total, by = "ward_id") %>%
    dplyr::left_join(immigrants, by = "ward_id") %>%
    dplyr::left_join(visible_minority_total, by = "ward_id") %>%
    dplyr::left_join(visible_minority, by = "ward_id") %>%
    dplyr::mutate(
      bachelor_share = dplyr::if_else(education_total > 0, bachelor_or_higher / education_total, NA_real_),
      immigrant_share = dplyr::if_else(immigrant_total > 0, immigrants / immigrant_total, NA_real_),
      visible_minority_share = dplyr::if_else(
        visible_minority_total > 0,
        visible_minority / visible_minority_total,
        NA_real_
      )
    )
}

read_area_data <- function(area_file) {
  area_sheet <- suppressMessages(
    readxl::read_excel(area_file, sheet = 1, skip = 11)
  )

  tibble::tibble(
    ward_id = as.integer(area_sheet[[1]]),
    area_sq_km = as.numeric(area_sheet[[2]])
  ) %>%
    dplyr::filter(!is.na(ward_id))
}

read_ward_shapes <- function(shapefile_path) {
  sf::st_read(shapefile_path, quiet = TRUE) %>%
    dplyr::transmute(
      ward_id = as.integer(AREA_S_CD),
      ward_name = AREA_NAME
    ) %>%
    sf::st_as_sf()
}

build_candidate_palette <- function(citywide_totals) {
  candidates <- citywide_totals$candidate
  base_palette <- grDevices::hcl.colors(length(candidates), palette = "Dark 3")
  names(base_palette) <- candidates

  accent_palette <- c(
    "#B77AD7",
    "#C8D400",
    "#97A3B3",
    "#6F7E8F",
    "#B8860B",
    "#7A5C70"
  )

  top_candidates <- head(candidates, length(accent_palette))
  base_palette[top_candidates] <- accent_palette[seq_along(top_candidates)]
  base_palette
}

build_demographic_config <- function() {
  list(
    median_income = list(
      label = "Median Household Income",
      description = "Median total household income in 2020",
      axis_formatter = scales::label_dollar(scale_cut = scales::cut_short_scale()),
      value_formatter = scales::label_dollar(accuracy = 1)
    ),
    bachelor_share = list(
      label = "Bachelor's Degree or Higher",
      description = "Share of residents aged 15+ with a bachelor's degree or higher",
      axis_formatter = scales::label_percent(accuracy = 1),
      value_formatter = scales::label_percent(accuracy = 0.1)
    ),
    immigrant_share = list(
      label = "Immigrant Share",
      description = "Share of residents in private households who are immigrants",
      axis_formatter = scales::label_percent(accuracy = 1),
      value_formatter = scales::label_percent(accuracy = 0.1)
    ),
    visible_minority_share = list(
      label = "Visible Minority Share",
      description = "Share of residents in private households identified as visible minorities",
      axis_formatter = scales::label_percent(accuracy = 1),
      value_formatter = scales::label_percent(accuracy = 0.1)
    ),
    median_age = list(
      label = "Median Age",
      description = "Median age of residents",
      axis_formatter = scales::label_number(accuracy = 0.1),
      value_formatter = scales::label_number(accuracy = 0.1)
    ),
    population_density = list(
      label = "Population Density",
      description = "Residents per square kilometre",
      axis_formatter = scales::label_comma(accuracy = 1),
      value_formatter = scales::label_comma(accuracy = 1)
    )
  )
}

make_quantile_bins <- function(values, formatter, n = 5) {
  clean_values <- as.numeric(values)
  quantiles <- unique(
    stats::quantile(
      clean_values,
      probs = seq(0, 1, length.out = n + 1),
      na.rm = TRUE,
      names = FALSE
    )
  )

  if (length(quantiles) <= 2) {
    return(
      factor(
        rep("All wards", length(clean_values)),
        levels = "All wards",
        ordered = TRUE
      )
    )
  }

  bins <- cut(clean_values, breaks = quantiles, include.lowest = TRUE, ordered_result = TRUE)

  labels <- vapply(
    seq_len(length(quantiles) - 1),
    function(i) {
      paste0(formatter(quantiles[i]), " to ", formatter(quantiles[i + 1]))
    },
    character(1)
  )

  factor(labels[as.integer(bins)], levels = labels, ordered = TRUE)
}

format_metric_value <- function(value, variable, config) {
  config[[variable]]$value_formatter(value)
}

build_ward_tooltips <- function(ward_summary, results_long, config) {
  top_candidates <- results_long %>%
    dplyr::group_by(ward_id) %>%
    dplyr::arrange(dplyr::desc(vote_share), .by_group = TRUE) %>%
    dplyr::slice_head(n = 3) %>%
    dplyr::mutate(line = paste0(candidate, ": ", scales::percent(vote_share, accuracy = 0.1))) %>%
    dplyr::summarise(top_candidates_html = paste(line, collapse = "<br/>"), .groups = "drop")

  ward_summary %>%
    dplyr::left_join(top_candidates, by = "ward_id") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      tooltip_html = paste0(
        "<strong>", ward_name, "</strong><br/>",
        "Winner: ", winner, " (", scales::percent(winner_share, accuracy = 0.1), ")<br/>",
        "Total votes: ", scales::comma(total_votes), "<br/><br/>",
        "<strong>Top candidates</strong><br/>", top_candidates_html, "<br/><br/>",
        "<strong>Demographics</strong><br/>",
        "Median income: ", format_metric_value(median_income, "median_income", config), "<br/>",
        "Bachelor's+: ", format_metric_value(bachelor_share, "bachelor_share", config), "<br/>",
        "Immigrant share: ", format_metric_value(immigrant_share, "immigrant_share", config), "<br/>",
        "Visible minority share: ", format_metric_value(visible_minority_share, "visible_minority_share", config), "<br/>",
        "Median age: ", format_metric_value(median_age, "median_age", config), "<br/>",
        "Population density: ", format_metric_value(population_density, "population_density", config), " / sq. km"
      )
    ) %>%
    dplyr::ungroup()
}

create_dashboard2_data <- function(base_dir = dashboard2_dir, path_overrides = list()) {
  data_paths <- resolve_data_paths(base_dir, path_overrides)
  validate_input_paths(data_paths)

  mayor_results <- read_mayor_results(data_paths$mayor_file)
  census_metrics <- read_census_metrics(data_paths$census_file)
  area_data <- read_area_data(data_paths$area_file)
  ward_shapes <- read_ward_shapes(data_paths$ward_shapefile)
  demographic_config <- build_demographic_config()

  ward_summary <- mayor_results$ward_summary %>%
    dplyr::left_join(census_metrics, by = "ward_id") %>%
    dplyr::left_join(area_data, by = "ward_id") %>%
    dplyr::mutate(population_density = population / area_sq_km)

  ward_summary <- build_ward_tooltips(
    ward_summary = ward_summary,
    results_long = mayor_results$results_long,
    config = demographic_config
  )

  ward_sf <- ward_shapes %>%
    dplyr::left_join(ward_summary, by = c("ward_id", "ward_name")) %>%
    dplyr::arrange(ward_id)

  candidate_palette <- build_candidate_palette(mayor_results$citywide_totals)
  featured_candidates <- head(mayor_results$citywide_totals$candidate, 3)

  list(
    paths = data_paths,
    ward_sf = ward_sf,
    ward_attributes = sf::st_drop_geometry(ward_sf),
    results_long = mayor_results$results_long %>% dplyr::arrange(ward_id, dplyr::desc(vote_share)),
    citywide_totals = mayor_results$citywide_totals,
    candidate_palette = candidate_palette,
    featured_candidates = featured_candidates,
    demographic_config = demographic_config
  )
}

dashboard2_data <- create_dashboard2_data(dashboard2_dir)

d2_find_data_dir <- function(base_dir = dashboard2_dir) {
  env_dir <- Sys.getenv("TORONTO_DATA_DIR", unset = "")

  required_files <- c(
    "2023 Office of the Mayor.xlsx",
    "2023-WardProfiles-2011-2021-CensusData.xlsx",
    "25-ward-model-december-2018-wgs84-latitude-longitude/WARD_WGS84.shp"
  )

  candidates <- unique(c(
    file.path(base_dir, "data"),
    base_dir,
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
      "Could not find all required Dashboard 2 data files.\nChecked:\n- ",
      paste(candidates, collapse = "\n- ")
    ),
    call. = FALSE
  )
}

d2_stage_file_in_temp <- function(path, subdir = "toronto-dashboard-files") {
  staged_dir <- file.path(tempdir(), subdir)
  dir.create(staged_dir, recursive = TRUE, showWarnings = FALSE)

  staged_path <- file.path(staged_dir, basename(path))
  ok <- file.copy(path, staged_path, overwrite = TRUE)
  if (!ok) {
    stop("Could not stage file for reading: ", path, call. = FALSE)
  }

  staged_path
}

d2_stage_shapefile_in_temp <- function(shapefile_path) {
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

d2_read_sf_compat <- function(shapefile_path) {
  direct_read <- try(sf::st_read(shapefile_path, quiet = TRUE), silent = TRUE)
  if (!inherits(direct_read, "try-error")) {
    return(direct_read)
  }

  staged_path <- d2_stage_shapefile_in_temp(shapefile_path)
  sf::st_read(staged_path, quiet = TRUE)
}

d2_clean_numeric <- function(x) {
  out <- gsub(",", "", as.character(x))
  out <- gsub("%", "", out)
  suppressWarnings(as.numeric(out))
}

d2_clean_candidate_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_replace_all("[‘’]", "'")
}

d2_canonical_candidate_name <- function(x) {
  cleaned_name <- d2_clean_candidate_name(x)

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

d2_is_candidate_result_row <- function(candidate, votes) {
  !is.na(candidate) &
    candidate != "" &
    !is.na(votes) &
    !candidate %in% c("Mayor", "Total", "Totals", "TOTAL") &
    !stringr::str_detect(candidate, "^City Ward\\s+\\d+\\s+Totals$")
}

d2_find_optional_input_file <- function(file_name, base_dir = dashboard2_dir) {
  env_dir <- Sys.getenv("TORONTO_DATA_DIR", unset = "")
  project_dir <- dirname(base_dir)

  candidate_paths <- unique(c(
    file.path(base_dir, "data", file_name),
    file.path(base_dir, file_name),
    file.path(project_dir, "data", file_name),
    file.path(project_dir, file_name),
    file.path(project_dir, "Dashboard 1", "data", file_name),
    file.path(project_dir, "Dashboard 1", file_name),
    file.path(env_dir, file_name)
  ))
  existing_paths <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_paths) == 0) {
    return(file.path(base_dir, "data", file_name))
  }

  normalizePath(existing_paths[1], mustWork = TRUE)
}

d2_read_mayor_results_exact <- function(path) {
  staged_path <- d2_stage_file_in_temp(path)
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
    votes <- d2_clean_numeric(raw[[ncol(raw)]])

    tibble(
      ward_no = ward_no,
      candidate = candidate,
      votes = votes
    ) |>
      filter(row_number() >= 4) |>
      mutate(candidate = d2_canonical_candidate_name(candidate)) |>
      filter(d2_is_candidate_result_row(candidate, votes))
  })
}

d2_read_turnout_exact <- function(path) {
  staged_path <- d2_stage_file_in_temp(path)
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

d2_read_turnout_or_empty_exact <- function(path) {
  if (!file.exists(path)) {
    message("Turnout workbook not found. Continuing without turnout metrics.")
    return(tibble(
      ward_no = integer(),
      total_eligible = numeric(),
      number_voted = numeric(),
      turnout_pct = numeric()
    ))
  }

  d2_read_turnout_exact(path)
}

d2_read_census_metrics_exact <- function(path) {
  staged_path <- d2_stage_file_in_temp(path)
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

d2_format_percent_or_na <- function(x, accuracy = 0.1) {
  if (length(x) == 0 || is.na(x[1])) {
    return("N/A")
  }

  percent(x[1] / 100, accuracy = accuracy)
}

d2_format_bin_labels <- function(breaks, var_name) {
  if (identical(var_name, "median_income")) {
    format_break <- function(x) comma(round(x, 0))
  } else {
    format_break <- function(x) number(round(x, 1), accuracy = 0.1, trim = TRUE)
  }

  vapply(
    seq_len(length(breaks) - 1),
    function(i) {
      paste0(
        if (i == 1) "[" else "(",
        format_break(breaks[i]),
        ", ",
        format_break(breaks[i + 1]),
        "]"
      )
    },
    character(1)
  )
}

d2_make_binned_summary <- function(df, var_name, var_label) {
  x <- df[[var_name]]
  q <- unique(quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE))

  if (length(q) < 3) {
    q <- pretty(range(x, na.rm = TRUE), n = 5)
  }

  bin_labels <- d2_format_bin_labels(q, var_name)

  df |>
    mutate(bin = cut(.data[[var_name]], breaks = q, include.lowest = TRUE, labels = bin_labels)) |>
    filter(!is.na(bin)) |>
    group_by(candidate, bin) |>
    summarise(avg_vote_share = mean(vote_share, na.rm = TRUE), .groups = "drop") |>
    mutate(demographic = var_label)
}

d2_data_dir <- d2_find_data_dir(dashboard2_dir)
d2_mayor_file <- file.path(d2_data_dir, "2023 Office of the Mayor.xlsx")
d2_turnout_file <- d2_find_optional_input_file("2023-mayoral-by-election-voter-statistics.xlsx", dashboard2_dir)
d2_census_file <- file.path(d2_data_dir, "2023-WardProfiles-2011-2021-CensusData.xlsx")
d2_ward_shp <- file.path(
  d2_data_dir,
  "25-ward-model-december-2018-wgs84-latitude-longitude",
  "WARD_WGS84.shp"
)

stopifnot(
  file.exists(d2_mayor_file),
  file.exists(d2_census_file),
  file.exists(d2_ward_shp)
)

d2_wards_map <- d2_read_sf_compat(d2_ward_shp) |>
  mutate(
    ward_no = as.integer(AREA_S_CD),
    ward_name = as.character(AREA_NAME),
    ward_label = paste0("Ward ", ward_no)
  )

d2_mayor_long <- d2_read_mayor_results_exact(d2_mayor_file)
d2_turnout_df <- d2_read_turnout_or_empty_exact(d2_turnout_file)
d2_census_df <- d2_read_census_metrics_exact(d2_census_file)

d2_ward_totals <- d2_mayor_long |>
  group_by(ward_no) |>
  summarise(total_votes = sum(votes, na.rm = TRUE), .groups = "drop")

d2_results <- d2_mayor_long |>
  left_join(d2_ward_totals, by = "ward_no") |>
  mutate(vote_share = 100 * votes / total_votes)

d2_candidate_totals <- d2_results |>
  group_by(candidate) |>
  summarise(city_votes = sum(votes, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(city_votes))

d2_top_candidates <- d2_candidate_totals |>
  slice_head(n = 3) |>
  pull(candidate)

d2_candidate_colors <- stats::setNames(
  c("#7c3aed", "#84cc16", "#334155"),
  d2_top_candidates
)

d2_overall_total_votes <- sum(d2_candidate_totals$city_votes, na.rm = TRUE)

d2_citywide_top3 <- d2_candidate_totals |>
  filter(candidate %in% d2_top_candidates) |>
  mutate(
    candidate = factor(candidate, levels = d2_top_candidates),
    citywide_share = 100 * city_votes / d2_overall_total_votes,
    fill = unname(d2_candidate_colors[as.character(candidate)])
  ) |>
  arrange(candidate) |>
  mutate(candidate = as.character(candidate))

d2_other_city_votes <- sum(
  d2_candidate_totals$city_votes[!d2_candidate_totals$candidate %in% d2_top_candidates],
  na.rm = TRUE
)

d2_citywide_donut_df <- d2_citywide_top3 |>
  transmute(candidate, city_votes, citywide_share, fill)

if (d2_other_city_votes > 0) {
  d2_citywide_donut_df <- bind_rows(
    d2_citywide_donut_df,
    tibble(
      candidate = "Other candidates",
      city_votes = d2_other_city_votes,
      citywide_share = 100 * d2_other_city_votes / d2_overall_total_votes,
      fill = "#d7d9d6"
    )
  )
}

d2_top_three_combined_share <- sum(d2_citywide_top3$citywide_share, na.rm = TRUE)

d2_winner_df <- d2_results |>
  filter(candidate %in% d2_top_candidates) |>
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

d2_metrics_df <- d2_census_df |>
  left_join(d2_turnout_df, by = "ward_no")

d2_results_full <- d2_results |>
  left_join(d2_metrics_df, by = "ward_no") |>
  left_join(
    d2_wards_map |>
      sf::st_drop_geometry() |>
      select(ward_no, ward_name),
    by = "ward_no"
  )

d2_map_base <- d2_wards_map |>
  left_join(d2_metrics_df, by = "ward_no") |>
  left_join(d2_winner_df, by = "ward_no")

d2_candidate_choices <- d2_top_candidates
d2_default_candidate <- d2_top_candidates[[1]]
d2_demographic_config <- NULL
d2_winner_legend_candidates <- d2_top_candidates
d2_demographic_choices <- c(
  "Median income" = "median_income",
  "Bachelor's degree share" = "bachelors_share",
  "Visible minority share" = "visible_minority_share",
  "Unemployment rate" = "unemployment_rate"
)
d2_candidate_palette <- d2_candidate_colors

d2_candidate_backgrounds <- list(
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

get_d2_candidate_background <- function(candidate_name) {
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

  candidate_background <- d2_candidate_backgrounds[[canonical_name]]

  if (is.null(candidate_background)) {
    return(list(role = "", support = "", platform = ""))
  }

  candidate_background
}

d2_ui <- function() {
  tagList(
    div(
      class = "dashboard-tab",
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
            actionButton("d2_reset_map", "Reset view", class = "reset-chip")
          )
        ),
        div(
          class = "control-grid",
          div(
            class = "control-card",
            tags$label("Map Mode"),
            radioButtons(
              inputId = "d2_map_mode",
              label = NULL,
              choiceNames = list("Leading candidate", "Support share"),
              choiceValues = c("winner", "support"),
              selected = "winner",
              inline = TRUE
            )
          ),
          div(
            class = "control-card",
            tags$label(`for` = "d2_candidate", "Candidate"),
            selectizeInput(
              inputId = "d2_candidate",
              label = NULL,
              choices = d2_candidate_choices,
              selected = d2_default_candidate,
              options = list(placeholder = "Choose a candidate")
            )
          ),
          div(
            class = "control-card",
            tags$label(`for` = "d2_demographic", "Compare by demographic"),
            selectInput(
              inputId = "d2_demographic",
              label = NULL,
              choices = d2_demographic_choices,
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
                plotlyOutput("d2_citywide_donut", height = 118)
              ),
              div(
                class = "summary-legend",
                lapply(seq_len(nrow(d2_citywide_top3)), function(i) {
                  tags$div(
                    class = "mini-legend-item",
                    tags$span(
                      class = "mini-legend-swatch",
                      style = paste0("background:", d2_citywide_top3$fill[i], ";")
                    ),
                    tags$span(class = "mini-legend-label", d2_citywide_top3$candidate[i]),
                    tags$span(
                      class = "mini-legend-value",
                      paste0(round(d2_citywide_top3$citywide_share[i], 1), "%")
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
          leafletOutput("d2_ward_map", height = 470)
        ),
        card(
          full_screen = TRUE,
          class = "viz-card",
          card_header("Demographic Comparison"),
          p(
            class = "panel-subtitle",
            "Average vote share by ward quintiles of the selected demographic, with leading citywide candidates shown for context."
          ),
          plotlyOutput("d2_demo_plot", height = 470)
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          class = "viz-card",
          card_header("Ward-Level Support vs. Demographics"),
          plotlyOutput("d2_scatter_plot", height = 430)
        ),
        card(
          full_screen = TRUE,
          class = "viz-card profile-card",
          card_header("Candidate Support Profile"),
          card_body(fill = TRUE, uiOutput("d2_profile_panel"))
        )
      )
    )
  )
}

ui <- page_navbar(
  title = "Toronto Civic Visualization Project",
  window_title = "Toronto Civic Visualization Project",
  theme = bs_theme(
    version = 5,
    bg = "#F4F1EA",
    fg = "#22313F",
    primary = "#2E6E9E",
    secondary = "#6B7A8F",
    success = "#4C8C5A",
    warning = "#B8860B"
  ),
  header = tags$head(
    tags$title("Toronto Civic Visualization Project"),
    tags$style(HTML(shared_styles))
  ),
  nav_panel("Dashboard 1: Who Votes in Toronto", d1_ui()),
  nav_panel("Dashboard 2: Political Preference", d2_ui())
)

server <- function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Dashboard 1 server logic
  # ---------------------------------------------------------------------------

  d1_selected_ward <- reactiveVal(NULL)

  observeEvent(input$d1_attr_select, {
    updateSelectInput(
      session,
      "d1_sub_select",
      choices = dashboard1_data$sub_choices[[input$d1_attr_select]],
      selected = unname(dashboard1_data$sub_choices[[input$d1_attr_select]][1])
    )
  }, ignoreInit = TRUE)

  observeEvent(input$d1_ward_map_shape_click, {
    clicked_ward <- as.numeric(input$d1_ward_map_shape_click$id)

    if (isTRUE(clicked_ward == d1_selected_ward())) {
      d1_selected_ward(NULL)
    } else {
      d1_selected_ward(clicked_ward)
    }
  }, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_click", source = "d1_scatter"), {
    click_data <- event_data("plotly_click", source = "d1_scatter")

    if (is.null(click_data$key)) {
      return()
    }

    clicked_ward <- as.numeric(click_data$key[[1]])

    if (isTRUE(clicked_ward == d1_selected_ward())) {
      d1_selected_ward(NULL)
    } else {
      d1_selected_ward(clicked_ward)
    }
  })

  d1_x_info <- reactive({
    d1_get_x_info(dashboard1_data$scatter_data, input$d1_attr_select, input$d1_sub_select)
  })

  d1_scatter_frame <- reactive({
    x_info <- d1_x_info()
    selected_id <- d1_selected_ward()

    dashboard1_data$scatter_data %>%
      mutate(x_value = x_info$x) %>%
      filter(!is.na(x_value)) %>%
      mutate(
        is_selected = if (is.null(selected_id)) FALSE else ward == selected_id,
        hover_text = paste0(
          "<b>Ward ", ward, " — ", ward_name, "</b><br>",
          "Voter turnout: ", round(percent_voted, 1), "%<br>",
          x_info$label, ": ", round(x_value, 1), "%"
        )
      )
  })

  output$d1_ward_map <- renderLeaflet({
    turnout_palette <- colorNumeric(
      palette = c("#F7F5EE", "#E7DDF6", "#7C3AED"),
      domain = dashboard1_data$scatter_data$percent_voted,
      na.color = "#d7d9d6"
    )
    selected_id <- d1_selected_ward()

    if (!is.null(dashboard1_data$map_sf) && nrow(dashboard1_data$map_sf) > 0) {
      map_data <- dashboard1_data$map_sf %>%
        mutate(is_selected = if (is.null(selected_id)) FALSE else ward == selected_id)

      labels <- sprintf(
        "<strong>Ward %s — %s</strong><br/>Turnout: <strong>%.1f%%</strong><br/>Votes: %s",
        map_data$ward,
        map_data$ward_name,
        map_data$percent_voted,
        scales::comma(map_data$number_voted)
      ) %>%
        lapply(htmltools::HTML)

      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
        addPolygons(
          layerId = ~ward,
          fillColor = ~turnout_palette(percent_voted),
          fillOpacity = 0.88,
          color = ~ifelse(is_selected, "#F1B24A", "#FFFFFF"),
          weight = ~ifelse(is_selected, 4, 1.4),
          opacity = 1,
          smoothFactor = 0.3,
          label = labels,
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
        ) %>%
        addLegend(
          position = "bottomright",
          pal = turnout_palette,
          values = ~percent_voted,
          title = "Voter turnout (%)",
          opacity = 1,
          labFormat = labelFormat(suffix = "%")
        )
    } else {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
        setView(lng = -79.38, lat = 43.72, zoom = 10)
    }
  })

  output$d1_scatter_plot <- renderPlotly({
    x_info <- d1_x_info()
    point_data <- d1_scatter_frame()
    selected_point <- point_data %>% filter(is_selected)
    r_value <- cor(point_data$x_value, point_data$percent_voted, use = "complete.obs")

    scatter_plot <- ggplot(
      point_data,
      aes(
        x = x_value,
        y = percent_voted,
        text = hover_text,
        key = ward
      )
    ) +
      geom_smooth(
        data = point_data,
        aes(x = x_value, y = percent_voted),
        inherit.aes = FALSE,
        method = "lm",
        se = FALSE,
        colour = "#6B7A8F",
        linewidth = 0.9
      ) +
      geom_point(
        aes(size = is_selected, fill = is_selected),
        shape = 21,
        colour = "#22313F",
        stroke = 1.1,
        alpha = 0.92
      ) +
      scale_fill_manual(values = c(`TRUE` = "#F1B24A", `FALSE` = "#2E6E9E"), guide = "none") +
      scale_size_manual(values = c(`TRUE` = 5, `FALSE` = 3.6), guide = "none") +
      labs(
        subtitle = paste0("Current relationship: r = ", round(r_value, 2)),
        x = x_info$label,
        y = "Voter turnout (%)"
      ) +
      plot_theme() +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_line(colour = "#E8E6E0")
      )

    if (nrow(selected_point) == 1) {
      scatter_plot <- scatter_plot +
        geom_text(
          data = selected_point,
          aes(label = ward_name),
          nudge_y = 1,
          family = "Avenir Next",
          size = 3.4,
          colour = "#22313F"
        )
    }

    ggplotly(scatter_plot, tooltip = "text", source = "d1_scatter") %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$d1_ward_profile <- renderUI({
    selected_id <- d1_selected_ward()

    if (is.null(selected_id)) {
      return(
        div(
          class = "empty-note",
          tags$strong("Select a ward on the map or scatter plot."),
          tags$br(),
          "The ward detail card will update with turnout, demographic context, and rank-based comparisons."
        )
      )
    }

    ward_row <- dashboard1_data$scatter_data %>% filter(ward == selected_id)

    if (nrow(ward_row) == 0) {
      return(NULL)
    }

    total_wards <- nrow(dashboard1_data$scatter_data)
    ranking_block <- tags$div(
      class = "ranking-list",
      build_d1_rank_row("Voter turnout", d1_ward_rank(dashboard1_data$scatter_data$percent_voted, ward_row$percent_voted), total_wards),
      build_d1_rank_row("Income $100k+", d1_ward_rank(dashboard1_data$scatter_data$pct_income_100k_plus, ward_row$pct_income_100k_plus), total_wards),
      build_d1_rank_row("Bachelor's+", d1_ward_rank(dashboard1_data$scatter_data$pct_bachelor_plus, ward_row$pct_bachelor_plus), total_wards),
      build_d1_rank_row("Age 65+", d1_ward_rank(dashboard1_data$scatter_data$pct_age_65_plus, ward_row$pct_age_65_plus), total_wards),
      build_d1_rank_row("Immigrants", d1_ward_rank(dashboard1_data$scatter_data$pct_immigrants, ward_row$pct_immigrants), total_wards)
    )

    metric_tiles <- tags$div(
      class = "d1-snapshot-grid",
      tags$div(
        class = "d1-snapshot-tile",
        tags$div(class = "metric-label", "Turnout"),
        tags$div(class = "metric-value", paste0(round(ward_row$percent_voted, 1), "%"))
      ),
      tags$div(
        class = "d1-snapshot-tile",
        tags$div(class = "metric-label", "Votes cast"),
        tags$div(class = "metric-value", scales::comma(ward_row$number_voted))
      ),
      tags$div(
        class = "d1-snapshot-tile",
        tags$div(class = "metric-label", "Income $100k+"),
        tags$div(class = "metric-value", paste0(round(ward_row$pct_income_100k_plus, 1), "%"))
      ),
      tags$div(
        class = "d1-snapshot-tile",
        tags$div(class = "metric-label", "Bachelor's+"),
        tags$div(class = "metric-value", paste0(round(ward_row$pct_bachelor_plus, 1), "%"))
      ),
      tags$div(
        class = "d1-snapshot-tile",
        tags$div(class = "metric-label", "Age 25-44"),
        tags$div(class = "metric-value", paste0(round(ward_row$pct_age_25_to_44, 1), "%"))
      ),
      tags$div(
        class = "d1-snapshot-tile",
        tags$div(class = "metric-label", "Immigrants"),
        tags$div(class = "metric-value", paste0(round(ward_row$pct_immigrants, 1), "%"))
      )
    )

    tagList(
      div(
        class = "profile-header",
        div(class = "hero-eyebrow", "Ward Profile"),
        h3(paste0("Ward ", ward_row$ward, " — ", ward_row$ward_name)),
        p(
          class = "profile-summary-line",
          paste0(
            "Turnout: ", round(ward_row$percent_voted, 1), "% | ",
            scales::comma(ward_row$number_voted), " votes cast from ",
            scales::comma(ward_row$eligible_voters), " eligible voters"
          )
        )
      ),
      div(
        class = "detail-shell",
        div(
          class = "profile-section",
          tags$h4("Snapshot"),
          metric_tiles
        ),
        div(
          class = "profile-section",
          tags$h4("Rankings among 25 wards"),
          ranking_block
        )
      )
    )
  })

  output$d1_insights_panel <- renderUI({
    x_info <- d1_x_info()
    selected_id <- d1_selected_ward()
    insight_df <- d1_scatter_frame()
    r_value <- cor(insight_df$x_value, insight_df$percent_voted, use = "complete.obs")
    direction_label <- ifelse(r_value > 0, "positive", "negative")
    strength_label <- ifelse(abs(r_value) > 0.6, "strong", ifelse(abs(r_value) > 0.3, "moderate", "weak"))
    top_ward <- insight_df %>% slice_max(percent_voted, n = 1, with_ties = FALSE)
    bottom_ward <- insight_df %>% slice_min(percent_voted, n = 1, with_ties = FALSE)
    city_turnout <- mean(insight_df$percent_voted, na.rm = TRUE)
    city_x <- mean(insight_df$x_value, na.rm = TRUE)

    selected_note <- if (!is.null(selected_id)) {
      selected_row <- insight_df %>% filter(ward == selected_id)

      if (nrow(selected_row) == 1) {
        turnout_diff <- round(selected_row$percent_voted - city_turnout, 1)
        x_diff <- round(selected_row$x_value - city_x, 1)

        div(
          class = "insight-note",
          tags$h4("Selected Ward vs. Toronto"),
          tags$p(
            paste0(
              "Ward ", selected_row$ward, " sits at ", round(selected_row$percent_voted, 1),
              "% turnout, which is ", ifelse(turnout_diff >= 0, "above", "below"), " the city average by ",
              abs(turnout_diff), " percentage points. Its current lens value is ",
              round(selected_row$x_value, 1), "%, ",
              ifelse(x_diff >= 0, "above", "below"), " the citywide average by ", abs(x_diff), " points."
            )
          )
        )
      }
    } else {
      div(
        class = "insight-note",
        tags$h4("Selected Ward vs. Toronto"),
        tags$p("Click a ward to compare its turnout and demographic profile with Toronto's overall average.")
      )
    }

    tagList(
      div(
        class = "insight-stack",
        div(
          class = "insight-note",
          tags$h4("Correlation"),
          tags$p(
            paste0(
              "There is a ", strength_label, " ", direction_label, " relationship (r = ",
              round(r_value, 2), ") between ", tolower(x_info$label),
              " and voter turnout across Toronto's 25 wards."
            )
          )
        ),
        div(
          class = "insight-note",
          tags$h4("Highest and Lowest Turnout"),
          tags$p(
            paste0(
              "The highest turnout appears in Ward ", top_ward$ward, " (", top_ward$ward_name,
              ") at ", round(top_ward$percent_voted, 1), "%. The lowest turnout appears in Ward ",
              bottom_ward$ward, " (", bottom_ward$ward_name, ") at ",
              round(bottom_ward$percent_voted, 1), "%."
            )
          )
        ),
        selected_note
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Dashboard 2 server logic
  # ---------------------------------------------------------------------------

  d2_selected_ward <- reactiveVal(NULL)

  observeEvent(input$d2_reset_map, {
    d2_selected_ward(NULL)
  })

  observeEvent(input$d2_ward_map_shape_click, {
    clicked_ward <- as.integer(input$d2_ward_map_shape_click$id)

    if (isTRUE(clicked_ward == d2_selected_ward())) {
      d2_selected_ward(NULL)
    } else {
      d2_selected_ward(clicked_ward)
    }
  }, ignoreNULL = TRUE)

  d2_selected_candidate_color <- reactive({
    d2_candidate_colors[[input$d2_candidate]]
  })

  d2_selected_candidate_results <- reactive({
    d2_results_full |>
      dplyr::filter(candidate == input$d2_candidate) %>%
      dplyr::select(
        ward_no,
        vote_share,
        votes,
        total_votes
      ) %>%
      dplyr::rename(
        selected_vote_share = vote_share,
        selected_votes = votes,
        selected_total_votes = total_votes
      )
  })

  d2_map_data <- reactive({
    selected_id <- d2_selected_ward()

    d2_map_base |>
      left_join(d2_selected_candidate_results(), by = "ward_no") |>
      dplyr::mutate(
        selected_vote_share = dplyr::coalesce(selected_vote_share, 0),
        selected_votes = dplyr::coalesce(selected_votes, 0),
        selected_total_votes = dplyr::coalesce(selected_total_votes, 0),
        is_selected = if (is.null(selected_id)) FALSE else ward_no == selected_id,
        winner_tooltip_html = glue::glue(
          "<strong>{ward_label}: {ward_name}</strong><br/>",
          "Winner: {winner}<br/>",
          "Winning share: {round(winner_vote_share, 1)}%<br/>",
          "Runner-up: {runner_up}<br/>",
          "Margin: {round(margin, 1)} pp"
        ),
        support_tooltip_html = glue::glue(
          "<strong>{ward_label}: {ward_name}</strong><br/>",
          "Candidate: {input$d2_candidate}<br/>",
          "Vote share: {round(selected_vote_share, 1)}%<br/>",
          "Votes: {comma(selected_votes)}<br/>",
          "Total ward votes: {comma(selected_total_votes)}"
        )
      )
  })

  output$d2_ward_map <- renderLeaflet({
    ward_map <- d2_map_data() |> sf::st_transform(4326)

    if (input$d2_map_mode == "winner") {
      winner_palette <- colorFactor(
        palette = d2_candidate_colors[d2_top_candidates],
        domain = d2_top_candidates,
        na.color = "#d7d9d6"
      )
      winner_legend_colors <- d2_candidate_colors[d2_top_candidates]

      leaflet(ward_map) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
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
        ) %>%
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
        palette = c("#F7F5EE", d2_selected_candidate_color()),
        domain = c(0, max_share)
      )

      leaflet(ward_map) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
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
        ) %>%
        addLegend(
          position = "bottomright",
          pal = palette_values,
          values = ~selected_vote_share,
          title = paste(input$d2_candidate, "vote share (%)"),
          opacity = 1
        )
    }
  })

  output$d2_demo_plot <- renderPlotly({
    label_lookup <- c(
      median_income = "Median income",
      bachelors_share = "Bachelor's degree share",
      visible_minority_share = "Visible minority share",
      unemployment_rate = "Unemployment rate"
    )
    var_name <- input$d2_demographic
    var_label <- label_lookup[[var_name]]

    summary_df <- d2_results_full |>
      filter(candidate %in% d2_top_candidates) |>
      d2_make_binned_summary(var_name, var_label) |>
      mutate(
        hover_text = glue::glue(
          "bin: {bin}<br>",
          "avg_vote_share: {sprintf('%.1f', avg_vote_share)}<br>",
          "candidate: {candidate}"
        )
      )

    g <- ggplot(
      summary_df,
      aes(x = bin, y = avg_vote_share, color = candidate, group = candidate, text = hover_text)
    ) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.8) +
      scale_color_manual(values = d2_candidate_colors) +
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

    scatter_widget <- ggplotly(g, tooltip = "text") |>
      config(displayModeBar = FALSE, responsive = TRUE)

    if (identical(var_name, "median_income")) {
      scatter_widget <- scatter_widget |>
        layout(xaxis = list(tickformat = ",.0f"))
    }

    scatter_widget
  })

  output$d2_citywide_donut <- renderPlotly({
    plot_df <- d2_citywide_donut_df |>
      mutate(
        hover_text = glue::glue(
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
            text = glue::glue(
              "<b>{round(d2_top_three_combined_share, 1)}%</b><br>",
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

  output$d2_scatter_plot <- renderPlotly({
    label_lookup <- c(
      median_income = "Median household income ($)",
      bachelors_share = "Bachelor's degree share (%)",
      visible_minority_share = "Visible minority share (%)",
      unemployment_rate = "Unemployment rate (%)"
    )
    var_name <- input$d2_demographic
    x_label <- label_lookup[[var_name]]

    plot_df <- d2_results_full |>
      filter(candidate %in% d2_top_candidates) |>
      mutate(
        x_hover_value = if (identical(var_name, "median_income")) {
          formatC(.data[[var_name]], format = "f", digits = 0, big.mark = ",")
        } else {
          sprintf("%.1f", .data[[var_name]])
        },
        hover_text = glue::glue(
          "candidate: {candidate}<br>",
          "{var_name}: {x_hover_value}<br>",
          "vote_share: {sprintf('%.1f', vote_share)}"
        )
      )

    g <- ggplot(
      plot_df,
      aes_string(x = var_name, y = "vote_share", color = "candidate")
    ) +
      geom_point(aes(text = hover_text), size = 3, alpha = 0.85) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.9) +
      scale_color_manual(values = d2_candidate_colors) +
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

    if (identical(var_name, "median_income")) {
      g <- g +
        scale_x_continuous(labels = scales::label_comma(accuracy = 1))
    }

    scatter_widget <- ggplotly(g, tooltip = "text") |>
      config(displayModeBar = FALSE, responsive = TRUE)

    if (identical(var_name, "median_income")) {
      scatter_widget <- scatter_widget |>
        layout(xaxis = list(tickformat = ",.0f"))
    }

    scatter_widget
  })

  output$d2_profile_panel <- renderUI({
    ward_now <- d2_selected_ward()

    if (!is.null(ward_now)) {
      ward_meta <- d2_metrics_df |> filter(ward_no == ward_now)
      ward_name_now <- d2_wards_map |>
        sf::st_drop_geometry() |>
        filter(ward_no == ward_now) |>
        pull(ward_name)
      ward_results <- d2_results_full |>
        filter(ward_no == ward_now, candidate %in% d2_top_candidates) |>
        arrange(desc(vote_share)) |>
        slice_head(n = 3)

      div(
        class = "profile-box",
        h3(glue::glue("Ward {ward_now}: {ward_name_now}")),
        p(class = "small-note", "Click another ward on the map to update this panel."),
        fluidRow(
          column(
            6,
            div(class = "stat-label", "Turnout"),
            div(class = "stat-value", d2_format_percent_or_na(ward_meta$turnout_pct))
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
            tags$li(glue::glue("{ward_results$candidate[i]}: {round(ward_results$vote_share[i], 1)}%"))
          })
        ),
        tags$hr(),
        tags$p(glue::glue("Bachelor's degree share: {round(ward_meta$bachelors_share, 1)}%")),
        tags$p(glue::glue("Visible minority share: {round(ward_meta$visible_minority_share, 1)}%")),
        tags$p(glue::glue("Unemployment rate: {round(ward_meta$unemployment_rate, 1)}%"))
      )
    } else {
      candidate_now <- input$d2_candidate
      candidate_background <- get_d2_candidate_background(candidate_now)

      top5 <- d2_results_full |>
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
              comma(d2_candidate_totals$city_votes[d2_candidate_totals$candidate == candidate_now])
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

app <- shiny::shinyApp(ui = ui, server = server)

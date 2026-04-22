library(shiny)
library(tidyverse)
library(sf)
library(leaflet)

# ── Data ──────────────────────────────────────────────────────────────────────
load(file = "data/data_processed.RData")
data <- data %>% # construct percentage values
  mutate(
    gndr_f          = if_else(gndr == "Female", 100L, 0L),
    gender          = gndr,
    conflicts       = conflicts      * 100,
    financial_diffs = financial_diffs * 100,
    relate          = relate         * 100,
    inc_diff_group  = factor(if_else(inc_diff == 1, "Difficulty", "No difficulty"),
                             levels = c("No difficulty", "Difficulty")),
    inc_diff        = inc_diff       * 100,
    felt_safe       = felt_safe      * 100,
    age_group = factor(case_when(
                  age < 25        ~ "< 25",
                  age < 35        ~ "25–34",
                  age < 55        ~ "35–54",
                  !is.na(age)     ~ "55+"),
                  levels = c("< 25", "25–34", "35–54", "55+")),
    inc_group = factor(case_when(
                  hinctnta <= 3   ~ "Low (1–3)",
                  hinctnta <= 7   ~ "Mid (4–7)",
                  !is.na(hinctnta)~ "High (8–10)"),
                  levels = c("Low (1–3)", "Mid (4–7)", "High (8–10)"))
  )

# ── Shapefiles (cached) ───────────────────────────────────────────────────────
if (!file.exists("data/map_regions_v3.rds") || !file.exists("data/map_countries_v3.rds")) {
  message("Preparing shapefiles...")

  map_eu <- st_read("data/NUTS_RG_20M_2021_3035.shp/", quiet = TRUE) %>%
    filter(LEVL_CODE == 2,
           !NUTS_ID %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5")) %>%
    rename(region = NUTS_ID) %>%
    select(region, CNTR_CODE, geometry) %>%
    st_transform(4326) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 1000)

  map_uk <- st_read("data/ITL2_JAN_2025_UK_BFE_8224540410987116282/", quiet = TRUE) %>%
    st_transform(4326) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 1000) %>%
    mutate(nuts1 = substr(str_replace(ITL225CD, "^TL", "UK"), 1, 3)) %>%
    group_by(region = nuts1) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    mutate(CNTR_CODE = "GB")

  map_regions <- bind_rows(map_eu, map_uk) %>%
    filter(CNTR_CODE %in% unique(data$cntry))

  map_countries <- map_regions %>%
    group_by(CNTR_CODE) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")

  saveRDS(map_regions,   "data/map_regions_v3.rds")
  saveRDS(map_countries, "data/map_countries_v3.rds")
} else {
  map_regions   <- readRDS("data/map_regions_v3.rds")
  map_countries <- readRDS("data/map_countries_v3.rds")
}

# ── Lookups ───────────────────────────────────────────────────────────────────

country_names <- c(
  AT = "Austria", 
  BE = "Belgium",
  CZ = "Czech Republic",
  FI = "Finland",  
  FR = "France",      
  GB = "United Kingdom",
  HU = "Hungary",
  IS = "Iceland", 
  PL = "Poland",      
  PT = "Portugal", 
  SI = "Slovenia"
)

cntry_codes   <- sort(unique(na.omit(data$cntry)))
cntry_labels  <- unname(country_names[cntry_codes])
cntry_labels  <- ifelse(is.na(cntry_labels), cntry_codes, cntry_labels)
cntry_choices <- setNames(cntry_codes, cntry_labels)

# All variables (Tab 1 map)
map_var_choices <- c(
  "Age (mean)"                                  = "age",
  "Years of education (mean)"                   = "eduyrs",
  "Share female (%)"                            = "gndr_f",
  "Everyone has fair educational chance (mean)" = "everyfair",
  "I had a fair educational chance (mean)"      = "ifair",
  "Satisfaction with education (mean)"          = "edu_satisf",
  "Conflict in household during childhood (%)"  = "conflicts",
  "Financial difficulties during childhood (%)" = "financial_diffs",
  "Job-qualification match (%)"                 = "relate",
  "Difficulty on present income (%)"            = "inc_diff",
  "Felt safe during childhood (%)"              = "felt_safe"
)

# Educational outcomes (Tab 2)
outcome_choices <- c(
  "Everyone has fair educational chance" = "everyfair",
  "I had a fair educational chance"      = "ifair",
  "Satisfaction with education"          = "edu_satisf",
  "Years of education"                   = "eduyrs"
)

# Within-country grouping variables
group_choices <- c(
  "Gender"       = "gender",
  "Age group"    = "age_group",
  "Income difficulty" = "inc_diff_group"
)

# ── Shared plot theme ─────────────────────────────────────────────────────────
theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(legend.position    = "none",
          panel.grid.major.x = element_blank(),
          plot.title         = element_text(face = "bold", color = "#ad1400"),
          plot.subtitle      = element_text(color = "#555555"))
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Perspectives on Fairness in Education across Europe",

  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', sans-serif; }

    /* ── Navbar ── */
    .navbar-default { background-color: #ad1400; border-color: #8a1000; }
    .navbar-default .navbar-brand,
    .navbar-default .navbar-brand:hover,
    .navbar-default .navbar-brand:focus { color: #ffffff; }
    .navbar-default .navbar-nav > li > a { color: #ffffff; }
    .navbar-default .navbar-nav > li > a:hover,
    .navbar-default .navbar-nav > li > a:focus {
      color: #ffffff; background-color: #8a1000;
    }
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:hover,
    .navbar-default .navbar-nav > .active > a:focus {
      color: #ffffff; background-color: #6e0d00;
    }

    /* ── Sidebar headers ── */
    .well   { background: #f8f9fa; border: none; box-shadow: none; }
    .well strong { color: #ad1400; }
    hr { border-color: #e8c0bb; }

    /* ── Info box ── */
    #info_box { background: #f9f0ef; border-left: 3px solid #ad1400;
                border-radius: 4px; padding: 12px;
                min-height: 60px; font-size: 13px; white-space: pre-line; }

    .violin-hint { color: #888; font-size: 12px; font-style: italic; margin-top: 4px; }

    /* ── Map legend ── */
    .info.legend { max-width: 140px; font-size: 11px; }
    .info.legend .legend-title { word-break: break-word; white-space: normal; }
  "))),

  # ── Tab 1: Map ──────────────────────────────────────────────────────────────
  tabPanel("Map",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("var", "Variable:", choices = map_var_choices, selected = "age"),
        p(style = "font-size:11px; color:#888; font-style:italic; margin-top:-6px;",
          "All variables are weighted using post-stratification weights."),
        radioButtons("level", "Map level:",
                     choices = c("Countries" = "country", "Regions (NUTS-2)" = "region"),
                     selected = "country"),
        hr(),
        strong("Selected region"),
        br(), br(),
        div(id = "info_box", htmlOutput("click_info")),
        conditionalPanel("input.level == 'region'",
          p(style = "margin-top:8px; font-size:11px; color:#888; font-style:italic;",
            "Note: Some NUTS-2 regions have small sample sizes. Treat regional estimates with caution.")
        )
      ),
      mainPanel(
        width = 9,
        leafletOutput("map", height = "620px")
      )
    )
  ),

  # ── Tab 2: Comparisons ──────────────────────────────────────────────────────
  tabPanel("Comparisons",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("comp_outcome", "Outcome:",
                    choices = outcome_choices, selected = "everyfair"),
        p(style = "font-size:11px; color:#888; font-style:italic; margin-top:-6px;",
          "All variables are weighted using post-stratification weights."),
        radioButtons("comp_type", "Comparison type:",
                     choices = c("Within country"    = "within",
                                 "Between countries" = "between"),
                     selected = "between"),

        conditionalPanel("input.comp_type == 'within'",
          hr(),
          selectInput("comp_country", "Country:",
                      choices = cntry_choices, selected = cntry_codes[1]),
          radioButtons("comp_group", "Group by:",
                       choices = group_choices, selected = "gender")
        ),

        conditionalPanel("input.comp_type == 'between'",
          hr(),
          selectInput("comp_countries", "Countries:",
                      choices  = cntry_choices,
                      multiple = TRUE,
                      selected = character(0)),
          actionButton("reset_countries", "Reset selection",
                       style = "margin-top: 4px; width: 100%;")
        )
      ),

      mainPanel(
        width = 9,
        plotOutput("comp_plot", height = "520px")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Tracks the between-country selection reliably on the server side
  comp_selection <- reactiveVal(character(0))

  # Reset button clears the between-country selection
  observeEvent(input$reset_countries, {
    comp_selection(character(0))
    updateSelectInput(session, "comp_countries", selected = character(0))
  })

  # Sync reactiveVal when the user manually changes the selectInput
  observeEvent(input$comp_countries, {
    comp_selection(input$comp_countries %||% character(0))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # ── Tab 1: Map ──────────────────────────────────────────────────────────────

  agg <- reactive({
    var  <- input$var
    base <- data %>% filter(!is.na(w1pspwght))
    if (input$level == "country") {
      base %>%
        group_by(id = cntry) %>%
        summarise(value = weighted.mean(.data[[var]], w1pspwght, na.rm = TRUE),
                  n     = sum(!is.na(.data[[var]])), .groups = "drop")
    } else {
      base %>%
        mutate(id = substr(region, 1, 4)) %>%
        group_by(id) %>%
        summarise(value = weighted.mean(.data[[var]], w1pspwght, na.rm = TRUE),
                  n     = sum(!is.na(.data[[var]])), .groups = "drop")
    }
  })
  # 
  map_sf <- reactive({
    base <- if (input$level == "country") {
      map_countries %>% rename(id = CNTR_CODE)
    } else {
      map_regions %>% rename(id = region)
    }
    left_join(base, agg(), by = "id")
  })

  pal <- reactive({
    colorNumeric("plasma", domain = map_sf()$value, na.color = "#cccccc")
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.5, minZoom = 3, maxZoom = 8)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 15, lat = 54, zoom = 4) %>%
      setMaxBounds(lng1 = -33, lat1 = 26, lng2 = 45, lat2 = 72)
  })

  observe({
    md  <- map_sf()
    p   <- pal()
    lbl <- names(map_var_choices)[map_var_choices == input$var]
    leafletProxy("map", data = md) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        fillColor    = ~p(value), fillOpacity = 0.75,
        color = "white", weight = 1, layerId = ~id,
        label        = ~paste0(id, ":  ", round(value, 2), "  (n = ", n, ")"),
        labelOptions = labelOptions(style = list("font-size" = "13px")),
        highlightOptions = highlightOptions(
          weight = 2, color = "#444", fillOpacity = 0.95, bringToFront = TRUE)
      ) %>%
      addLegend(pal = p, values = ~value, title = lbl,
                position = "bottomright", na.label = "No data", opacity = 0.85)
  })

  output$click_info <- renderUI({
    click <- input$map_shape_click
    if (is.null(click)) return(HTML("Click on a region for details."))
    md  <- map_sf()
    row <- md[md$id == click$id, ]
    if (nrow(row) == 0 || is.na(row$value))
      return(HTML(paste0("<b>", click$id, "</b><br>No data")))
    lbl  <- names(map_var_choices)[map_var_choices == input$var]
    name <- if (input$level == "country") {
      cn <- unname(country_names[click$id]); if (!is.na(cn)) cn else click$id
    } else click$id
    HTML(paste0("<b>", name, "</b><br>", lbl, ":&nbsp;&nbsp;", round(row$value, 2),
                "<br>Observations:&nbsp;&nbsp;", row$n))
  })

  # ── Tab 2: Comparisons ──────────────────────────────────────────────────────

  # Map click toggles country in/out of between-country comparison
  observeEvent(input$map_shape_click, ignoreInit = TRUE, {
    click    <- input$map_shape_click
    cntry_id <- if (input$level == "country") click$id else substr(click$id, 1, 2)
    if (!cntry_id %in% cntry_codes) return()
    current <- comp_selection()
    updated <- if (cntry_id %in% current) setdiff(current, cntry_id) else c(current, cntry_id)
    comp_selection(updated)
    updateSelectInput(session, "comp_countries", selected = updated)
  })

  output$comp_plot <- renderPlot({
    req(input$comp_outcome)
    outcome <- input$comp_outcome
    lbl     <- names(outcome_choices)[outcome_choices == outcome]

    if (input$comp_type == "within") {
      req(input$comp_country, input$comp_group)
      grp_var   <- input$comp_group
      grp_lbl   <- names(group_choices)[group_choices == grp_var]
      cntry_lbl <- unname(country_names[input$comp_country])
      if (is.na(cntry_lbl)) cntry_lbl <- input$comp_country

      plot_data <- data %>%
        filter(cntry == input$comp_country, !is.na(w1pspwght)) %>%
        transmute(group = .data[[grp_var]], value = .data[[outcome]], weight = w1pspwght) %>%
        filter(!is.na(group), !is.na(value))

      validate(need(nrow(plot_data) > 10, "Not enough data for this selection."))

      # Summary for mean labels
      means <- plot_data %>%
        group_by(group) %>%
        summarise(m = weighted.mean(value, weight, na.rm = TRUE), n = n(), .groups = "drop") %>%
        mutate(label = paste0("n=", n))

      inc_caption <- if (grp_var == "inc_diff_group")
        "Based on 'Feeling about household’s income nowadays': 1 = living comfortably, 2 = coping, 3 = difficult, 4 = very difficult. Values > 2 indicate difficulty."
      else NULL

      ggplot(plot_data, aes(x = group, y = value, fill = group)) +
        geom_violin(aes(weight = weight), trim = TRUE, alpha = 0.75, color = NA) +
        geom_boxplot(aes(weight = weight), width = 0.08, fill = "white", color = "#333333",
                     outlier.shape = NA, linewidth = 0.5) +
        geom_point(data = means, aes(x = group, y = m),
                   shape = 18, size = 4, color = "#222222") +
        geom_text(data = means, aes(x = group, y = min(plot_data$value) - 0.35, label = label),
                  size = 3.5, color = "#666666") +
        scale_fill_brewer(palette = "Set2") +
        labs(x = grp_lbl, y = lbl,
             title = paste(lbl, "by", grp_lbl),
             subtitle = cntry_lbl,
             caption = inc_caption) +
        theme_app() +
        theme(plot.caption = element_text(size = 9, color = "#888888", face = "italic",
                                          hjust = 0, margin = margin(t = 8)))

    } else {
      sel <- comp_selection()
      req(length(sel) > 0)

      lvls <- unname(na.omit(country_names[sel]))

      plot_data <- data %>%
        filter(cntry %in% sel, !is.na(w1pspwght)) %>%
        mutate(country = factor(unname(country_names[cntry]), levels = lvls),
               value   = .data[[outcome]],
               weight  = w1pspwght) %>%
        filter(!is.na(value), !is.na(country))

      validate(need(nrow(plot_data) > 0, "No data for selected countries."))

      # Sort countries by weighted mean (ascending) for readability
      country_order <- plot_data %>%
        group_by(country) %>%
        summarise(m = weighted.mean(value, weight, na.rm = TRUE), .groups = "drop") %>%
        arrange(m) %>%
        pull(country) %>%
        as.character()

      plot_data <- mutate(plot_data, country = factor(country, levels = country_order))

      means <- plot_data %>%
        group_by(country) %>%
        summarise(m = weighted.mean(value, weight, na.rm = TRUE), n = n(), .groups = "drop") %>%
        mutate(label = paste0("n=", n))

      ggplot(plot_data, aes(x = country, y = value, fill = country)) +
        geom_violin(aes(weight = weight), trim = TRUE, alpha = 0.75, color = NA) +
        geom_boxplot(aes(weight = weight), width = 0.08, fill = "white", color = "#333333",
                     outlier.shape = NA, linewidth = 0.5) +
        geom_point(data = means, aes(x = country, y = m),
                   shape = 18, size = 4, color = "#222222") +
        geom_text(data = means, aes(x = country, y = min(plot_data$value) - 0.35, label = label),
                  size = 3.2, color = "#666666") +
        scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
        labs(x = NULL, y = lbl,
             title = paste("Distribution:", lbl),
             subtitle = "Sorted by country mean  ◆ = mean") +
        theme_app() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    }
  })
}

shinyApp(ui, server)

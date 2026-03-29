library(shiny)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)
library(bslib)
library(bsicons)

# --- 1. DATA PREPARATION ---
if (!file.exists("data/processed_tram_segments.rds") || !file.exists("data/metadata.rds")) {
  stop("Data or metadata not found. Please run your data engineering script first.")
}

all_data <- readRDS("data/processed_tram_segments.rds") %>%
  st_transform(4326) %>%
  st_zm() %>%
  mutate(
    avg_speed_kph = round(avg_speed_kph, 1),
    dist_m = round(dist_m, 0)
  )

# Load metadata saved by engineering script
metadata <- readRDS("data/metadata.rds")

# --- OFFICIAL BRAND COLORS ---
city_palette <- c(
  "Manchester" = "#ffcc00", # Bee Network Yellow
  "Sheffield"  = "#FA5701", # Sheffield Orange
  "Edinburgh"  = "#9e1b34", # Edinburgh Burgundy
  "Blackpool"  = "#5a2d81", # Blackpool Purple
  "Leeds"      = "#e11a8c"  # First Bus Leeds Magenta
)

# --- 2. UI DESIGN ---
ui <- page_sidebar(
  title = "UK Tram & Leeds Bus: Performance Index",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1e3c72", font_scale = 1.25),
  
  sidebar = sidebar(
    width = 350,
    tags$div(
      style = "background: #1e3c72; color: white; padding: 25px; margin: -20px -20px 20px -20px; text-align: center;",
      tags$h4("Network Insights", style="margin:0; font-weight:bold; letter-spacing: 0.5px;")
    ),
    
    selectInput("city_focus", "Focus Geography:", 
                choices = c("All UK", sort(unique(all_data$city)))),
    
    # JUSTIFIED TEXT WITH LINE BREAKS
    tags$div(
      style = "font-size: 0.95rem; color: #444; line-height: 1.6; text-align: justify; padding: 0 15px;",
      tags$p("This dashboard compares high-frequency Leeds bus routes on future tram corridors 
              against existing UK tram networks on scheduled speeds (07:00\u201319:00)."),
      tags$p("By visualising performance, we highlight where dedicated tram infrastructure 
              outperforms traditional bus operations \u2014 and where some Leeds bus routes already hold their own.")
    ),
    
    hr(),
    
    # --- CENTER ALIGNED LEGENDS ---
    tags$div(
      style = "text-align: center; display: flex; flex-direction: column; align-items: center;",
      tags$strong("Speed Performance", 
                  style="font-size: 0.85rem; text-transform: uppercase; color: #999; letter-spacing: 1px; display: block; margin-bottom: 15px;"),
      
      tags$div(style="display:flex; align-items:center; margin-bottom:8px; width: fit-content;", 
               tags$div(style="width:30px; height:6px; background:#d73027; margin-right:12px; border-radius:2px;"), "< 12 kph"),
      tags$div(style="display:flex; align-items:center; margin-bottom:8px; width: fit-content;", 
               tags$div(style="width:30px; height:6px; background:#fc8d59; margin-right:12px; border-radius:2px;"), "12 - 18 kph"),
      tags$div(style="display:flex; align-items:center; margin-bottom:8px; width: fit-content;", 
               tags$div(style="width:30px; height:6px; background:#fee08b; margin-right:12px; border-radius:2px;"), "18 - 24 kph"),
      tags$div(style="display:flex; align-items:center; margin-bottom:8px; width: fit-content;", 
               tags$div(style="width:30px; height:6px; background:#91cf60; margin-right:12px; border-radius:2px;"), "24 - 40 kph"),
      tags$div(style="display:flex; align-items:center; margin-bottom:20px; width: fit-content;", 
               tags$div(style="width:30px; height:6px; background:#1a9850; margin-right:12px; border-radius:2px;"), "> 40 kph"),
      
      hr(style="width: 100%;"),
      
      tags$strong("Network Branding", 
                  style="font-size: 0.85rem; text-transform: uppercase; color: #999; letter-spacing: 1px; display: block; margin-bottom: 15px;"),
      
      lapply(names(city_palette), function(city) {
        tags$div(style="display:flex; align-items:center; margin-bottom:8px; width: fit-content;", 
                 tags$div(style=paste0("width:14px; height:14px; border-radius:50%; background:", city_palette[city], "; margin-right:12px; border: 1px solid #ddd;")), 
                 city)
      }),
      
      hr(style="width: 100%;"),
      
      # --- NEW DATA SOURCES SECTION ---
      tags$div(
        style = "margin-top: 10px; padding: 0 10px; font-size: 0.75rem; color: #888; text-align: center;",
        tags$strong("Data Sources & Timestamps", style="display:block; margin-bottom:5px; color:#666;"),
        tags$p(style="margin-bottom:3px;", "Timetables via ", 
               tags$a(href="https://data.bus-data.dft.gov.uk/timetable/download/", "DfT BODS", target="_blank")),
        tags$p(style="margin-bottom:3px;", paste0("Schedule Date Used: ", format(metadata$analysis_date, "%d %B %Y"))),
        tags$p(style="margin-bottom:0;", paste0("Last Processed: ", format(metadata$processed_timestamp, "%d %b %Y %H:%M")))
      )
    )
  ),
  
  # Metric Cards
  uiOutput("stat_cards"),
  
  # Map
  card(
    full_screen = TRUE,
    card_header(bs_icon("map-fill"), " Network Speed Map"),
    leafletOutput("map", height = "450px")
  ),
  
  # Charts
  layout_column_wrap(
    width = 1/2,
    card(
      card_header(bs_icon("graph-up"), " Hourly Speed Profile"),
      plotlyOutput("hourly_plot", height = "400px")
    ),
    card(
      card_header(bs_icon("bar-chart-fill"), " Route Performance Ranking"),
      plotlyOutput("route_plot", height = "400px")
    )
  )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    if (input$city_focus == "All UK") return(all_data)
    all_data %>% filter(city == input$city_focus)
  })
  
  # --- STAT CARDS ---
  output$stat_cards <- renderUI({
    data <- filtered_data()
    n_routes <- length(unique(data$route_short_name))
    avg_s    <- round(mean(data$avg_speed_kph, na.rm = TRUE), 1)
    min_s    <- round(min(data$avg_speed_kph, na.rm = TRUE), 1)
    max_s    <- round(max(data$avg_speed_kph, na.rm = TRUE), 1)
    
    card_bg <- if(input$city_focus == "All UK") "#1e3c72" else city_palette[input$city_focus]
    
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      value_box(
        title = "Unique Routes", value = n_routes,
        showcase = bs_icon("signpost-2"),
        theme = value_box_theme(bg = card_bg)
      ),
      value_box(
        title = "Average Network Speed", value = paste0(avg_s, " kph"),
        showcase = bs_icon("speedometer2"),
        theme = value_box_theme(bg = card_bg)
      ),
      value_box(
        title = "Slowest Segment", value = paste0(min_s, " kph"),
        showcase = bs_icon("stoplights"), 
        theme = value_box_theme(bg = card_bg)
      ),
      value_box(
        title = "Fastest Segment", value = paste0(max_s, " kph"),
        showcase = bs_icon("lightning-fill"),
        theme = value_box_theme(bg = card_bg)
      )
    )
  })
  
  # --- MAP ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -1.5, lat = 53.8, zoom = 6)
  })
  
  observe({
    data <- filtered_data()
    
    line_colors <- case_when(
      data$avg_speed_kph < 12 ~ "#d73027",
      data$avg_speed_kph < 18 ~ "#fc8d59",
      data$avg_speed_kph < 24 ~ "#fee08b",
      data$avg_speed_kph < 40 ~ "#91cf60",
      TRUE                   ~ "#1a9850"
    )
    
    proxy <- leafletProxy("map", data = data)
    proxy %>% clearShapes()
    proxy %>% addPolylines(
      color = line_colors, weight = 5, opacity = 0.85,
      highlightOptions = highlightOptions(weight = 8, color = "#222", bringToFront = TRUE),
      popup = ~paste0(
        "<div style='font-family:sans-serif;'>",
        "<b style='color:", city_palette[city], "; font-size:1.1rem;'>", city, "</b><br>",
        "<b>Route:</b> ", route_short_name, "<br>",
        "<b>Speed:</b> ", avg_speed_kph, " kph",
        "</div>"
      )
    )
    
    bbox <- st_bbox(data)
    proxy %>% fitBounds(
      as.numeric(bbox$xmin), as.numeric(bbox$ymin), 
      as.numeric(bbox$xmax), as.numeric(bbox$ymax)
    )
  })
  
  # --- PLOTS ---
  output$hourly_plot <- renderPlotly({
    h_data <- filtered_data() %>%
      st_drop_geometry() %>%
      group_by(city, hour_bin) %>%
      summarise(avg_speed = mean(avg_speed_kph), .groups = "drop")
    
    plot_ly(h_data, x = ~hour_bin, y = ~avg_speed, color = ~city, 
            colors = city_palette, type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{fullData.name}</b><br>Hour: %{x}:00<br>Speed: %{y:.1f} kph<extra></extra>") %>%
      layout(
        title = list(text = "Avg Speed by Hour", font = list(size = 20)),
        xaxis = list(title = "Hour of Day", dtick = 2, tickfont = list(size = 14)),
        yaxis = list(title = "kph", rangemode = "tozero", tickfont = list(size = 14)),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25, font = list(size = 14)),
        margin = list(t = 60, b = 100)
      ) %>% config(displayModeBar = FALSE)
  })
  
  output$route_plot <- renderPlotly({
    r_data <- filtered_data() %>%
      st_drop_geometry() %>%
      group_by(city, route_short_name) %>%
      summarise(avg_speed = mean(avg_speed_kph), .groups = "drop") %>%
      arrange(avg_speed)
    
    plot_ly(r_data, x = ~avg_speed, y = ~reorder(paste(city, route_short_name), avg_speed), 
            color = ~city, colors = city_palette, type = "bar", orientation = "h",
            hovertemplate = "Route: %{y}<br>Speed: %{x:.1f} kph<extra></extra>") %>%
      layout(
        title = list(text = "Route Ranking", font = list(size = 20)),
        xaxis = list(title = "Speed (kph)", tickfont = list(size = 14)),
        yaxis = list(
          title = "", 
          tickfont = list(size = 14), 
          ticksuffix = "   "
        ),
        showlegend = FALSE,
        margin = list(l = 220, t = 60, r = 30)
      ) %>% config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
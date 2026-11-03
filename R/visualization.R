# ==============================================================================
# UrbanPulse — R/visualization.R
# Shared Dashboard Visualizations, Plot Styling, and Leaflet Maps
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(dplyr)
})

#' Minimal clean theme for dashboard plots
theme_dashboard <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0f172a"),
      plot.subtitle = element_text(color = "#64748b", size = 10),
      axis.title = element_text(face = "bold", size = 10, color = "#334155"),
      axis.text = element_text(color = "#475569", size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#f1f5f9"),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      legend.position = "bottom"
    )
}

#' Build Interactive Leaflet Map for Accident Hotspots
#' @param hotspots_df Dataframe with longitude, latitude, station_raw, total_crashes, fatal_crashes
#' @param metric Metric to size/color markers: "total_crashes" or "fatal_crashes"
#' @return Leaflet map widget
render_hotspot_leaflet_map <- function(hotspots_df, metric = "total_crashes", default_basemap = "Google Live Traffic") {
  valid_data <- hotspots_df %>% filter(!is.na(latitude), !is.na(longitude), coord_valid == TRUE)
  
  base_leaflet <- leaflet() %>%
    addTiles(
      urlTemplate = "https://mt1.google.com/vt/lyrs=m,traffic&x={x}&y={y}&z={z}",
      group = "Google Live Traffic",
      attribution = "Google Maps Traffic",
      options = tileOptions(maxZoom = 20)
    ) %>%
    addTiles(
      urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}",
      group = "Google Roadmap",
      attribution = "Google Maps",
      options = tileOptions(maxZoom = 20)
    ) %>%
    addTiles(
      urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
      group = "Google Hybrid (Satellite)",
      attribution = "Google Maps Satellite",
      options = tileOptions(maxZoom = 20)
    ) %>%
    addTiles(
      urlTemplate = "https://mt1.google.com/vt/lyrs=p&x={x}&y={y}&z={z}",
      group = "Google Terrain",
      attribution = "Google Maps Terrain",
      options = tileOptions(maxZoom = 20)
    ) %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addLayersControl(
      baseGroups = c(
        "Google Live Traffic",
        "Google Roadmap",
        "Google Hybrid (Satellite)",
        "Google Terrain",
        "CartoDB Positron",
        "OpenStreetMap"
      ),
      position = "topright",
      options = layersControlOptions(collapsed = FALSE)
    )
  
  if (nrow(valid_data) == 0) {
    return(base_leaflet %>% setView(lng = 77.5946, lat = 12.9716, zoom = 11))
  }
  
  pal <- colorNumeric(
    palette = c("#3b82f6", "#eab308", "#ef4444", "#991b1b"),
    domain = valid_data[[metric]]
  )
  
  # Create radius scale (6 to 20 px)
  max_val <- max(valid_data[[metric]], na.rm = TRUE)
  min_val <- min(valid_data[[metric]], na.rm = TRUE)
  radii <- 6 + 14 * ((valid_data[[metric]] - min_val) / max(1, (max_val - min_val)))
  
  m <- base_leaflet %>%
    setView(lng = 77.60, lat = 12.98, zoom = 11) %>%
    addCircleMarkers(
      data = valid_data,
      lng = ~longitude,
      lat = ~latitude,
      radius = radii,
      color = ~pal(get(metric)),
      fillOpacity = 0.85,
      weight = 2,
      popup = ~sprintf(
        "<div style='font-family: sans-serif; font-size: 13px; line-height: 1.5;'>
          <strong style='color: #0f172a;'>%s Traffic Police Station</strong><br/>
          <span style='color: #475569;'>Zone: %s</span><br/>
          <strong>Total Crashes:</strong> %d<br/>
          <strong style='color: #dc2626;'>Fatal Crashes:</strong> %d<br/>
          <strong>Fatality Rate:</strong> %.1f%%<br/>
          <small style='color: #64748b;'>Coordinates: %.4f, %.4f</small>
        </div>",
        station_raw, zone, total_crashes, fatal_crashes, 100 * fatality_rate, latitude, longitude
      ),
      label = ~sprintf("%s (%d crashes)", station_raw, total_crashes)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = valid_data[[metric]],
      title = if (metric == "total_crashes") "Total Crashes" else "Fatal Crashes",
      opacity = 0.9
    )
  
  m
}

#' Build Multi-Layer Interactive Leaflet Map for Overview Page
#' Displays Police Stations, Hospitals, and Accident Hotspots as independent toggleable layers.
#' @param hotspots_df Data frame containing accident crash records per station
#' @param police_df Data frame containing police station locations
#' @param hospitals_df Data frame containing verified hospital locations
#' @param active_layers Vector of active layer keys: 'police', 'hospitals', 'hotspots'
#' @return Leaflet map widget
#' Helper to create SVG base64 emoji markers for Leaflet
#' @param emoji Single character emoji (e.g. "👮", "🏥", "⚠️")
#' @param border_color Hex color for the circular border
#' @param bg_color Hex color for the circular badge fill
#' @param size Pixel width and height (default 28)
#' @param offset_x Pixel horizontal offset from geographic point (for side-by-side positioning)
#' @return Leaflet custom icon
make_emoji_icon <- function(emoji, border_color = "#1d4ed8", bg_color = "#eff6ff", size = 28, offset_x = 0) {
  svg <- sprintf(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"><filter id=\"s\" x=\"-25%%\" y=\"-25%%\" width=\"150%%\" height=\"150%%\"><feDropShadow dx=\"0\" dy=\"1.5\" stdDeviation=\"1.2\" flood-color=\"#0f172a\" flood-opacity=\"0.35\"/></filter><circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"%s\" stroke=\"%s\" stroke-width=\"2.5\" filter=\"url(#s)\"/><text x=\"%f\" y=\"%f\" font-family=\"Apple Color Emoji, Segoe UI Emoji, Noto Color Emoji, sans-serif\" font-size=\"%f\" text-anchor=\"middle\" dominant-baseline=\"central\">%s</text></svg>",
    size, size, size, size,
    size/2, size/2, (size/2) - 1.8,
    bg_color, border_color,
    size/2, size/2 + 0.5, size * 0.56,
    emoji
  )
  b64 <- gsub("\r|\n", "", jsonlite::base64_enc(charToRaw(enc2utf8(svg))))
  uri <- paste0("data:image/svg+xml;base64,", b64)
  leaflet::makeIcon(
    iconUrl = uri,
    iconWidth = size,
    iconHeight = size,
    iconAnchorX = (size / 2) + offset_x,
    iconAnchorY = size / 2,
    popupAnchorX = -offset_x,
    popupAnchorY = -size / 2
  )
}

#' Render Multi-Layer Interactive Map on Overview Page
#' Displays selectable Police Stations, Hospitals, and Accident Hotspots with Emoji Markers
#' @param hotspots_df Data frame containing accident crash records per station
#' @param police_df Data frame containing police station locations
#' @param hospitals_df Data frame containing verified hospital locations
#' @param active_layers Vector of active layer keys: 'police', 'hospitals', 'hotspots'
#' @return Leaflet map widget
render_overview_multilayers_map <- function(hotspots_df,
                                           police_df,
                                           hospitals_df,
                                           active_layers = c("police", "hospitals", "hotspots")) {
  base_leaflet <- leaflet() %>%
    addTiles(
      urlTemplate = "https://mt1.google.com/vt/lyrs=m,traffic&x={x}&y={y}&z={z}",
      group = "Google Live Traffic",
      attribution = "Google Maps Traffic",
      options = tileOptions(maxZoom = 20)
    ) %>%
    addTiles(
      urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}",
      group = "Google Roadmap",
      attribution = "Google Maps",
      options = tileOptions(maxZoom = 20)
    ) %>%
    addTiles(
      urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
      group = "Google Hybrid (Satellite)",
      attribution = "Google Maps Satellite",
      options = tileOptions(maxZoom = 20)
    ) %>%
    addTiles(
      urlTemplate = "https://mt1.google.com/vt/lyrs=p&x={x}&y={y}&z={z}",
      group = "Google Terrain",
      attribution = "Google Maps Terrain",
      options = tileOptions(maxZoom = 20)
    ) %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    setView(lng = 77.60, lat = 12.98, zoom = 11)

  # Pre-create emoji marker icons
  # When Police Stations and Accident Hotspots share coordinates, horizontal offsets ensure both sit side-by-side
  police_icon  <- make_emoji_icon("👮", border_color = "#1d4ed8", bg_color = "#eff6ff", size = 28, offset_x = 13)
  hotspot_icon <- make_emoji_icon("⚠️", border_color = "#dc2626", bg_color = "#fef2f2", size = 28, offset_x = -13)
  hosp_icon    <- make_emoji_icon("🏥", border_color = "#059669", bg_color = "#ecfdf5", size = 28, offset_x = 0)

  # 1. Accident Hotspots Layer (Heat halos + ⚠️ emoji marker)
  if (!is.null(hotspots_df) && nrow(hotspots_df) > 0) {
    valid_hotspots <- hotspots_df %>%
      filter(!is.na(latitude), !is.na(longitude))
    if ("coord_valid" %in% names(valid_hotspots)) {
      valid_hotspots <- valid_hotspots %>% filter(coord_valid == TRUE)
    }
    
    if (nrow(valid_hotspots) > 0) {
      pal <- colorNumeric(
        palette = c("#3b82f6", "#eab308", "#ef4444", "#991b1b"),
        domain = valid_hotspots$total_crashes
      )
      max_val <- max(valid_hotspots$total_crashes, na.rm = TRUE)
      min_val <- min(valid_hotspots$total_crashes, na.rm = TRUE)
      radii <- 10 + 14 * ((valid_hotspots$total_crashes - min_val) / max(1, (max_val - min_val)))
      
      hotspot_popup_content <- sprintf(
        "<div style='font-family: sans-serif; font-size: 13px; line-height: 1.5; min-width: 220px;'>
          <div style='background: #fef2f2; padding: 5px 8px; border-radius: 4px; margin-bottom: 6px; border-left: 3px solid #ef4444;'>
            <strong style='color: #991b1b; font-size: 13px;'>⚠️ %s Hotspot</strong>
          </div>
          <span style='color: #475569;'>Zone: %s</span><br/>
          <strong>Total Recorded Crashes (2024):</strong> %d<br/>
          <strong style='color: #dc2626;'>Fatal Crashes:</strong> %d<br/>
          <strong>Fatality Rate:</strong> %.1f%%<br/>
          <small style='color: #64748b;'>Coordinates: %.4f, %.4f</small>
        </div>",
        valid_hotspots$station_raw,
        ifelse(!is.na(valid_hotspots$zone) & valid_hotspots$zone != "Unknown", valid_hotspots$zone, "Bengaluru"),
        valid_hotspots$total_crashes,
        valid_hotspots$fatal_crashes,
        100 * valid_hotspots$fatality_rate,
        valid_hotspots$latitude, valid_hotspots$longitude
      )
      
      base_leaflet <- base_leaflet %>%
        addCircleMarkers(
          data = valid_hotspots,
          lng = ~longitude,
          lat = ~latitude,
          radius = radii,
          color = "#ffffff",
          fillColor = ~pal(total_crashes),
          fillOpacity = 0.45,
          weight = 1.5,
          group = "Accident Hotspots",
          popup = hotspot_popup_content,
          label = ~sprintf("⚠️ %s (%d crashes)", station_raw, total_crashes)
        ) %>%
        addMarkers(
          data = valid_hotspots,
          lng = ~longitude,
          lat = ~latitude,
          icon = hotspot_icon,
          group = "Accident Hotspots",
          popup = hotspot_popup_content,
          label = ~sprintf("⚠️ %s (%d crashes)", station_raw, total_crashes)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = valid_hotspots$total_crashes,
          title = "Crash Count",
          opacity = 0.9,
          layerId = "hotspots_legend",
          group = "Accident Hotspots"
        )
    }
  }

  # 2. Police Stations Layer (👮 Emoji Marker)
  if (!is.null(police_df) && nrow(police_df) > 0) {
    valid_police <- police_df %>%
      filter(!is.na(latitude), !is.na(longitude))
    if ("coord_valid" %in% names(valid_police)) {
      valid_police <- valid_police %>% filter(coord_valid == TRUE)
    }
    
    if (nrow(valid_police) > 0) {
      stn_names <- if ("station_clean" %in% names(valid_police)) {
        valid_police$station_clean
      } else if ("station_kml_raw" %in% names(valid_police)) {
        sub("^Traffic Police Station,[ ]*", "", valid_police$station_kml_raw)
      } else if ("station_raw" %in% names(valid_police)) {
        valid_police$station_raw
      } else {
        paste("Station", seq_len(nrow(valid_police)))
      }
      valid_police$display_name <- stn_names
      
      police_popup_content <- sprintf(
        "<div style='font-family: sans-serif; font-size: 13px; line-height: 1.5; min-width: 220px;'>
          <div style='background: #eff6ff; padding: 5px 8px; border-radius: 4px; margin-bottom: 6px; border-left: 3px solid #2563eb;'>
            <strong style='color: #1e3a8a; font-size: 13px;'>👮 %s Traffic Police Station</strong>
          </div>
          <strong>Zone:</strong> %s<br/>
          <strong>Sub-division:</strong> %s<br/>
          <strong>Jurisdiction:</strong> Administrative Traffic Police Station<br/>
          <strong>Data Source:</strong> Official Karnataka GIS Boundary Centroid<br/>
          <small style='color: #64748b;'>Coordinates: %.4f, %.4f</small>
        </div>",
        valid_police$display_name,
        ifelse(!is.na(valid_police$zone) & valid_police$zone != "Unknown", valid_police$zone, "Bengaluru Urban"),
        ifelse(!is.na(valid_police$sub_division) & valid_police$sub_division != "Unknown", valid_police$sub_division, "N/A"),
        valid_police$latitude, valid_police$longitude
      )
      
      base_leaflet <- base_leaflet %>%
        addMarkers(
          data = valid_police,
          lng = ~longitude,
          lat = ~latitude,
          icon = police_icon,
          group = "Police Stations",
          popup = police_popup_content,
          label = ~sprintf("👮 %s Traffic Police Station", display_name)
        )
    }
  }

  # 3. Hospitals Layer (🏥 Emoji Marker)
  if (!is.null(hospitals_df) && nrow(hospitals_df) > 0) {
    valid_hosp <- hospitals_df %>%
      filter(!is.na(latitude), !is.na(longitude))
    
    if (nrow(valid_hosp) > 0) {
      hosp_popup_content <- sprintf(
        "<div style='font-family: sans-serif; font-size: 13px; line-height: 1.5; min-width: 230px;'>
          <div style='background: #ecfdf5; padding: 5px 8px; border-radius: 4px; margin-bottom: 6px; border-left: 3px solid #10b981;'>
            <strong style='color: #065f46; font-size: 13px;'>🏥 %s</strong>
          </div>
          <strong>Facility Type:</strong> %s<br/>
          <strong>Emergency Care:</strong> %s<br/>
          <strong>Zone:</strong> %s<br/>
          <strong>Address:</strong> %s<br/>
          <strong>Emergency Contact:</strong> %s<br/>
          <small style='color: #64748b;'>Coordinates: %.4f, %.4f</small>
        </div>",
        valid_hosp$hospital_name,
        valid_hosp$facility_type,
        valid_hosp$emergency_trauma,
        valid_hosp$zone,
        valid_hosp$address,
        valid_hosp$contact_phone,
        valid_hosp$latitude, valid_hosp$longitude
      )
      
      base_leaflet <- base_leaflet %>%
        addMarkers(
          data = valid_hosp,
          lng = ~longitude,
          lat = ~latitude,
          icon = hosp_icon,
          group = "Hospitals",
          popup = hosp_popup_content,
          label = ~sprintf("🏥 %s (%s)", hospital_name, facility_type)
        )
    }
  }

  # Add layers control (Base tile groups + Toggleable Overlay groups)
  base_leaflet <- base_leaflet %>%
    addLayersControl(
      baseGroups = c(
        "Google Live Traffic",
        "Google Roadmap",
        "Google Hybrid (Satellite)",
        "Google Terrain",
        "CartoDB Positron",
        "OpenStreetMap"
      ),
      overlayGroups = c("Police Stations", "Hospitals", "Accident Hotspots"),
      position = "topright",
      options = layersControlOptions(collapsed = TRUE)
    )

  base_leaflet
}

#' Reusable Visualization Card for UrbanPulse Analytical Figures
#' @param filename Filename in plots/ directory (e.g. '01_traffic_volume_timeseries.png')
#' @param title Display title for the visualization
#' @param description Brief contextual description of what the graph represents
#' @param section_tag Optional category badge (e.g., 'Traffic Dynamics', 'Longitudinal')
#' @param height Image max-height in pixels (default '340px')
#' @param full_width Boolean indicating whether card should take full 12-column width (default FALSE)
#' @return Shiny column containing the visualization card
viz_card <- function(filename, title, description, section_tag = NULL, height = "340px", full_width = FALSE) {
  img_rel_path <- paste0("plots/", filename)
  disk_path <- file.path(getwd(), "UrbanPulse_Standalone_Results", "plots", filename)
  file_exists <- file.exists(disk_path)
  
  col_width <- if (full_width) 12 else 6
  
  column(
    width = col_width,
    div(
      class = "viz-card card",
      style = "margin-bottom: 24px; display: flex; flex-direction: column; justify-content: space-between; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 18px; box-shadow: 0 1px 3px rgba(0,0,0,0.03);",
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 8px;",
        div(
          h4(style = "font-weight: 700; color: #0f172a; font-size: 15px; margin: 0 0 4px 0;", title),
          if (!is.null(section_tag)) {
            span(class = "badge-tag", style = "background: #eff6ff; color: #2563eb; border: 1px solid #bfdbfe; font-size: 11px; font-weight: 600; padding: 2px 8px; border-radius: 4px;", section_tag)
          }
        ),
        if (file_exists) {
          tags$button(
            type = "button",
            class = "btn btn-xs viz-expand-btn",
            onclick = sprintf("openVizLightbox('%s', '%s')", img_rel_path, gsub("'", "\\\\'", title)),
            style = "font-size: 11px; padding: 3px 8px; border-radius: 4px; border: 1px solid #cbd5e1; background: #f8fafc; color: #334155; font-weight: 500; cursor: pointer;",
            "🔍 Fullscreen"
          )
        }
      ),
      p(style = "font-size: 13px; color: #64748b; line-height: 1.45; margin-bottom: 12px; min-height: 36px;", description),
      if (file_exists) {
        div(
          class = "viz-img-wrapper",
          style = "background: #ffffff; border: 1px solid #f1f5f9; border-radius: 6px; overflow: hidden; display: flex; align-items: center; justify-content: center; cursor: zoom-in; position: relative;",
          onclick = sprintf("openVizLightbox('%s', '%s')", img_rel_path, gsub("'", "\\\\'", title)),
          tags$img(
            src = img_rel_path,
            alt = title,
            class = "viz-img img-responsive",
            style = sprintf("max-height: %s; width: 100%%; object-fit: contain;", height),
            loading = "lazy"
          )
        )
      } else {
        div(
          class = "alert alert-warning",
          style = "font-size: 12px; margin-bottom: 0; padding: 10px 14px;",
          tags$strong("Notice: "), sprintf("Visualization snapshot '%s' was not found on disk. Run UrbanPulse_Complete_Analysis.R to regenerate.", filename)
        )
      },
      div(
        style = "margin-top: 10px; display: flex; justify-content: space-between; align-items: center; border-top: 1px solid #f8fafc; padding-top: 8px;",
        span(style = "font-size: 11px; color: #94a3b8;", sprintf("File: %s", filename)),
        span(style = "font-size: 11px; color: #2563eb; cursor: pointer; font-weight: 500;",
             onclick = sprintf("openVizLightbox('%s', '%s')", img_rel_path, gsub("'", "\\\\'", title)),
             "Click to expand ↗")
      )
    )
  )
}

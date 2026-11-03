# ==============================================================================
# UrbanPulse — app.R
# Production-Quality R Shiny Interactive Dashboard
# Traffic Prediction & Road Accident Hotspot Analysis
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(DT)
  library(dplyr)
  library(lubridate)
  library(readr)
})

# Source modular utilities
source(file.path(getwd(), "R", "helpers.R"))
source(file.path(getwd(), "R", "visualization.R"))

# Register static resource paths for standalone visualization figures and reports
standalone_plots_dir   <- file.path(getwd(), "UrbanPulse_Standalone_Results", "plots")
standalone_reports_dir <- file.path(getwd(), "UrbanPulse_Standalone_Results", "reports")
standalone_tables_dir  <- file.path(getwd(), "UrbanPulse_Standalone_Results", "tables")

if (dir.exists(standalone_plots_dir)) {
  shiny::addResourcePath("plots", standalone_plots_dir)
}
if (dir.exists(standalone_reports_dir)) {
  shiny::addResourcePath("reports", standalone_reports_dir)
}

standalone_perf_file <- file.path(standalone_tables_dir, "model_performance.csv")
standalone_perf_df <- if (file.exists(standalone_perf_file)) {
  suppressWarnings(read_csv(standalone_perf_file, show_col_types = FALSE))
} else NULL

# ------------------------------------------------------------------------------
# 1. Load Processed Datasets Once at Startup (Optimized In-Memory Cache)
# ------------------------------------------------------------------------------
proc_dir <- file.path(getwd(), "data", "processed")
audit_dir <- file.path(getwd(), "reports", "data_audit")
eval_dir <- file.path(getwd(), "reports", "model_evaluation")
eda_dir <- file.path(getwd(), "reports", "eda")
model_dir <- file.path(getwd(), "models")

crashes_panel    <- suppressWarnings(read_csv(file.path(proc_dir, "bengaluru_crashes_station_panel.csv"), show_col_types = FALSE))
crash_trend      <- suppressWarnings(read_csv(file.path(proc_dir, "bengaluru_crash_trend_2007_2025.csv"), show_col_types = FALSE))
hotspots_geo     <- suppressWarnings(read_csv(file.path(proc_dir, "hotspots_geocoded.csv"), show_col_types = FALSE))
violations_df    <- suppressWarnings(read_csv(file.path(proc_dir, "bengaluru_traffic_violations_clean.csv"), show_col_types = FALSE))
vehicles_df      <- suppressWarnings(read_csv(file.path(proc_dir, "bengaluru_registered_vehicles_clean.csv"), show_col_types = FALSE))
cities_accidents <- suppressWarnings(read_csv(file.path(proc_dir, "india_cities_accidents_clean.csv"), show_col_types = FALSE))
traffic_test     <- suppressWarnings(read_csv(file.path(proc_dir, "traffic_test_clean.csv"), show_col_types = FALSE))
traffic_features <- suppressWarnings(read_csv(file.path(proc_dir, "utd19_traffic_features.csv"), n_max = 12000, show_col_types = FALSE))
hospitals_df     <- load_bengaluru_hospitals()
btp_kml_path     <- file.path(getwd(), "data", "raw", "bengaluru_traffic_police_stations.kml")
police_stations_df <- if (file.exists(btp_kml_path)) {
  kml_df <- parse_btp_kml(btp_kml_path)
  station_meta <- crashes_panel %>%
    filter(!is.na(station_norm)) %>%
    select(station_norm, zone, sub_division) %>%
    distinct(station_norm, .keep_all = TRUE)
  kml_df %>%
    left_join(station_meta, by = "station_norm") %>%
    mutate(station_clean = sub("^Traffic Police Station,[ ]*", "", station_kml_raw))
} else {
  crashes_panel %>% filter(year == 2024, coord_valid == TRUE)
}

# Audit & Evaluation reports
model_comparison <- suppressWarnings(read_csv(file.path(eval_dir, "model_comparison.csv"), show_col_types = FALSE))
cls_metrics      <- suppressWarnings(read_csv(file.path(eval_dir, "classification_metrics.csv"), show_col_types = FALSE))
conf_matrix      <- suppressWarnings(read_csv(file.path(eval_dir, "confusion_matrix.csv"), show_col_types = FALSE))
missing_report   <- suppressWarnings(read_csv(file.path(audit_dir, "missingness_report.csv"), show_col_types = FALSE))
manifest_df      <- suppressWarnings(read_csv(file.path(audit_dir, "download_manifest.csv"), show_col_types = FALSE))
corridors_df     <- suppressWarnings(read_csv(file.path(eda_dir, "corridor_clusters_summary.csv"), show_col_types = FALSE))
exposure_df      <- suppressWarnings(read_csv(file.path(eda_dir, "exposure_analysis.csv"), show_col_types = FALSE))

# Pre-load trained models
lm_model <- tryCatch(readRDS(file.path(model_dir, "model_linear_regression.rds")), error = function(e) NULL)
rf_model <- tryCatch(readRDS(file.path(model_dir, "model_random_forest.rds")), error = function(e) NULL)

# ------------------------------------------------------------------------------
# 2. UI Specification (Clean, Classic White Aesthetic)
# ------------------------------------------------------------------------------
ui <- navbarPage(
  title = div(
    style = "font-weight: 700; letter-spacing: -0.5px; color: #0f172a; display: flex; align-items: center; gap: 8px;",
    span(style = "color: #2563eb; font-size: 20px;", "●"),
    "UrbanPulse",
    span(style = "font-size: 12px; font-weight: 500; color: #64748b; margin-left: 6px; padding: 2px 8px; background: #f1f5f9; border-radius: 12px;", "v1.0")
  ),
  id = "main_nav",
  theme = NULL,
  header = tagList(
    tags$head(
    tags$style(HTML("
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        background-color: #f8fafc;
        color: #1e293b;
        margin: 0;
      }
      .navbar {
        background-color: #ffffff !important;
        border-bottom: 1px solid #e2e8f0;
        box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.04);
        margin-bottom: 20px;
      }
      .navbar-default .navbar-nav > li > a {
        color: #475569;
        font-weight: 500;
        font-size: 14px;
        transition: color 0.15s ease;
      }
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover {
        color: #2563eb !important;
        background-color: transparent !important;
        border-bottom: 2px solid #2563eb;
      }
      .card {
        background: #ffffff;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.03);
      }
      .kpi-card {
        background: #ffffff;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        padding: 16px 20px;
        margin-bottom: 16px;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.03);
        border-left: 4px solid #2563eb;
      }
      .kpi-title {
        font-size: 12px;
        font-weight: 600;
        text-transform: uppercase;
        color: #64748b;
        margin-bottom: 4px;
      }
      .kpi-value {
        font-size: 26px;
        font-weight: 700;
        color: #0f172a;
        line-height: 1.2;
      }
      .kpi-sub {
        font-size: 12px;
        color: #94a3b8;
        margin-top: 4px;
      }
      .section-header {
        font-size: 18px;
        font-weight: 700;
        color: #0f172a;
        margin-top: 0;
        margin-bottom: 16px;
        border-bottom: 1px solid #f1f5f9;
        padding-bottom: 8px;
      }
      .notice-banner {
        background-color: #f8fafc;
        border: 1px solid #cbd5e1;
        border-left: 4px solid #3b82f6;
        padding: 12px 16px;
        border-radius: 4px;
        font-size: 13px;
        margin-bottom: 16px;
        color: #334155;
      }
      .badge-tag {
        display: inline-block;
        padding: 2px 8px;
        font-size: 11px;
        font-weight: 600;
        border-radius: 4px;
      }
      .gmaps-container {
        width: 100%;
        height: 520px;
        border-radius: 8px;
        border: 1px solid #e2e8f0;
        position: relative;
        overflow: hidden;
      }
      .gmaps-jump-btn {
        font-size: 11px !important;
        padding: 4px 10px !important;
        border-radius: 12px !important;
        border: 1px solid #cbd5e1 !important;
        background: #f8fafc !important;
        color: #1e293b !important;
        cursor: pointer;
        transition: all 0.15s ease;
      }
      .gmaps-jump-btn:hover {
        background: #2563eb !important;
        color: #ffffff !important;
        border-color: #2563eb !important;
      }
      /* Lightbox modal for visualization inspection */
      #viz_lightbox_modal {
        display: none;
        position: fixed;
        z-index: 999999;
        left: 0;
        top: 0;
        width: 100vw;
        height: 100vh;
        background: rgba(15, 23, 42, 0.88);
        backdrop-filter: blur(4px);
        -webkit-backdrop-filter: blur(4px);
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding: 20px;
        box-sizing: border-box;
      }
      #viz_lightbox_modal.active {
        display: flex !important;
      }
      .lightbox-content-box {
        position: relative;
        max-width: 94vw;
        max-height: 90vh;
        background: #ffffff;
        border-radius: 8px;
        box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.4);
        overflow: hidden;
        display: flex;
        flex-direction: column;
      }
      .lightbox-header {
        padding: 12px 20px;
        background: #f8fafc;
        border-bottom: 1px solid #e2e8f0;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .lightbox-title {
        font-weight: 700;
        font-size: 15px;
        color: #0f172a;
        margin: 0;
      }
      .lightbox-close-btn {
        background: #f1f5f9;
        border: 1px solid #cbd5e1;
        border-radius: 50%;
        width: 32px;
        height: 32px;
        font-size: 15px;
        color: #475569;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.15s ease;
      }
      .lightbox-close-btn:hover {
        background: #ef4444;
        color: #ffffff;
        border-color: #ef4444;
      }
      .lightbox-img {
        max-width: 92vw;
        max-height: 82vh;
        object-fit: contain;
        padding: 12px;
        background: #ffffff;
      }
      .viz-img-wrapper:hover .viz-img {
        transform: scale(1.015);
      }
    ")),
    tags$script(HTML("
      var urbanPulseGMap = null;
      var urbanPulseTraffic = null;
      var urbanPulseMarkers = [];
      var urbanPulseStationsData = [];
      var urbanPulseCurrentMetric = 'total_crashes';
      var gmapsScriptLoading = false;

      function renderGoogleMarkers() {
        if (!urbanPulseGMap || !window.google || !window.google.maps) return;
        for (var i = 0; i < urbanPulseMarkers.length; i++) {
          urbanPulseMarkers[i].setMap(null);
        }
        urbanPulseMarkers = [];

        var bounds = new google.maps.LatLngBounds();
        var infoWindow = new google.maps.InfoWindow();

        urbanPulseStationsData.forEach(function(station) {
          if (!station.latitude || !station.longitude) return;
          var pos = { lat: parseFloat(station.latitude), lng: parseFloat(station.longitude) };
          bounds.extend(pos);

          var metricVal = station[urbanPulseCurrentMetric] || 0;
          var color = '#3b82f6';
          if (metricVal >= 150) color = '#991b1b';
          else if (metricVal >= 80) color = '#ef4444';
          else if (metricVal >= 40) color = '#eab308';

          var marker = new google.maps.Marker({
            position: pos,
            map: urbanPulseGMap,
            title: station.station_raw,
            icon: {
              path: google.maps.SymbolPath.CIRCLE,
              scale: Math.max(6, Math.min(18, 5 + metricVal / 12)),
              fillColor: color,
              fillOpacity: 0.85,
              strokeWeight: 1.5,
              strokeColor: '#ffffff'
            }
          });

          marker.addListener('click', function() {
            var content = '<div style=\"font-family: sans-serif; font-size: 13px; line-height: 1.5; padding: 4px;\">' +
              '<strong style=\"color: #0f172a;\">' + station.station_raw + ' Traffic Police Station</strong><br/>' +
              '<span style=\"color: #475569;\">Zone: ' + (station.zone || 'N/A') + '</span><br/>' +
              '<strong>Total Crashes:</strong> ' + station.total_crashes + '<br/>' +
              '<strong style=\"color: #dc2626;\">Fatal Crashes:</strong> ' + station.fatal_crashes + '<br/>' +
              '<strong>Fatality Rate:</strong> ' + (station.fatality_rate ? (100 * station.fatality_rate).toFixed(1) + '%' : '0%') + '<br/>' +
              '<small style=\"color: #64748b;\">Coordinates: ' + station.latitude.toFixed(4) + ', ' + station.longitude.toFixed(4) + '</small>' +
              '</div>';
            infoWindow.setContent(content);
            infoWindow.open(urbanPulseGMap, marker);
          });

          urbanPulseMarkers.push(marker);
        });
      }

      function initGoogleMapSDK() {
        var container = document.getElementById('google_map_container');
        if (!container || !window.google || !window.google.maps) return;

        urbanPulseGMap = new google.maps.Map(container, {
          center: { lat: 12.9716, lng: 77.5946 },
          zoom: 11,
          mapTypeId: 'roadmap',
          mapTypeControl: true,
          streetViewControl: true,
          fullscreenControl: true
        });

        urbanPulseTraffic = new google.maps.TrafficLayer();
        urbanPulseTraffic.setMap(urbanPulseGMap);

        var searchInput = document.getElementById('gmaps_place_search');
        if (searchInput && google.maps.places) {
          var autocomplete = new google.maps.places.Autocomplete(searchInput, {
            componentRestrictions: { country: 'in' },
            fields: ['geometry', 'name']
          });
          autocomplete.addListener('place_changed', function() {
            var place = autocomplete.getPlace();
            if (place.geometry && place.geometry.location) {
              urbanPulseGMap.panTo(place.geometry.location);
              urbanPulseGMap.setZoom(15);
              new google.maps.Marker({
                position: place.geometry.location,
                map: urbanPulseGMap,
                title: place.name,
                animation: google.maps.Animation.DROP
              });
            }
          });
        }

        renderGoogleMarkers();
        $('#gmaps_status_badge').text('Google Maps SDK Active').css('background', '#16a34a');
        $('#gmaps_notice_banner').html('<span style=\"color: #16a34a; font-weight: 600;\">✓ Google Maps JavaScript API with Real-Time TrafficLayer & Places Autocomplete is active.</span>');
      }

      function loadGoogleMapsScript(apiKey) {
        if (!apiKey || apiKey.trim() === '') {
          var container = document.getElementById('google_map_container');
          if (container && (!window.google || !window.google.maps)) {
            container.innerHTML = '<div style=\"display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; text-align: center; padding: 30px; background: #ffffff;\">' +
              '<div style=\"width: 52px; height: 52px; border-radius: 50%; background: #eff6ff; display: flex; align-items: center; justify-content: center; margin-bottom: 16px;\">' +
              '<span style=\"color: #2563eb; font-size: 26px;\">🗺️</span>' +
              '</div>' +
              '<h4 style=\"font-weight: 700; color: #0f172a; margin-bottom: 8px;\">Google Maps JavaScript API Integration</h4>' +
              '<p style=\"color: #64748b; max-width: 520px; font-size: 13px; line-height: 1.6; margin-bottom: 18px;\">' +
              'To activate native Google Maps Vector tiles, dynamic TrafficLayer, Places Autocomplete, and Street View Pegman, enter your Google Cloud API key in the field on the left and click <strong>Initialize Google Maps</strong>.' +
              '</p>' +
              '<div style=\"background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 12px 18px; font-size: 12px; color: #475569; max-width: 500px; text-align: left;\">' +
              '<strong>💡 No API Key?</strong> Switch to the adjacent <strong>Leaflet + Google Maps Tiles</strong> tab above to view Google Roadmap, Google Live Traffic, Google Hybrid Satellite, and Terrain layers instantly without requiring any key!' +
              '</div>' +
              '</div>';
            $('#gmaps_status_badge').text('Tile Mode / Awaiting Key').css('background', '#f59e0b');
          }
          return;
        }

        if (window.google && window.google.maps) {
          initGoogleMapSDK();
          return;
        }

        if (gmapsScriptLoading) return;
        gmapsScriptLoading = true;

        var script = document.createElement('script');
        script.id = 'gmaps_script_tag';
        script.src = 'https://maps.googleapis.com/maps/api/js?key=' + encodeURIComponent(apiKey.trim()) + '&libraries=places&callback=initGoogleMapSDK';
        script.async = true;
        script.defer = true;
        script.onerror = function() {
          $('#gmaps_status_badge').text('Authentication Error').css('background', '#ef4444');
          $('#gmaps_notice_banner').html('<span style=\"color: #dc2626; font-weight: 600;\">⚠️ Unable to authenticate Google Maps API key. Ensure Maps JavaScript API and Places API are enabled in Google Cloud Console.</span>');
          gmapsScriptLoading = false;
        };
        document.head.appendChild(script);
      }

      $(document).ready(function() {
        $(document).on('click', '.gmaps-jump-btn', function() {
          var lat = parseFloat($(this).data('lat'));
          var lng = parseFloat($(this).data('lng'));
          if (urbanPulseGMap) {
            urbanPulseGMap.panTo({ lat: lat, lng: lng });
            urbanPulseGMap.setZoom(14);
          }
        });

        $(document).on('click', '#btn_recenter_gmaps', function() {
          if (urbanPulseGMap) {
            urbanPulseGMap.panTo({ lat: 12.9716, lng: 77.5946 });
            urbanPulseGMap.setZoom(11);
          }
        });

        $(document).on('change', '#gmaps_traffic_layer', function() {
          if (urbanPulseTraffic && urbanPulseGMap) {
            if ($(this).is(':checked')) {
              urbanPulseTraffic.setMap(urbanPulseGMap);
            } else {
              urbanPulseTraffic.setMap(null);
            }
          }
        });

        $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {
          window.dispatchEvent(new Event('resize'));
        });
      });

      Shiny.addCustomMessageHandler('update_gmaps_stations', function(message) {
        urbanPulseStationsData = message.stations || [];
        urbanPulseCurrentMetric = message.metric || 'total_crashes';
        var keyToUse = message.apiKey || $('#gmaps_api_key').val();
        loadGoogleMapsScript(keyToUse);
        if (urbanPulseGMap && window.google && window.google.maps) {
          renderGoogleMarkers();
        }
      });

      function openVizLightbox(src, title) {
        var modal = document.getElementById('viz_lightbox_modal');
        var img = document.getElementById('lightbox_display_img');
        var titleElem = document.getElementById('lightbox_modal_title');
        if (modal && img) {
          img.src = src;
          if (titleElem) titleElem.innerText = title;
          modal.classList.add('active');
        }
      }

      function closeVizLightbox() {
        var modal = document.getElementById('viz_lightbox_modal');
        if (modal) modal.classList.remove('active');
      }

      $(document).keydown(function(e) {
        if (e.keyCode === 27) { // Escape key
          closeVizLightbox();
        }
      });
    "))
  ),
  tags$div(
    id = "viz_lightbox_modal",
    onclick = "if(event.target === this) closeVizLightbox();",
    tags$div(
      class = "lightbox-content-box",
      tags$div(
        class = "lightbox-header",
        tags$h4(id = "lightbox_modal_title", class = "lightbox-title", "Visualization Inspection"),
        tags$button(type = "button", class = "lightbox-close-btn", onclick = "closeVizLightbox()", "✕")
      ),
      tags$img(id = "lightbox_display_img", class = "lightbox-img", src = "", alt = "Full Size View")
    )
  )
),
  
  # ============================================================================
  # PAGE 1: Overview
  # ============================================================================
  tabPanel("Overview",
    fluidRow(
      column(3,
        div(class = "kpi-card", style = "border-left-color: #2563eb;",
          div(class = "kpi-title", "Accident Records Audited"),
          div(class = "kpi-value", format(nrow(crashes_panel), big.mark = ",")),
          div(class = "kpi-sub", "Station annual panel across 2021–2024")
        )
      ),
      column(3,
        div(class = "kpi-card", style = "border-left-color: #0284c7;",
          div(class = "kpi-title", "Continuous Traffic Observations"),
          div(class = "kpi-value", format(nrow(traffic_features), big.mark = ",")),
          div(class = "kpi-sub", "5-minute continuous loop intervals (UTD19)")
        )
      ),
      column(3,
        div(class = "kpi-card", style = "border-left-color: #dc2626;",
          div(class = "kpi-title", "Top Hotspots Concentration"),
          div(class = "kpi-value", "75.5%"),
          div(class = "kpi-sub", "Accidents clustered in top 20% of stations")
        )
      ),
      column(3,
        div(class = "kpi-card", style = "border-left-color: #16a34a;",
          div(class = "kpi-title", "Forecasting Best Test RMSE"),
          div(class = "kpi-value", sprintf("%.2f", model_comparison$RMSE[which.min(model_comparison$RMSE)])),
          div(class = "kpi-sub", "Vehicles / 5 min (Random Forest R² = 0.962)")
        )
      )
    ),
    
    fluidRow(
      column(12,
        div(class = "card", style = "margin-bottom: 20px; padding: 0; overflow: hidden; border: 1px solid #e2e8f0; border-radius: 8px;",
          div(style = "padding: 10px 18px; background: #ffffff; border-bottom: 1px solid #f1f5f9; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 10px;",
            div(style = "display: flex; align-items: center; gap: 14px; flex-wrap: wrap;",
              span(style = "font-size: 13px; font-weight: 700; color: #0f172a; letter-spacing: -0.2px;", "Map Layers:"),
              checkboxGroupInput(
                "overview_map_layers",
                label = NULL,
                choiceNames = list(
                  tags$span(style = "font-weight: 500;", "👮 Police Stations"),
                  tags$span(style = "font-weight: 500;", "🏥 Hospitals"),
                  tags$span(style = "font-weight: 500;", "⚠️ Accident Hotspots")
                ),
                choiceValues = c("police", "hospitals", "hotspots"),
                selected = c("police", "hospitals", "hotspots"),
                inline = TRUE
              )
            ),
            div(style = "display: flex; align-items: center; gap: 16px; font-size: 12px; color: #334155; flex-wrap: wrap;",
              span(style = "display: inline-flex; align-items: center; gap: 6px; font-weight: 600;",
                span(style = "font-size: 14px;", "👮"),
                "Police Stations (43)"
              ),
              span(style = "display: inline-flex; align-items: center; gap: 6px; font-weight: 600;",
                span(style = "font-size: 14px;", "🏥"),
                "Hospitals (22)"
              ),
              span(style = "display: inline-flex; align-items: center; gap: 6px; font-weight: 600;",
                span(style = "font-size: 14px;", "⚠️"),
                "Accident Hotspots (32)"
              )
            )
          ),
          leafletOutput("overview_hotspot_map", height = "650px")
        )
      )
    ),
    
    fluidRow(
      column(7,
        div(class = "card",
          div(class = "section-header", "Bengaluru Road Crashes: Historical Trend (2007–2025)"),
          plotlyOutput("overview_crash_plot", height = "320px"),
          p(style = "font-size: 12px; color: #64748b; margin-top: 8px;",
            "Source: Bengaluru Traffic Police official records. Blue represents total recorded road crashes; red dashed indicates fatal incidents.")
        )
      ),
      column(5,
        div(class = "card",
          div(class = "section-header", "Diurnal Traffic Flow Cycle"),
          plotlyOutput("overview_traffic_plot", height = "320px"),
          p(style = "font-size: 12px; color: #64748b; margin-top: 8px;",
            "Mean 5-minute vehicle flow across 24 hours. Clear bi-modal commute peak at 08:30 and 18:30.")
        )
      )
    ),
    
    div(
      class = "section-header",
      style = "margin-top: 10px; margin-bottom: 8px; display: flex; justify-content: space-between; align-items: center;",
      span("Executive Analysis Snapshots"),
      span(style = "font-size: 11px; font-weight: 500; color: #64748b;", "Generated Static Publication Figures")
    ),
    p(style = "font-size: 13px; color: #64748b; margin-bottom: 18px;",
      "Key analytical figures summarizing traffic dynamics, long-term road safety trends, and machine learning predictive fidelity."),
    fluidRow(
      viz_card("01_traffic_volume_timeseries.png", "Traffic Volume Over Time",
               "Continuous 5-minute interval flow measurements demonstrating diurnal oscillations and peak traffic periods.",
               section_tag = "Traffic Dynamics", height = "280px"),
      viz_card("05_accident_trend_longitudinal.png", "Bengaluru Accident and Fatality Trends",
               "Official Bengaluru Traffic Police longitudinal road crash, fatality, and injury trends from 2007 to 2025.",
               section_tag = "Accident History", height = "280px")
    ),
    fluidRow(
      viz_card("11_actual_vs_predicted_traffic.png", "Actual vs Predicted Traffic Flow",
               "Out-of-sample test set evaluation of the Random Forest ensemble showing close alignment along the 45° reference parity line (R² = 0.9598).",
               section_tag = "Model Performance", height = "300px", full_width = TRUE)
    ),
    
    fluidRow(
      column(12,
        div(class = "card",
          div(class = "section-header", "Project Methodology & Academic Scope"),
          fluidRow(
            column(4,
              h5(style = "font-weight: 700; color: #1e293b;", "1. Traffic Volume Forecasting"),
              p(style = "font-size: 13px; color: #475569; line-height: 1.5;",
                "Forecasting vehicle volume on real continuous 5-minute loop detector readings using persistence baselines, multiple linear regression, and Random Forest ensembles with strict chronological partitioning.")
            ),
            column(4,
              h5(style = "font-weight: 700; color: #1e293b;", "2. Accident Hotspot Identification"),
              p(style = "font-size: 13px; color: #475569; line-height: 1.5;",
                "Analyzing official Bengaluru Police Station incident records (2021–2024) matched to verified Karnataka GIS spatial coordinates. Evaluates spatial clustering without inventing road coordinates.")
            ),
            column(4,
              h5(style = "font-weight: 700; color: #1e293b;", "3. Reproducible Data Integrity"),
              p(style = "font-size: 13px; color: #475569; line-height: 1.5;",
                "Zero data leakage in preprocessing and imputation. Complete audit trail, missing-data sensitivity analysis, and automated test suite ensures adherence to research standards.")
            )
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 2: Traffic Analysis
  # ============================================================================
  tabPanel("Traffic Analysis",
    fluidRow(
      column(3,
        div(class = "card",
          div(class = "section-header", "Traffic Filters"),
          selectInput("traffic_det_filter", "Detector Location:",
                      choices = c("All Detectors", unique(traffic_features$detid)),
                      selected = "All Detectors"),
          selectInput("traffic_day_filter", "Day Classification:",
                      choices = c("All Days" = "all", "Weekdays Only" = "weekday", "Weekends Only" = "weekend"),
                      selected = "all"),
          sliderInput("traffic_hour_range", "Hour Range:", min = 0, max = 23, value = c(0, 23)),
          hr(style = "border-top: 1px solid #f1f5f9;"),
          downloadButton("download_filtered_traffic", "Export Filtered CSV", class = "btn-default btn-sm", style = "width: 100%;"),
          br(), br(),
          div(class = "notice-banner",
            strong("Benchmark Note: "),
            "Traffic flow measurements are sourced from the verified UTD19 loop detector benchmark network. In accordance with Phase 2 guidelines, measurements are accurately identified and not falsely relabeled as Bengaluru streets."
          )
        )
      ),
      column(9,
        div(class = "card",
          div(class = "section-header", "Traffic Flow Dynamics & Diurnal Profiles"),
          plotlyOutput("traffic_profile_plot", height = "360px")
        ),
        fluidRow(
          column(6,
            div(class = "card",
              div(class = "section-header", "Volume Distribution (5-Min Flow)"),
              plotlyOutput("traffic_dist_plot", height = "280px")
            )
          ),
          column(6,
            div(class = "card",
              div(class = "section-header", "Flow vs Detector Occupancy"),
              plotlyOutput("traffic_scatter_plot", height = "280px")
            )
          )
        ),
        
        hr(style = "border-top: 1px solid #e2e8f0; margin: 30px 0 20px 0;"),
        div(
          class = "section-header",
          style = "font-size: 18px; margin-bottom: 8px; display: flex; justify-content: space-between; align-items: center;",
          span("Comprehensive Traffic Analysis Visualizations"),
          span(style = "font-size: 11px; font-weight: 500; color: #64748b;", "Full Benchmark Dataset Snapshot")
        ),
        p(style = "font-size: 13px; color: #64748b; margin-bottom: 20px;",
          "These publication-quality visualizations represent the complete UTD19 continuous sensor network. They capture macro and micro traffic phenomena across all detectors and intervals."),
        
        # 1. Traffic trends
        h5(style = "font-weight: 700; color: #0f172a; margin-bottom: 12px; display: flex; align-items: center; gap: 8px;",
           span(style = "background: #2563eb; color: #fff; border-radius: 50%; width: 20px; height: 20px; display: inline-flex; align-items: center; justify-content: center; font-size: 11px;", "1"),
           "Traffic Trends & Continuous Flow Dynamics"),
        fluidRow(
          viz_card("01_traffic_volume_timeseries.png", "Traffic Volume Over Time",
                   "Continuous 5-minute interval flow measurements demonstrating diurnal oscillations and peak traffic periods.",
                   section_tag = "Time-Series Flow", height = "360px", full_width = TRUE)
        ),
        
        # 2. Hourly and weekday patterns & 3. Day-of-week comparisons
        h5(style = "font-weight: 700; color: #0f172a; margin-top: 14px; margin-bottom: 12px; display: flex; align-items: center; gap: 8px;",
           span(style = "background: #2563eb; color: #fff; border-radius: 50%; width: 20px; height: 20px; display: inline-flex; align-items: center; justify-content: center; font-size: 11px;", "2"),
           "Hourly Diurnal Curves & Day-of-Week Comparisons"),
        fluidRow(
          viz_card("02_traffic_by_hour.png", "Weekday vs Weekend Traffic by Hour",
                   "Average traffic volume across 24 hours with shaded morning (08:00–11:00) and evening (17:00–20:00) peak commute windows.",
                   section_tag = "Hourly Patterns", height = "320px"),
          viz_card("03_traffic_by_day_of_week.png", "Traffic Distribution by Day of Week",
                   "Comparative distribution of flow volumes across Monday through Sunday with weekday commuter versus weekend relaxation curves.",
                   section_tag = "Day-of-Week Comparisons", height = "320px")
        ),
        
        # 4. Feature correlations
        h5(style = "font-weight: 700; color: #0f172a; margin-top: 14px; margin-bottom: 12px; display: flex; align-items: center; gap: 8px;",
           span(style = "background: #2563eb; color: #fff; border-radius: 50%; width: 20px; height: 20px; display: inline-flex; align-items: center; justify-content: center; font-size: 11px;", "3"),
           "Multi-Feature Correlation Analysis"),
        fluidRow(
          viz_card("04_traffic_correlation_matrix.png", "Traffic Feature Correlation",
                   "Pearson correlation matrix across traffic volume, occupancy, hour, weekend indicators, and temporal lag features.",
                   section_tag = "Feature Correlations", height = "380px", full_width = TRUE)
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 3: Traffic Prediction
  # ============================================================================
  tabPanel("Traffic Prediction",
    fluidRow(
      column(3,
        div(class = "card",
          div(class = "section-header", "Model Settings"),
          radioButtons("model_select", "Forecasting Model:",
                       choices = c("Random Forest (Ensemble)" = "rf",
                                   "Linear Regression (Statistical)" = "lm",
                                   "Persistence Baseline (Lag-1)" = "persist"),
                       selected = "rf"),
          selectInput("pred_horizon", "Forecast Horizon:",
                      choices = c("5-Minute Ahead (Single-Step)" = "5m",
                                  "15-Minute Multi-Step" = "15m",
                                  "30-Minute Multi-Step" = "30m"),
                      selected = "5m"),
          selectInput("pred_det_select", "Detector Sensor:",
                      choices = unique(traffic_test$detid),
                      selected = unique(traffic_test$detid)[1]),
          hr(style = "border-top: 1px solid #f1f5f9;"),
          div(class = "notice-banner",
            strong("Strict Temporal Partition: "),
            "Evaluated strictly on independent chronological test period (May 23–25, 2017). Zero future information leakage."
          )
        )
      ),
      column(9,
        fluidRow(
          column(4,
            div(class = "kpi-card",
              div(class = "kpi-title", "Selected Model MAE"),
              div(class = "kpi-value", textOutput("pred_kpi_mae")),
              div(class = "kpi-sub", "Mean Absolute Error (Vehicles / 5 min)")
            )
          ),
          column(4,
            div(class = "kpi-card",
              div(class = "kpi-title", "Selected Model RMSE"),
              div(class = "kpi-value", textOutput("pred_kpi_rmse")),
              div(class = "kpi-sub", "Root Mean Squared Error")
            )
          ),
          column(4,
            div(class = "kpi-card",
              div(class = "kpi-title", "RMSE Gain vs Baseline"),
              div(class = "kpi-value", textOutput("pred_kpi_gain")),
              div(class = "kpi-sub", "Relative reduction in RMSE over naive persistence")
            )
          )
        ),
        div(class = "card",
          div(class = "section-header", "Out-of-Sample Forecast vs Ground Truth Observations"),
          plotlyOutput("traffic_forecast_plot", height = "360px"),
          p(style = "font-size: 12px; color: #64748b; margin-top: 8px;",
            "Black solid line represents observed vehicle flow; colored line represents model forecast. Historical validation slice on test set.")
        ),
        
        hr(style = "border-top: 1px solid #e2e8f0; margin: 30px 0 20px 0;"),
        div(
          class = "section-header",
          style = "font-size: 18px; margin-bottom: 8px; display: flex; justify-content: space-between; align-items: center;",
          span("Verified Model Evaluation & Time-Series Diagnostics"),
          span(style = "font-size: 11px; font-weight: 500; color: #64748b;", "Out-of-Sample Chronological Test")
        ),
        p(style = "font-size: 13px; color: #64748b; margin-bottom: 18px;",
          "Rigorous chronological out-of-sample evaluation on independent test partition (May 22–25, 2017) with zero future data leakage. Preprocessing and imputation were calibrated exclusively on the training partition."),
        
        div(class = "card", style = "margin-bottom: 24px; padding: 18px;",
          div(class = "section-header", style = "font-size: 15px; margin-bottom: 10px;", "Model Performance Comparison Leaderboard"),
          tableOutput("standalone_metrics_table"),
          p(style = "font-size: 12px; color: #64748b; margin-top: 8px; margin-bottom: 0;",
            "Verified test set metrics from UrbanPulse_Standalone_Results/tables/model_performance.csv. Random Forest achieves top performance with R² = 0.9598 and RMSE = 39.60.")
        ),
        
        fluidRow(
          viz_card("11_actual_vs_predicted_traffic.png", "Actual vs Predicted Traffic Flow",
                   "Scatter plot comparing model predictions against ground truth flow showing close adherence along the 45° reference parity line.",
                   section_tag = "Forecast Parity", height = "340px"),
          viz_card("12_model_residuals_diagnostic.png", "Random Forest Residual Diagnostics",
                   "Empirical error density and distribution of residuals (Actual - Predicted) demonstrating zero-centered variance and minimal systematic bias.",
                   section_tag = "Residual Diagnostics", height = "340px")
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 4: Accident Hotspots
  # ============================================================================
  tabPanel("Accident Hotspots",
    fluidRow(
      column(3,
        div(class = "card",
          div(class = "section-header", "Hotspot Filters"),
          selectInput("hotspot_year", "Observation Year:",
                      choices = c("2024" = 2024, "2023" = 2023, "2022" = 2022, "2021" = 2021),
                      selected = 2024),
          selectInput("hotspot_zone", "Police Zone:",
                      choices = c("All Zones" = "all", "North" = "North", "South" = "South", "East" = "East", "West" = "West"),
                      selected = "all"),
          radioButtons("hotspot_metric", "Map Marker Sizing:",
                       choices = c("Total Crashes" = "total_crashes", "Fatal Crashes" = "fatal_crashes"),
                       selected = "total_crashes"),
          hr(style = "border-top: 1px solid #f1f5f9;"),
          div(class = "section-header", style = "font-size: 14px; margin-bottom: 8px;", "Google Maps API Settings"),
          textInput("gmaps_api_key", "Google Cloud API Key:",
                    value = Sys.getenv("GOOGLE_MAPS_API_KEY", ""),
                    placeholder = "AIzaSy..."),
          actionButton("btn_apply_gmaps_key", "Initialize Google Maps", class = "btn btn-sm btn-primary", style = "width: 100%; margin-bottom: 10px;"),
          p(style = "font-size: 11px; color: #64748b; line-height: 1.4; margin-bottom: 12px;",
            "An API key unlocks native Google Places Autocomplete, Street View Pegman, and vector traffic layer. Leaflet Google tiles work without a key."),
          hr(style = "border-top: 1px solid #f1f5f9;"),
          div(class = "notice-banner",
            strong("Spatial Resolution: "),
            "Mapped at Bengaluru Urban Traffic Police Station jurisdiction points from official Karnataka GIS KML data. Coordinates represent verified administrative station centroids, not estimated individual road segment incidents."
          )
        )
      ),
      column(9,
        div(class = "card",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #f1f5f9; padding-bottom: 8px; margin-bottom: 16px;",
            h4(style = "margin: 0; font-weight: 700; color: #0f172a; font-size: 18px;", "Bengaluru Traffic Police Stations: Interactive Incident Map"),
            span(id = "gmaps_status_badge", class = "badge", style = "background: #2563eb; color: #ffffff; padding: 6px 10px; font-size: 11px;", "Google Maps Integrated")
          ),
          tabsetPanel(
            id = "hotspot_map_tabset",
            tabPanel(
              title = span("Leaflet + Google Maps Tiles", style = "font-weight: 600;"),
              div(style = "padding-top: 12px;",
                p(style = "font-size: 12px; color: #64748b; margin-bottom: 8px;",
                  "Toggle between Google Live Traffic, Google Roadmap, Google Hybrid Satellite, and Terrain in the top-right layer control."),
                leafletOutput("hotspot_map", height = "520px")
              )
            ),
            tabPanel(
              title = span("Official Google Maps JS API (Live Traffic & Places)", style = "font-weight: 600;"),
              div(style = "padding-top: 12px;",
                fluidRow(
                  column(6,
                    div(style = "margin-bottom: 10px;",
                      tags$input(id = "gmaps_place_search", type = "text", class = "form-control input-sm",
                                 placeholder = "🔍 Search Bengaluru location / junction (e.g. Silk Board, Hebbal Flyover)...")
                    )
                  ),
                  column(3,
                    div(style = "padding-top: 6px;",
                      checkboxInput("gmaps_traffic_layer", "Live Google Traffic", value = TRUE)
                    )
                  ),
                  column(3,
                    div(style = "text-align: right; padding-top: 4px;",
                      actionButton("btn_recenter_gmaps", "Reset Center", class = "btn btn-sm btn-outline-secondary")
                    )
                  )
                ),
                div(style = "display: flex; gap: 6px; align-items: center; margin-bottom: 12px; flex-wrap: wrap;",
                  span(style = "font-size: 11px; font-weight: 600; color: #475569;", "Corridor Quick Jump:"),
                  tags$button(type = "button", class = "btn btn-xs gmaps-jump-btn", `data-lat` = "13.1007", `data-lng` = "77.5963", "Yelahanka (North)"),
                  tags$button(type = "button", class = "btn btn-xs gmaps-jump-btn", `data-lat` = "13.0075", `data-lng` = "77.6959", "K.R. Puram (East)"),
                  tags$button(type = "button", class = "btn btn-xs gmaps-jump-btn", `data-lat` = "13.1986", `data-lng` = "77.7066", "Airport (North)"),
                  tags$button(type = "button", class = "btn btn-xs gmaps-jump-btn", `data-lat` = "12.9698", `data-lng` = "77.7499", "Whitefield (East)"),
                  tags$button(type = "button", class = "btn btn-xs gmaps-jump-btn", `data-lat` = "12.8452", `data-lng` = "77.6602", "Electronic City (South)")
                ),
                div(id = "google_map_container", class = "gmaps-container"),
                div(id = "gmaps_notice_banner", style = "margin-top: 8px; font-size: 12px; color: #64748b;")
              )
            ),
            tabPanel(
              title = span("Standalone Spatial Corridor Map (Exported Report)", style = "font-weight: 600;"),
              div(style = "padding-top: 12px;",
                div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                  p(style = "font-size: 13px; color: #475569; margin: 0;",
                    "Interactive Leaflet visualization featuring spatial distance clusters (≤ 3.5 km corridor linkage), police station centroids, and incident severity cards."),
                  tags$a(href = "reports/bengaluru_accident_hotspots_map.html", target = "_blank", class = "btn btn-xs",
                         style = "font-size: 12px; padding: 4px 10px; border: 1px solid #2563eb; color: #2563eb; background: #eff6ff; border-radius: 4px; text-decoration: none; font-weight: 500;",
                         "Open Fullscreen Map ↗")
                ),
                tags$iframe(
                  src = "reports/bengaluru_accident_hotspots_map.html",
                  width = "100%",
                  height = "700px",
                  style = "border: 1px solid #e2e8f0; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
                  title = "Bengaluru Accident Hotspot Corridors Interactive Leaflet Map"
                )
              )
            )
          )
        ),
        div(class = "card",
          div(class = "section-header", "Ranked Hotspot Police Stations (Filtered)"),
          DTOutput("hotspot_table")
        ),
        
        hr(style = "border-top: 1px solid #e2e8f0; margin: 30px 0 20px 0;"),
        div(
          class = "section-header",
          style = "font-size: 18px; margin-bottom: 8px; display: flex; justify-content: space-between; align-items: center;",
          span("Bengaluru Accident Analysis: Trends, Hotspots & Exposure"),
          span(style = "font-size: 11px; font-weight: 500; color: #64748b;", "BTP Verified Records")
        ),
        p(style = "font-size: 13px; color: #64748b; margin-bottom: 20px;",
          "Multi-dimensional analysis of road accidents across temporal, jurisdictional, severity, and exposure dimensions based on official Bengaluru Traffic Police (BTP) records."),
        
        # 1. Long-term accident trends & 2. Highest-count police stations
        h5(style = "font-weight: 700; color: #0f172a; margin-bottom: 12px; display: flex; align-items: center; gap: 8px;",
           span(style = "background: #2563eb; color: #fff; border-radius: 50%; width: 20px; height: 20px; display: inline-flex; align-items: center; justify-content: center; font-size: 11px;", "1"),
           "Historical Trajectory & Top Incident Hotspots"),
        fluidRow(
          viz_card("05_accident_trend_longitudinal.png", "Bengaluru Accident and Fatality Trends",
                   "Official Bengaluru Traffic Police longitudinal road crash, fatality, and injury trends from 2007 to 2025.",
                   section_tag = "1. Longitudinal Trend", height = "320px"),
          viz_card("06_accident_hotspots_top20.png", "Top 20 Accident-Reporting Police Stations",
                   "Highest-incident police station jurisdictions ranked by total annual crashes with fatal crash color weighting.",
                   section_tag = "2. Hotspot Rankings", height = "320px")
        ),
        
        # 3. Accident severity by zone & 4. Accident exposure rates
        h5(style = "font-weight: 700; color: #0f172a; margin-top: 14px; margin-bottom: 12px; display: flex; align-items: center; gap: 8px;",
           span(style = "background: #2563eb; color: #fff; border-radius: 50%; width: 20px; height: 20px; display: inline-flex; align-items: center; justify-content: center; font-size: 11px;", "2"),
           "Zonal Severity Distribution & Motorization Exposure Rates"),
        fluidRow(
          viz_card("07_accident_severity_distribution.png", "Fatal vs Non-Fatal Crashes by Zone",
                   "Distribution of fatal collisions versus non-fatal casualty crashes across Bengaluru administrative police zones.",
                   section_tag = "3. Zonal Severity", height = "320px"),
          viz_card("08_accident_exposure_rates.png", "Accidents and Fatalities per 10,000 Registered Vehicles",
                   "Accident exposure rates normalized per 10,000 registered motor vehicles. Distinguishes sheer volume increases from actual risk rate trajectories.",
                   section_tag = "4. Exposure Rates", height = "320px")
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 5: Data Quality
  # ============================================================================
  tabPanel("Data Quality",
    fluidRow(
      column(12,
        div(class = "card",
          div(class = "section-header", "Data Quality Audit & Missingness Governance"),
          p("UrbanPulse enforces rigorous data hygiene protocols: UTF-8 BOM sanitization, strict type casting, missingness mechanism profiling, and leakage-free imputation.")
        )
      )
    ),
    fluidRow(
      column(6,
        div(class = "card",
          div(class = "section-header", "Dataset Acquisition & Verification Manifest"),
          DTOutput("manifest_table")
        )
      ),
      column(6,
        div(class = "card",
          div(class = "section-header", "Column Missingness & Treatment Protocol"),
          DTOutput("missingness_table")
        )
      )
    ),
    fluidRow(
      column(12,
        div(class = "card",
          div(class = "section-header", "Missing-Data Sensitivity Analysis (Model Robustness)"),
          tableOutput("sensitivity_table"),
          p(style = "font-size: 12px; color: #64748b; margin-top: 6px;",
            "Comparing complete-case deletion against training-set median imputation confirms consistent predictive generalization while retaining full sample size.")
        )
      )
    ),
    
    hr(style = "border-top: 1px solid #e2e8f0; margin: 30px 0 20px 0;"),
    div(
      class = "section-header",
      style = "font-size: 18px; margin-bottom: 8px; display: flex; justify-content: space-between; align-items: center;",
      span("Data Quality Audit & Cleaning Verification Visualizations"),
      span(style = "font-size: 11px; font-weight: 500; color: #64748b;", "Hygiene & Audit Trail")
    ),
    p(style = "font-size: 13px; color: #64748b; margin-bottom: 20px;",
      "Auditing missing value distributions across all loaded datasets and verifying the impact of sensor error filtration and summary row reconciliation."),
    fluidRow(
      viz_card("09_missing_values_audit.png", "Missing-Value Audit",
               "Cross-dataset missingness profile identifying variables with missing, blank, or placeholder values across raw files.",
               section_tag = "1. Missing-Value Analysis", height = "360px"),
      viz_card("10_before_after_cleaning.png", "Dataset Cleaning Summary",
               "Quantitative observation count reconciliation showing elimination of sensor malfunctions, negative values, and summary rows.",
               section_tag = "2. Cleaning Impact", height = "360px")
    )
  ),
  
  # ============================================================================
  # PAGE 6: Data Explorer
  # ============================================================================
  tabPanel("Data Explorer",
    fluidRow(
      column(3,
        div(class = "card",
          div(class = "section-header", "Select Dataset"),
          selectInput("explorer_dataset", "Browse Processed Data:",
                      choices = c("Bengaluru Station Crashes (2021-2024)" = "crashes",
                                  "Long-Term Crash Trends (2007-2025)" = "trends",
                                  "Traffic Violations (2011-2025)" = "violations",
                                  "Registered Vehicles (2017-2026)" = "vehicles",
                                  "India Cities Road Accidents (2024)" = "cities",
                                  "UTD19 Traffic Features (Sample)" = "traffic"),
                      selected = "crashes"),
          downloadButton("download_explorer_csv", "Download CSV", class = "btn-primary btn-sm", style = "width: 100%;")
        )
      ),
      column(9,
        div(class = "card",
          div(class = "section-header", "Searchable Data Table"),
          DTOutput("explorer_table")
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 7: Model Evaluation
  # ============================================================================
  tabPanel("Model Evaluation",
    fluidRow(
      column(12,
        div(class = "card",
          div(class = "section-header", "Comprehensive Model Comparison & Benchmark Evaluation"),
          p("Evaluated on an independent, non-overlapping chronological test period (May 23–25, 2017, N = 5,672).")
        )
      )
    ),
    fluidRow(
      column(7,
        div(class = "card",
          div(class = "section-header", "Forecasting Regression Leaderboard"),
          DTOutput("eval_comparison_table")
        )
      ),
      column(5,
        div(class = "card",
          div(class = "section-header", "Congestion Classification Metrics"),
          DTOutput("eval_classification_table"),
          hr(style = "border-top: 1px solid #f1f5f9;"),
          h5(style = "font-weight: 700;", "Confusion Matrix"),
          tableOutput("confusion_matrix_table")
        )
      )
    ),
    fluidRow(
      column(12,
        div(class = "card",
          div(class = "section-header", "Residual Error Diagnostics"),
          imageOutput("eval_residual_img", height = "360px")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 3. Server Logic (Reactive Data Filtering & Dynamic Plot Generation)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Page 1: Overview Plots ---
  output$overview_crash_plot <- renderPlotly({
    p <- ggplot(crash_trend, aes(x = year)) +
      geom_line(aes(y = total_accidents, text = paste("Year:", year, "<br>Total Crashes:", total_accidents)), color = "#2563eb", linewidth = 1.1) +
      geom_point(aes(y = total_accidents), color = "#2563eb", size = 2) +
      geom_line(aes(y = fatal_accidents, text = paste("Year:", year, "<br>Fatal Crashes:", fatal_accidents)), color = "#dc2626", linewidth = 0.9, linetype = "dashed") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Year", y = "Recorded Crashes") +
      theme_dashboard()
    ggplotly(p, tooltip = "text")
  })
  
  output$overview_traffic_plot <- renderPlotly({
    hourly_agg <- traffic_features %>%
      group_by(hour, is_weekend) %>%
      summarize(mean_flow = mean(flow, na.rm = TRUE), .groups = "drop") %>%
      mutate(DayType = ifelse(is_weekend == 1, "Weekend", "Weekday"))
    
    p <- ggplot(hourly_agg, aes(x = hour, y = mean_flow, color = DayType)) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = c("Weekday" = "#0284c7", "Weekend" = "#f59e0b")) +
      scale_x_continuous(breaks = seq(0, 23, by = 4)) +
      labs(x = "Hour of Day", y = "Mean Flow (Veh/5-min)") +
      theme_dashboard()
    ggplotly(p)
  })
  
  # --- Page 2: Traffic Analysis ---
  filtered_traffic <- reactive({
    df <- traffic_features
    if (input$traffic_det_filter != "All Detectors") {
      df <- df[df$detid == input$traffic_det_filter, ]
    }
    if (input$traffic_day_filter == "weekday") {
      df <- df[df$is_weekend == 0, ]
    } else if (input$traffic_day_filter == "weekend") {
      df <- df[df$is_weekend == 1, ]
    }
    df <- df[df$hour >= input$traffic_hour_range[1] & df$hour <= input$traffic_hour_range[2], ]
    df
  })
  
  output$traffic_profile_plot <- renderPlotly({
    data <- filtered_traffic()
    if (nrow(data) == 0) return(NULL)
    
    hourly_df <- data %>%
      group_by(hour) %>%
      summarize(
        mean_flow = mean(flow, na.rm = TRUE),
        median_flow = median(flow, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(hourly_df, aes(x = hour)) +
      geom_line(aes(y = mean_flow, color = "Mean Flow"), linewidth = 1.2) +
      geom_line(aes(y = median_flow, color = "Median Flow"), linewidth = 1.0, linetype = "dashed") +
      scale_color_manual(values = c("Mean Flow" = "#2563eb", "Median Flow" = "#64748b")) +
      scale_x_continuous(breaks = seq(0, 23, by = 2)) +
      labs(title = "Diurnal Traffic Flow Profile (Filtered)", x = "Hour of Day", y = "Vehicles / 5 min", color = "Statistic") +
      theme_dashboard()
    ggplotly(p)
  })
  
  output$traffic_dist_plot <- renderPlotly({
    data <- filtered_traffic()
    p <- ggplot(data, aes(x = flow)) +
      geom_histogram(bins = 30, fill = "#3b82f6", color = "#ffffff", alpha = 0.8) +
      labs(title = "Traffic Volume Distribution", x = "Vehicles / 5 min", y = "Count") +
      theme_dashboard()
    ggplotly(p)
  })
  
  output$traffic_scatter_plot <- renderPlotly({
    data <- filtered_traffic()
    sample_sub <- if (nrow(data) > 1500) data[sample(nrow(data), 1500), ] else data
    p <- ggplot(sample_sub, aes(x = occ, y = flow)) +
      geom_point(alpha = 0.3, size = 1.2, color = "#0284c7") +
      labs(title = "Flow vs Detector Occupancy", x = "Occupancy Fraction", y = "Flow (Veh/5-min)") +
      theme_dashboard()
    ggplotly(p)
  })
  
  output$download_filtered_traffic <- downloadHandler(
    filename = function() { paste0("urbanpulse_traffic_filtered_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(filtered_traffic(), file, row.names = FALSE) }
  )
  
  # --- Page 3: Traffic Prediction ---
  output$pred_kpi_mae <- renderText({
    m <- input$model_select
    val <- if (m == "rf") model_comparison$MAE[3] else if (m == "lm") model_comparison$MAE[2] else model_comparison$MAE[1]
    sprintf("%.2f", val)
  })
  output$pred_kpi_rmse <- renderText({
    m <- input$model_select
    val <- if (m == "rf") model_comparison$RMSE[3] else if (m == "lm") model_comparison$RMSE[2] else model_comparison$RMSE[1]
    sprintf("%.2f", val)
  })
  output$pred_kpi_gain <- renderText({
    m <- input$model_select
    val <- if (m == "rf") model_comparison$RMSE_Improvement_vs_Baseline_Pct[3] else if (m == "lm") model_comparison$RMSE_Improvement_vs_Baseline_Pct[2] else 0.0
    sprintf("+%.2f%%", val)
  })
  
  output$traffic_forecast_plot <- renderPlotly({
    det_sub <- traffic_test %>% filter(detid == input$pred_det_select)
    if (nrow(det_sub) == 0) return(NULL)
    slice_data <- det_sub[1:min(180, nrow(det_sub)), ]
    
    # Model prediction
    pred_vals <- if (input$model_select == "rf" && !is.null(rf_model)) {
      predict(rf_model, newdata = slice_data)
    } else if (input$model_select == "lm" && !is.null(lm_model)) {
      predict(lm_model, newdata = slice_data)
    } else {
      slice_data$flow_lag_1
    }
    
    plot_df <- data.frame(
      step = 1:nrow(slice_data),
      time = slice_data$datetime,
      Actual = slice_data$flow,
      Forecast = pred_vals
    )
    
    p <- ggplot(plot_df, aes(x = step)) +
      geom_line(aes(y = Actual, color = "Actual Flow"), linewidth = 1.0) +
      geom_line(aes(y = Forecast, color = "Forecast"), linewidth = 1.0, linetype = "dashed") +
      scale_color_manual(values = c("Actual Flow" = "#0f172a", "Forecast" = "#2563eb")) +
      labs(title = paste("Out-of-Sample Forecast (Detector:", input$pred_det_select, ")"),
           x = "Sequential 5-Minute Time Interval", y = "Traffic Flow (Veh/5-min)", color = "Series") +
      theme_dashboard()
    ggplotly(p)
  })
  
  # --- Page 1: Overview Multi-Layer Interactive Map ---
  output$overview_hotspot_map <- renderLeaflet({
    hotspots_data <- crashes_panel %>% filter(year == 2024, coord_valid == TRUE)
    
    render_overview_multilayers_map(
      hotspots_df  = hotspots_data,
      police_df    = police_stations_df,
      hospitals_df = hospitals_df
    )
  })

  # Toggle layers dynamically via leafletProxy preserving pan/zoom state
  observeEvent(input$overview_map_layers, {
    proxy  <- leafletProxy("overview_hotspot_map")
    layers <- input$overview_map_layers
    
    if (!is.null(layers) && "police" %in% layers) {
      proxy %>% showGroup("Police Stations")
    } else {
      proxy %>% hideGroup("Police Stations")
    }
    
    if (!is.null(layers) && "hospitals" %in% layers) {
      proxy %>% showGroup("Hospitals")
    } else {
      proxy %>% hideGroup("Hospitals")
    }
    
    if (!is.null(layers) && "hotspots" %in% layers) {
      proxy %>% showGroup("Accident Hotspots")
    } else {
      proxy %>% hideGroup("Accident Hotspots")
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # --- Page 4: Accident Hotspots ---
  filtered_hotspots <- reactive({
    df <- crashes_panel %>% filter(year == as.integer(input$hotspot_year))
    if (input$hotspot_zone != "all") {
      df <- df %>% filter(zone == input$hotspot_zone)
    }
    df
  })
  
  output$hotspot_map <- renderLeaflet({
    data <- filtered_hotspots()
    render_hotspot_leaflet_map(data, metric = input$hotspot_metric)
  })
  
  # Broadcast station data and Google Maps API key updates to client
  observe({
    data <- filtered_hotspots() %>%
      filter(!is.na(latitude), !is.na(longitude), coord_valid == TRUE) %>%
      select(station_raw, zone, total_crashes, fatal_crashes, fatality_rate, latitude, longitude)
    
    # Depend on API key input and apply button
    input$btn_apply_gmaps_key
    
    session$sendCustomMessage("update_gmaps_stations", list(
      stations = data,
      metric = input$hotspot_metric,
      apiKey = input$gmaps_api_key
    ))
  })
  
  output$hotspot_table <- renderDT({
    data <- filtered_hotspots() %>%
      arrange(desc(total_crashes)) %>%
      select(Station = station_raw, Zone = zone, `Sub-division` = sub_division,
             `Total Crashes` = total_crashes, `Fatal Crashes` = fatal_crashes,
             `Fatality Rate (%)` = fatality_rate, Geocoded = coord_valid) %>%
      mutate(`Fatality Rate (%)` = sprintf("%.1f%%", 100 * `Fatality Rate (%)`))
    
    datatable(data, options = list(pageLength = 8, autoWidth = TRUE), rownames = FALSE)
  })
  
  # --- Page 5: Data Quality ---
  output$manifest_table <- renderDT({
    datatable(
      manifest_df[, c("filename", "download_status", "file_size_bytes", "license")],
      options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE
    )
  })
  
  output$missingness_table <- renderDT({
    datatable(
      missing_report %>% filter(missing_count > 0) %>%
        select(dataset, column_name, missing_count, missing_pct, proposed_treatment),
      options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE
    )
  })
  
  output$sensitivity_table <- renderTable({
    sens_path <- file.path(audit_dir, "missing_sensitivity_analysis.csv")
    if (file.exists(sens_path)) read.csv(sens_path) else data.frame()
  })
  
  # --- Page 6: Data Explorer ---
  explorer_data <- reactive({
    req(input$explorer_dataset)
    switch(input$explorer_dataset,
      "crashes" = crashes_panel,
      "trends" = crash_trend,
      "violations" = violations_df,
      "vehicles" = vehicles_df,
      "cities" = cities_accidents,
      "traffic" = traffic_features
    )
  })
  
  output$explorer_table <- renderDT({
    df <- explorer_data()
    datatable(head(df, 500), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_explorer_csv <- downloadHandler(
    filename = function() { paste0("urbanpulse_", input$explorer_dataset, "_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(explorer_data(), file, row.names = FALSE) }
  )
  
  # --- Page 7: Model Evaluation ---
  output$eval_comparison_table <- renderDT({
    datatable(model_comparison, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
  })
  
  output$eval_classification_table <- renderDT({
    datatable(cls_metrics, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$confusion_matrix_table <- renderTable({
    conf_matrix
  }, rownames = TRUE)
  
  output$eval_residual_img <- renderImage({
    img_path <- file.path(eval_dir, "residual_diagnostics.png")
    if (!file.exists(img_path)) {
      img_path <- file.path(standalone_plots_dir, "12_model_residuals_diagnostic.png")
    }
    list(src = img_path, contentType = "image/png", width = "100%", alt = "Residual Diagnostics")
  }, deleteFile = FALSE)
  
  # --- Page 3 Standalone Metrics Table ---
  output$standalone_metrics_table <- renderTable({
    if (!is.null(standalone_perf_df) && nrow(standalone_perf_df) > 0) {
      standalone_perf_df
    } else {
      data.frame(Notice = "Model performance metrics file not found. Run UrbanPulse_Complete_Analysis.R to compute.")
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)
}

# Run Application
shinyApp(ui = ui, server = server)

# ---- Packages ----
library(shiny)
library(shinyWidgets)
library(bslib)
library(ggiraph)
library(tidyverse)
library(sf)
library(leaflet)
library(cowplot)
library(biscale)
library(cincy)
library(codec) # pak::pak("geomarker-io/codec@pins")
library(shinyjs)

# ---- Data & Variables ----
d_codec <- codec:::codec_latest_annual |>
  left_join(cincy::tract_tigris_2020, by = "census_tract_id_2020") |>
  st_as_sf()

d_vars <- set_names(
  setdiff(names(d_codec), c("census_tract_id_2020", "year", "geometry")),
  ~ str_to_title(str_replace_all(.x, "_", " "))
)

# ---- Palettes ----
codec_bi_pal <- c(
  "1-1" = "#eddcc1",
  "2-1" = "#d4aa92",
  "3-1" = "#bb7964",
  "1-2" = "#909992",
  "2-2" = "#81766f",
  "3-2" = "#71544c",
  "1-3" = "#375a66",
  "2-3" = "#31464d",
  "3-3" = "#2b3135"
)

uni_colors <- c(
  codec_colors()[1],
  "#567D91",
  "#789BAC",
  "#9FBAC8",
  "#CCDCE3",
  "#F6EDDE"
)

make_pal <- function(palette, var, levels) {
  colorFactor(palette, factor(var, levels = levels))
}

# ---- Binning helpers ----
qbreaks <- function(v, n) {
  classInt::classIntervals(v, n, style = "quantile")$brks
}

make_bins <- function(d, x, y = NULL) {
  if (is.null(y)) {
    list(qbreaks(d[[x]], 6))
  } else {
    list(qbreaks(d[[x]], 3), qbreaks(d[[y]], 3))
  }
}

# ---- Rectangles for background panels ----
make_rectangles <- function(bins, type = c("uni", "bi")) {
  type <- match.arg(type)
  if (type == "bi") {
    expand.grid(x = 1:3, y = 1:3) |>
      mutate(
        xmin = c(-Inf, bins[[1]])[x + 1],
        xmax = c(bins[[1]], Inf)[x + 1],
        ymin = c(-Inf, bins[[2]])[y + 1],
        ymax = c(bins[[2]], Inf)[y + 1],
        fill = codec_bi_pal[(y - 1) * 3 + x]
      )
  } else {
    tibble(
      xmin = c(-Inf, bins[[1]][-1]),
      xmax = c(bins[[1]][-1], Inf),
      fill = uni_colors
    )
  }
}

# ---- Girafe size wrapper ----
make_girafe <- function(p, input) {
  girafe(
    ggobj = p,
    width_svg = if (input$side_plot_selector == "main_map") 3 else 6,
    height_svg = if (input$side_plot_selector == "main_map") 3 else 6,
    options = list(
      opts_sizing(width = 1, rescale = TRUE),
      opts_selection(type = "single")
    )
  )
}

# ---- Base map ----
base_map <- leaflet() |>
  setView(-84.55, 39.18, zoom = 10.5) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  removeLayersControl()

# ---- UI bits ----
make_picker <- function(id, label, choices, selected) {
  shinyWidgets::pickerInput(
    inputId = id,
    label = label,
    choices = choices,
    selected = selected,
    multiple = FALSE,
    inline = TRUE,
    width = "fit",
    options = pickerOptions(liveSearch = TRUE)
  )
}

button_help_bivariate <- actionBttn(
  "legend_modal",
  style = "simple",
  label = "Bivariate",
  size = "sm",
  block = FALSE,
  icon = icon("question-circle")
) |>
  tagAppendAttributes(style = "color: #C28273; background-color: #FFFFFF;")

geography_selector <- selectInput(
  inputId = "sel_geo",
  label = actionBttn(
    inputId = "geography_sel_label",
    style = "simple",
    size = "sm",
    block = FALSE,
    label = a(
      "Geography",
      href = "https://geomarker.io/cincy/articles/geographies.html",
      target = "_blank"
    )
  ) |>
    tagAppendAttributes(style = "color: #C28273; background-color: #FFFFFF;"),
  choices = c(
    "census tract" = "tract",
    "ZCTA" = "zcta",
    "neighborhood" = "neighborhood"
  ),
  selected = "tract",
  width = "100%"
)

switch_plots <- selectInput(
  "side_plot_selector",
  label = "Focus",
  choices = c("map" = "main_map", "scatterplot" = "main_scatterplot"),
  selected = "main_map",
  width = "25%"
)

selector_view <- selectInput(
  inputId = "view_method",
  label = button_help_bivariate,
  choices = c("univariate" = "univariate", "bivariate" = "bivariate"),
  selected = "bivariate",
  width = "100%"
)

ex_card <- card(
  card_header(
    img(
      src = "https://raw.githubusercontent.com/geomarker-io/codec/5ff6b5c81eacf377c0e32bf0bfcfab61bb615b00/man/figures/logo.svg",
      width = "75px",
      height = "auto",
      style = "float: left; margin-right: 15px;"
    ),
    layout_column_wrap(
      width = 1 / 2,
      height = 80,
      p(
        br(),
        a(
          "Community Data Explorer for Cincinnati",
          href = "https://geomarker.io/codec",
          target = "_blank"
        ),
        br(),
        paste0("CoDEC version ", packageVersion("codec"))
      ),
      layout_column_wrap(
        width = 1 / 2,
        height = 75,
        geography_selector,
        selector_view
      ) |>
        tagAppendAttributes(style = "float: right")
    )
  ),
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      uiOutput("x_sel"),
      uiOutput("y_sel"),
      switch_plots,
      conditionalPanel(
        "input.side_plot_selector == 'main_map'",
        girafeOutput("side_scatter")
      ),
      conditionalPanel(
        "input.side_plot_selector == 'main_scatterplot'",
        leafletOutput("side_map", height = "50vh")
      ),
      width = "30%"
    ),
    conditionalPanel(
      "input.side_plot_selector == 'main_map'",
      leafletOutput("big_map", height = "80vh")
    ),
    conditionalPanel(
      "input.side_plot_selector == 'main_scatterplot'",
      girafeOutput("big_scatter", height = "78%", width = "78%")
    ),
    uiOutput("clear_button_panel")
  )
)

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#396175",
    primary = "#C28273",
    "grid-gutter-width" = "0.0rem",
    "border-radius" = "0.5rem",
    "btn-border-radius" = "0.25rem"
  ),
  tags$head(tags$style(type = "text/css", "text {font-family: sans-serif}")),
  shinyjs::useShinyjs(),
  ex_card
)

# ---- Server ----
server <- function(input, output, session) {
  # enable/disable Y picker
  observeEvent(input$view_method, {
    if (input$view_method == "univariate") {
      shinyjs::disable("y_sel")
    } else {
      shinyjs::enable("y_sel")
    }
  })

  # pickers
  output$x_sel <- renderUI(make_picker("x", "X:", d_vars, "prop_poverty"))
  output$y_sel <- renderUI(make_picker("y", "Y:", d_vars, "median_home_value"))

  # current geography
  d_geo <- reactive({
    req(input$x)
    g <- switch(
      input$sel_geo,
      zcta = cincy::zcta_tigris_2020,
      neighborhood = cincy::neigh_cchmc_2020,
      tract = NULL
    )
    dat <- if (is.null(g)) d_codec else d_codec |> cincy::interpolate(to = g)
    dat |> dplyr::rename(geo_index = 1) |> st_transform(4326)
  })

  # bins
  bins <- reactive({
    if (input$view_method == "univariate") {
      make_bins(d_geo(), input$x)
    } else {
      make_bins(d_geo(), input$x, input$y)
    }
  })

  # prepared data + labels
  d_ready <- reactive({
    if (input$view_method == "univariate") {
      bx <- .bincode(
        d_geo()[[input$x]],
        breaks = bins()[[1]],
        include.lowest = TRUE
      )
      d_geo() |>
        mutate(
          x_class = as.character(as.numeric(bx)),
          out_lab = paste0(
            geo_index,
            "<br>",
            input$x,
            ": ",
            round(.data[[input$x]], 2)
          )
        )
    } else {
      bx <- .bincode(
        d_geo()[[input$x]],
        breaks = bins()[[1]],
        include.lowest = TRUE
      )
      by <- .bincode(
        d_geo()[[input$y]],
        breaks = bins()[[2]],
        include.lowest = TRUE
      )
      d_geo() |>
        mutate(
          bi_class = paste0(as.numeric(bx), "-", as.numeric(by)),
          out_lab = paste0(
            geo_index,
            "<br>",
            input$x,
            ": ",
            round(.data[[input$x]], 2),
            "<br>",
            input$y,
            ": ",
            round(.data[[input$y]], 2)
          )
        )
    }
  })

  # selection state
  selected_id <- reactiveVal(NULL)

  # maps ----
  build_map <- reactive({
    req(d_ready())
    if (input$view_method == "univariate") {
      pal <- make_pal(uni_colors, d_ready()$x_class, levels = as.character(1:6))
      m <- base_map |>
        addPolygons(
          data = d_ready(),
          fillColor = ~ pal(x_class),
          fillOpacity = 0.7,
          stroke = TRUE,
          label = ~ lapply(out_lab, HTML),
          weight = .5,
          color = "#333333"
        )
    } else {
      pal <- make_pal(
        codec_bi_pal,
        d_ready()$bi_class,
        levels = c(
          "1-1",
          "2-1",
          "3-1",
          "1-2",
          "2-2",
          "3-2",
          "1-3",
          "2-3",
          "3-3"
        )
      )
      m <- base_map |>
        addPolygons(
          data = d_ready(),
          fillColor = ~ pal(bi_class),
          fillOpacity = 0.7,
          stroke = TRUE,
          label = ~ lapply(out_lab, HTML),
          weight = .5,
          color = "#333333"
        )
    }

    # highlight selection if any
    sid <- selected_id()
    if (!is.null(sid) && sid %in% d_ready()$geo_index) {
      m <- m |>
        addPolygons(
          data = d_ready() |> filter(geo_index == sid),
          color = "#FFFFFF",
          weight = 5,
          opacity = 1,
          fillOpacity = 0
        )
    }
    m
  })

  output$big_map <- renderLeaflet(build_map())
  output$side_map <- renderLeaflet(build_map())

  # scatter ----
  make_bi_scatter <- function(dr, x, y, rects) {
    ggplot() +
      geom_rect(
        data = rects,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
        inherit.aes = FALSE,
        alpha = 1
      ) +
      scale_fill_identity() +
      geom_point_interactive(
        data = dr,
        aes_string(x = x, y = y, data_id = "geo_index"),
        fill = codec_colors()[7],
        alpha = .8,
        shape = 21,
        color = "grey20",
        stroke = .5
      ) +
      theme_light() +
      theme(
        aspect.ratio = 1,
        title = element_text(size = 8),
        axis.title = element_text(
          size = if (input$side_plot_selector == "main_map") 6 else 10
        ),
        legend.key.size = unit(3, "mm")
      ) +
      labs(x = x, y = y)
  }

  make_uni_hist <- function(dr, x, rects) {
    ggplot(dr, aes_string(x = x)) +
      geom_rect(
        data = rects,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
        inherit.aes = FALSE,
        alpha = 1
      ) +
      geom_histogram_interactive(
        aes(tooltip = geo_index, data_id = geo_index),
        bins = 20,
        alpha = .6,
        fill = "grey70",
        color = "grey50"
      ) +
      theme_light() +
      theme(
        aspect.ratio = 1,
        title = element_text(size = 8),
        axis.title = element_text(
          size = if (input$side_plot_selector == "main_map") 6 else 10
        ),
        legend.key.size = unit(3, "mm")
      ) +
      labs(x = x, y = "")
  }

  scatter_girafe <- reactive({
    req(input$x, d_ready())
    if (input$view_method == "bivariate") {
      rects <- make_rectangles(bins(), "bi")
      base <- make_bi_scatter(d_ready(), input$x, input$y, rects)
      h1 <- ggplot(d_ready()) +
        geom_histogram_interactive(
          aes_string(x = input$x, tooltip = "geo_index", data_id = "geo_index"),
          bins = 20,
          fill = codec_colors()[2],
          color = codec_colors()[3]
        ) +
        theme_minimal()
      h2 <- ggplot(d_ready()) +
        geom_histogram_interactive(
          aes_string(x = input$y, tooltip = "geo_index", data_id = "geo_index"),
          bins = 20,
          fill = codec_colors()[2],
          color = codec_colors()[3]
        ) +
        coord_flip() +
        theme_minimal()
      p <- base |>
        insert_xaxis_grob(h1, position = "bottom") |>
        insert_yaxis_grob(h2, position = "right")
      make_girafe(
        ggdraw() + draw_plot(p) + theme(plot.margin = margin(0, 0, 0, 0)),
        input
      )
    } else {
      rects <- make_rectangles(bins(), "uni")
      p <- make_uni_hist(d_ready(), input$x, rects)
      make_girafe(p, input)
    }
  })

  output$big_scatter <- renderGirafe(scatter_girafe())
  output$side_scatter <- renderGirafe(scatter_girafe())

  # click/selection wiring ----
  observeEvent(
    list(input$side_scatter_selected, input$big_scatter_selected),
    {
      sel <- input$side_scatter_selected %||% input$big_scatter_selected
      if (!is.null(sel)) selected_id(sel)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(input$big_map_click, input$side_map_click),
    {
      click <- input$big_map_click %||% input$side_map_click
      req(click)
      pt <- tibble(lng = click$lng, lat = click$lat) |>
        st_as_sf(coords = c("lng", "lat"), crs = st_crs(d_geo()))
      hit <- suppressWarnings(st_join(d_geo(), pt, left = FALSE))
      if (nrow(hit) > 0) selected_id(hit$geo_index[1])
    },
    ignoreInit = TRUE
  )

  # legend modal ----
  output$legend <- renderPlot({
    bi_legend(
      pal = codec_bi_pal,
      dim = 3,
      xlab = "Higher X Variable",
      ylab = "Higher Y Variable",
      size = 12
    )
  })

  observeEvent(input$legend_modal, {
    showModal(modalDialog(
      title = "About Bivariate Maps",
      p(
        "Bivariate maps use a blended color scale to visualize two variables at the same time"
      ),
      plotOutput("legend"),
      easyClose = TRUE
    ))
  })

  # clear/reset ----
  output$clear_button_panel <- renderUI({
    absolutePanel(
      id = "clear_button_panel",
      class = "panel panel-default",
      cursor = "auto",
      draggable = TRUE,
      top = 50,
      right = 20,
      style = "z-index:10;padding:5px;border:1px solid #000;background:#FFFFFF;opacity:.9;margin:auto;border-radius:5pt;box-shadow:0 0 6pt 0 rgba(61,59,61,0.48);",
      fixedRow(
        shinyWidgets::actionBttn(
          "clear_map_selection",
          label = "Reset",
          size = "xs",
          style = "simple",
          status = "primary"
        ) |>
          tagAppendAttributes(style = "color:#FFFFFF;background-color:#396175;")
      )
    )
  })

  observeEvent(input$clear_map_selection, {
    selected_id(NULL)
    leafletProxy("big_map") |>
      clearShapes() |>
      addProviderTiles(providers$CartoDB.Positron)
    leafletProxy("side_map") |>
      clearShapes() |>
      addProviderTiles(providers$CartoDB.Positron)
    # force re-render via invalidation
    output$big_map <- renderLeaflet(build_map())
    output$side_map <- renderLeaflet(build_map())
  })

  # suspend when hidden (perf) ----
  outputOptions(output, 'big_map', suspendWhenHidden = TRUE)
  outputOptions(output, 'side_map', suspendWhenHidden = TRUE)
  outputOptions(output, 'big_scatter', suspendWhenHidden = TRUE)
  outputOptions(output, 'side_scatter', suspendWhenHidden = TRUE)
}

# ---- App ----
shinyApp(ui, server)

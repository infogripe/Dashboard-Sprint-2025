library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(plotly)
library(rio)
library(MetBrewer)
library(leaflet)
library(sf)


# Auxiliary functions -------------------------------------------------------------------------

# Plotly for UF -------------------------------------------------------------------------------

plot_grafico_sprint_uf <- function(df, UF, palette) {
  
  quants <- rev(unique(df$quantile))
  colors <- palette[seq_along(quants)]
  
  max_valor <- df |> filter(uf == UF, is.finite(df$maxvalues)) |> select(maxvalues, cases) |> max(na.rm = TRUE)
  df_max_round <- ceiling(max_valor / 50) * 50
  df_max_round2 <- df_max_round + (df_max_round * 0.1)
  df2 <- df |> filter(uf == UF) |> mutate(maxvalues = ifelse(is.infinite(maxvalues), df_max_round2, maxvalues))
  
  fig <- plot_ly()
  for(i in seq_along(quants)) {
    fig <- fig |> 
      add_trace(
        data = df2 |> filter(quantile == quants[i]),
        type = 'scatter', 
        mode = 'lines',
        x = ~date, 
        y = ~maxvalues,
        text = ~message,
        fill = 'tozeroy',
        line = list(color = colors[i]),
        fillcolor = colors[i],
        name = quants[i],
        hovertemplate = paste0("<b>", quants[i],"</b>: %{text}<extra></extra>")
      )
  }
  
  fig |>
    add_trace(
      data = df2,
      type = 'scatter', mode = 'lines',
      x = ~date, y = ~cases,
      line = list(color = "black"),
      name = "Cases",
      hovertemplate = "<b>Cases</b>: %{y:,.0f}<extra></extra>"
    ) |>
    layout(
      hovermode = "x unified",
      xaxis = list(title = "Date", showgrid = FALSE, zeroline = FALSE),
      yaxis = list(title = "Dengue cases", showgrid = FALSE, zeroline = FALSE),
      legend = list(itemclick = FALSE, itemdoubleclick = FALSE)
    )
}

plot_grafico_sprint_uf_forecast <- function(df, UF, palette) {
  
  quants <- rev(unique(df$quantile))
  colors <- palette[seq_along(quants)]
  
  max_valor <- df |> filter(uf == UF, is.finite(df$maxvalues)) |> select(maxvalues, cases) |> max(na.rm = TRUE)
  df_max_round <- ceiling(max_valor / 50) * 50
  df_max_round2 <- df_max_round + (df_max_round * 0.1)
  df2 <- df |> filter(uf == UF) |> mutate(maxvalues = ifelse(is.infinite(maxvalues), df_max_round2, maxvalues))
  
  fig <- plot_ly()
  for(i in seq_along(quants)) {
    fig <- fig |> 
      add_trace(
        data = df2 |> filter(quantile == quants[i]),
        type = 'scatter', 
        mode = 'lines',
        x = ~date, 
        y = ~maxvalues,
        text = ~message,
        fill = 'tozeroy',
        line = list(color = colors[i]),
        fillcolor = colors[i],
        name = quants[i],
        hovertemplate = paste0("<b>", quants[i],"</b>: %{text}<extra></extra>")
      )
  }
  
  fig |>
    add_trace(
      data = df2,
      type = 'scatter', mode = 'lines',
      x = ~date, y = ~cases,
      line = list(color = "black"),
      name = "Predicted cases",
      hovertemplate = "<b>Predicted cases</b>: %{y:,.0f}<extra></extra>"
    ) |>
    layout(
      hovermode = "x unified",
      xaxis = list(title = "Date", showgrid = FALSE, zeroline = FALSE),
      yaxis = list(title = "Dengue cases (forecast)", showgrid = FALSE, zeroline = FALSE),
      legend = list(itemclick = FALSE, itemdoubleclick = FALSE)
    )
}



# Hover message for plotly --------------------------------------------------------------------

message_hover <- function(df){
  df |> 
    mutate(
      message = case_when(
        quantile == "Below the median,\ntypical" ~ paste0("<=", format(round(maxvalues), 
                                                                      nsmall=0, 
                                                                      big.mark=",")),
        quantile == "Moderately high,\nfairly typical" ~ paste0(">", format(round(minvalues), 
                                                                             nsmall=0, 
                                                                             big.mark=","),
                                                                " and <=", format(round(maxvalues), 
                                                                                 nsmall=0, 
                                                                                 big.mark=",")),
        quantile == "Fairly high,\natypical" ~ paste0(">", format(round(minvalues), 
                                                                   nsmall=0, 
                                                                   big.mark=","),
                                                      " and <=", format(round(maxvalues), 
                                                                       nsmall=0, 
                                                                       big.mark=",")),
        quantile == "Exceptionally high,\nvery atypical" ~ paste0(">", format(round(minvalues), 
                                                                               nsmall=0, 
                                                                               big.mark=","))
      )
    )
}


# Reading data --------------------------------------------------------------------------------

observed <- import("forecasts/tbl.sprint.uf.week.forecast.csv")
df.prob.22_23 <- import(file = "forecasts/tbl.sprint.uf.week.train1.csv")
df.prob.23_24 <- import(file = "forecasts/tbl.sprint.uf.week.train2.csv")
df.prob.24_25 <- import(file = "forecasts/tbl.sprint.uf.week.train3.csv")
shape <- read_sf("data/shape_macro_simp.gpkg")
ufshape <- read_sf("data/shape_uf_simp.gpkg")
uf_labels <- readRDS("data/uf_labels.rds")

weeks.22_23 <- df.prob.22_23 |> select(date, week) |> unique() |> mutate(date = as.Date(date)) |> as_tibble()
weeks.23_24 <- df.prob.23_24 |> select(date, week) |> unique() |> mutate(date = as.Date(date)) |> as_tibble()
weeks.24_25 <- df.prob.24_25 |> select(date, week) |> unique() |> mutate(date = as.Date(date)) |> as_tibble()

weeks <- weeks.22_23 |> 
  rbind(weeks.23_24, weeks.24_25)

observed <- import("data/dengue.csv.gz") |> 
  filter(
    date >= "2022-10-09"
  ) |> 
  group_by(
    uf, macroregional_geocode, date, epiweek
  ) |> 
  summarise(
    cases = sum(casos)
  ) |> 
  mutate(
    date = as.Date(date),
    year.s.first = lubridate::year(date)
  ) |> 
  ungroup() |> 
  left_join(
    weeks
  )


# Datasets for Brazil and UFs -----------------------------------------------------------------

# modifications to the dataset structure so we can reuse Freitas et al. 2025 code

# t1br <- import("data/t1br.csv.gz") |> 
#   message_hover()
# t2br <- import("data/t2br.csv.gz") |> 
#   message_hover()
# t3br <- import("data/t3br.csv.gz") |> 
#   message_hover()

t1uf <- import("forecasts/tbl.sprint.uf.week.train1.csv") |> 
  select(-starts_with("lower")) |> 
  pivot_longer(cols = c(starts_with("upper")), names_to = "quantile", values_to = "maxvalues") |> 
  group_by(week) |> 
  mutate(
    quantile = case_when(
      quantile == "upper_50" ~ "Abaixo da mediana,\ntípico",
      quantile == "upper_80" ~ "Moderadamente alto,\nrazoavelmente típico",
      quantile == "upper_90" ~ "Relativamente alto,\natípico",
      quantile == "upper_95" ~ "Excepcionalmente alto,\nmuito atípico"
    ),
    minvalues = lag(maxvalues),
    minvalues = replace_na(minvalues, 0),
    season = "2022-2023",
    epiweek = lubridate::epiweek(date),
    epiyear = lubridate::year(date),
    year.s.first = min(epiyear)
  ) |> 
  relocate(
    c(date, cases, epiweek, epiyear, pred), .after = last_col()
  ) |> 
  relocate(week) |> 
  as_tibble() |> 
  message_hover()

t2uf <- import("forecasts/tbl.sprint.uf.week.train2.csv") |> 
  select(-starts_with("lower")) |> 
  pivot_longer(cols = c(starts_with("upper")), names_to = "quantile", values_to = "maxvalues") |> 
  group_by(week) |> 
  mutate(
    quantile = case_when(
      quantile == "upper_50" ~ "Abaixo da mediana,\ntípico",
      quantile == "upper_80" ~ "Moderadamente alto,\nrazoavelmente típico",
      quantile == "upper_90" ~ "Relativamente alto,\natípico",
      quantile == "upper_95" ~ "Excepcionalmente alto,\nmuito atípico"
    ),
    minvalues = lag(maxvalues),
    minvalues = replace_na(minvalues, 0),
    season = "2023-2024",
    epiweek = lubridate::epiweek(date),
    epiyear = lubridate::year(date),
    year.s.first = min(epiyear)
  ) |> 
  relocate(
    c(date, cases, epiweek, epiyear, pred), .after = last_col()
  ) |> 
  relocate(week) |> 
  as_tibble() |> 
  message_hover()

t3uf <- import("forecasts/tbl.sprint.uf.week.train3.csv") |> 
  select(-starts_with("lower")) |> 
  pivot_longer(cols = c(starts_with("upper")), names_to = "quantile", values_to = "maxvalues") |> 
  group_by(week) |> 
  mutate(
    quantile = case_when(
      quantile == "upper_50" ~ "Abaixo da mediana,\ntípico",
      quantile == "upper_80" ~ "Moderadamente alto,\nrazoavelmente típico",
      quantile == "upper_90" ~ "Relativamente alto,\natípico",
      quantile == "upper_95" ~ "Excepcionalmente alto,\nmuito atípico"
    ),
    minvalues = lag(maxvalues),
    minvalues = replace_na(minvalues, 0),
    season = "2024-2025",
    epiweek = lubridate::epiweek(date),
    epiyear = lubridate::year(date),
    year.s.first = min(epiyear)
  ) |> 
  relocate(
    c(date, cases, epiweek, epiyear, pred), .after = last_col()
  ) |> 
  relocate(week) |> 
  as_tibble() |> 
  message_hover()

forecast_uf <- import("forecasts/tbl.sprint_update.uf.week.forecast.csv") |> 
  select(-starts_with("lower")) |> 
  pivot_longer(cols = c(starts_with("upper")), names_to = "quantile", values_to = "maxvalues") |> 
  group_by(week) |> 
  mutate(
    quantile = case_when(
      quantile == "upper_50" ~ "Abaixo da mediana,\ntípico",
      quantile == "upper_80" ~ "Moderadamente alto,\nrazoavelmente típico",
      quantile == "upper_90" ~ "Relativamente alto,\natípico",
      quantile == "upper_95" ~ "Excepcionalmente alto,\nmuito atípico"
    ),
    minvalues = lag(maxvalues),
    minvalues = replace_na(minvalues, 0),
    season = "2025-2026",
    epiweek = lubridate::epiweek(date),
    epiyear = lubridate::year(date),
    year.s.first = min(epiyear),
    cases = pred
  ) |> 
  relocate(
    c(date, cases, epiweek, epiyear, pred), .after = last_col()
  ) |> 
  relocate(week) |> 
  as_tibble()



# Color palettes ------------------------------------------------------------------------------

region_colors <- c(
  "North" = "#3CAACF",
  "Northeast" = "#D4EC88",
  "Mid-West" = "#66C07E",
  "Southeast" = "#A383D9",
  "South" = "#DC8E4B"
)

pal <- colorFactor(palette = region_colors, domain = shape$region_en)
pal3 <- c("#EE9188","#F4AD7F","#F9D894","#C4E7E9")


# UI ------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    # garante que o dropdown do input select fique por cima do controle de zoom do leaflet
    tags$style(HTML("
      .selectize-dropdown {
        z-index: 10000 !important;
      }
    "))
  ),
  
  titlePanel("Sprint 2025 - Beat It model"),
  
  tabsetPanel(
    # 1) Publication
    tabPanel("Detalhes",
             tags$h3("Sprint 2025 - Beat It model"),
             tags$p(
               "Authors: ",
               "Leonardo Soares Bastosa", tags$sup("1"), ", ",
               "Laís Picinini Freitas", tags$sup("2")
             ),
             tags$p(tags$sup("1"), "Programa de Computação Científica - PROCC, Fundação Oswaldo Cruz, Rio de Janeiro, RJ, Brazil"),
             tags$p(tags$sup("2"), "Escola Nacional de Saúde Pública - ENSP, Fundação Oswaldo Cruz, Rio de Janeiro, RJ, Brazil"),
             
             tags$h4("Libraries and Dependencies"),
             tags$p(
               "The model is implemented in R using the tidyverse for data preparation and INLA for inference."
             ),
             
             tags$h4("Data and Variables"),
             tags$p(
               "This model depends on case counts only."
             ),
             
             tags$h4("How was the data pre-processed?"),
             tags$p(
               "The data is aggregated by macrorregion, and for each macrorregion forecasts were performed. We considered cases from 2015 onwards, ignoring data previous to 2015 because of dengue expansion and introduction to other arboviroses."
             ),

             tags$h4("How were the variables selected?"),
             tags$p(
               "There was no variable selection."
             ),
             
             tags$h4("Model Training"),
             tags$p(
               "It is a negative binomial with year indpendent Gaussian random effects and a cyclic second order random walk weekly random effects. The posterior distribution for all parameters were estimated for the training dataset and, the numbe rof cases for the target dataset were estimated based on the posterior predictive distribution. Samples from this distribution were taken by macroregion and for the three target sets."
             ),
             tags$p(
               "The intervals were computed based on quantiles from samples of the posterior predictive distribution using a Monte Carlo approach."
             ),
    ),
    
    # 2) Federal Units (states)
    tabPanel("Federal Units (states)",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput(
                   inputId = "uf",
                   label = "Choose the Federation Unit:",
                   choices = sort(unique(observed$uf)),
                   selected = sort(unique(observed$uf))[1]
                 ),
                 leafletOutput("mapInsertUF", width = "100%", height = "300px")
               ),
               mainPanel(
                 width = 9,
                 h3("Temporada 2022 - 2023"),
                 plotlyOutput("plotUF1", width = "100%", height = "600px"),
                 h3("Temporada 2023 - 2024"),
                 plotlyOutput("plotUF2", width = "100%", height = "600px"),
                 h3("Temporada 2024 - 2025"),
                 h5("Até semana epidemiológica 22 (25/05/2025)"),
                 plotlyOutput("plotUF3", width = "100%", height = "600px"),
                 h3("Forecasting 2025 - 2026"),
                 h5("A partir da semana epidemiológica 41 (05/10/2025)"),
                 plotlyOutput("plotUF4", width = "100%", height = "600px")
               )
             )
    ),
    
  ), 
  
  tags$hr(),
  tags$footer(
    style = "width:100%;text-align:center;font-size:0.8em;color:#666;padding:5px 0;",
    "Based on Picinini Freitas, L. et al (2025). A statistical model for forecasting probabilistic epidemic bands for dengue cases in Brazil. ",
    tags$a(
      href = "https://doi.org/10.1016/j.idm.2025.07.014",
      target = "_blank",
      "https://doi.org/10.1016/j.idm.2025.07.014"
    )
  )
)



# SERVER --------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # 1) UF
  output$plotUF1 <- renderPlotly({
    uf <- input$uf
    plot_grafico_sprint_uf(t1uf, uf, pal3)
  })
  
  output$plotUF2 <- renderPlotly({
    uf <- input$uf
    plot_grafico_sprint_uf(t2uf, uf, pal3)
  })
  
  output$plotUF3 <- renderPlotly({
    uf <- input$uf
    plot_grafico_sprint_uf(t3uf, uf, pal3)
  })
  
  output$plotUF4 <- renderPlotly({
    uf <- input$uf
    plot_grafico_sprint_uf_forecast(forecast_uf, uf, pal3)
  })
  
  output$mapInsertUF <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(data = shape,
                  color = "white",
                  weight = 1,
                  fillColor = ~pal(region_en),
                  fillOpacity = 0.7,
                  label = ~paste("Health district:", macroregional, "/", uf_name)
      ) |>
      addPolygons(data = ufshape,
                  fill = FALSE,
                  color = "black",
                  weight = 2
      ) |>
      addLabelOnlyMarkers(data = uf_labels,
                          lng = ~X, lat = ~Y,
                          label = ~abbrev_state,
                          labelOptions = labelOptions(
                            noHide = TRUE,
                            direction = "center",
                            textOnly = TRUE,
                            style = list(
                              "color" = "black",
                              "font-size" = "12px",
                              "font-weight" = "bold"
                            )
                          ))
  })
  
}


# RUN APP -------------------------------------------------------------------------------------

shinyApp(ui, server)

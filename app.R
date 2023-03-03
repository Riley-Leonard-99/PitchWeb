# Load Packages

library(shiny)
library(tidyverse)
library(fmsb)
library(plotly)
library(ggridges)
library(shinythemes)
library(kableExtra)
library(glue)
library(shinycssloaders)
library(DT)


# Load Static Dataframes

# setwd("/Users/rileyleonard99/Documents/Baseball/PitchWeb")

WebData <- read_csv("Data/WebDataFinal.csv")
WebSummaryData <- read_csv("Data/WebSummaryData.csv")
PitchScore <- read_csv("Data/PitchScoreFull.csv")
ArsenalScore <- read_csv("Data/ArsenalDataFull.csv")
Names <- read_csv("Data/Names.csv")
Metrics <- read_csv("Data/Metrics.csv")



# Static Wrangling

WebData <- WebData %>%
  mutate(`BB %` = 100 - `BB %`,
         `Vertical Break` = 100 - `Vertical Break`,
         `Horizontal Break` = round(100 * percent_rank(abs(horz_break_vs_avg)), digits = 0))

ArsenalScore <- ArsenalScore %>%
  mutate(`Arsenal Score` = round(100*percent_rank(`Arsenal Score`), digits = 0))

PitchScore <- PitchScore %>%
  rename(`Pitch %` = `Pitch Percent`) %>%
  mutate(`Pitch %` = format(`Pitch %`, nsmall = 1))


LeagueAverages <- WebSummaryData %>%
  group_by(Type) %>%
  summarize(Velocity = format(round(mean(Velocity), digits = 1), nsmall = 1),
            `Spin Rate` = format(round(mean(`Spin Rate`), digits = 0), nsmall = 0),
            BA = format(round(mean(BA), digits = 3), nsmall = 3),
            xBA = format(round(mean(xBA), digits = 3), nsmall = 3),
            ISO = format(round(mean(ISO), digits = 3), nsmall = 3),
            BABIP = format(round(mean(BABIP), digits = 3), nsmall = 3),
            SLG = format(round(mean(SLG), digits = 3), nsmall = 3),
            wOBA = format(round(mean(wOBA), digits = 3), nsmall = 3),
            xwOBA = format(round(mean(xwOBA), digits = 3), nsmall = 3),
            `Swing %` = format(round(100*mean(`Swing %`), digits = 1), nsmall = 1),
            `Whiff %` = format(round(100*mean(`Whiff %`), digits = 1), nsmall = 1),
            `K %` = format(round(mean(`K %`), digits = 1), nsmall = 1),
            `BB %` = format(round(mean(`BB %`), digits = 1), nsmall = 1),
            `Vertical Break` = format(round(mean(`Vertical Break`), digits = 1), nsmall = 1),
            `Horizontal Break` = format(round(mean(`Horizontal Break`), digits = 1), nsmall = 1))


WebSummaryData <- WebSummaryData %>%
  mutate(`Pitch %` = format(`Pitch %`, nsmall = 1),
         Velocity = format(Velocity, nsmall = 1),
         BA = format(BA, nsmall = 3),
         xBA = format(xBA, nsmall = 3),
         ISO = format(ISO, nsmall = 3),
         BABIP = format(BABIP, nsmall = 3),
         SLG = format(SLG, nsmall = 3),
         wOBA = format(wOBA, nsmall = 3),
         xwOBA = format(xwOBA, nsmall = 3),
         `Swing %` = format(round(100*`Swing %`, digits = 1), nsmall = 1),
         `Whiff %` = format(round(100*`Whiff %`, digits = 1), nsmall = 1),
         `K %` = format(`K %`, nsmall = 1),
         `BB %` = format(`BB %`, nsmall = 1),
         `Vertical Break` = format(`Vertical Break`, nsmall = 1),
         `Horizontal Break` = format(`Horizontal Break`, nsmall = 1))


PercentileData <- WebData %>%
  rename(Type = type,
         Pitches = pitches,
         `Pitch %` = pitch_percent,
         `Swing %` = `Swing Rate`) %>%
  select(full_name, Type, Pitches, `Pitch %`, Velocity, `Spin Rate`, BA, xBA,
         ISO, BABIP, SLG, wOBA, xwOBA, `Swing %`, `Whiff %`, `K %`, `BB %`,
         `Vertical Break`, `Horizontal Break`)



# Define UI

ui <- navbarPage(theme = shinytheme("paper"),
                  title = "PitchWeb",
                  tabPanel("Web Plot",
                           sidebarPanel(
                             selectizeInput(inputId = "playerWeb",
                                            choices = Names$full_name,
                                            label = "Select Player:",
                                            selected = "Sandy Alcantara"),
                             checkboxGroupInput("type_selection", "Filter by Pitch Type:",
                                                choices = list("4-Seam Fastball",
                                                               "Changeup",
                                                               "Curveball",
                                                               "Cutter",
                                                               "Eephus",
                                                               "Knuckle Curve",
                                                               "Other",
                                                               "Sinker",
                                                               "Slider",
                                                               "Split-Finger",
                                                               "Sweeper"),
                                                selected = c("4-Seam Fastball",
                                                             "Changeup",
                                                             "Curveball",
                                                             "Cutter",
                                                             "Eephus",
                                                             "Knuckle Curve",
                                                             "Other",
                                                             "Sinker",
                                                             "Slider",
                                                             "Split-Finger",
                                                             "Sweeper")),
                             p("Note: Selected pitch types will only generate a plot layer if featured in the pitch arsenal of the selected pitcher."),
                             submitButton("Generate Plot"),
                             width = 3
        ),
        mainPanel(
          plotOutput(outputId = "web_chart", height = 580, width = 650) %>% withSpinner(color="#0894fc"),
          p("Note: The above visualization plots the percentile values of each metric. Exact measures can be found below."),
          dataTableOutput(outputId = "web_summary_table"),
          HTML("<br><br>"))
        ),
        tabPanel("Pitch Score",
                 sidebarPanel(
                   selectizeInput(inputId = "playerScore",
                                  choices = Names$full_name,
                                  label = "Select Player:",
                                  selected = "Shohei Ohtani"),
                   submitButton("Generate Plot"),
                   width = 3
                   ),
                 mainPanel(
                   plotOutput(outputId = "score_chart", height = 580, width = 650) %>% withSpinner(color="#0894fc"),
                   p("Note: The above visualization plots the Pitch Score of each pitch in the arsenal of the selected pitcher, with the dashed line representing the league average. The exact formula for the Pitch Score calculation can be found",
                     tags$a("here.", href = "https://drive.google.com/file/d/1wrVJy_clddTIuZkgX6uRRENnzNdBumyV/view?usp=sharing", 
                            target = "_blank")),
                   dataTableOutput(outputId = "score_summary_table"),
                   HTML("<br><br>"))
                 ),
        tabPanel("Arsenal Map",
                 sidebarPanel(
                 selectizeInput(inputId = "playerMap",
                                choices = Names$full_name,
                                label = "Select Player:",
                                selected = "Triston McKenzie"),
                 submitButton("Locate Player"),
                 width = 3
                 ),
                 mainPanel(
                   plotlyOutput(outputId = "arsenal_map", height = 650, width = 650) %>% withSpinner(color="#0894fc"))
                 ),
        tabPanel("Density Plot",
                 sidebarPanel(
                   selectizeInput(inputId = "playerDensity",
                                  choices = Names$full_name,
                                  label = "Select Player:",
                                  selected = "Justin Verlander"),
                   radioButtons("density_selection", "Select Metric:",
                                      choices = Metrics$MetricsNames,
                                      selected = "Velocity"),
                   submitButton("Generate Plot"),
                   width = 3
                 ),
                 mainPanel(
                   plotOutput(outputId = "density_plot", height = 500, width = 650) %>% withSpinner(color="#0894fc"),
                   HTML("<br>"),
                   p("Note: The above visualization plots the league-wide distribution of the selected metric for each pitch in the selected player's pitch arsenal, with the vertical line representing the (unweighted) league average. Triangles represent the metric value of the given pitch for the selected player."),
                   dataTableOutput(outputId = "ridge_summary_table"),
                   HTML("<br><br>"))
        ),
        tabPanel("Developers & Sources",
                 tags$p("PitchWeb was created by",
                        tags$a("Riley Leonard.", href = "https://www.linkedin.com/in/riley-leonard-9653791a6/", target = "_blank")),
                 tags$p(
                   "Data was sourced from",
                   tags$a(" MLB Statcast,", href="https://baseballsavant.mlb.com/statcast_search", target = "_blank"),
                   "and the application was programmed in R with tools from the",
                   tags$code("fmsb"),
                   ", ",
                   tags$code("shiny"),
                   ", and",
                   tags$code("tidyverse"),
                   "packages."),
                 tags$p("This application was last updated in March 2023.")
        ),
        tags$head(tags$style(HTML('* {font-family: "Helvetica Neue"};')))
    )


server <- function(input, output) {
  
  # Create Web Plot
  
    output$web_chart <- renderPlot({
      
      validate(
        need(input$type_selection != "", "Please select at least one pitch.")
      )
      
      pretypes <- WebData %>% 
        filter(full_name == input$playerWeb) %>%
        filter(type %in% input$type_selection) %>%
        select(type)
      
      validate(
        need(nrow(pretypes) != 0,
             "Sorry! The pitcher that you have selected does not have any pitches of the given specifications, according to our data. Please adjust your filters or select a different pitcher.")
      )
      
      types <- pretypes$type
      
      web_pitch_data <- WebData %>% 
        filter(full_name == input$playerWeb) %>%
        filter(type %in% input$type_selection) %>%
        select(BA, xBA, ISO, Velocity, `Spin Rate`, `Vertical Break`, 
               `Horizontal Break`, `Whiff %`, `K %`, `BB %`)
      
      web_chart_data <- rbind(rep(100, ncol(web_pitch_data)) , rep(0, ncol(web_pitch_data)) , web_pitch_data)
      
      op <- par(family = "Helvetica",
                font.main = 1,
                cex.main = 1.4)
      
      radarchart(web_chart_data,
                 axistype = 1,
                 axislabcol = "grey75", 
                 caxislabels = seq(0, 100, 20), 
                 calcex = 0.9,
                 seg = 5,
                 plty = 1,
                 plwd = 3,
                 pcol = c(2, 3, 4, 5, 6, 7, "#FB61D7"),
                 pfcol = alpha(c(2, 3, 4, 5, 6, 7, "#FB61D7"), 0.25),
                 cglty = 1,
                 cglcol = "grey75",
                 cglwd = 1,
                 vlcex = 1,
                 title = glue("{input$playerWeb} Pitch Arsenal (2022)"))
        legend(x = 0.9, y = 1.3, 
               legend = c(glue("{types}")), 
               bty = "n", pch = 20, 
               col = alpha(c(2, 3, 4, 5, 6, 7, "#FB61D7"), 0.25), 
               text.col = "grey50", 
               cex = 1, 
               pt.cex = 2)
        par(op)
        
    })
    
    # Create Web Table
    
    output$web_summary_table <- renderDataTable({
      datatable(WebSummaryData %>%
                  filter(full_name == input$playerWeb) %>%
                  select(-full_name) %>%
                  arrange(desc(Pitches)),
                options = list(dom = 't', scrollX = T))
    })
    
    
    
    # Create Score Plot
    
    output$score_chart <- renderPlot({
      
      ScoreData1 <- PitchScore %>%
        filter(full_name == input$playerScore) %>%
        select(`Pitch Type`, `Pitch Score`) %>%
        pivot_wider(names_from = `Pitch Type`, values_from = `Pitch Score`)
      
      validate(
        need(ncol(ScoreData1) > 2,
             "Sorry! The pitcher that you have selected has fewer than three unique pitches, according to our data. Please select a different pitcher.")
      )
      
      ScoreData2 <- rbind(rep(50, ncol(ScoreData1)), ScoreData1)
      ScoreData3 <- rbind(rep(100, ncol(ScoreData1)), rep(0, ncol(ScoreData1)) , ScoreData2)
      
      op <- par(family = "Helvetica",
                font.main = 1,
                cex.main = 1.4)
      
      radarchart(ScoreData3,
                 axistype = 1,
                 axislabcol = "grey75", 
                 caxislabels = seq(0, 100, 20), 
                 pty = c(32, 16),
                 calcex = 0.9,
                 seg = 5,
                 plty = c(2, 1),
                 plwd = 3,
                 pcol = c(alpha(1, 0.4), 4),
                 pfcol = c(alpha(1, 0.1), alpha(4, 0.25)),
                 cglty = 1,
                 cglcol = "grey75",
                 cglwd = 1,
                 vlcex = 1,
                 title = glue("{input$playerScore} Pitch Arsenal (2022)")) 
      
      par(op)
      
    })
    
    
    # Create Score Table
    
    output$score_summary_table <- renderDataTable({
      datatable(PitchScore %>%
                  filter(full_name == input$playerScore) %>%
                  select(`Pitch Type`, Pitches, `Pitch %`, 
                         `Prevention Score`, `Whiff Score`, `Pitch Score`) %>%
                  arrange(desc(Pitches)),
                options = list(dom = 't'))
    })
    
    # Create Arsenal Map
    
    output$arsenal_map <- renderPlotly({
      
      PlayerScore <- ArsenalScore %>%
        filter(full_name == input$playerMap)
      
      fig <- ggplot() +
        scale_x_continuous(name = "Prevention Score",
                           breaks = seq(0, 100, 25),
                           limits =c(0, 100)) +
        scale_y_continuous(name = "Whiff Score",
                           breaks = seq(0, 100, 25),
                           limits =c(0, 100)) +
        geom_rect(aes(xmin = 50, ymin = 50, xmax = 100, ymax = 100), 
                  fill = "#0894fc", alpha = 0.2) +
        geom_rect(aes(xmin = 0, ymin = 0, xmax = 50, ymax = 50), 
                  fill = "palevioletred2", alpha = 0.2) +
        geom_point(data = ArsenalScore, 
                   aes(x = `Prevention Score`, 
                       y = `Whiff Score`,
                       size = Pitches,
                       text =  paste('Player: ', full_name, "\n",
                                     'Pitches: ', Pitches, "\n",
                                     'Prevention Score: ', `Prevention Score`, "\n",
                                     'Whiff Score: ', `Whiff Score`, "\n",
                                     'Arsenal Score: ', `Arsenal Score`, "\n",
                                     sep = "")),
                   shape = 21,
                   colour = "#0894fc", 
                   fill = "grey100") +
        geom_point(data = PlayerScore, 
                   aes(x = `Prevention Score`, 
                       y = `Whiff Score`,
                       size = Pitches,
                       text =  paste('Player: ', full_name, "\n",
                                     'Pitches: ', Pitches, "\n",
                                     'Prevention Score: ', `Prevention Score`, "\n",
                                     'Whiff Score: ', `Whiff Score`, "\n",
                                     'Arsenal Score: ', `Arsenal Score`, "\n",
                                     sep = "")),
                   shape = 21,
                   colour = "#0894fc", 
                   fill = "#0894fc") +
        theme_minimal() +
        theme(plot.margin = margin(0, 0, 1, 0, "cm"))
      
      font <- list(
        family = "Helvetica Neue",
        size = 14,
        color = 'gray0')
      
      ggplotly(fig, dynamicTicks = TRUE, tooltip = 'text')  %>%
        layout(dragmode = 'pan',
               font = font)
    })
    
    
    # Create Density Plot
    
    output$density_plot <- renderPlot({
      
      RidgeTypesData <- WebSummaryData %>%
        filter(full_name == input$playerDensity)
      
      RidgeTypes <- RidgeTypesData$Type
      
      RidgeData <- WebSummaryData %>%
        filter(Type %in% RidgeTypes)
      
      Variable <- input$density_selection
      
      ggplot(data = RidgeData, aes(x = as.numeric(.data[[Variable]]), y = Type)) +
        geom_density_ridges(aes(fill = Type, color = Type), 
                            quantile_lines = TRUE,
                            quantiles = 2,
                            show.legend = FALSE,
                            size = 1,
                            scale = 1.5,
                            alpha = 0.2) +
        geom_point(data = RidgeTypesData, 
                   aes(x = as.numeric(.data[[Variable]]), y = Type, color = Type),
                   position = position_nudge(y = case_when(length(RidgeTypes) == 1 ~ -0.03,
                                                           length(RidgeTypes) == 2 ~ -0.06,
                                                           length(RidgeTypes) == 3 ~ -0.09,
                                                           length(RidgeTypes) == 4 ~ -0.11, 
                                                           length(RidgeTypes) == 5 ~ -0.13,
                                                           length(RidgeTypes) == 6 ~ -0.16,
                                                           length(RidgeTypes) == 7 ~ -0.18)),
                   show.legend = FALSE,
                   shape = 17,
                   size = 6) +
        xlab(glue("{input$density_selection}")) +
        theme_minimal() +
        theme(text = element_text(family = "Helvetica"),
              axis.title = element_text(size = 12))
      
      
    })
    
    # Create Ridge Table
    
    output$ridge_summary_table <- renderDataTable({
      
      RidgeTypesData <- WebSummaryData %>%
        filter(full_name == input$playerDensity)
      
      RidgeTypes <- RidgeTypesData$Type
      
      RidgeData <- WebSummaryData %>%
        filter(Type %in% RidgeTypes)
      
      Variable <- input$density_selection
      
      RidgeSummaryData <- RidgeTypesData %>%
        select(Type, Pitches, `Pitch %`, .data[[Variable]])
      
      LeagueAverageData <- LeagueAverages %>%
        select(Type, .data[[Variable]]) %>%
        rename(`League Average` = 2)
      
      RidgePercentileData <- WebData %>%
        filter(full_name == input$playerDensity) %>%
        select(type, .data[[Variable]]) %>%
        rename(Type = type,
               `Percentile (Overall)` = 2)
      
      RidgeTableData <- left_join(RidgeSummaryData, LeagueAverageData, by = "Type")
      
      RidgeTableData <- RidgeTableData %>%
        mutate(`League Average` = as.numeric(`League Average`),
               Difference = as.numeric(.data[[Variable]]) - `League Average`)
      
      RidgeTableData <- left_join(RidgeTableData, RidgePercentileData, by = "Type")
      
      onedigit <- c("Velocity", "Swing %", "Whiff %", "K %", "BB %", "Horizontal Break", "Vertical Break")
      
      threedigits <- c("BA", "xBA", "ISO", "BABIP", "SLG", "wOBA", "xwOBA")
      
      RidgeTableData <- RidgeTableData %>%
        mutate(Difference = format(round(Difference, digits = case_when(Variable == "Spin Rate" ~ 0,
                                                                        Variable %in% onedigit ~ 1,
                                                                        Variable %in% threedigits ~ 3)), 
                                   nsmall = case_when(Variable == "Spin Rate" ~ 0, 
                                                      Variable %in% onedigit ~ 1,
                                                      Variable %in% threedigits ~ 3)))
      
      RidgeTableData <- RidgeTableData %>%
        mutate(`League Average` = format(round(`League Average`, digits = case_when(Variable == "Spin Rate" ~ 0,
                                                                        Variable %in% onedigit ~ 1,
                                                                        Variable %in% threedigits ~ 3)), 
                                   nsmall = case_when(Variable == "Spin Rate" ~ 0, 
                                                      Variable %in% onedigit ~ 1,
                                                      Variable %in% threedigits ~ 3)))
      
      RidgeTableData <- RidgeTableData %>%
        mutate(`League Average` = as.character(`League Average`))
      
      datatable(RidgeTableData %>%
                  arrange(desc(Type)),
                options = list(dom = 't'))
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

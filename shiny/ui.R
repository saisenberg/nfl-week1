ui <- fluidPage(
  
  titlePanel(title = h4('NFL Fanbase Sentiment (2018)'), windowTitle = 'NFL Fanbase Sentiment (2018)'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4(strong('Options:')),
      pickerInput(inputId = 'team_choice', 'Select Team(s):', choices = sort(unique(res$Team)), multiple = T, 
                  options = list(`actions-box`=T, `none-selected-text`='No teams selected', `selected-text-format`= "count > 31", `count-selected-text` = 'All teams selected'), selected = sort(unique(res$Team))),
      pickerInput(inputId = 'week_choice', 'Select Week(s):', choices = sort(unique(res$Week)), multiple = T, 
                  options = list(`actions-box`=T, `none-selected-text`='No weeks selected', `selected-text-format`= paste0("count >", max(res$Week)-1), `count-selected-text` = 'All weeks selected'), selected = sort(unique(res$Week))),
      checkboxGroupButtons(inputId = 'result_choice', 'Select Game Result(s)', choices = c('W', 'T', 'L'), 
                           selected = sort(unique(as.character(res$Result))), size = 'sm', direction = 'horizontal'),
      sliderInput(inputId = 'margin_choice', label = 'Select Game Margin:', min = min(res$Margin), max = max(res$Margin), 
                  value = c(min, max), ticks = T, step = 1, post = 'pts')
    ), # sidebarPanel
    
    mainPanel(
      
      tabsetPanel(type = 'tabs',
                  
                  tabPanel(title = 'Weekly Sentiment', plotlyOutput(outputId = 'scatterplot')),
                  tabPanel(title = 'Team Averages', plotlyOutput(outputId = 'team_scatterplot')),
                  tabPanel(title = 'Polarizing Games', plotlyOutput(outputId = 'game_barplot')),
                  tabPanel(title = 'Data Table', div(dataTableOutput('data_table'), style = 'font-size:80%')),
                  tabPanel(title = 'README', htmlOutput(outputId = 'readme'))
                  
      ) # tabsetPanel
      
      
      
    ) # mainPanel
    
  ) # sidebarLayout
  
) # fluidPage
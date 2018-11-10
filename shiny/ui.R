ui <- fluidPage(
  
  titlePanel(title = h4('NFL Fanbase Sentiment (2018)'), windowTitle = 'NFL Fanbase Sentiment (2018)'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4(strong('Options:')),
      pickerInput(inputId = 'team_choice', 'Select Team(s):', choices = sort(unique(res$Team)), multiple = T, 
                  options = list(`actions-box`=T), selected = sort(unique(res$Team))),
      checkboxGroupButtons(inputId = 'week_choice', 'Select Week(s):', choices = sort(unique(res$Week)), 
                           selected = sort(unique(res$Week)), size = 'sm', direction = 'horizontal'),
      checkboxGroupButtons(inputId = 'result_choice', 'Select Game Result(s)', choices = c('W', 'T', 'L'), 
                           selected = sort(unique(as.character(res$Result))), size = 'sm', direction = 'horizontal'),
      sliderInput(inputId = 'margin_choice', label = 'Select Game Margin:', min = min(res$Margin), max = max(res$Margin), 
                  value = c(min, max), ticks = T, step = 1, post = 'pts')
      
    ), # sidebarPanel
    
    mainPanel(
      
      tabsetPanel(type = 'tabs',
                  
                  tabPanel(title = 'Weekly Sentiment', plotlyOutput(outputId = 'scatterplot')),
                  tabPanel(title = 'Team Averages', plotlyOutput(outputId = 'team_scatterplot')),
                  tabPanel(title = 'Data Table', div(dataTableOutput('data_table'), style = 'font-size:80%')),
                  tabPanel(title = 'README', htmlOutput(outputId = 'readme'))
                  
      ) # tabsetPanel
      
      
      
    ) # mainPanel
    
  ) # sidebarLayout
  
) # fluidPage
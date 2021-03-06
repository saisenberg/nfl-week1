server <- function(input, output, session){
  
  week_choices <- reactive(filter(res, Team %in% input$team_choice))
  
  res_v0 <- reactive(filter(res,
                            Result %in% input$result_choice,
                            Margin >= input$margin_choice[1],
                            Margin <= input$margin_choice[2])) # filtered df for drop-down team list
  observe(
    {updatePickerInput(session = session, inputId = 'week_choice', choices = sort(unique(week_choices()$Week)), 
                       selected = sort(unique(week_choices()$Week)))}
  ) # observe for drop-down team list
  
  res_f1 <- reactive(
    filter(res_v0(), Team %in% input$team_choice,
           Week %in% input$week_choice,
           Result %in% input$result_choice,
           Margin >= input$margin_choice[1],
           Margin <= input$margin_choice[2]
    )
  ) # reactive (selected teams)
  
  res_f2 <- reactive(
    filter(res_v0(), !(Team %in% input$team_choice),
           Result %in% input$result_choice,
           Margin >= input$margin_choice[1],
           Margin <= input$margin_choice[2]
    )
  ) # reactive (non-selected teams)
  
  team_res_v0 <- reactive(filter(res,
                                 Week %in% input$week_choice,
                                 Result %in% input$result_choice,
                                 Margin >= input$margin_choice[1],
                                 Margin <= input$margin_choice[2]) %>% 
                            group_by(Team) %>% 
                            summarise(Positivity = round(mean(Positivity),2), Negativity = round(mean(Negativity),2)) %>% 
                            mutate(Net.Positivity = Positivity - Negativity) %>% 
                            left_join(team_res[,c('Team', 'WinPct', 'WinPct_scale', 'PointSize')], by = 'Team')
  ) # reactive (team averages)
  
  game_res_v0 <- reactive(games %>% 
                            filter((Winner.Team %in% input$team_choice) | (Loser.Team %in% input$team_choice),
                                   Week %in% input$week_choice,
                                   Margin >= input$margin_choice[1],
                                   Margin <= input$margin_choice[2]) %>% 
                            dplyr::arrange(-Difference.Net.Pos) %>% 
                            top_n(30, Difference.Net.Pos)
  ) # reactive (polarizing games)
  
  team_res_f1 <- reactive(filter(team_res_v0(), Team %in% input$team_choice))
  team_res_f2 <- reactive(filter(team_res_v0(), !(Team %in% input$team_choice)))
  
  output$scatterplot <- renderPlotly({
    p <- ggplot(res_f1(), aes(x=Positivity, y=Negativity, color=Result, Team.Full=Team.Full, Game = Game, 
                              Net.Positivity=Net.Positivity)) +
      theme_minimal() +
      geom_point(alpha = 0.8, aes(size=Margin_scale)) +
      scale_color_manual(values = result_scale, drop=F) +
      scale_x_continuous(limits = c(min(res$Positivity)-0.50, max(res$Positivity)+0.50)) +
      scale_y_continuous(limits = c(min(res$Negativity)-0.50, max(res$Negativity)+0.50)) +
      scale_size_identity()
    p2 <- p + geom_point(data=res_f2(), alpha=0.15, aes(size=Margin_scale))
    ggplotly(p2, tooltip = c('Team.Full', 'Game', 'Net.Positivity'))
  }) # scatterplot
  
  output$team_scatterplot <- renderPlotly({
    t <- ggplot(team_res_f1(), aes(x=Positivity, y=Negativity, color=WinPct_scale, size = PointSize, Team=Team, WinPct = WinPct)) + 
      geom_point(alpha = 0.8) + 
      theme_minimal() +
      scale_color_manual(values = winpct_scale, drop=F) +
      scale_x_continuous(limits = c(min(team_res_v0()$Positivity)-0.50, max(team_res_v0()$Positivity)+0.50)) +
      scale_y_continuous(limits = c(min(team_res_v0()$Negativity)-0.50, max(team_res_v0()$Negativity)+0.50)) +
      labs(colour = 'Win Pct.') +
      scale_size(guide = 'none')
    t2 <- t + geom_point(data=team_res_f2(), alpha=0.15)
    ggplotly(t2, tooltip = c('Team', 'WinPct', 'Positivity', 'Negativity'))
  })
  
  output$game_barplot <- renderPlotly({
    g <- ggplot(game_res_v0(), aes(x=reorder(gameID, -Difference.Net.Pos), y=Difference.Net.Pos, fill=Margin, Game.Result=Game.Result)) + 
      theme_minimal() +
      geom_bar(stat="identity", color="darkgreen") +
      scale_fill_gradient2(low = muted('forestgreen'), high = 'forestgreen') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab('Game') +
      ylab('Difference') +
      ggtitle('Difference between Winner and Loser Net Positivity')
    ggplotly(g, tooltip = c('Game.Result', 'Difference.Net.Pos'))
  })
  
  output$data_table <- renderDataTable({
    res_f1() %>% 
      select(c('Week', 'Team', 'Opponent', 'Result', 'Team.Score', 'Oppt.Score', 'Margin', 'Positivity', 'Negativity', 'Net.Positivity')) %>% 
      rename('Net Pos.' = 'Net.Positivity', 'Oppt.' = 'Opponent', 'Pos.' = 'Positivity', 'Neg.' = 'Negativity') %>% 
      mutate('Margin' = Team.Score - Oppt.Score)
  }, options = list(pageLength = 10, lengthMenu = c(10, 25, 50))) # data_table
  
  output$readme <- renderText({
    HTML(html_text)
  })
  
} # server
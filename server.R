shinyServer(function(input, output, session) {
  
  observe({
      
      output$ratingprice_fig <- renderPlotly(
        
        ca_data %>% 
          filter(award %in% c("Bronze", "Silver", "Gold", "Double Gold")) %>% 
           plot_ly(x = ~ln_price, 
                   y = ~score, 
                   type = "scatter", 
                   mode = "markers",
                   frame = ~region,
                   text = ~price,
                   marker = list(size = 9,
                                 color = 'rgba(255, 182, 193, .9)',
                                 line = list(color = 'rgba(152, 0, 0, .8)',
                                             width = 2))) %>% 
          
            animation_opts(
              1000
            ) %>% 
          
            animation_slider(
              currentvalue = list(prefix = "Wine region: ")
            ) %>% 
             
            layout(
             
              title = "Relationship between rating and price by region",
              xaxis = list(
              title = "Price (in natural log)"
                      ),
              yaxis = list(
              title = "Judge score"
                      )
             )
       )
      
  })
      
  observe({
    
    output$qualityprice_fig <- renderPlot(
      
      ca_winemag_data %>% 
        filter(ca == 1) %>% 
        ggplot(aes(ln_price, score, fill = data)) +
        geom_point(color = "lightsteelblue4") +
        theme_bw() +
        facet_wrap(~data) +
        labs(title = "Relationship between rating and price - Comparison",
             x = "Price (in natural log)",
             y = "Judge rating",
             fill = "Rating Source:") +
        xlim(1.5, 6) +
        theme(plot.title = element_text(hjust = 0.5, size = 17),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              text = element_text(family = "Times"),
              legend.position = "bottom",
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 14),
              strip.text = element_text(size = 12))
    )
    
  })

  output$rating_price_sf <- renderPlot(
    
    sf_data %>% 
      filter(award %in% c("Bronze", "Silver", "Gold", "Double Gold"), competition_year != 2014) %>% 
      mutate(award = factor(award, levels = c("Bronze", "Silver", "Gold", "Double Gold"), ordered = TRUE),
             competition_year = factor(competition_year, levels = c(2018, 2017, 2016, 2015, 2014), ordered = TRUE)) %>% 
      ggplot(aes(award, ln_price, fill = competition_year)) +
      geom_boxplot(alpha = 0.8) +
      theme_bw() +
      facet_wrap(~competition_year, ncol = 2) +
      theme(plot.title = element_text(hjust = 0.5, size = 17),
            axis.title.x = element_text(vjust = -1.25, size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.position = "bottom",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            strip.background = element_rect(color = "gray73"),
            strip.text = element_text(size = 14, face = "bold")) +
      labs(title = "Relationship between award and price - SF Competition",
           x = "Award",
           y = "Price (in natural log)",
           fill = "Competition Year") +
      scale_fill_manual(values = c("lightcyan1", "lightcyan2", "lightcyan3", "lightcyan4"))
  )
  
  output$wordcloud <- renderPlot(
    
    ca_data_wdesc %>% 
      filter(award == "Bronze", wm_region == input$review_region) %>% 
      mutate(obs = row_number(), description = as.character(description)) %>% 
      select(obs, description) %>% 
      unnest_tokens(word, description) %>% 
      anti_join(stop_words) %>% 
      full_join(attribute_data, by = "word") %>% 
      filter(attribute == 1) %>%
      mutate(tvar = ifelse(category == input$review_attribute, 0, 1)) %>%
      count(word, tvar, sort = TRUE) %>%
      acast(word ~ tvar, value.var = "n", fill = 0) %>% 
      comparison.cloud(colors = c("gray20", "gray80"), 
                       scale = c(1.25, 0.75),
                       max.words = 50, 
                       random.order = FALSE)
  )
  
  output$wordcloud2 <- renderPlot(
    
    ca_data_wdesc %>% 
      filter(award == input$review_comp, wm_region == input$review_region) %>% 
      mutate(obs = row_number(), description = as.character(description)) %>% 
      select(obs, description) %>% 
      unnest_tokens(word, description) %>% 
      anti_join(stop_words) %>% 
      full_join(attribute_data, by = "word") %>%
      filter(attribute == 1) %>% 
      mutate(tvar = ifelse(category == input$review_attribute, 0, 1)) %>%
      count(word, tvar, sort = TRUE) %>%
      acast(word ~ tvar, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("gray20", "gray80"), 
                       scale = c(1.25, 0.75),
                       max.words = 50, 
                       random.order = FALSE))

  observe({

    if (input$reviewsummary_region == "Napa") {

      t <- winemag_data %>%
        filter(country == "US", province == "California")

      t <- ca_data_wdesc %>%
        filter(wm_region == "Napa") %>%
        mutate(obs = row_number(), description = as.character(description)) %>%
        filter(award %in% c("Bronze", "Silver", "Gold")) %>%
        select(obs, description, award) %>%
        group_by(award) %>%
        unnest_tokens(word, description) %>%
        anti_join(stop_words) %>%
        full_join(attribute_data, by = "word") %>%
        filter(is.na(category) == FALSE, award != "") %>%
        mutate(tmp_obs = n()) %>%
        group_by(award, category) %>%
        summarise(count_obs = max(tmp_obs),
                  cat_count = n(),
                  cat_pct = (cat_count/count_obs)*100) %>%
        mutate(cat_pct = round(cat_pct, 1)) %>%
        full_join(award_cat, by = c("award", "category")) %>%
        arrange(award, category) %>%
        select(award, category, cat_pct)

      t$cat_pct[is.na(t$cat_pct)] <- 0

      t2 <- t %>%
        group_by(award) %>%
        mutate(xnum = row_number()) %>%
        select(-category) %>%
        spread(xnum, cat_pct)

      l <- length(colnames(t2))
      tmp <- paste0("x", as.character(1:(l-1)))
      t2 <- as.data.frame(t2)
      colnames(t2) <- c("y", tmp)
      data <- as.data.frame(t2)
      top_labels <- as.vector(sapply(unique(as.vector(t$category)), toproper))

      output$barchart <- renderPlotly(

        plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
                marker = list(color = 'rgba(38, 24, 74, 0.8)',
                              line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
          add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
          add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
          add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) %>%
          add_trace(x = ~x5, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%
          add_trace(x = ~x6, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%

          layout(barmode = "stack",
                 yaxis = list(title = "",
                              categoryorder = "array",
                              categoryarray = c("Bronze", "Silver", "Gold")
                 ),

                 xaxis = list(title = "",
                              zeroline = FALSE,
                              showticklabels = FALSE),
                 paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
                 margin = list(l = 120, r = 10, t = 140, b = 80),
                 showlegend = TRUE
                 ) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 / 2, y = data$y,
                          text = paste(data[,"x1"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 / 2, y = data$y,
                          text = paste(data[,"x2"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 + data$x3 / 2, y = data$y,
                          text = paste(data[,"x3"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 + data$x3 + data$x4 / 2, y = data$y,
                          text = paste(data[,"x4"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 + data$x3 + data$x4 + data$x5 / 2, y = data$y,
                          text = paste(data[,"x5"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 + data$x3 + data$x4 + data$x5 + data$x6 / 2, y = data$y,
                          text = paste(data[,"x5"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'paper',
                          x = c(13 / 2, 12 + 13 / 2, 12 + 13 + 8 / 2, 12 + 13 + 8 + 56 / 2,
                                12 + 13 + 8 + 56 + 3 / 2, 12 + 13 + 8 + 56 + 3 + 3, 2),
                          y = 1.025,
                          text = top_labels,
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(67, 67, 67)'),
                          showarrow = FALSE))

    } else {

      t <- winemag_data %>%
        filter(country == "US", province == "California")
      #View(t)

      t <- ca_data_wdesc %>%
        filter(wm_region == "Sonoma") %>%

        mutate(obs = row_number(), description = as.character(description)) %>%
        filter(award %in% c("Bronze", "Silver", "Gold")) %>%
        select(obs, description, award) %>%
        group_by(award) %>%
        unnest_tokens(word, description) %>%
        anti_join(stop_words) %>%
        full_join(attribute_data, by = "word") %>%
        filter(is.na(category) == FALSE, award != "") %>%
        mutate(tmp_obs = n()) %>%
        group_by(award, category) %>%
        summarise(count_obs = max(tmp_obs),
                  cat_count = n(),
                  cat_pct = (cat_count/count_obs)*100) %>%
        mutate(cat_pct = round(cat_pct, 1)) %>%
        full_join(award_cat, by = c("award", "category")) %>%
        arrange(award, category) %>%
        select(award, category, cat_pct)

      t$cat_pct[is.na(t$cat_pct)] <- 0

      t2 <- t %>%
        group_by(award) %>%
        mutate(xnum = row_number()) %>%
        select(-category) %>%
        spread(xnum, cat_pct)

      l <- length(colnames(t2))
      tmp <- paste0("x", as.character(1:(l-1)))
      t2 <- as.data.frame(t2)
      colnames(t2) <- c("y", tmp)
      data <- as.data.frame(t2)
      top_labels <- as.vector(sapply(unique(as.vector(t$category)), toproper))

      output$barchart <- renderPlotly(

        plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
                marker = list(color = 'rgba(38, 24, 74, 0.8)',
                              line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
          add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
          add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
          add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) %>%
          add_trace(x = ~x5, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%
          add_trace(x = ~x6, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%

          layout(barmode = "stack",
                 yaxis = list(title = "",
                              categoryorder = "array",
                              categoryarray = c("Bronze", "Silver", "Gold")
                 ),

                 xaxis = list(title = "",
                              zeroline = FALSE,
                              showticklabels = FALSE),
                 paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
                 margin = list(l = 120, r = 10, t = 140, b = 80),
                 showlegend = FALSE,
                 showlegend = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 / 2, y = data$y,
                          text = paste(data[,"x1"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 / 2, y = data$y,
                          text = paste(data[,"x2"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 + data$x3 / 2, y = data$y,
                          text = paste(data[,"x3"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 + data$x3 + data$x4 / 2, y = data$y,
                          text = paste(data[,"x4"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 + data$x3 + data$x4 + data$x5 / 2, y = data$y,
                          text = paste(data[,"x5"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'y',
                          x = data$x1 + data$x2 + data$x3 + data$x4 + data$x5 + data$x6 / 2, y = data$y,
                          text = paste(data[,"x5"], '%'),
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(248, 248, 255)'),
                          showarrow = FALSE) %>%

          add_annotations(xref = 'x', yref = 'paper',
                          x = c(22 / 2, 12 + 13 / 2, 12 + 13 + 8 / 2, 12 + 13 + 8 + 56 / 2,
                                12 + 13 + 8 + 56 + 3 / 2, 12 + 13 + 8 + 56 + 3 + 3, 2),
                          y = 1.025,
                          text = top_labels,
                          font = list(family = 'Arial', size = 12,
                                      color = 'rgb(67, 67, 67)'),
                          showarrow = FALSE))

    }

  })

})
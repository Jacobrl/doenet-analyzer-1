library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(tidytext)
library(igraph)
library(ggraph)
library(wordcloud)
library(tm)  # Add the tm package for pairwise_count


doenetid <- "_pdiqrEQqDLsTCucSaMdw1"
raw <- stream_in(file(
  paste0(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=",
    doenetid
  )
))

# clean the data, test the functions
events <- raw$events[[1]]
dates <- pull_dates(events)
cleaned_versions <- clean_events(events, min(dates), max(dates))

ui <- fluidPage(
  titlePanel("Doenet Data Analysis"),
  mainPanel(
    tabsetPanel(
      tabPanel("Graph 1", plotOutput("graph1")),
      tabPanel("Graph 2", plotOutput("graph2")),
      tabPanel("Graph 3", plotOutput("unless_web1")),
      tabPanel("Graph 4", plotOutput("unless_web2")),
      tabPanel("Word Cloud 1", plotOutput("wordcloud1")),
      tabPanel("Word Cloud 2", plotOutput("wordcloud2"))
    ),
    selectInput("componentType_dropdown", 
                                  "Select Type Of Question",
                                  choices = c("answer",
                                              "choiceInput",
                                              "textInput"))
  )
)

server <- function(input, output) {
  
  # Process the data for Graph 1
  filtered_text1 <- reactive({
    
    select_input <- input$componentType_dropdown
    
    cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      filter(componentType == paste0("", select_input)) %>%
      select(userId, pageNumber, responseText) %>%
      filter(!is.null(responseText)) %>%
      mutate(responseText = as.character(responseText)) %>%
      filter(!responseText %in% c('NULL', ' ', paste0("", (-1E6:1E6)))) %>%
      select(userId, pageNumber, responseText) %>%
      unnest_tokens(word, responseText) %>% 
      anti_join(stop_words) %>%
      filter(!word %in% c(paste0("", (-1E6:1E6)))) %>%
      widyr::pairwise_count(word, userId, sort = TRUE, upper = FALSE)  # Use the tm package here
  })
  
  # Process the data for Graph 2
  filtered_text2 <- reactive({
    
    select_input <- input$componentType_dropdown
    
    cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      filter(componentType == paste0("", select_input)) %>%
      select(userId, pageNumber, responseText) %>%
      filter(!is.null(responseText)) %>%
      mutate(responseText = as.character(responseText)) %>%
      filter(!responseText %in% c('NULL', ' ', paste0("", (-1E6:1E6)))) %>%
      select(userId, pageNumber, responseText) %>%
      widyr::pairwise_count(responseText, userId, sort = TRUE, upper = FALSE)  # Use the tm package here
  })
  
  # Generate plots using igraph
  output$graph1 <- renderPlot({
    graph <- filtered_text1() %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
      theme_void()
    print(graph)
  })
  
  output$graph2 <- renderPlot({
    graph <- filtered_text2() %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
      theme_void()
    print(graph)
  })
  
  output$unless_web1 <- renderPlot({
    filtered_data1 <- filtered_text1() %>%
      filter(n >= 50)
    
    if (nrow(filtered_data1) == 0) {
      # If no data passes the filter, create a default plot
      default_plot <- ggplot() +
        geom_text(aes(x = 0.5, y = 0.5, label = "No available data to analyze"),
                  size = 5, color = "red", hjust = 0.5, vjust = 0.5) +
        theme_void()
      print(default_plot)
    } else {
      # Create the graph using filtered_data1
      graph <- filtered_data1 %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
        geom_node_point(size = 5) +
        geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
        theme_void()
      print(graph)
    }
  })
  
  output$unless_web2 <- renderPlot({
    filtered_data2 <- filtered_text2() %>%
      filter(n >= 50)
    
    if (nrow(filtered_data2) == 0) {
      # If no data passes the filter, create a default plot
      default_plot <- ggplot() +
        geom_text(aes(x = 0.5, y = 0.5, label = "No available data to analyze"),
                  size = 5, color = "red", hjust = 0.5, vjust = 0.5) +
        theme_void()
      print(default_plot)
    } else {
      # Create the graph using filtered_data2
      graph <- filtered_data2 %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
        geom_node_point(size = 5) +
        geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
        theme_void()
      print(graph)
    }
  })
  
  
  output$wordcloud1 <- renderPlot({
    wordcloud(filtered_text1(), scale = c(2, 1), min.freq = 50, colors = rainbow(30))
  })
  
  output$wordcloud2 <- renderPlot({
    wordcloud(filtered_text2(), scale = c(2, 1), min.freq = 50, colors = rainbow(30))
  })
}

shinyApp(ui, server)

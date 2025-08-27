# --------------------------------------------------
# Load Required Packages
# --------------------------------------------------
library(shiny)
library(shinythemes)
library(httr)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(scales) 

# --------------------------------------------------
# Secure API Key Handling
# --------------------------------------------------
Sys.setenv(MY_YT_API_KEY = "YOUR_API_KEY_HERE")
yt_api_key <- Sys.getenv("MY_YT_API_KEY")

if (yt_api_key == "" || yt_api_key == "YOUR_API_KEY_HERE") {
  stop("Please set your YouTube API key using Sys.setenv(MY_YT_API_KEY = 'your_real_key_here')")
}

# --------------------------------------------------
# Helper Functions
# --------------------------------------------------
# 1. Search YouTube for videos
retrieveYoutubeResults <- function(api_key, query_text) {
  base_search_url <- "https://www.googleapis.com/youtube/v3/search"
  search_url <- paste0(
    base_search_url, "?part=snippet&q=", URLencode(query_text),
    "&maxResults=5&type=video&key=", api_key
  )
  
  resp <- GET(search_url)
  if (http_error(resp)) stop("Error occurred while calling the YouTube Search API.")
  
  parsed_result <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
  
  if (!is.null(parsed_result$items) && length(parsed_result$items) > 0) {
    data.frame(
      video_id    = parsed_result$items$id.videoId,
      video_title = parsed_result$items$snippet.title,
      thumb_url   = parsed_result$items$snippet.thumbnails.medium.url,
      stringsAsFactors = FALSE
    )
  } else {
    stop("No videos found with the given search term.")
  }
}

# 2. Fetch stats (views, likes, comments)
fetchVideoStats <- function(api_key, vid_id) {
  base_video_url <- "https://www.googleapis.com/youtube/v3/videos"
  stats_url <- paste0(base_video_url, "?part=statistics&id=", vid_id, "&key=", api_key)
  
  resp <- GET(stats_url)
  if (http_error(resp)) stop("Error occurred while calling the YouTube Videos API.")
  
  result_data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  
  # If items is empty, no data is returned
  stats_info <- result_data$items$statistics
  list(
    views    = as.numeric(stats_info$viewCount),
    likes    = as.numeric(stats_info$likeCount),
    comments = ifelse(is.null(stats_info$commentCount), 0, as.numeric(stats_info$commentCount))
  )
}

# --------------------------------------------------
# UI
# --------------------------------------------------
ui <- fluidPage(
  
  # Choose an advanced theme and add custom CSS
  theme = shinytheme("superhero"),
  
  tags$head(
    tags$style(HTML("
      /* ----- Global Body Style ----- */
      body {
        background: linear-gradient(to right, #0f2027, #203a43, #2c5364) !important;
        color: #ffffff !important;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      /* ----- Panel and Well ----- */
      .panel, .well {
        background-color: rgba(255, 255, 255, 0.08) !important;
        border: none !important;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.3) !important;
      }
      
      /* ----- Inputs & Buttons ----- */
      .form-control {
        background-color: #222222 !important;
        border: 1px solid #555555 !important;
        color: #ffffff !important;
      }
      .btn {
        background-color: #f39c12 !important; 
        border: none !important;
      }
      .btn:hover {
        background-color: #e67e22 !important;
      }
      
      /* ----- Titles & Headers ----- */
      h1, h2, h3, h4 {
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      /* ----- Notification Style ----- */
      .shiny-notification {
        background-color: #222222 !important;
        color: #ffffff !important;
        border: 1px solid #555555 !important;
      }
      
      /* ----- Plot background ----- */
      .shiny-plot-output {
        background-color: rgba(255, 255, 255, 0.1) !important;
        border-radius: 6px !important;
        padding: 10px !important;
      }
    "))
  ),
  
  # Page Title
  titlePanel("YouTube Video Statistics"),
  
  # Layout
  sidebarLayout(
    sidebarPanel(
      width = 4,
      textInput("txtSearch", label = "Enter a YouTube Video Title:", value = ""),
      actionButton("btnSearch", "Search Videos", icon = icon("search"), width = "100%"),
      br(), hr(),
      uiOutput("videoSelection"),
      actionButton("btnClear", "Clear All Selections", icon = icon("trash"), width = "100%")
    ),
    
    mainPanel(
      width = 8,
      h4(icon("youtube"), " Currently Selected Videos"),
      uiOutput("chosenVideosUI"),
      hr(),
      actionButton("btnCompare", "Compare Selected Videos", icon = icon("chart-bar"), width = "100%"),
      br(),
      
      h4(icon("chart-bar"), "View Count Comparison"),
      plotOutput("plotViews", height = "300px"),
      
      h4(icon("chart-bar"), "Like Count Comparison"),
      plotOutput("plotLikes", height = "300px"),
      
      h4(icon("chart-bar"), "Comment Count Comparison"),
      plotOutput("plotComments", height = "300px")
    )
  )
)

# --------------------------------------------------
# Server
# --------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive values to store search results and selected videos
  rv <- reactiveValues(
    search_results = NULL,
    chosen_videos  = data.frame()
  )
  
  # 1. Perform the search when the user clicks "Search Videos"
  observeEvent(input$btnSearch, {
    text_query <- trimws(input$txtSearch)
    if (text_query == "") return(NULL)
    
    # Try retrieving data from YouTube
    results_found <- tryCatch({
      retrieveYoutubeResults(yt_api_key, text_query)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
    
    rv$search_results <- results_found
  })
  
  # 2. Dynamically render search results for video selection
  output$videoSelection <- renderUI({
    req(rv$search_results)
    if (nrow(rv$search_results) == 0) return(NULL)
    
    tagList(
      lapply(seq_len(nrow(rv$search_results)), function(idx) {
        vid_id <- rv$search_results$video_id[idx]
        
        tags$div(
          style = "margin-bottom: 10px;",
          img(src = rv$search_results$thumb_url[idx], width = "120px", height = "70px", 
              style = "border-radius: 6px; box-shadow: 0 0 5px rgba(0,0,0,0.3);"),
          p(rv$search_results$video_title[idx]),
          actionButton(
            inputId = paste0("select_", vid_id),
            label   = "Add This Video",
            width   = "100%"
          )
        )
      })
    )
  })
  
  # 3. When a user clicks "Add This Video", add that video to chosen_videos
  observeEvent(rv$search_results, {
    req(rv$search_results)
    lapply(rv$search_results$video_id, function(current_id) {
      observeEvent(input[[paste0("select_", current_id)]], {
        # Check if already added
        if (!current_id %in% rv$chosen_videos$video_id) {
          new_row <- rv$search_results[rv$search_results$video_id == current_id, ]
          # Replace the video_index label with the first 8 characters of the video title
          new_row$video_index <- substr(new_row$video_title, 1, 8)
          
          rv$chosen_videos <- rbind(rv$chosen_videos, new_row)
        }
      })
    })
  })
  
  # 4. Display chosen videos with "Remove" button
  output$chosenVideosUI <- renderUI({
    if (nrow(rv$chosen_videos) == 0) return(NULL)
    
    tagList(
      lapply(seq_len(nrow(rv$chosen_videos)), function(idx) {
        vid_id <- rv$chosen_videos$video_id[idx]
        
        tags$div(
          style = "margin-bottom: 10px;",
          img(src = rv$chosen_videos$thumb_url[idx], width = "150px",
              style = "border-radius: 6px; box-shadow: 0 0 5px rgba(0,0,0,0.3);"),
          tags$strong(rv$chosen_videos$video_index[idx]),
          p(rv$chosen_videos$video_title[idx]),
          actionButton(
            inputId = paste0("remove_", vid_id),
            label   = "Remove",
            icon    = icon("trash")
          )
        )
      })
    )
  })
  
  # 5. Remove selected video from chosen_videos
  observe({
    req(nrow(rv$chosen_videos) > 0)
    lapply(rv$chosen_videos$video_id, function(current_id) {
      observeEvent(input[[paste0("remove_", current_id)]], {
        rv$chosen_videos <- subset(rv$chosen_videos, video_id != current_id)
      })
    })
  })
  
  # 6. Clear all selected videos when "Clear All Selections" is clicked
  observeEvent(input$btnClear, {
    rv$chosen_videos <- data.frame()
  })
  
  # 7. Compare stats for selected videos
  observeEvent(input$btnCompare, {
    if (nrow(rv$chosen_videos) == 0) {
      showNotification("No videos to compare.", type = "warning")
      return(NULL)
    }
    
    stats_data_list <- lapply(rv$chosen_videos$video_id, function(x_id) {
      tryCatch({
        fetchVideoStats(yt_api_key, x_id)
      }, error = function(e) {
        showNotification(paste("Error fetching stats for video:", x_id), type = "error")
        return(NULL)
      })
    })
    
    # Build a data frame with stats
    stats_df <- data.frame(
      VideoIndex   = rv$chosen_videos$video_index,
      ViewCount    = sapply(stats_data_list, function(x) if (is.null(x)) NA else x$views),
      LikeCount    = sapply(stats_data_list, function(x) if (is.null(x)) NA else x$likes),
      CommentCount = sapply(stats_data_list, function(x) if (is.null(x)) NA else x$comments)
    )
    
    # Remove any rows with all NA stats
    stats_df <- stats_df[!(
      is.na(stats_df$ViewCount) & 
        is.na(stats_df$LikeCount) & 
        is.na(stats_df$CommentCount)
    ), ]
    if (nrow(stats_df) == 0) {
      showNotification("No valid videos to compare.", type = "warning")
      return(NULL)
    }
    
    # Define the x-scale to display the truncated video titles
    x_scale <- scale_x_discrete(
      limits = stats_df$VideoIndex,
      labels = stats_df$VideoIndex
    )
    
    # Define the y-scale with numeric breaks and comma formatting
    y_scale <- scale_y_continuous(
      labels = comma,
      breaks = pretty_breaks(n = 5),
      expand = expansion(mult = c(0, 0.1))
    )
    
    # Use theme_light and override elements to keep a dark look
    custom_theme <- theme_light(base_size = 14) +
      theme(
        panel.background  = element_rect(fill = "grey20"),
        plot.background   = element_rect(fill = "grey20"),
        panel.grid.major  = element_line(color = "grey50"),
        panel.grid.minor  = element_line(color = "grey40"),
        axis.line         = element_line(color = "white"),
        axis.ticks        = element_line(color = "white"),
        axis.text.x       = element_text(angle = 45, hjust = 1, color = "white"),
        axis.text.y       = element_text(color = "white"),
        axis.title.x      = element_text(color = "white"),
        axis.title.y      = element_text(color = "white"),
        plot.title        = element_text(color = "white")
      )
    
    # 7a. View Count Plot
    output$plotViews <- renderPlot({
      ggplot(stats_df, aes(x = VideoIndex, y = ViewCount)) +
        geom_bar(stat = "identity", fill = "#9b59b6") +
        x_scale + y_scale +
        labs(x = "Video", y = "Views") +
        custom_theme
    })
    
    # 7b. Like Count Plot
    output$plotLikes <- renderPlot({
      ggplot(stats_df, aes(x = VideoIndex, y = LikeCount)) +
        geom_bar(stat = "identity", fill = "#f1c40f") +
        x_scale + y_scale +
        labs(x = "Video", y = "Likes") +
        custom_theme
    })
    
    # 7c. Comment Count Plot
    output$plotComments <- renderPlot({
      ggplot(stats_df, aes(x = VideoIndex, y = CommentCount)) +
        geom_bar(stat = "identity", fill = "#2ecc71") +
        x_scale + y_scale +
        labs(x = "Video", y = "Comments") +
        custom_theme
    })
  })
}

# --------------------------------------------------
# Run the Application
# --------------------------------------------------
shinyApp(ui = ui, server = server)

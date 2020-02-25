v <- "0.3.1"
vdt <- "2020-02-25"
nmax <- 1000
header_title <- "Kepo"

library(textclean)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(textdata)
library(rtweet)
library(shiny)
library(shinyalert)
library(DT)
library(bs4Dash)
library(shinyWidgets)

faq <- function(q, a){
  sprintf("<strong>%s</strong><br/>%s<br/><br/>", q, a)
}

faqtext <- 
  p(HTML(faq(q = "How many tweets collected?",
                 a = sprintf("Current version will collect about %s (or less) tweets each time query", nmax))
         ),
    HTML(faq(q = "I found an issue/error, where do I report it?", a = "https://github.com/aephidayatuloh/kepo/issues"))
    )

ui <- bs4DashPage(navbar = bs4DashNavbar(skin = "light", 
                                         status = "white", 
                                         border = FALSE, 
                                         sidebarIcon = "bar", 
                                         compact = FALSE, 
                                         leftUi = fluidRow(img(src = "img/logo_animation.png", width = "8%"), 
                                                           h3(header_title, style = "color:#000;margin-top: 10px;font-weight:bold;margin-left: 20px;")),
                                         rightUi = actionBttn(inputId = "help", label = NULL, style = "pill", icon = icon("question-circle"))
), 
sidebar = bs4DashSidebar(inputId = "sider", disable = TRUE), 
body = bs4DashBody(
  useShinyalert(),
  br(),
  fluidRow(
    column(4, 
           bs4Card(width = 12, title = NULL, headerBorder = FALSE, maximizable = FALSE, closable = FALSE, collapsible = FALSE,
                   fluidRow(
                     textInput("userid", NULL, placeholder = "username or @username", width = "63%"),
                     actionButton("stalking", "Kepoin", class = "btn btn-info", icon = icon("twitter"), width = "35%",
                                  style = "height: 38px;background:#fff;color:#4dc3eb;font-weight:bold;")
                   )
           ),
           uiOutput("user"),
           uiOutput("latest_tweet_ui")
           ),
    column(8,
           uiOutput("source_tweets_ui"),
           uiOutput("nchar_tweets_ui")
           ),
    column(6,
           uiOutput("hashtags_ui")
           ),
    column(6,
           uiOutput("most_mentioned_ui")
           ),
    column(12,
           uiOutput("time_tweets_ui")
           ),
    column(12,
           uiOutput("sentiment_tweets_ui")
           )
  )
), 
controlbar = NULL, 
footer = bs4DashFooter(copyrights = HTML("&copy; 2020 @aephidayatuloh"), right_text = sprintf("version %s (%s)", v, vdt)), 
title = sprintf("%s - v%s", header_title, v), 
old_school = FALSE, 
sidebar_mini = FALSE, 
sidebar_collapsed = TRUE, 
controlbar_collapsed = TRUE, 
controlbar_overlay = NULL, 
enable_preloader = FALSE, 
loading_duration = 2, 
loading_background = "blue")

server <- function(input, output, session){
  tk <- readRDS("data/tkkepooo.rds")
  
  shinyalert(title = "Welcome, Kepo!", 
             text = sprintf("Kepo v%s (%s)<br/><br/>Just give me a username and I'll handle for you", v, vdt), 
             imageUrl = "img/logo_animation.png", imageWidth = 400, imageHeight = 150, confirmButtonText = "Get Started", 
             html = TRUE)
  
  tweets <- eventReactive(input$stalking, {
    if(input$userid == ""){
      shinyalert(title = "Ooops!", 
                 text = "Please provide the username.", 
                 type = "error",
                 html = TRUE)
      return(NULL)
    } else {
      # isolate(input$userid)
      shinyalert(title = "Please Wait", 
                 text = paste0("Collecting and extracting tweets..."), 
                 imageUrl = "img/loader.gif", imageWidth = 200, imageHeight = 200,
                 closeOnEsc = FALSE, closeOnClickOutside = FALSE, showConfirmButton = FALSE,
                 # type = "info",
                 html = TRUE)
      
      userid <- trimws(gsub("@", "", input$userid), "both")
      message(sprintf("Username : @%s at %s", userid, Sys.time()))
      start_time <<- Sys.time()
      tweets <- rtweet::get_timelines(userid, n = nmax, token = tk)
      ntweets <<- nrow(tweets)
      
      if(ntweets == 0){
        shinyalert(title = "Sorry, that page does not exist.", 
                   text = sprintf("Is @%s really a twitter username?
                                  <br/>
                                  <br/>
                                  <ul style='margin-left:40px;text-align:left;list-style-type:disc;'>
                                  <li>Check the spelling</li>
                                  <li>Username has been changed or deleted</li>
                                  <li>Account is private</li>
                                  </ul>
                                  You can check on https://twitter.com/%s.", userid, userid), 
                   type = "error", html = TRUE
        )
        return(NULL)
      } else {
        
        sources <- tweets %>%
          mutate(source = str_replace_all(source, "  ", " ")) %>%
          count(source) %>%
          arrange(desc(n)) %>%
          mutate(pct = round(n/sum(n)*100, 2)) %>% 
          rename(Source = source, `Number of Tweets` = n, Percentage = pct)
        
        mentioned <- tweets %>%
          filter(!is.na(mentions_screen_name)) %>%
          unnest(cols = mentions_screen_name) %>%
          count(mentions_screen_name) %>%
          mutate(mentions_screen_name = reorder(mentions_screen_name, n)) %>% 
          head(10) %>% 
          rename(`Mentioned Username` = mentions_screen_name, `Times Mentioned` = n)
        
        times <- tweets %>% 
          filter(as.Date(created_at) >= as.Date(paste0(format(Sys.Date()-365, "%Y"),"-",format(Sys.Date(),"%m-%d")))) %>% 
          mutate(created_date = as.Date(created_at)) %>% 
          count(created_date, is_retweet) %>% 
          mutate(is_retweet = if_else(is_retweet, "Retweet", "Original"),
                 is_retweet = factor(is_retweet, levels = c("Retweet", "Original")))
        
        nchars <- tweets %>%
          filter(!is_retweet) %>%
          select(display_text_width)
        
        hashtags <- tweets %>%
          filter(!is.na(hashtags)) %>%
          unnest(cols = c(hashtags)) %>%
          mutate(hashtags = tolower(hashtags)) %>% 
          count(hashtags) %>%
          arrange(desc(n)) %>% 
          head(10) %>%
          mutate(hashtags = reorder(hashtags, n))
        
        tweet_content <- tweets$text %>%
          replace_html() %>% # replace html with blank
          replace_url()  %>% # replace URLs with blank
          replace_emoji() %>%
          replace_tag(tweets, pattern = "@([A-Za-z0-9_]+)", replacement = "") %>%  # remove mentions
          replace_hash(tweets, pattern = "#([A-Za-z0-9_]+)", replacement = "") %>% # remove hashtags
          replace_number() %>%
          replace_emoticon() %>%
          gsub("[0-9]", "", .)
        
        tweet_content <- tweet_content %>%
          strip()
        
        tweets$tidy_tweet <- as.character(tweet_content)
        
        tweet_words <- tweets %>%
          select(status_id,
                 screen_name,
                 tidy_tweet,
                 created_at) %>%
          unnest_tokens(word, tidy_tweet)
        
        my_stop_words <- tibble(
          word = c("https", "t.co", "rt", "amp", "rstats", "gt"),
          lexicon = "twitter"
        )
        all_stop_words <- stop_words %>%
          bind_rows(my_stop_words)
        
        suppressWarnings({
          no_numbers <- tweet_words %>%
            filter(is.na(as.numeric(word)))
        })
        
        no_stop_words <- no_numbers %>%
          anti_join(all_stop_words, by = "word")
        
        afinn <- read.csv("data/afinn.csv", header = TRUE, stringsAsFactors = FALSE)
        bing <- read.csv("data/bing.csv", header = TRUE, stringsAsFactors = FALSE)
        nrc <- read.csv("data/nrc.csv", header = TRUE, stringsAsFactors = FALSE)
        
        nrc_words <- no_stop_words %>%
          inner_join(bing, by = "word") %>%
          inner_join(afinn, by = "word") %>%
          inner_join(nrc, by = "word", suffix = c("_bing", "_nrc"))
        
        sentiment_posneg <- nrc_words %>%
          # Count by word and sentiment
          count(word, sentiment_bing) %>%
          # Group by sentiment
          group_by(sentiment_bing) %>%
          # Take the top 10 words for each sentiment
          top_n(n = 10, wt = n) %>%
          slice(1:10) %>% 
          ungroup() %>%
          mutate(n = case_when(sentiment_bing == "negative" ~ -1*n, TRUE ~ as.numeric(n))) %>% 
          mutate(word = reorder(word, n))
        
        sentiment_feel <- nrc_words %>%
            # Count by word and sentiment
            filter(!sentiment_nrc %in% c("positive", "negative")) %>%
            count(word, sentiment_nrc) %>%
            # Group by sentiment
            group_by(sentiment_nrc) %>%
            # Take the top 10 words for each sentiment
            top_n(n = 10, wt = n) %>% #count(sentiment_nrc)
            slice(1:10) %>% 
            ungroup() %>%
            mutate(word = reorder(word, n))
        
        list(tweets_data = tweets,
             sources = sources,
             mentioned = mentioned,
             times = times,
             nchars = nchars,
             hashtags = hashtags,
             sentiment_posneg = sentiment_posneg,
             sentiment_feel = sentiment_feel)
      }
    }
  })
  
  observeEvent(input$stalking, {
    output$user <- renderUI({
      isolate(input$userid)
      if(is.null(tweets())){
        return(NULL)
      } else {
        bs4UserCard(width = 12, type = 2, imageElevation = 4,
                    src = unique(tweets()$tweets_data$profile_image_url),
                    status = "info",
                    title = unique(tweets()$tweets_data$name),
                    subtitle = paste0('@', unique(tweets()$tweets_data$screen_name)),
                    elevation = 4,
                    h6(sprintf("Details (Based on the last about %s statuses)", nrow(tweets()$tweets_data))),
                    
                    bs4Table(
                      cardWrap = FALSE,
                      bordered = FALSE,
                      striped = TRUE,
                      headTitles = c("", ""),
                      bs4TableItems(
                        bs4TableItem("Account Location"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$tweets_data$location)))
                        ),
                      bs4TableItems(
                        bs4TableItem("Country"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$tweets_data$country)[complete.cases(unique(tweets()$tweets_data$country))], collapse = "-"))
                        ),
                      bs4TableItems(
                        bs4TableItem("Account Created At"),
                        bs4TableItem(dataCell = TRUE, paste(unique(tweets()$tweets_data$account_created_at), "UTC"))
                        ),
                      bs4TableItems(
                        bs4TableItem("Followers"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$tweets_data$followers_count)))
                        ),
                      bs4TableItems(
                        bs4TableItem("Friends"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$tweets_data$friends_count)))
                        ),
                      bs4TableItems(
                        bs4TableItem("Statuses"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$tweets_data$statuses_count)))
                        ),
                      bs4TableItems(
                        bs4TableItem("Retweets"),
                        bs4TableItem(dataCell = TRUE, paste0(sum(tweets()$tweets_data$is_retweet)))
                        ),
                      bs4TableItems(
                        bs4TableItem("Favourites"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$tweets_data$favourites_count)))
                        )
                      )
                    )
      }
    })
    
  })
  
  observeEvent(input$stalking, {
    output$latest_tweet_ui <- renderUI({
      isolate(input$userid)
      if(!is.null(tweets())){
        bs4SocialCard(width = 12, solidHeader = TRUE, collapsible = TRUE, closable = FALSE, 
                      title = "Latest Tweet", src = unique(tweets()$tweets_data$profile_image_url),
                      subtitle = paste(tweets()$tweets_data$created_at[1], "UTC"),
                      tweets()$tweets_data$text[1]
        )
      } else {
        return(NULL)
      }
    })
  })
  
  observeEvent(input$stalking, {
    if(is.null(tweets())){
      return(NULL)
    } else if(nrow(tweets()$tweets_data) == 0){
      return(NULL)
    } else {
      output$source_tweets_ui <- renderUI({
        isolate(input$userid)
        output$source_tweets <- renderDataTable({
          tweets()$sources %>%
            datatable(options = list(pageLength = 5), rownames = FALSE)
        })
        
        bs4Card(width = 12, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = paste("Tweet Sources", sprintf(" (Based on the last about %s statuses)", nrow(tweets()))),
                dataTableOutput("source_tweets")
        )
        
      })
    }
  })
  
  observeEvent(input$stalking, {
    output$most_mentioned_ui <- renderUI({
      if(!is.null(tweets())){
        # output$most_mentioned <- renderDataTable({
        output$most_mentioned <- renderPlot({
          isolate(input$userid)
          tweets()$mentioned %>%
            ggplot(aes(x = `Mentioned Username`, y = `Times Mentioned`)) + 
            geom_col(fill = "lightblue") +
            coord_flip() + 
            theme_light()
        }, height = 450)
        bs4Card(width = 12, height = 500, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = "The Most 10 Mentioned",
                plotOutput("most_mentioned")
        )
      } else {
        return(NULL)
      }
    })
  })

    observeEvent(input$stalking, {
    output$nchar_tweets_ui <- renderUI({
      if(!is.null(tweets())){
        output$nchar_tweets <- renderPlot({
          isolate(input$userid)
          tweets()$nchars %>%
            ggplot(aes(x = display_text_width)) +
            geom_histogram(fill = "lightblue", color = "white", bins = 30) +
            scale_x_continuous(breaks = seq(0, 300, by = 70)) +
            labs(x = "Number of character in a tweet") +
            theme_light()
        })
        bs4Card(width = 12, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = "Distribution of Number of Character in Tweets",
                plotOutput("nchar_tweets")
        )
      } else {
        return(NULL)
      }
    })
  })
    
    observeEvent(input$stalking, {
      output$time_tweets_ui <- renderUI({
        if(!is.null(tweets())){
          output$time_tweets <- renderPlot({
            isolate(input$userid)
            tweets()$times %>% 
              ggplot(aes(x = created_date, y = n, group = is_retweet, color = is_retweet)) +
              geom_line() +
              scale_fill_manual(values = c("lightblue", "peach")) +
              labs(x = "Tweets Date", y = "Number of Statuses", color = "Status Type", caption = "* Filtered for the past year") +
              theme_light()
          })
          bs4Card(width = 12, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                  title = "Number of Tweets Daily",
                  plotOutput("time_tweets")
          )
        } else {
          return(NULL)
        }
      })
    })
  
  observeEvent(input$stalking, {
    output$hashtags_ui <- renderUI({
      if(!is.null(tweets())){
        # output$hashtags <- renderDataTable({
        output$hashtags <- renderPlot({
          isolate(input$userid)
          tweets()$hashtags %>%
            ggplot(aes(x = hashtags, y = n)) + 
            geom_col(fill = "lightblue") +
            labs(x = "Hashtags", y = "Number of Tweets") +
            coord_flip() + 
            theme_light()
        }, height = 450)
        bs4Card(width = 12, height = 500, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = "The Most 10 #Hashtags",
                plotOutput("hashtags")
        )
      } else {
        return(NULL)
      }
    })
  })
  
  observeEvent(input$stalking, {
    if(is.null(tweets())){
      return(NULL)
    } else if(nrow(tweets()$tweets_data) == 0){
      return(NULL)
    } else {
      
      output$sentiment_tweets_ui <- renderUI({
        isolate(input$userid)
        
        output$sentiment_posneg <- renderPlot({
          isolate(input$userid)
          
          # Set up the plot with aes()
          tweets()$sentiment_posneg %>% 
            ggplot(aes(x = word, y = abs(n), fill = sentiment_bing)) +
            geom_col() +
            labs(y = "# occurences", fill = "Sentiment") +
            coord_flip() +
            theme_light() + 
            theme(legend.position = "top")
        })
        
        output$sentiment_feel <- renderPlot({
          isolate(input$userid)
          
          # Set up the plot with aes()
          sentiments <- tweets()$sentiment_feel %>% 
            ggplot(aes(x = word, y = n, fill = sentiment_nrc)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ sentiment_nrc, ncol = 4, scales = "free_y") +
            labs(y = "# occurences") +
            coord_flip() +
            theme_light()
          
          elapsed_time <- Sys.time() - start_time
          shinyalert(type = "success", title = "Done!", timer = 10*1000,
                     text = sprintf("Elapsed time: %s %s\nWaiting for the outputs", round(elapsed_time, 2), attributes(elapsed_time)$units))
          sentiments
        })
        
        bs4Card(width = 12, height = 600, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = fluidRow(h5("Sentiment Analysis", style="margin-top:5px;margin-right:20px;")),
                fluidRow(
                  column(4,
                         plotOutput("sentiment_posneg", height = 550)
                         ),
                  column(8,
                         plotOutput("sentiment_feel", height = 550)
                         )
                  )
                )
        })
    }
  })
  
  observeEvent(input$help, {
    showModal(
      modalDialog(easyClose = FALSE, 
                  title = fluidRow(img(src="img/help.png", width = 25, height = 25), h5("FAQ", style="font-weight:bold;margin-left:10px;")), 
                  faqtext, 
                  footer = modalButton("Close"))
      )
  })
}

shinyApp(ui, server)

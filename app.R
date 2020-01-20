require(shiny)
library(shinydashboard)
## TEXT ANALYSIS LIBRARIES
library(stringr)
library(tm)
library(qdap)
library(NLP)
## DATA MANIPULATION LIBRARIES
library(dplyr)
library(reshape2)
library(data.table)
## VISUALIZATION LIBRARIES
library(scales)
library(RColorBrewer)
library(plotly)
library(wordcloud)
library(ggplot2)
library(ggrepel)
## WEB ACCESS LIBRARIES
library(jsonlite)

options(shiny.sanitize.errors = TRUE)

allsotu <- readRDS("./data/SOTUdata.rds")
entities <- readRDS("./data/entities.rds")

ui <- dashboardPage(
  dashboardHeader(title = "SOTU Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pick a Speech", tabName = "byspeech", icon = icon("commenting")),
      menuItem("Compare All Speeches", tabName = "comparespeeches", icon = icon("microphone")),
      menuItem("Compare Presidents", tabName = "comparepresidents", icon = icon("users")),
      menuItem("Words By President", tabName = "wordsbypresident", icon = icon("line-chart")),
      menuItem("Words Through Time", tabName = "throughtime", icon = icon("calendar")),
      menuItem("Help and Acknowledgments", tabName = "help", icon = icon("question-circle")),
      tags$br(),tags$br(),
      tags$ul(tags$i("A ",
             tags$a(href="https://www.brandonkopp.com","brandonkopp.com"), " creation.")),
      tags$style(type="text/css",
                 ".shiny-output-error {visibility: hidden;}",
                 ".shiny-output-error:before {visibility: hidden;}"),
      tags$style('.entity_style {
      border: 0px solid white !important;
      color: white !important;
      }'),
      tags$script(
        'function moveVal(itemid) {
          var txt=document.getElementById(itemid).innerHTML;
          document.getElementById("entityword").value=txt;
          var element = document.getElementById("entityword");
          var event = new Event("change");
          element.dispatchEvent(event);
        }'
      )
    )
  ),
  dashboardBody(
    tabItems(
      ##### COMPARE PRESIDENTS #####
      tabItem(tabName = "comparepresidents",
              fluidRow(
                box(title = "Select President #1",width=6,status="info",solidHeader = TRUE,
                    column(12,style="min-width:200px", align="center",
                           htmlOutput("presImage1")
                    ),
                    column(12, style="height:50px;margin-top:-15px", align="center",
                           selectInput("selcomppres1","", choices = unique(allsotu$president), 
                                       selected="Donald Trump", selectize = FALSE,
                                       width = 200)
                    )
                ),
                box(title = "Select President #2",width=6,status="warning",solidHeader = TRUE,
                    column(12, style="min-width:200px;",align="center",
                           htmlOutput("presImage2")
                    ),
                    column(12, style="height:50px; margin-top:-15px",align="center",
                           selectInput("selcomppres2","", choices = unique(allsotu$president), 
                                       selected="Barack Obama", selectize = FALSE,
                                       width = 200)
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("pres1speeches",width = 3),
                valueBoxOutput("pres1words",width = 3),
                valueBoxOutput("pres2speeches",width = 3),
                valueBoxOutput("pres2words",width = 3)
              ),
              fluidRow(
                box(title="Most Frequent Words Across All Speeches",width = 6,status = "info",solidHeader = TRUE,
                    sliderInput("pres1seltopn", "Top n Words:", 5, 50, 15, step=5),
                    plotOutput("pres1freqplot", height="500")
                ),
                box(title="Most Frequent Words Across All Speeches",width = 6,status = "warning",solidHeader = TRUE,
                    sliderInput("pres2seltopn", "Top n Words:", 5, 50, 15, step=5),
                    plotOutput("pres2freqplot", height="500")
                )
              ),
              fluidRow(
                box(title="Comparison Wordcloud",width = 6,status = "success",
                    sliderInput("compselwords", "Max # of Words:", 50, 500, 250, step=50),
                    plotOutput("prescompcloud", height="500"),
                    footer = HTML("A comparison wordcloud shows the relative frequency of word usage. Word size in this cloud indicates how many <b>more</b> times it was used by one president than the other.")
                ),
                box(title="Commonality Wordcloud",width = 6,status = "success",
                    sliderInput("commselwords", "Max # of Words:", 50, 500, 250, step=50),
                    plotOutput("prescommcloud", height="500"),
                    footer = "A commonality wordcloud also shows the relative frequency of words, but only those that appear in speeches by both presidents."
                )
              ),
              fluidRow(
                box(title="Quantitative Comparisons",height=525,width = 6,status = "success",
                    column(6,
                           selectInput("xvar","X-Axis Variable", choices = c("Number of Words","Type Token Ratio (TTR)", 
                                                                             "Flesch-Kincaid Grade", "Flesch-Kincaid Age","Sentiment"),
                                       selected = "Number of Words")
                    ),
                    column(6,
                           selectInput("yvar","Y-Axis Variable", choices = c("Number of Words","Type Token Ratio (TTR)", 
                                                                             "Flesch-Kincaid Grade", "Flesch-Kincaid Age","Sentiment"),
                                       selected = "Flesch-Kincaid Grade")
                    ),
                    plotOutput("compscatter", height="375")
                ),
                box(title="Word Usage",height=525,width = 6,status = "success",
                    column(9,
                           textInput("comppresword","Word to search:",
                                     value="economy, war",
                                     placeholder = "Enter term(s) separated by commas")     
                    ),
                    column(3,
                           tags$br(),
                           actionButton("submitcomppres", "Submit")  
                    ),
                    plotOutput("compbar", height="375")
                )
              )
      ),
      ############################
      ##### COMPARE SPEECHES #####
      tabItem(tabName = "comparespeeches",
              fluidRow(
                box(title = "Select Variables",width=12,status="primary",solidHeader = TRUE,
                    column(4,
                           selectInput("xvarspeech","X-Axis Variable", choices = c("Number of Words","Type Token Ratio (TTR)", 
                                                                                   "Flesch-Kincaid Grade", "Flesch-Kincaid Age","Sentiment","Year"),
                                       selected = "Number of Words")
                    ),
                    column(4,
                           selectInput("yvarspeech","Y-Axis Variable", choices = c("Number of Words","Type Token Ratio (TTR)", 
                                                                                   "Flesch-Kincaid Grade", "Flesch-Kincaid Age","Sentiment"),
                                       selected = "Flesch-Kincaid Grade")
                    ),
                    column(4,
                           selectInput("cvarspeech","Select Color Variable:",c("Party","Delivery Method","Era","President"),
                                       selected = "Party")
                    )
                ),
                
                fluidRow(
                  column(12,
                         box(title="Compare All Speeches on Various Metrics",width = 12,status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("speechscatter", height=600, width="100%")
                         )
                  )
                )
              )),
      ############################
      ###### BY SPEECH #####
      tabItem(tabName = "byspeech",
              fluidRow(
                box(title = "Select a President and Speech",width = 9,status="primary",solidHeader = TRUE,
                    column(3, style="min-width:175px",
                           htmlOutput("presSpeechImage1")
                    ),
                    column(3,style="min-width:150px",
                           selectInput("presspeech","Select President:", choices = unique(allsotu$president), 
                                       selected = "Donald Trump", selectize = FALSE, width=150),
                           selectInput("yearspeech","Select Year:", choices = unique(allsotu$year), 
                                       selectize = FALSE, width=150)
                    ),
                    column(6,
                           HTML(paste0("<h4>Take a Deep Dive</h4>",
                                       "<p style='font-size:1.1em'>This page allows you to dig into a specific speech. ", 
                                       "Select a president and a year and the page will update with metrics of interest from ",
                                       "a particular speech, including key words and phrases and sentiment.</p>"))
                           
                    )
                ),
                fluidRow(
                  valueBoxOutput("politicalparty", width=3),
                  valueBoxOutput("deliverymethod", width=3)
                )
              ),
              fluidRow(
                valueBoxOutput("spcwords",width = 3),
                valueBoxOutput("sentwords",width = 3),
                valueBoxOutput("spcttr",width = 3),
                valueBoxOutput("spcfkgrade",width = 3)
              ),
              fluidRow(
                box(title="Word Frequency",width = 12,status = "primary",
                    solidHeader = TRUE,
                    column(6,
                           sliderInput("seltopn", "Top n Words:", 5, 50, 15, step=5),
                           plotOutput("freqplot", height="500")
                    ),
                    column(6,
                           sliderInput("selwords", "Max # of Words:", 50, 500, 250, step=50),
                           plotOutput("speechcloud", height="500")
                    )
                )
              ),
              fluidRow(
                box(title="Sentiment Analysis",width = 12,status = "primary",
                    solidHeader = TRUE,
                    column(6,
                           tags$h3("Sentiment Across Speech"),
                           tags$p("This plot shows the polarity, or the relative use of positively and negatively valenced words, for each sentence in the speech."),
                           tags$br(),
                           plotOutput("spsentplot", height="500")
                    ),
                    column(6,
                           sliderInput("selsentwords", "Max # of Words:", 50, 500, 250, step=50),
                           plotOutput("sentcloud", height="500")
                    )
                )
              ),
              fluidRow(
                box(title="New Words",width = 12,status = "primary",
                    solidHeader = TRUE,
                    tags$h3(textOutput("newwordslabel")),
                    tags$head(tags$style("#newwords{font-size:1.2em}")),
                    tags$p(htmlOutput("newwords")),
                    footer = "Some of the never before seen words are mispellings or words that were combined in error during transcription or data collection for this app."
                )
              ),
              fluidRow(
                box(title="People, Places, and Things",width = 12,status = "primary",
                    solidHeader = TRUE,
                    column(3,
                           tags$h3(textOutput("ent_people_label")),
                           tags$hr(),
                           tags$head(tags$style("#ent_people{font-size:1.1em}")),
                           tags$p(htmlOutput("ent_people"))
                    ),
                    column(3,
                           tags$h3(textOutput("ent_places_label")),
                           tags$hr(),
                           tags$head(tags$style("#ent_places{font-size:1.1em}")),
                           tags$p(htmlOutput("ent_places"))
                    ),
                    column(3,
                           tags$h3(textOutput("ent_groups_label")),
                           tags$hr(),
                           tags$head(tags$style("#ent_groups{font-size:1.1em}")),
                           tags$p(htmlOutput("ent_groups"))
                    ),
                    column(3,
                           tags$h3(textOutput("ent_laws_label")),
                           tags$hr(),
                           tags$head(tags$style("#ent_laws{font-size:1.1em}")),
                           tags$p(htmlOutput("ent_laws")),
                           htmlOutput('entityfield')
                    ),
                    footer = "Entities extracted in Python using the spaCy package."
                )
              ),
              fluidRow(
                box(title="Words in Context",width = 12,status = "primary",
                    solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             textInput("speechword","Words to search:",
                                       value="economy, jobs, employment|employed",
                                       placeholder = "Enter terms separated by commas")
                      ),
                      column(2,
                             tags$br(),
                             actionButton("submitspeech", "Submit")
                      )),
                    dataTableOutput("speechwordsincontext")
                    
                )
              )
      ),
      ##############################
      ##### WORDS BY PRESIDENT ######
      tabItem(tabName = "wordsbypresident",
              fluidRow(
                box(title = "Select a President and Enter Search Terms",width = 12,status="primary",solidHeader = TRUE,
                    column(3,
                           selectInput("pres","Select President:", choices = unique(allsotu$president), 
                                       selected="Donald Trump", selectize = FALSE)
                    ),
                    column(6,
                           textInput("presword","Words to search:",
                                     value="economy, employment|jobs, pay|wages, stock market, tariff|tax, trade",
                                     placeholder = "Enter terms separated by commas"),
                           actionButton("submitpres", "Submit")
                    ),footer = "It may take up to a minute for charts to appear below. Please be patient. \n To read more about how to optimize your search terms visit the 'Help and Acknowledgments' tab."
                )
              ),
              fluidRow(
                box(title="Word Usage in the State of the Union",width = 12,status = "primary",
                    solidHeader = TRUE,
                    plotOutput("wordplot", height=700,width = "100%")
                ),
                box(title="Words in Context",width = 12,status = "primary",
                    solidHeader = TRUE,
                    dataTableOutput("preswordsincontext")
                )
              )
      ),
      ##############################
      ##### WORDS THROUGH TIME #####
      tabItem(tabName = "throughtime",
              fluidRow(
                box(title = "Enter Search Terms",width=8,status="primary",solidHeader = TRUE,
                    column(5,
                           textInput("words","Words To Search:",value="War, Peace",
                                     placeholder = "Enter up to 3 terms separated by commas"),
                           actionButton("submit", "Submit")
                    ),
                    column(4,
                           selectInput("colorvar","Select Color Variable:",c("Party","Delivery Method"),
                                       selected = "Party")
                    )
                ),
                box(title = "Controls",width=4,status="primary",solidHeader = TRUE,
                    sliderInput("scale", "Select Circle Scale Size:", 4, 40, 20, step=4)
                )
              ),
              
              fluidRow(
                box(title="Word Usage in the State of the Union",width = 12,status = "primary",
                    solidHeader = TRUE,
                    plotOutput("timeplot", height=700,width = "100%"),
                    downloadButton('download_timeplot', 'Download Plot')
                )
              )
      ),
      #####################################
      ##### HELP AND ACKNOWLEDGEMENTS #####
      tabItem(tabName = "help",
              tags$h2("The State of the Union Exploration App"),
              tags$p("With this application, you can conduct your own text analysis on every State of the Union Address from George Washington's 1790 address up to President Obama's final address in 2016."),
              tags$p("I plan to add to this application as new speeches are given and as I learn more about text analysis, so stop back."),
              tags$p("This application is divided into five pieces."),
              tags$ul( 
                tags$li("Pick a Speech - Choose a particular speech and explore word frequencies through charts and wordclouds. Also, view sentiment (the use of positively and negatively valenced words) across the speech."),
                tags$li("Compare All Speeches - View a scatterplot of various speech metrics like number of words or sentiment."),
                tags$li("Compare Presidents - Choose two presidents and see how their use of words compare to one another"),
                tags$li("Words by President - Choose a president and enter your own search terms and see how often that president used those words across their presidency."),
                tags$li("Words Through Time - Enter your own search terms and see how the use of those words has changed over time in SOTU addresses")
              ),
              tags$h2("Getting Started"),
              tags$h3("Saving Plots"),
              tags$p("You can save any of the plots by right-clicking on them and selecting 'Save Image As...'"),
              tags$h3("Entering Search Terms"),
              tags$p("For any place where there is an open text box where you can enter words, there are a couple things to keep in mind:"),
              tags$ul(
                tags$li("Separate each search term with a comma."),
                tags$li("Capitalization doesn't matter. For example, whether you search for 'Lincoln' or 'lincoln' all instances of the word 'Lincoln' will be found"),
                tags$li("You cannot use partial words. You must type in exactly what you are looking for. If you want both 'manufacturing' and manufacturer', you cannot simply type in 'manuf'."),
                tags$li("If you would like to search for multiple, similar words and have them combined in your analysis use the pipe operator ('|'). For example, typing 'manufacturing|manufacturer' will return all instance of either word. Think of the pipe as the word 'or'.")
              ),
              tags$h3("Color Coding For Summary Statistic Boxes"),
              tags$p("On several pages, there are boxes that show a summary statistic (e.g., the number of words used). If the box is green, that means it is greater than or equal to the average on that metric. If it is red, that means it is less than the average.  This color coding is not consistent on all pages."),
              tags$h2("Glossary"),
              tags$ul(
                tags$li(tags$b("Comparison Wordcloud"), " - A comparison cloud is different from a regular wordcloud in that the size of the words indicates the relative use of that word. Because a word does not appear for one president, doesn't mean that he didn't use it. It just means the other president used it more."),
                tags$li(tags$b("Flesch-Kincaid Age Level"), " - Similar to the Grade Level number, this number indicates the approximate age that one would need to be to understand the content of the speech. This is linearly related to grade level."),
                tags$li(tags$b("Flesch-Kincaid Grade Level"), " - The Flesch-Kincaid test is a measure of readability. This number indicates the approximate grade level one would have to be in to undrestand the content in the speech."),
                tags$li(tags$b("Sentiment"), " - The relative use of positive and negative words in a block of words (e.g., a speech, a sentence, etc.).  Sentiment values greater than 0 indicate a greater use of positively valenced words (e.g., good, peace). Values less than 0 indicate greater use of negatively valenced words (e.g., bad, war). Values equal to 0 indicate either the use of all neutral words or equal use of positive and negative words."),
                tags$li(tags$b("Token Type Ratio (TTR)"), " - The number of unique words (types) divided by the total number of words (tokens). The closer this is to 1.0, the more diverse the choice in words. Higher numbers signal more difficult to understand text.")
                ),
              tags$h2("Acknowledgments"),
              tags$p("To build this app, I borrowed work from a number of different sources. I would like to thank them. Please visit the sites below for more information."),
              tags$ul(
                tags$li(tags$a(href="http://stateoftheunion.onetwothree.net/texts/index.html","onetwothree.net"), " - This site has full text from every State of the Union Address which was the heart of the analysis in this app. Brad Borevitz is the owner of the page and I would like to thank him for his effort."),
                tags$li(tags$a(href="http://australianpolitics.com/united-states-of-america/president/list-of-presidents-of-the-united-states","australianpolitics.com"), " - This site has a table with some basic information about each president. I primarily used this to get political parties."),
                tags$li(tags$a(href="http://www.presidency.ucsb.edu/sou.php","presidency.ucsb.edu"), " - This page has information about whether a speech was delivered in the form of a speech in front of congress (i.e., orally) or in writing."),
                tags$li(tags$a(href="http://www.history.com/topics/us-presidents","history.com"), " - This is the page where I got the presidential photos."),
                tags$li(tags$a(href="http://www.r-bloggers.com/statistics-meets-rhetoric-a-text-analysis-of-i-have-a-dream-in-r/","R-Blogger Article on Sentiment Analysis"), " - This article was very helpful in learning to use qdap and create my functions for sentiment analysis. Most of the code was borrowed directly."),
                tags$li(tags$a(href="https://rstudio.github.io/shinydashboard/","Shiny Dashboard on Github"), " - This application was built using R, R Studio, Shiny, and Shiny Dashboard. Check out this webpage to get started with building your own.")
              ),
              tags$h2("About Me"),
              tags$p("My name is Brandon. I work as a research psychologist for the U.S. government and am starting to dabble in the field of data science. I built this application as a means of learning Python, R, data visualization, and text analysis. I love programming and data analysis and plan to build many more applications like this."),
              tags$p("If any of this sparked your curiosity, check out ", tags$a(href="http://rpubs.com/brandonkopp", "my RPubs.com page"), " where I am writing a series of how-to articles about the various R packages, data visualizations, and analytical techniques used in this application."),
              tags$p("You can also check out my personal homepage, ", tags$a(href="https://brandonkopp.com","brandonkopp.com"), " for more information about me and my various other hobbies."),
              tags$h3("Thanks for Viewing!")
      )
      ######################################
      )
    )
)

server <- function(input, output, session) { 
  source("global.R", local=TRUE)
  
  ##### COMPARE PRESIDENTS FUNCTIONS #####
  output$presImage1 = renderUI({
    filename <- unique(allsotu[allsotu$president == input$selcomppres1, "links"])
    tags$img(src = filename, alt=input$selcomppres1, width=200, style="border-radius:7px")  
  })
  
  output$presImage2 = renderUI({
    filename <- unique(allsotu[allsotu$president == input$selcomppres2, "links"])
    tags$img(src = filename, alt=input$selcomppres2, width=200, style="border-radius:7px")  
  })
  
  output$pres1speeches <- renderValueBox({
    speeches <- nrow(allsotu[allsotu$president == input$selcomppres1, ])
    valueBox(
      value = formatC(speeches, digits = 0, format = "f"),
      subtitle = "Number of Speeches",
      icon = icon("microphone"),
      color = "aqua"
    )
  })
  
  output$pres1words <- renderValueBox({
    words <- mean(allsotu[allsotu$president == input$selcomppres1, "tokens"])
    valueBox(
      value = formatC(words, digits = 0, format = "f"),
      subtitle = "Avg. Words per Speech",
      icon = icon("book"),
      color = "aqua"
    )
  })
  
  output$pres2speeches <- renderValueBox({
    speeches <- nrow(allsotu[allsotu$president == input$selcomppres2, ])
    valueBox(
      value = formatC(speeches, digits = 0, format = "f"),
      subtitle = "Number of Speeches",
      icon = icon("microphone"),
      color = "yellow"
    )
  })
  
  output$pres2words <- renderValueBox({
    words <- mean(allsotu[allsotu$president == input$selcomppres2, "tokens"])
    valueBox(
      value = formatC(words, digits = 0, format = "f"),
      subtitle = "Avg. Words per Speech",
      icon = icon("book"),
      color = "yellow"
    )
  })
  
  pres1_tdm <- reactive({
    withProgress(message = 'Calculating Word Frequencies for President #1', value = 0, {
        speech <- allsotu[allsotu$president == input$selcomppres1, "speechtext"]
        speech <- paste(speech, collapse = " ")
        make_tdm(speech)
    })
  })
  
  output$pres1freqplot <- renderPlot({
    topncount(pres1_tdm(), top=input$pres1seltopn,col="lightskyblue")
  })
  
  pres2_tdm <- reactive({
    withProgress(message = 'Calculating Word Frequencies for President #2', value = 0, {
      speech <- allsotu[allsotu$president == input$selcomppres2, "speechtext"]
      speech <- paste(speech, collapse = " ")
      make_tdm(speech)
    })
  })
  
  output$pres2freqplot <- renderPlot({
    topncount(pres2_tdm(), top=input$pres2seltopn,col="orange")
  })
  
  output$prescompcloud <- renderPlot({
    withProgress(message = 'Building Comparison Wordcloud', value = 0, {
      pres1 <- paste(allsotu[allsotu$president ==input$selcomppres1, "speechtext" ], collapse = " ")
      pres2 <- paste(allsotu[allsotu$president ==input$selcomppres2, "speechtext" ], collapse = " ")
      
      x <- data.frame(c(input$selcomppres1,input$selcomppres2), rbind(pres1,pres2), stringsAsFactors = FALSE)
      
      names(x) <- c("president","speechtext")
      
      suppressWarnings(compcloud(x, max = input$compselwords))
    })
  })
  
  output$prescommcloud <- renderPlot({
    withProgress(message = 'Building Commonality Wordcloud', value = 0, {
      pres1 <- paste(allsotu[allsotu$president ==input$selcomppres1, "speechtext" ], collapse = " ")
      pres2 <- paste(allsotu[allsotu$president ==input$selcomppres2, "speechtext" ], collapse = " ")
      
      x <- data.frame(c(input$selcomppres1,input$selcomppres2), rbind(pres1,pres2), stringsAsFactors = FALSE)
      
      names(x) <- c("president","speechtext")
      
      suppressWarnings(commcloud(x, max = input$commselwords))
    })
  })
  
  output$compscatter <- renderPlot({
    xIn <- switch(input$xvar, 
                  "Number of Words" = "tokens",
                  "Type Token Ratio (TTR)" = "ttr",
                  "Flesch-Kincaid Grade" = "fkgrade",
                  "Flesch-Kincaid Age" = "fkage",
                  "Sentiment" = "polarity"
    )
    yIn <- switch(input$yvar, 
                  "Number of Words" = "tokens",
                  "Type Token Ratio (TTR)" = "ttr",
                  "Flesch-Kincaid Grade" = "fkgrade",
                  "Flesch-Kincaid Age" = "fkage",
                  "Sentiment" = "polarity"
    )
    withProgress(message = 'Building Speech Comparison Scatterplot', value = 0, {
        df <- allsotu[allsotu$president %in% c(input$selcomppres1,input$selcomppres2), c("president","year","delivery","tokens","ttr","fkgrade","fkage","polarity")]
        df$order[df$president == input$selcomppres1] <- input$selcomppres1
        df$order[df$president == input$selcomppres2] <- input$selcomppres2
        df$order <- factor(df$order, c(input$selcomppres1,input$selcomppres2), ordered = TRUE)
        
        ggplot(df,aes_string(x=xIn, y=yIn)) +
          geom_point(aes(color=order,pch=delivery),size=3, alpha=0.6) +
          geom_text_repel(aes(label=year), vjust=1.7, size=4) +
          scale_color_manual(values = c("lightskyblue","orange")) +
          expand_limits(y=0, x=0) +
          labs(
            title = "Word Count",
            x = input$xvar,
            y = input$yvar,
            color = "President",
            pch= "Delivery Method"
          ) +
          theme(plot.title = element_blank(),
                plot.background = element_rect(fill = 'white', colour = 'white'),
                panel.border = element_rect(fill = NA, color = 'white', size = 2),
                panel.background = element_rect(fill = 'white', colour = 'white'),
                panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
                panel.grid.minor = element_blank(),
                axis.text = element_text(size = 12, color="black", face="bold"),
                axis.title = element_text(size = 14, face="bold", color = "black"),
                axis.ticks = element_blank(),
                axis.line = element_line(colour = "black", size=1),
                legend.background = element_blank(),
                legend.key = element_blank(),
                legend.text = element_text(size = 12, color= "black"),
                legend.title = element_text(size = 14,face="bold"),
                legend.position = "right") +
          guides(colour = guide_legend(override.aes = list(size=4)),
                 pch = guide_legend(override.aes = list(size=4)))
    })
  })
  
  output$compbar <- renderPlot({
    input$submitcomppres
    withProgress(message = 'Building Word Comparison Bar Chart', value = 0, {
        df <- allsotu[allsotu$president %in% c(input$selcomppres1,input$selcomppres2), ]
        
        wordlist <- isolate(stringsplitter(input$comppresword))
        counts <- strcounts(df, wordlist, only=TRUE)
        
        speech_count <- df %>% group_by(president) %>%
          summarize(speechcount = n())
        
        new <- counts %>% group_by(name, term) %>%
          summarize(count = sum(count)) %>%
          left_join(speech_count, by=c("name"="president")) %>%
          mutate(rate = count/speechcount)
        
        new$name <- factor(new$name, c(input$selcomppres1,input$selcomppres2), ordered = TRUE)
        
        ggplot(new,aes(x=term, y=round(rate,2))) +
          geom_bar(stat="identity",aes(fill=name), position="dodge") +
          scale_fill_manual(values = c("lightskyblue","orange")) +
          scale_y_continuous(expand = c(0,0)) +
          labs(
            title = "Word Count",
            x = "Word",
            y = "Average Uses per Speech",
            fill = "President"
          ) +
          theme(plot.title = element_blank(),
                plot.background = element_rect(fill = 'white', colour = 'white'),
                panel.border = element_rect(fill = NA, color = 'white', size = 2),
                panel.background = element_rect(fill = 'white', colour = 'white'),
                panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
                panel.grid.minor = element_blank(),
                axis.text = element_text(size = 12, color="black", face="bold"),
                axis.title = element_text(size = 14, face="bold", color = "black"),
                axis.ticks = element_blank(),
                axis.line = element_line(colour = "black", size=1),
                legend.background = element_blank(),
                legend.key = element_blank(),
                legend.text = element_text(size = 12, color= "black"),
                legend.title = element_text(size = 14,face="bold"),
                legend.position = "right")
    })
    
  })
  ######################################
  ##### COMPARE SPEECHES FUNCTIONS #####
  output$speechscatter <- renderPlotly({
    xInSp <- switch(input$xvarspeech, 
                    "Number of Words" = "tokens",
                    "Type Token Ratio (TTR)" = "ttr",
                    "Flesch-Kincaid Grade" = "fkgrade",
                    "Flesch-Kincaid Age" = "fkage",
                    "Sentiment" = "polarity",
                    "Year" = "year"
    )
    yInSp <- switch(input$yvarspeech, 
                    "Number of Words" = "tokens",
                    "Type Token Ratio (TTR)" = "ttr",
                    "Flesch-Kincaid Grade" = "fkgrade",
                    "Flesch-Kincaid Age" = "fkage",
                    "Sentiment" = "polarity"
    )
    
    cInSp <- switch(input$cvarspeech, 
                    "Party" = "Party",
                    "Delivery Method" = "delivery",
                    "Era" = "timespan",
                    "President" = "president"
    )
    
    df <- allsotu[ ,c("president","Party","year","delivery","tokens","ttr","fkgrade","fkage","polarity","merge","timespan")]
    
    g <- ggplot(df,aes_string(x=xInSp, y=yInSp)) +
      geom_point(aes_string(color=cInSp,pch=cInSp, text="merge"),size=2, alpha=0.5) +
      labs(
        x = input$xvarspeech,
        y = input$yvarspeech,
        color = input$cvarspeech,
        pch= input$cvarspeech
      ) +
      theme(plot.title = element_blank(),
            plot.background = element_rect(fill = 'white', colour = 'white'),
            panel.border = element_rect(fill = NA, color = 'white', size = 2),
            panel.background = element_rect(fill = 'white', colour = 'white'),
            panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 10, color="black", face="bold"),
            axis.title = element_text(size = 12, face="bold", color = "black"),
            axis.ticks = element_blank(),
            axis.line = element_line(colour = "black", size=1),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size = 10, color= "black"),
            legend.title = element_text(size = 12,face="bold"),
            legend.position = "right") +
      guides(colour = guide_legend(override.aes = list(size=4)),
             pch = guide_legend(override.aes = list(size=4)))
    
    (gg <- ggplotly(g, tooltip = "text", height=650, width=900))
  })
  
  ###########################################  
  ###### BY SPEECH FUNCTIONS ######
  observe({
    newchoice <- allsotu[allsotu$president == input$presspeech, "year"]
    newchoice <- sort(newchoice, decreasing=TRUE)
    updateSelectInput(session, inputId = "yearspeech",choices = newchoice)
  })
  
  speech_text <- reactive({
    allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, "speechtext"]
  })
  
  speech_tdm <- reactive({
    withProgress(message = 'Calculating Word Frequency', value = 0, {
      make_tdm(speech_text())
    })
  })
  
  output$freqplot <- renderPlot({
    withProgress(message = 'Building Most Used Terms Plot', value = 0, {
      topncount(speech_tdm(), top=input$seltopn)
    })
  })
  
  output$speechcloud <- renderPlot({
    withProgress(message = 'Building Wordcloud', value = 0, {
      suppressWarnings(cloud(speech_tdm(), words = input$selwords))
    })
  })
    
  output$spsentplot <- renderPlot({
    withProgress(message = 'Calculating Sentiment Analysis', value = 0, {
      spsentgraph(speech_text())
    })
  })
  
  output$sentcloud <- renderPlot({
    withProgress(message = 'Building Sentiment Wordcloud', value = 0, {
      sentiment <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, c("pos.words","neg.words")]
      suppressWarnings(spsentcloud(sentiment, maxwords = input$selsentwords))
    })
  })
  
  output$spcwords <- renderValueBox({
    words <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, "tokens"]
    valueBox(
      value = formatC(words, digits = 0, format = "f"),
      subtitle = "Number of Words",
      icon = icon("book"),
      color = if (words >= mean(allsotu$tokens)) "green" else "red"
    )
  })
  
  output$sentwords <- renderValueBox({
    sent <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, "polarity"]
    valueBox(
      value = formatC(sent, digits = 3, format = "f"),
      subtitle = "Overall Sentiment",
      icon = icon("smile-o"),
      color = if (sent >= 0) "green" else "red"
    )
  })
  
  output$spcttr <- renderValueBox({
    ttr <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, "ttr"]
    valueBox(
      value = formatC(ttr, digits = 3, format = "f"),
      subtitle = "Type Token Ratio (TTR)",
      icon = icon("calculator"),
      color = if (ttr >= mean(allsotu$ttr)) "green" else "red"
    )
  })
  
  output$spcfkgrade <- renderValueBox({
    fkg <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, "fkgrade"]
    valueBox(
      value = formatC(fkg, digits = 1, format = "f"),
      subtitle = "Flesch-Kincaid Grade Score",
      icon = icon("mortar-board"),
      color = if (fkg >= mean(allsotu$fkg)) "green" else "red"
    )
  })

  output$deliverymethod <- renderValueBox({
    fkg <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, "delivery"]
    valueBox(
      value = simpleCap(fkg),
      subtitle = "Delivery Method",
      icon = if (fkg == "Oral") icon("microphone") else icon("file"),
      color = "green"
    )
  })
  
  output$politicalparty <- renderValueBox({
    fkg <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, "Party"]
    valueBox(
      value = simpleCap(fkg),
      subtitle = "Political Party",
      icon = icon("flag-usa"),
      color = "green"
    )
  })
  
  output$presSpeechImage1 <- renderUI({
    filename <- unique(allsotu[allsotu$president == input$presspeech, "links"])
    tags$img(src = filename, alt=input$presspeech, width=165, style="border-radius:7px")  
  })
  
  output$speechwordsincontext <- renderDataTable({
    input$submitspeech
    withProgress(message = 'Extracting Words in Context', value = 0, {
      wordlist <- isolate(stringsplitter(input$speechword))
      subPres <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech, ]
      context(sotu=subPres,wordlist)
    })
  }, options = list(
    lengthMenu = list(c(10, 20, 30, -1), c('10', '20', '30', 'All')),
    pageLength = 10))
  
  output$newwords <- renderUI({
    new_words <- entities[entities$president == input$presspeech & entities$year == input$yearspeech, "new_word"][[1]]
    ents <- do.call(paste, c(as.list(paste0("<a id='", change_text(sort(new_words)), "' href='#' onclick=moveVal('",change_text(sort(new_words)),"')>", sort(new_words),"</a>")), sep = ", "))
    HTML(ents)
  })
  
  output$newwordslabel <- renderText({
    num_new_words <- length(entities[entities$president == input$presspeech & entities$year == input$yearspeech, "new_word"][[1]])
    paste(num_new_words, "words/names never before used in a State of the Union Address")
  })
  
  output$ent_people <- renderUI({
    new_words <- entities[entities$president == input$presspeech & entities$year == input$yearspeech, "people"][[1]]
    ents <- do.call(paste, c(as.list(paste0("<a id='", change_text(sort(new_words)), "' href='#' onclick=moveVal('",change_text(sort(new_words)),"')>", sort(new_words),"</a>")), sep = ", "))
    HTML(ents)
  })
  
  output$ent_people_label <- renderText({
    num_new_words <- length(entities[entities$president == input$presspeech & entities$year == input$yearspeech, "people"][[1]])
    paste0("People (", num_new_words, ")")
  })
  
  output$ent_places <- renderUI({
    new_words <- entities[entities$president == input$presspeech & entities$year == input$yearspeech, "places"][[1]]
    ents <- do.call(paste, c(as.list(paste0("<a id='", change_text(sort(new_words)), "' href='#' onclick=moveVal('",change_text(sort(new_words)),"')>", sort(new_words),"</a>")), sep = ", "))
    HTML(ents)
  })
  
  output$ent_places_label <- renderText({
    num_new_words <- length(entities[entities$president == input$presspeech & entities$year == input$yearspeech, "places"][[1]])
    paste0("Places (", num_new_words, ")")
  })
  
  output$ent_groups <- renderUI({
    new_words <- entities[entities$president == input$presspeech & entities$year == input$yearspeech, "groups"][[1]]
    ents <- do.call(paste, c(as.list(paste0("<a id='", change_text(sort(new_words)), "' href='#' onclick=moveVal('",change_text(sort(new_words)),"')>", sort(new_words),"</a>")), sep = ", "))
    HTML(ents)
  })
  
  output$ent_groups_label <- renderText({
    num_new_words <- length(entities[entities$president == input$presspeech & entities$year == input$yearspeech, "groups"][[1]])
    paste0("Groups (", num_new_words, ")")
  })
  
  output$ent_laws <- renderUI({
    new_words <- entities[entities$president == input$presspeech & entities$year == input$yearspeech, "laws"][[1]]
    ents <- do.call(paste, c(as.list(paste0("<a id='", change_text(sort(new_words)), "' href='#' onclick=moveVal('",change_text(sort(new_words)),"')>", sort(new_words),"</a>")), sep = ", "))
    HTML(ents)
  })
  
  output$ent_laws_label <- renderText({
    num_new_words <- length(entities[entities$president == input$presspeech & entities$year == input$yearspeech, "laws"][[1]])
    paste0("Laws (", num_new_words, ")")
  })
  
  observeEvent(input$showspeech, {
    showModal(modalDialog(
      title = "Speech Text",
      speechtext(),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ### TEXTBOX TO DRIVE MODAL
  output$entityfield <- renderUI({
    HTML('<input type="text" class="entity_style" id="entityword" value="">')
  })
  
  textval <- reactive({input$entityword})
  
  contexttable <- reactive({
      row <- allsotu[allsotu$president == input$presspeech & allsotu$year == input$yearspeech,  ]
      wordlist <- stringsplitter(textval())
      cx <- context(sotu=row, wordlist)
      paste("<span style='font-size:1.1em'><ul><li>", paste(cx$sentence, collapse="</li><li>"), "</li></ul></span>")
  })
 
  wiki_text <- reactive({
    wk <- get_wiki(textval())
    format_wiki(wk)
  }) 
  
  observeEvent(textval(), {
    if(textval() != ""){
        showModal(modalDialog(
        title = paste0("'",textval(), "' in Context"),
        HTML(paste(contexttable(), wiki_text())),
        easyClose = TRUE,
        footer = "Sorry, haven't figure out how to get it not to scoll yet.",
        id="entity-modal"
      ))
    }
  })
  
  
  #############################  
  #### WORDS BY PRESIDENT FUNCTIONS #####
  output$wordplot <- renderPlot({
    input$submitpres
    withProgress(message = 'Calculating Word Use by President', value = 0, {
      wordlist <- isolate(stringsplitter(input$presword))
      subPres <- allsotu[allsotu$president == input$pres, ]
      
      wordsByPres(df = subPres,words = wordlist, pres=input$pres, stop=TRUE)
    })
  })
  
  output$preswordsincontext = renderDataTable({
    input$submitpres
    withProgress(message = 'Extracting Words in Context', value = 0, {
      wordlist <- isolate(stringsplitter(input$presword))
      subPres <- allsotu[allsotu$president == input$pres, ]
      context(sotu=subPres,wordlist)
    })
  })
  
  ################################
  ###### WORDS THROUGH TIME FUNCTIONS ##########
  output$timeplot <- renderPlot({
    input$submit
    
    plotTimeplot(for_print=FALSE)
  })
  
  plotTimeplot <- function(for_print=FALSE) {
    dataIn <- switch(input$colorvar, 
                     "Party" = "party",
                     "Delivery Method" = "delivery"
    )
    withProgress(message = 'Building Word Through Time Plot', value = 0, {
        wordlist <- isolate(stringsplitter(input$words))
        
        wordsOverTime(df=allsotu,words = wordlist, stop=TRUE, colorvar=dataIn,
                      leg=input$colorvar, scale=input$scale, forprint=for_print)
    })
  }
  
  output$download_timeplot = downloadHandler(
    filename = 'words_through_time.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 2500, height = 2200,
                       res = 200, units = "px")
      }
      ggsave(file, plot = plotTimeplot(for_print=TRUE), device = device)
    })
  #######################################  
  ##### BY PRESIDENT FUNCTIONS #####
  output$presImage = renderUI({
    filename <- unique(allsotu[allsotu$president == input$selbypres, "links"])
    tags$img(src = filename, alt=input$selbypres)  
  })
  
  output$bypresspeeches <- renderValueBox({
    speeches <- nrow(allsotu[allsotu$president == input$selbypres, ])
    valueBox(
      value = formatC(speeches, digits = 0, format = "f"),
      subtitle = "Number of Speeches",
      icon = icon("microphone"),
      color = "green"
    )
  })
  
  output$bypreswords <- renderValueBox({
    words <- mean(allsotu[allsotu$president == input$selbypres, "tokens"])
    valueBox(
      value = formatC(words, digits = 0, format = "f"),
      subtitle = "Avg. Number of Words",
      icon = icon("book"),
      color = if (words >= mean(allsotu$tokens)) "green" else "red"
    )
  })
  
  output$bypresttr <- renderValueBox({
    ttr <- mean(allsotu[allsotu$president == input$selbypres, "ttr"])
    valueBox(
      value = formatC(ttr, digits = 3, format = "f"),
      subtitle = "Avg. Type Token Ratio (TTR)",
      icon = icon("calculator"),
      color = if (ttr >= mean(allsotu$types)) "green" else "red"
    )
  })
  
  output$bypresfkgrade <- renderValueBox({
    fkg <- mean(allsotu[allsotu$president == input$selbypres, "fkgrade"])
    valueBox(
      value = formatC(fkg, digits = 1, format = "f"),
      subtitle = "Avg. Flesch-Kincaid Grade Score",
      icon = icon("mortar-board"),
      color = if (fkg >= mean(allsotu$ttr)) "green" else "red"
    )
  })
  ########################################

  }

shinyApp(ui, server)
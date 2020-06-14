#install.packages('shiny')
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
#install.packages("shinythemes")
library(shinythemes)
#install.packages('plotly')
library(plotly)
#install.packages('shinyjs')
library(shinyjs)
#install.packages('shinycssloaders')
library(shinycssloaders)
#install.packages('shinyWidgets')
library(shinyWidgets)
library(wordcloud2)

dc <- read.csv('dc-wikia-data.csv', stringsAsFactors = F)
names(dc) <- tolower(names(dc))

marvel <- read.csv('marvel-wikia-data.csv', stringsAsFactors = F)
names(marvel) <- tolower(names(marvel))


#data cleaning
mydata <- bind_rows(list('dc' = dc, 'marvel' = marvel), .id = 'universe') %>%
  separate(col='urlslug', into =c('heroCodeName', 'actorName'), sep='\\(') %>%
  mutate(heroCodeName = gsub('wiki', '', heroCodeName),
         heroCodeName = gsub('_', ' ', heroCodeName),
         heroCodeName = gsub('[[:punct:]]', '', heroCodeName),
         align = ifelse(align == '', 'Unknown', align))%>%
  rename(identity = id) %>%
  filter(identity !='',
         hair !='',
         eye !='') %>%
  select(-actorName, -gsm)

#loading screen
load_data <- function() {
  Sys.sleep(1)
  hide("loading_page")
  show("main_content")
}

theme1 <- theme_minimal() +
  theme(text = element_text(family = 'Montserrat'),
        plot.title = element_text(hjust = 0.5, 
                                  size = 20, 
                                  family = "Kodchasan", 
                                  colour = myred,
                                  face = "bold"),
        panel.background = element_rect(fill = "transparent",colour = NA), 
        legend.position = "bottom",
        legend.justification = "center")

myblue<-'#042E6F'
myred<-'#FC2F1E'


data1<- strsplit(mydata$heroCodeName,' ')
data2<- unlist(data1) 
data3<-table(data2)
data4<-data.frame(data3)%>%
  arrange(desc(Freq))
wordcloud1<-wordcloud2(data4, shape = 'triangle',size = 1,color = 'random-light', backgroundColor = grey,minRotation = -pi/2, maxRotation = -pi/2 )




#ui part
ui <- fluidPage(
  # wordcloud
  
  

  #show loading
  useShinyjs(),
  div(
    id = "loading_page",
    h1("Loading...")
  ), #close div
  hidden(
    div(
      id = "main_content",
      "Let's see characters of Marvel and DC!!!"
    ) #close div
  ), #close hidden

  theme = shinytheme("slate"),
  
  

  
  
  #set background
  tags$h2(""),
  setBackgroundImage(
    src = "vs.jpg"
  ),
  
  
  
  titlePanel('Marvel vs DC'),
  
  sidebarLayout(position = "right",
    sidebarPanel(
      
      width=3,
      tags$style(".well {background-color:#00000000;}"),
      
      imageOutput("Image",height=3),
      
      selectInput('Gender', 'Pick a Gender:', 
                  choices=c('All', 
                            'Male'='Male Characters',
                            'Female'='Female Characters')),
      
      
      
      selectInput('Align', 'Good Guy or Bad Guy?', 
                  choices=c('All', 
                            'Good'='Good Characters',
                            'Neutral'='Neutral Characters',
                            'Bad'='Bad Characters',
                            'Unknown'='Unknown')),
      
      selectInput('Identity', 'Pick an Identity', 
                  choices=c('All', 
                            'Secret'='Secret Identity',
                            'Public'='Public Identity')),
      helpText(HTML('<p style="color:#bdbfbe;font-weight:bold; font-size: 11pt">Author:</p>')),
      helpText(HTML('<p style="color:#bdbfbe;font-weight:bold; font-size: 11pt">Jie Li</p>')),
      helpText(HTML('<p style="color:#bdbfbe;font-weight:bold; font-size: 11pt">Qingdu Meng</p>')),
      helpText(HTML('<p style="color:#bdbfbe;font-weight:bold; font-size: 11pt">Ying Lin</p>')),

      
      helpText(HTML('<p style="color:#bdbfbe;font-weight:bold; font-size: 11pt">Data Source:</p>')),
      helpText(  a("Avengers",  target="_blank",
                   href="https://www.kaggle.com/fivethirtyeight/fivethirtyeight-comic-characters-dataset")
      )
    ), # closes sidebarPanel
   
    
    mainPanel(
      
      
      #img(src='mcu+vs+dceu.png', align = "top"),
      tabsetPanel(
        tabPanel('Character_num',    withSpinner(plotlyOutput('newC'),type=7,color='white')),
        tabPanel('Character_analysis',    withSpinner(plotOutput('radar',  width = "100%"),type=7,color='white')),
        tabPanel('WordCloud', wordcloud2Output('wordcloud1'),wordcloud2Output('wordcloud2')),
         
        tabPanel('The Data', dataTableOutput('theData'))
      )  #closes tabsetPanel

    ) # closes mainPanel
    
  ) # closes sidebarLayout  
) # closes fluidPage

server <- function(input, output){
  load_data()

  # Take inputs created in ui function
  plotData <- reactive({
    
    rows=T
    if(input$Gender != 'All'){rows=rows & mydata$sex==input$Gender }
    if(input$Align != 'All'){rows=rows & mydata$align==input$Align }
    if(input$Identity != 'All'){rows=rows & mydata$identity==input$Identity }
    
    mydata[rows, ] %>%
      group_by(year, universe) %>%
      summarise(num = n() )
  })
  
  
  # This builds the New Character plot
  output$newC <- renderPlotly({
    
    #mydata %>%
    #  group_by(year, universe) %>%
    #  summarise(num = n()) %>%
    Sys.sleep(0.3)
    ggplot(plotData(), aes(x = year, y = num, color = universe)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = c(myblue, myred)) +
      labs(title = "New characters in DC vs Marvel",
           x = "Year", 
           y = "New Characters",
           color = 'universe') +
      theme1
    
  
  })
  
  #insert image gender part
  output$Image <- renderImage({
    
    filename <- normalizePath(file.path('./www',
                                        paste('image', input$Gender, '.png', sep='')))
    
    # Return a list containining the filename
    list(src = filename,align = "right")
    
  }, deleteFile = FALSE)
 
  
  
  #the radar plot data cleaning
  filtered_appearance <- mydata %>%
    select(align, hair, eye, identity, sex) %>%
    filter(align %in% c("Good Characters", "Bad Characters"),
           !is.na(hair),
           !is.na(eye)) %>%
    mutate(appearance = paste0(hair, ',\n', eye))
  
  app_data_full <- filtered_appearance %>%
    select("Alignment" = align, appearance, identity, sex) %>%
    group_by(Alignment, appearance) %>%
    summarize(num = n()) %>%
    group_by(Alignment) %>%
    mutate(rank = rank(desc(num), ties.method = "first")) 
  
  #radar plot data function
  app_data <- reactive({
    
    rows=T
    if (input$Gender != 'All'){rows=rows & mydata$sex==input$Gender }
    if (input$Identity != 'All'){rows=rows & mydata$identity==input$Identity }
    
    app_data_full[rows, ] %>%
      filter(rank <= 10) %>%
      mutate(perc = num/sum(num)) %>%
      select(- rank)
  })
  
  
  output$radar <- renderPlot({
    Sys.sleep(0.3)
    ggplot(app_data(), aes(x = appearance, y = perc*100, colour = Alignment, fill = Alignment, group = Alignment)) +
      geom_point(size = 2) + 
      geom_polygon(alpha = 0.2) + 
      labs(title = "Appearance by Alignment",
           y = "%",
           x = NULL)  + 
      scale_colour_manual(values = c(myblue, myred)) +
      scale_fill_manual(values = c(myblue, myred)) +
      theme(axis.text.x = element_text(size = 13)) +
      coord_polar() +
     theme_minimal() +
      
      theme(
        legend.position = 'top',
        
        title = element_text(size = 15),
        axis.text.y = element_text(size = 15)
        )
    
  }, height = 600, width = 900)
  # This builds the radar plot
  
  
  #wordcloud



  output$wordcloud1 <- renderWordcloud2(
    wordcloud1) 

  # This is the dataset
  output$theData <- renderDataTable(mydata)  
  
}
  

shinyApp(ui, server)

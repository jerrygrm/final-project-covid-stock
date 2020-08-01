library(shiny) 
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(quantmod)
library(xts)
library(DT)
library(readr)

cfm_cities <- read_csv("/Users/jerrymino/Desktop/final project 科研/time_series_covid19_confirmed_US.csv")
dth_cities <-  read_csv("/Users/jerrymino/Desktop/final project 科研/time_series_covid19_deaths_US.csv")
mydata <- read_csv("/Users/jerrymino/Desktop/final project 科研/daily.csv")
mydata$date <- as.character(mydata$date)
mydata$date <- unlist(lapply(mydata$date,function(x){
  temp1 <- substr(x,1,4)
  temp2 <- substr(x,5,6)
  temp3 <- substr(x,7,8)
  paste0(temp1,"-",temp2,"-",temp3)
}))

mydata$date <- as.Date(mydata$date)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage( 
  dashboardHeader(title="us covid19 vs stock"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Covid19 World Map",tabName = "Map",icon =icon("globe-americas") ),
      menuItem("US city pandemic vs stock",tabName = "PlotA",icon=icon("chart-bar")),
      menuItem("US state pandemic vs stock",tabName = "PlotB",icon=icon("chart-bar"))
    )
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = "Map",
              fluidRow(
                
                box(sliderInput("cfm_size", "Size of Confirmed Circles:",
                                min = 0, max = 1000, value = 500, animate = TRUE),
                    sliderInput("cfm_opc", "Opacity of Confirmed Circles:",
                                min = 0, max = 1, value = 0.5, animate = TRUE),
                ),
                box(sliderInput("dth_size", "Size of Deaths Circles:",
                                min = 0, max = 3000, value = 1500, animate = TRUE),
                    sliderInput("dth_opc", "Opacity of Deaths Circles:",
                                min = 0, max = 1, value = 0.3, animate = TRUE),
                ),
                box(radioButtons("choice","Please choose a variable",choices =c("Confirmed","Deaths"))),
                box(leafletOutput("mymap"),p(),
                    actionButton("recalc", "New points"),width=12))),
      tabItem(tabName = "PlotA",
              fluidRow(
                box(width = 6,
                    h4("Correlation between Stocks and Confirmed Cases in US"),
                    selectInput('n','Choose a Stock',names(stock_close_prizes)),
                    selectInput('m','Choose a city in US',names(confirmed_US))),
                box(width = 6,
                    h4("Correlation between Stocks and Death Toll in US"),
                    selectInput('y','Choose a Stock',names(stock_close_prizes)),
                    selectInput('x','Choose a city in US',names(deaths_US))),
                box(width = 6,
                    plotOutput("Confirmed")),
                box(width = 6,
                    plotOutput("Deaths"))
                
              )),
      tabItem(tabName = "PlotB",
              fluidRow(
                box(width = 4,
                    textInput('stock',
                              h3('Stock Symbol'),
                              value = 'FB'),
                    selectInput("choose_state",h3("state"),choices = unique(mydata$state)),
                    selectInput('var','Choose a variable of COVID-19',names(mydata)),
                    
                    dateRangeInput('dates', h3('Date range'),
                                   start  ='2020-01-01',
                                   end    = "2020-07-01",
                                   min    = "2019-07-01",
                                   max    = "2020-07-01"),
                    
                    checkboxInput("log",
                                  "Plot y axis on log scale", 
                                  value = TRUE
                    )),
                box(width = 8,
                    textOutput("corr_value"),
                    br(),
                    plotOutput("plot1"),
                    plotOutput("plot2"))),
              fluidRow(
                tabBox(title="Data",width = 12,
                       tabPanel(title = "stockdata",
                                dataTableOutput("table1")),
                       tabPanel(title="dailydata",dataTableOutput("table2"))
                )
                
              )
      ) ))) 

server <- function(input, output) { 
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    if(input$choice=="Deaths"){
      content <- paste0(sep = "<br/>",
                        dth_cities$Combined_Key,
                        "<br><a>Deaths:  </a >",dth_cities$today,
                        "</br><a>New Deaths:  </a >",dth_cities$today-dth_cities$yesterday)
      leaflet(dth_cities) %>%
        setView(-95, 35, zoom = 3.2)%>%
        addTiles() %>%
        addCircles(lng = ~Long_ , lat = ~Lat, weight = 0,radius = ~sqrt(today) * input$dth_size,
                   popup = content,
                   fillColor = "purple", fillOpacity = input$dth_opc)
    }
    else{
      content <- paste0(sep = "<br/>",
                        cfm_cities$Combined_Key,
                        "<br><a>Confirmed:  </a >",cfm_cities$today,
                        "</br><a>New Confirmed:  </a >",cfm_cities$today-cfm_cities$yesterday)
      leaflet(cfm_cities) %>%
        setView(-95, 35, zoom = 3.2)%>%
        addTiles() %>%
        addCircles(lng = ~Long_ , lat = ~Lat, weight = 0,radius = ~sqrt(today) * input$cfm_size,
                   popup = content,
                   fillColor = "orange", fillOpacity = input$cfm_opc)
    }
  })
  
  output$Confirmed <- renderPlot({
    
    df<- merge(confirmed_US, stock_close_prizes, Date = intersect(names(confirmed_US), names(stock_close_prizes)),
               Date.confirmed_US = Date, Date.stock_close_prizes = Date)
    ggplot(df, aes_string(x = input$m, y= input$n, group=1)) + geom_line()
    
  })
  
  output$Deaths <- renderPlot({
    
    df<- merge(deaths_US, stock_close_prizes, Date = intersect(names(deaths_US), names(stock_close_prizes)),
               Date.deaths_US = Date, Date.stock_close_prizes = Date)
    ggplot(df, aes_string(x = input$x, y= input$y, group=1)) + geom_line()
    
  })
  
  plotdata1 <- reactive({
    x <- getSymbols(input$stock,
                    from = input$dates[1],
                    to = input$dates[2],
                    auto.assign = FALSE)
    x
  })
  
  output$table1 <- renderDataTable({
    x <- plotdata1()
    x
  })
  
  
  output$plot1 <- renderPlot({
    x <- plotdata1()
    chartSeries(x,
                type = "line", log.scale = input$log, TA = NULL)
  })
  
  plotdata2 <- reactive({
    tempdata <- subset(mydata,mydata$state==input$choose_state)
    tempdata <- subset(tempdata,
                       tempdata$date>=input$dates[1]&tempdata$date<=input$dates[2])
    tempdata$date <- as.character(tempdata$date)
    rownames(tempdata) <- NULL
    data.frame(tempdata[,c(1,2,3)])
  })
  
  output$table2 <- renderDataTable(
    plotdata2()
  )
  
  output$plot2 <- renderPlot({
    tempdata <- plotdata2()
    tempdata$date <- as.Date(tempdata$date)
    plot(tempdata[,c(1,3)],type = "l",col='blue',main = paste0("positive in ",input$choose_state))  
  })
  
  output$corr_value <- renderText({
    data1 <- plotdata1()
    data1 <- data.frame(data1[,1])
    data1$date <- rownames(data1)
    data2 <- plotdata2()[,c(1,3)]
    data3 <- merge(data1,data2,by = "date")
    value1 <- cor(data3[,2],data3[,3]) 
    paste0("The correlation coefficient between ",input$stock," and ",
           input$choose_state," is:",value1)
  })
  
 
  
} 
shinyApp(ui, server)


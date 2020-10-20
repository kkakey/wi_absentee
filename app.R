library(shiny)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(lubridate)
library(tidyverse)
library(scales)
library(DT)
source("config.R")

# import data
df <- geojsonio::geojson_read("output/wi_counties.geojson", what = "sp")

df$date = as.Date(df$date)
cv_min_date = as.Date(min(df$date),"%Y-%m-%d")
current_date = as.Date(max(df$date),"%Y-%m-%d")


ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    navbarPage("WI Absentee Ballots",
        tabPanel("Main",
    leafletOutput("map", width="100%", height="850px"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                  width = 330, height = 710,
    h4("Wisconsin Absentee Data, 2020"),
    absolutePanel(top = 60, right = 10,
                  selectInput("variable", "Select:",
                              list(`Absentee Ballot Data (%, Cumulative)` = list("% of Absentee Ballot Requests"="prop_reg_absentee_applied", "% of Absentee Ballots Sent Out"="prop_ballot_sent", "% of Absentee Ballots Returned"="prop_ballot_returned"),
                                   `Weekly Change in Absentee Ballot Requests (%)` = list("Weekly Percentage Change"="oneweek_absentee_app_prop"),
                                   `Voter Demographics (%)` = list("% of Voting Age Population Registered to Vote"="prop_vap_reg", "% of Citizen Voting Age Population Registered to Vote"="prop_cvap_reg"),
                                   `Voter Demographics (Raw Counts)` = list("Registered Voters"="reg_voter", "Voting Age Population"="vap_est", "Citizen Voting Age Population"="cvap_est","Total Population"="total_est"))),
                  uiOutput('date.input'),
                  span(uiOutput("textoutput", width = "100%"), style="font-weight: 400; font-size: 16px;", align = "center"),
                  checkboxInput("legend", "Show legend", TRUE),
                  checkboxInput("table", "Show Data Table", F),
                  plotlyOutput("plot",  width = "100%")
    )),
    conditionalPanel(condition = "input.table == true",
                     absolutePanel(bottom = 170, right = 20,left = 20,height=300,width=300,top="auto",fixed = TRUE,
                                   draggable = TRUE,
                     DT::dataTableOutput("table"),
                     class = 'text-center', downloadButton('x3', 'Download Data'))
                     ),


    absolutePanel(id = "controls2", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 130, left = 20, right = "auto", bottom = "auto",
                  width = 200, height = 110, style = "background: #FBFAF7",
                  span(uiOutput("total_requests"), style="font-weight: 400; font-size: 16px; padding: 1px;", align = "center")
            )),
    tabPanel("About",
    
        h3("About"),
        br(),
        h5("This project is to help Wisconsinites, activists, and election-analysts examine and track Wisconsin absentee ballot data for the 2020 Presidential Election."),
        h5("With more people than ever casting a ballot via mail this year due to Covid-19, I wanted to visually track the number of absentee ballot requests from September 1st (the first-day data on absentee ballots became available) up until Election Day in every Wisconsin county."),
        h5("Users can see day-by-day in each county the proportion of registered voters that have requested an absentee ballot, the proportion of requested ballots that have been sent back to voters, and also the proportion of those absentee ballots that have been received by election officials and cased to vote. Additionally, this project allows users to analyze voter registration and estimated voting-eligible population data by county."),
        h5(HTML(paste0("I was inspired to create this project as part of the MapBox ", tags$a(href="https://www.mapbox.com/elections-challenge-2020", "Elections Mapping Challenge"), ". I plan to continue expanding this project, to include absentee ballot data from past election cycles, update it to track data for future election cycles, and potentially expand to more states and records of voting modes."))),
        br(),
        h5("Stay safe, and happy voting!"),
        br(),
        br(),
        h5(HTML(paste0("Data Sources: ", 
           tags$a(href="https://elections.wi.gov/", "Wisconsin Elections Commission "), " and ", 
           tags$a(href="https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/cvap.2018.html", "the American Community Survey.")))),
        h5(HTML(paste0("Created by: ", tags$a(href="https://github.com/kkakey", "kkakey"))))
        
    )
  ))



server <- function(input, output, session) {
    
   table_df <-  reactive({
       df[df$date==input$date, c("NAME", "date", input$variable)]
    })
   
   table_df2 <-  reactive({
     df[df$date==input$date, c("NAME", input$variable)]
   })
    
    output$table = DT::renderDataTable({

        if (input$variable %in% c("reg_voter", "vap_est", "cvap_est", "total_est")) {
          DT::datatable(as.tibble(table_df2()), options = list(pageLength = 5, 
                                                              info = FALSE, lengthChange = FALSE), rownames=F, colnames=c("Name","Count"))
        }
      
        else {
        DT::datatable(as.tibble(table_df()), options = list(pageLength = 5, 
                                        info = FALSE, lengthChange = FALSE), rownames=F, colnames=c("Name","Date","")) %>% 
        formatPercentage(c(input$variable), 2)
        }
 
    })
    

    output$date.input <- renderUI({
        if (input$variable %in% c("prop_reg_absentee_applied","prop_ballot_sent", "prop_ballot_returned", "oneday_absentee_app_prop")) {
        sliderInput("date",
                    "Date:",
                    min = as.Date(cv_min_date,"%Y-%m-%d"),
                    max = as.Date(current_date,"%Y-%m-%d"),
                    value=as.Date(cv_min_date),
                    timeFormat="%d %b")
        }
        else if (input$variable == "oneweek_absentee_app_prop") {
            sliderInput("week",
                        "Week:",
                        min = 1,
                        max = max(df$week),
                        step = 1, value=max(df$week)-1)
        }
 
    })
    

    
    
    
    output$plot <- renderPlotly({
        req(input$variable)
        req(input$date)
        
        if (input$variable=="prop_reg_absentee_applied") {
        p<- df %>%
           as.tibble() %>%
            group_by(date) %>% mutate(`Avg. Absentee Request` = mean(prop_reg_absentee_applied)) %>%
            ggplot( ., aes(date, `Avg. Absentee Request`))+
            geom_line(color="#9E9AC8") +
            geom_line(color="#9E9AC8", size=1, aes(text =sprintf("Date: <b>%s</b><br>Avg. Absentee Requests: <b>%g%%</b>", date, round(`Avg. Absentee Request`,2)*100))) +
            xlab("")  + theme_minimal() + scale_y_continuous(labels = scales::percent, limits = c(.2,.4)) +
            geom_vline(xintercept = as.numeric(ymd(input$date)), linetype="solid",
                       color = "black", size=.5)
           
        
        ggplotly(p, tooltip=c("text"))
        }
        
        else if (input$variable=="prop_ballot_sent") {
            p<- df %>%
                as.tibble() %>%
                group_by(date) %>% mutate(`Avg. Absentee Sent` = mean(prop_ballot_sent)) %>%
                ggplot( ., aes(date, `Avg. Absentee Sent`))+
                geom_line(color="#9E9AC8") +
                geom_line(color="#9E9AC8", size=1, aes(text =sprintf("Date: <b>%s</b><br>Avg. Absentee Sent: <b>%g%%</b>", date, round(`Avg. Absentee Sent`,2)*100))) +
                xlab("")  + theme_minimal() + scale_y_continuous(labels = scales::percent) +
                geom_vline(xintercept = as.numeric(ymd(input$date)), linetype="solid",
                           color = "black", size=.5)
            
            
            ggplotly(p, tooltip=c("text"))
            
        }
        
        else if (input$variable=="prop_ballot_returned") {
            p<- df %>%
                as.tibble() %>%
                group_by(date) %>% mutate(`Avg. Absentee Returned` = mean(prop_ballot_returned)) %>%
                ggplot( ., aes(date, `Avg. Absentee Returned`))+
                geom_line(color="#9E9AC8") +
                geom_line(color="#9E9AC8", size=1, aes(text =sprintf("Date: <b>%s</b><br>Avg. Absentee Returned: <b>%g%%</b>", date, round(`Avg. Absentee Returned`,2)*100))) +
                xlab("")  + theme_minimal() + scale_y_continuous(labels = scales::percent) +
                geom_vline(xintercept = as.numeric(ymd(input$date)), linetype="solid",
                           color = "black", size=.5)
            
            
            ggplotly(p, tooltip=c("text"))
            
        }
        
        else if (input$variable=="oneweek_absentee_app_prop") {
          req(input$week)
          p<- df %>%
            as.tibble() %>% group_by(week) %>%
            mutate(`Avg. Change in Absentee Ballot Requests` = mean(oneweek_absentee_app_prop)) %>%
            ggplot( ., aes(week, `Avg. Change in Absentee Ballot Requests`))+
            geom_point() + geom_line() + xlab("") +
            theme_minimal() + scale_y_continuous(labels = scales::percent) +
            geom_vline(xintercept = as.numeric(input$week), linetype="solid",
                       color = "black", size=.5) 
          
          ggplotly(p, tooltip=c("text"))
        }
        
        else {
          NULL
          }

    })
      
    #create summary text
    output$total_requests <- renderUI({
        print( paste('Total Number of Absentee Ballots Requested Statewide:',   comma_format(digits = 12)(unique(df$total_absentee_requests)) )   )
        })  
    
    
    observe({
      if (input$variable %in% c("reg_voter", "cvap_est", "vap_est", "total_est", "prop_vap_reg", "prop_cvap_reg")) {
        output$textoutput <- renderUI({
          b <- '<br/><br/>'
          c1 <- "Need to register to vote or request an absentee ballot? Go to: " 
          c2 <- tags$a(href="https://myvote.wi.gov/en-us/", "https://myvote.wi.gov/en-us/")
          c3 <- "or" 
          c4 <- tags$a(href="https://vote.gov/", "https://vote.gov/")
          c <- "Vote and Stay Safe!"
          c5 <- '<br/><br/><br/><br/><br/><br/>'
          
          HTML(paste(b, c1, c2, c3, c4, c, c5, sep = '<br/><br/>'))

        })  
      }
      else {
        output$textoutput <- renderUI({
        NULL })
      }
    })
        

    filteredData <- reactive({

        if (input$variable=="oneweek_absentee_app_prop") {
            req(input$week)
            df[df$week==as.numeric(input$week), ]
        }
      
        else {
            shiny::validate(
                need(input$date %in% unique(df$date), "No data reported that day. Please choose another!")
        )
        df[df$date==input$date, ]
        }
    })
    


    pastweekdata <- reactive({
        req(input$week)
        df[df$week==as.numeric(input$week)-1, ]
    })
    

    labels <- reactive({

        if (input$variable=="prop_reg_absentee_applied") {
            labels <- sprintf(
                "<strong>%s</strong><br/>%g%% Absentee Requests",
                filteredData()$NAME,  filteredData()$prop_reg_absentee_applied*100
            ) %>% lapply(htmltools::HTML)
        }

        else if (input$variable=="prop_ballot_sent") {
            labels <- sprintf(
                "<strong>%s</strong><br/>%g%% Absentee Ballots Sent",
                filteredData()$NAME,  filteredData()$prop_ballot_sent*100
            ) %>% lapply(htmltools::HTML)
        }

        else if (input$variable=="prop_ballot_returned") {
            labels <- sprintf(
                "<strong>%s</strong><br/>%g%% Absentee Ballots Returned",
                filteredData()$NAME,  filteredData()$prop_ballot_returned*100
            ) %>% lapply(htmltools::HTML)
        }
        
        else if (input$variable=="oneweek_absentee_app_prop") {
            labels <- sprintf(
                "<strong>%s</strong><br/><strong>%g%%</strong> Change in Requests From Last Week<br/>%s Absentee Ballot Requests This Week<br/>%s Absentee Ballot Requests Last Week",
                filteredData()$NAME,  filteredData()$oneweek_absentee_app_prop*100, 
                filteredData()$oneweek_absentee_app.x, pastweekdata()$oneweek_absentee_app.x
            ) %>% lapply(htmltools::HTML)
        }

        

        else  {
            var <- input$variable

            labels <- sprintf(
                "<strong>%s</strong><br/>%g",
                filteredData()$NAME,  filteredData()[[var]]
            ) %>% lapply(htmltools::HTML)
        }
    })


    output$map <- renderLeaflet({

        leaflet(df) %>%
            addTiles() %>% setView(-89.2, 44.3, 7.4) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = apikey))
    })



    observe({
        if (input$variable=="prop_reg_absentee_applied") {
            pal <- colorBin(palette="Purples", domain = df$prop_reg_absentee_applied, bins=5, pretty = T)
        }
        
        else if (input$variable=="prop_ballot_sent") {
            pal <- colorQuantile(palette="Purples", domain = df$prop_ballot_sent, n=5)
            
        }
        
        else if (input$variable=="prop_ballot_returned") {
            pal <- colorQuantile(palette="Purples", domain = df$prop_ballot_returned, n=5)
        }
        
        
        else if (input$variable %in% c("prop_vap_reg", "prop_cvap_reg")) {
            var <- input$variable
            pal <- colorBin(palette="YlOrBr", domain = df$prop_cvap_reg, bins=5, na.color = "#FFFFE5")
            
        }
        
        
        else if (input$variable %in% c("reg_voter", "vap_est", "cvap_est", "total_est")) {
            var <- input$variable
            pal <- colorQuantile(palette="YlOrBr", domain = df$total_est, n=7, na.color = "#FFFFE5")
            
        }
        
        
        else if (input$variable=="oneweek_absentee_app_prop") {
            pal <- colorQuantile(palette="RdYlGn", domain = df$oneweek_absentee_app_prop, n=10)
        }
        
        
        leafletProxy("map", data = filteredData()) %>%
            addPolygons( weight = 1,
                          opacity = 1,
                          color = "#100c08",
                          dashArray = "1",
                          fillOpacity = 1,
                          fillColor = ~pal(get(input$variable)),
                          highlight = highlightOptions(
                              weight = 5,
                              color = "#808080",
                              dashArray = "",
                              fillOpacity = 6,
                              bringToFront = T),
                          label = labels(),
                          labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"))
            
    })

    
    
    
    observe({
        proxy <- leafletProxy("map", data = df)

        
        if (input$variable=="prop_reg_absentee_applied") {
            pal <- colorBin(palette="Purples", domain = df$prop_reg_absentee_applied, bins=5, pretty = T)
        }
        
        else if (input$variable=="prop_ballot_sent") {
            pal <- colorQuantile(palette="Purples", domain = df$prop_ballot_sent, n=5)
        }
        
        else if (input$variable=="prop_ballot_returned") {
            pal <- colorQuantile(palette="Purples", domain = df$prop_ballot_returned, n=5)
        }
        
        else if (input$variable %in% c("prop_vap_reg", "prop_cvap_reg")) {
            var <- input$variable
            pal <- colorBin(palette="YlOrBr", domain = df$prop_cvap_reg, bins=5, na.color = "#FFFFE5")
        }
        
        else if (input$variable %in% c("reg_voter", "vap_est", "cvap_est", "total_est")) {
            var <- input$variable
            pal <- colorQuantile(palette="YlOrBr", domain = min(df$reg_voter):max(df$total_est), n=7, na.color = "#FFFFE5")
        }
        
        else if (input$variable=="oneweek_absentee_app_prop") {
            pal <- colorQuantile(palette="RdYlGn", domain = df$oneweek_absentee_app_prop, n=10)
        }
        
     
        proxy %>% clearControls()
        if (input$legend) {
            if (input$variable %in% c("prop_reg_absentee_applied","prop_ballot_sent", "prop_ballot_returned")) {
            proxy %>% addLegend(position = "bottomright", title="",na.label = "No data",
                                pal = pal, 
                                values = ~get(input$variable),
                                labFormat = function(type, cuts, p) {
                                    n = length(cuts)
                                    paste0(cuts[-n]*100,"%", " &ndash; ", cuts[-1]*100,"%")})
            }
            
            else if (input$variable=="oneweek_absentee_app_prop"){
                
                proxy %>% addLegend(position = "bottomright", title="",na.label = "No data",
                                    pal = pal, 
                                    values = ~get(input$variable),
                                    labFormat = function(type, cuts, p) {
                                        n = length(cuts)
                                        paste0(round(cuts[-n], 2)*100,"%", " &ndash; ", round(cuts[-1],2)*100,"%")}
                )
            }
            
            else if (input$variable=="oneday_absentee_app_prop"){
                
                proxy %>% addLegend(position = "bottomright", title="",na.label = "No data",
                                    pal = pal, 
                                    values = ~get(input$variable),
                                    labFormat = function(type, cuts, p) {
                                        n = length(cuts)
                                        paste0(round(cuts[-n], 4)*100,"%", " &ndash; ", round(cuts[-1],4)*100,"%")}
                )
            }
            
            else if (input$variable %in% c("prop_vap_reg", "prop_cvap_reg")) {
        
            proxy %>% addLegend(position = "bottomright", title="",na.label = "No data",
                                pal = pal, values = ~prop_cvap_reg,
                                labFormat = labelFormat(suffix = "%"))
                }
            
            
            else if (input$variable %in% c("reg_voter", "vap_est", "cvap_est", "total_est")) {

                var <- input$variable
                
                proxy %>% addLegend(position = "bottomright", title="", na.label = "NA",

                    pal = pal, values = min(df$reg_voter):max(df$total_est), 
                    labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0(round(cuts[-n]), " &ndash; ", round(cuts[-1]))}
                    )
                
            }
            
        }
    })
    
    output$x3 = downloadHandler('wi-absentee-data.csv', content = function(file) {
      if (input$variable %in% c("reg_voter", "vap_est", "cvap_est", "total_est")) {
        write.csv(table_df2(), file, row.names=FALSE)
      }
      
      else {
        write.csv(table_df(), file, row.names=FALSE)
      }
    })
    
    
}

shinyApp(ui, server)


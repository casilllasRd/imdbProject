library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

imdb_data <- read_csv(file='data/clean-imdb.csv', col_names=TRUE)

imdbHeader <- dashboardHeader(title='IMDB Film Data Analysis')

imdbSidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            text='Genre Analysis', 
            tabName='genreAnalysis'),
        
        menuItem(
            text='Directors Analysis', 
            tabName='directorAnalysis'),
        
        menuItem(
            text='Rating Analysis', 
            tabName='ratingAnalysis')
    )
)

imdbBody <- dashboardBody(
    tabItems(
        tabItem(
            tabName='genreAnalysis',
            
            fluidPage(
                fluidRow(
                    box(title='Count of Movies by Genre', width=8,
                        plotlyOutput(outputId='genreTimeLG')),
                    
                    box(title='Count of Movies by Genre\nIn the year:', width=4, 
                        plotlyOutput(outputId='genreLoli'))
                ),
                fluidRow(
                    box(title='Data Modifiers', width=4,
                        selectInput(
                            inputId='genreSelect', label='Select Genres: ', 
                            choices=c(str_sort(unique(imdb_data$primary_genre))), 
                            multiple=TRUE, selected=c('Action', 'Adventure', 'Comedy')),
                        
                        sliderInput(
                            inputId='genreYearSlider', label='Select a Year: ',
                            min=1980, max=2020, step=5, value=2020)),
                    
                    box(title='Average Runtime by Genre', width=8,
                        plotlyOutput(outputId='genreRuntime'))
                )
            )
        ),
        
        tabItem(
            tabName='directorAnalysis',
            fluidPage(
                fluidRow(
                    
                    box(title='', width=2,
                        numericInput(inputId='directorN', label='Choose Top N Directors', value=5, min=1, max=25, step=1),
                        
                        tableOutput(outputId='directorTable')),
                    
                    box(title='', width=10, height=700,
                        selectInput(
                            inputId='dirGenreSelect', label='Select Genres: ',
                            choices=c(str_sort(unique(imdb_data$primary_genre))),
                            multiple=TRUE, selected=c('Action', 'Adventure', 'Comedy')),
                        
                        plotlyOutput(outputId='directorPlot', height=550)
                    )
                )
            )
        ),
        
        tabItem(
            tabName='ratingAnalysis',
            fluidPage(
                fluidRow(
                    box(title = '', width = 8,
                        plotOutput(outputId = 'ratingTimeLG'))
                )
            )
        )
    )
)

imdbUI <- dashboardPage(header=imdbHeader, sidebar=imdbSidebar, body=imdbBody)

imdbServer <- function(input, output){
    
    output$genreTimeLG <- renderPlotly({
        if(is.null(input$genreSelect)){
            ggplot()+ labs(title='Please Select a genre below') + xlim(1978,2022) + ylim(0,50) + theme_bw()
        } else{
            genreLineGraph = imdb_data %>%
                select(year, primary_genre) %>%
                subset(., subset= primary_genre %in% input$genreSelect) %>%
                subset(., subset= year <= input$genreYearSlider) %>%
                group_by(year) %>%
                count(primary_genre) %>%
                mutate(Year=year, Count=n, Genre=primary_genre) %>%
                ggplot(mapping=aes(x=Year, y=Count, color=Genre)) +
                geom_line(size=1, alpha=0.5) +
                geom_point(size=2) +
                theme_bw() +
                xlab(label='Year') +
                ylab(label='Count') +
                xlim(1978, 2020) +
                ylim(0, 52)

            ggplotly(genreLineGraph, tooltip=c('all')) %>%
                config(displayModeBar=FALSE) %>%
                layout(
                    yaxis = list(fixedrange=TRUE),
                    xaxis = list(fixedrange=TRUE)
                )
            
        }
    })
    
    output$genreLoli <- renderPlotly({
        if(is.null(input$genreSelect)){
            ggplot()
        } else {
            genreLolipop = imdb_data %>%
                select(year, primary_genre) %>%
                subset(., subset= primary_genre %in% input$genreSelect) %>%
                subset(., subset= year == input$genreYearSlider) %>%
                count(primary_genre) %>%
                mutate(primary_genre = fct_reorder(primary_genre, n)) %>%
                ggplot() +
                geom_segment(mapping=aes(x=primary_genre, y=0, xend=primary_genre, yend=n, color=primary_genre)) +
                geom_point(mapping=aes(x=primary_genre, y=n, color=primary_genre, text=paste0('Genre: ', primary_genre, '\nCount: ', n))) +
                coord_flip() +
                labs(title=input$genreYearSlider)+
                theme_bw() +
                theme(
                    legend.position='none',
                    axis.title=element_blank()
                )
            
            ggplotly(genreLolipop, tooltip=c('text')) %>%
                config(displayModeBar=FALSE) %>%
                layout(
                    yaxis=list(fixedrange=TRUE),
                    xaxis=list(fixedrange=TRUE)
                )
        }
    })
    
    output$genreRuntime <- renderPlotly({
        if(is.null(input$genreSelect)){
            ggplot()
        } else{
            genreRuntime = imdb_data %>%
                select(year, primary_genre, runtime) %>%
                subset(., subset= primary_genre %in% input$genreSelect) %>%
                subset(., subset = year <= input$genreYearSlider) %>%
                group_by(primary_genre, year) %>%
                mutate(Runtime = as.integer(mean(runtime)), Year=year, Genre=primary_genre) %>%
                ggplot(mapping=aes(x=Year, y=Runtime, color=Genre)) +
                geom_line(size=1, alpha=0.5) +
                geom_point(size=2) +
                theme_bw() +
                xlab(label='Year') +
                ylab(label='Average Runtime') +
                xlim(1978, 2022) +
                ylim(0, 175)
            
            ggplotly(genreRuntime) %>%
                config(displayModeBar=FALSE) %>%
                layout(
                    yaxis=list(fixedrange=TRUE),
                    xaxis=list(fixedrange=TRUE)
                )
        }
    })
    
    output$directorTable <- renderTable({
        imdb_data%>%
            select(director) %>%
            count(director) %>%
            arrange(desc(n)) %>%
            slice_head(n=input$directorN) 
    })
    
    output$directorPlot <- renderPlotly({
        if(is.null(input$dirGenreSelect)){
            ggplot() + labs(title='Please Select a Genre') + xlim(0,9) + ylim(0, 200) + theme_bw()
        } else {
            dir <- imdb_data %>%
                select(director) %>%
                count(director) %>%
                arrange(desc(n)) %>%
                slice_head(n=input$directorN)
            
            dirPlot <- imdb_data %>%
                select(director, runtime, stars, primary_genre, title) %>%
                subset(., subset= director %in% dir$director) %>%
                subset(., subset= primary_genre %in% input$dirGenreSelect) %>%
                ggplot() + 
                geom_point(mapping=aes(runtime, stars, color=director, shape=primary_genre, size=1.5, alpha=0.75,
                                       text=paste0('Director: ', director, '\nTitle: ', title,'\nGenre: ', primary_genre, 
                                                   '\nStars: ', stars, '\nRuntime: ', runtime, ' min.'))) +
                xlab(label = 'Runtime') +
                ylab(label = 'Stars') +
                xlim(75, 220) +
                ylim(0, 9) +
                theme_bw() +
                theme(
                    legend.position='none'
                )
            
            ggplotly(dirPlot, tooltip=c('text')) %>%
                config(displayModeBar=FALSE) %>%
                layout(
                    yaxis=list(fixedrange=TRUE),
                    xaxis=list(fixedrange=TRUE)
                )
        }
    })
    
    output$ratingTimeLG <- renderPlot({
        imdb_data[, c('year', 'rating')] %>%
            group_by(year) %>%
            count(rating) %>%
            ggplot(mapping = aes(x = year, y = n, color = rating)) +
            geom_line() + 
            geom_point() +
            xlab(label = 'year') +
            ylab(label = 'Count') +
            theme_bw()
    })
}

shinyApp(ui=imdbUI, server=imdbServer)

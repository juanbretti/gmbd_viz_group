# Libraries
library(shiny)
library(tidyverse)
library(skimr)
library(ggpmisc)
library(lubridate)
library(ggridges)
library(scales)
# library(PerformanceAnalytics)
# Maps
library(maps)
# https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true
library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

# Environment
df <- read_delim(file="madrid_transactions.csv", delim = ',', locale = locale(decimal_mark = '.'))
theme_set(theme_ridges())

# Skim
text_skim <- skim(df)

# # Correlation plot
# df_mod <- df %>%
#     mutate_if(is.character, function(x) as.numeric(as.factor(x))) %>%
#     select(-tx_date_proc)
# chart_correlation1 <- chart.Correlation(df_mod, histogram=TRUE, pch=19)

df_mod_class_top <- function(df, class, group, top) {
    df_mod <- df %>% 
        rename(`class`= !!class,
               `group`= !!group)
    
    df_top_class <- df_mod %>% 
        group_by(`class`) %>% 
        summarise(Total=sum(`amount`)) %>% 
        top_n(top, Total) %>%
        arrange(desc(Total))
    
    df_top_group <- df_mod %>% 
        group_by(`group`) %>% 
        summarise(Total=sum(`amount`)) %>% 
        top_n(5, Total) %>%
        arrange(desc(Total))
    
    df_grouped <- df_mod %>% 
        mutate(top_class=ifelse(`class` %in% df_top_class[['class']], `class`, 'Other')) %>% 
        mutate(top_class=factor(top_class, levels=c(df_top_class[['class']], 'Other'))) %>% 
        mutate(top_group=ifelse(`group` %in% df_top_group[['group']], `group`, 'Other')) %>% 
        mutate(top_group=factor(top_group, levels=c(df_top_group[['group']], 'Other')))
    
    return(df_grouped)
}

plot_distribution1 <- function(df, class) {
    mu <- df %>% 
        group_by(top_class) %>% 
        summarise(grp.mean=mean(amount))
    
    plot_distribution1 <- ggplot(df, aes(x=amount, color=top_class, fill=top_class)) +
        geom_density(alpha=0.1)+
        geom_vline(data=mu, aes(xintercept=grp.mean, color=top_class), linetype="dashed")+
        labs(title="Amount", 
             subtitle=paste('Amount distribution per', class), 
             x="Amount in log scale", 
             y = "Density", 
             fill=class, color=class)+
        scale_x_log10() +
        scale_fill_viridis_d(name=class) +
        theme(axis.text.y=element_blank())
    
    plot_boxplot1 <- ggplot(df, aes(x=amount, y=top_class, fill=top_class)) +
        geom_boxplot(alpha=0.1) +
        scale_x_log10() +
        theme(legend.position = "none") +
        scale_fill_viridis_d() +
        labs(title="Box-plot of the amount", 
             subtitle=paste('Amount distribution per', class, 'using a box-plot'), 
             x="Amount in log scale", 
             y = class, 
             fill=class, color=class)
    
    return(list(plot_distribution1, plot_boxplot1))
}

plot_distribution2 <- function(df, class) {
    df_sort <- df %>% 
        group_by(top_class) %>% 
        summarise(n_=n(),
                  sum_=sum(amount),
                  mean_=mean(amount)) %>% 
        arrange((sum_)) %>% 
        .[['top_class']]
    
    df_grouped <- df %>% 
        group_by(top_class, hour) %>% 
        summarise(n_=n(),
                  sum_=sum(amount),
                  mean_=mean(amount)) %>% 
        mutate(top_class = factor(top_class, df_sort))
    
    gg <- ggplot(df_grouped, aes(y=top_class, x=sum_, fill=top_class)) +
        geom_density_ridges(alpha=0.1) +
        labs(title = paste(class, "spendings"),
             subtitle = "Aggregated by hours",
             y = class,
             x = "Total spending") +
        theme(legend.position = "none") +
        scale_fill_viridis_d()
    
    return(gg)
}


plot_distribution_country_category <- function(df, class, group) {
    mu <- df %>% 
        group_by(top_class) %>% 
        summarise(grp.mean=mean(amount))
    
    gg <- ggplot(df, aes(x=amount, color=top_class, fill=top_class)) +
        facet_wrap(vars(top_group)) +
        geom_density(alpha=0.1)+
        geom_vline(data=mu, aes(xintercept=grp.mean, color=top_class), linetype="dashed")+
        labs(title = paste(class, "spendings"),
             subtitle = paste("One plot per", group),
             y = 'Density',
             x = "Amount per operation") +
        coord_cartesian(xlim = c(0, 200), ylim = c(0, 0.02)) +
        theme(legend.position = "none",
              axis.text.y=element_blank())
    
    return(gg)
}

map_location <- function(df, class) {
    # https://rpubs.com/Lluis_Ramon/Prestantacion_ggplot2_ggmap
    # https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
    data(wrld_simpl)
    world_ggmap <- fortify(wrld_simpl, region = "ISO2")
    
    df_grouped <- df %>% 
        group_by(`customer_country`, `category`) %>% 
        summarise(n_=n(),
                  amount_=sum(amount)) %>% 
        mutate(row_=row_number())
    
    df_top_country <- df %>% 
        group_by(`customer_country`) %>% 
        summarise(Total=sum(`amount`)) %>% 
        top_n(5, Total) %>%
        arrange(desc(Total)) %>% 
        .[['customer_country']]
    
    world_ggmap_mean <- world_ggmap %>%
        group_by(id) %>%
        summarise(long = mean(long), lat = mean(lat)) %>% 
        filter(id %in% df_top_country)
    
    gg <- ggplot(df_grouped) +
        geom_map(aes(map_id = customer_country, fill = n_), map = world_ggmap) +
        expand_limits(x = world_ggmap$long, y = world_ggmap$lat) +
        geom_text(aes(x=long, y=lat, label = id), data=world_ggmap_mean, size = 3, hjust = 0.5, color='red')+
        scale_fill_viridis_c()+
        theme_void() +
        theme(legend.position = "none")
    
    if (class=='Category') {
        gg <- gg + facet_wrap(vars(category))
    }
    
    return(gg)
}

plot_distribution_hourofday <- function(df, class) {
    df_grouped <- df %>% 
        mutate(day_hour_=floor_date(tx_date_proc, "hour"),
               hour_=make_datetime(
                   hour = hour(tx_date_proc),
                   min = minute(tx_date_proc),
                   sec = second(tx_date_proc)))
    
    gg <- ggplot(df_grouped, aes(x=hour, color=weekday, fill=weekday)) +
        facet_wrap(vars(top_class)) +
        geom_density(alpha=0.1) +
        theme(axis.text.x=element_text(angle=90, hjust=1),
              axis.text.y=element_blank()) +
        labs(title = paste(class, "spendings"),
             subtitle = paste("One plot per", class),
             y = 'Density',
             x = "Hour of the day")
    
    return(gg)
}

plot_heatmap_hourofday <- function(df, class) {
    df_grouped <- df %>% 
        group_by(top_class, hour) %>% 
        summarise(n_=n(),
                  sum_=sum(amount),
                  mean_=mean(amount))
    
    gg <- ggplot(df_grouped, aes(x=hour, y=top_class, fill=sum_)) +
        geom_tile(alpha=0.8) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        scale_fill_viridis_c(name = "Spenditure") +
        labs(title = "Spendings",
             subtitle = "Total spenditure",
             y = class,
             x = "Hour of the day")
    
    return(gg)
}

ui <- navbarPage(title = "Citibank",
                 tabPanel("Top rules",
                          sidebarLayout(
                              sidebarPanel(
                                  div(img(src="logo.png",height=194/2,width=300/2), style="text-align: center;"),
                                  br(),
                                  selectInput('class', 'Class', choices=c('Country', 'Category'), selected = 'Country'),
                                  sliderInput('top', 'Top', min = 1, max = 50, value = 5),
                                  br(),
                                  span("Select the top rules based on the proposed sorting criteria. It's recommended the 'category'.")
                              ),
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Map", 
                                                       span("Use the drop down or click to selct a node."),
                                                       br(),
                                                       br(),
                                                       plotOutput('map_location')
                                              ),
                                              tabPanel("Distribution", 
                                                       plotOutput('plot_distribution1_1'),
                                                       br(),
                                                       span("Click the objects to display the association rule description."),
                                                       br(),
                                                       plotOutput('plot_distribution1_2'),
                                                       br(),
                                                       plotOutput('plot_distribution2'),
                                                       br(),
                                                       plotOutput('plot_distribution_country_category')
                                              ),
                                              tabPanel("Hour of the day", 
                                                       span("Click the objects to display the association rule description."),
                                                       br(),
                                                       plotOutput('plot_distribution_hourofday'),
                                                       br(),
                                                       plotOutput('plot_heatmap_hourofday')
                                              )
                                  )
                              )
                          )
                 ),
                 tabPanel("Descriptive study",
                          sidebarLayout(
                              sidebarPanel(
                                  div(img(src="logo.png",height=194/2,width=300/2), style="text-align: center;"),
                                  br(),
                                  span("General descriptive information of the transactions and rules")
                              ),
                              mainPanel(
                                  h3("Datase summary"),
                                  verbatimTextOutput('skim')
                              )
                          )
                 )
)

server <- function(input, output) {
    
    # Skim
    output$skim <- renderPrint(text_skim)
    
    # Distribution
    observeEvent(c(input$class, input$top), ignoreNULL = FALSE, ignoreInit = FALSE, {
        # Selection
        class <- if(input$class=='Category') 'category' else 'customer_country'
        group <- if(input$class=='Category') 'customer_country' else 'category'
        group2 <- if(input$class=='Category') 'Country' else 'Category'
        # Dataset
        print(class)
        df_mod <- df_mod_class_top(df, class, group, input$top)

        plot_temp <- plot_distribution1(df_mod, input$class)
        output$plot_distribution1_1 <- renderPlot(plot_temp[1])
        output$plot_distribution1_2 <- renderPlot(plot_temp[2])

        output$plot_distribution2 <- renderPlot(plot_distribution2(df_mod, input$class))

        output$plot_distribution_country_category <- renderPlot(plot_distribution_country_category(df_mod, input$class, group2))

        output$map_location <- renderPlot(map_location(df, input$class))

        output$plot_distribution_hourofday <- renderPlot(plot_distribution_hourofday(df_mod, input$class))

        output$plot_heatmap_hourofday <- renderPlot(plot_heatmap_hourofday(df_mod, input$class))
    })
    
}

shinyApp(ui = ui, server = server)

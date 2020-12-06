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
library(maptools)
library(maps)

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

# Distribution plot of the amount
plot_distribution1 <- function(class, top) {
    df_mod <- df %>% 
        rename(`class`= !!class)
    
    df_top <- df_mod %>% 
        group_by(`class`) %>% 
        summarise(Total=sum(`amount`)) %>% 
        top_n(top, Total) %>%
        arrange(desc(Total))
    
    df_grouped <- df_mod %>% 
        mutate(top_class=ifelse(`class` %in% df_top[['class']], `class`, 'Other')) %>% 
        mutate(top_class=factor(top_class, levels=c(df_top[['class']], 'Other')))
    
    mu <- df_grouped %>% 
        group_by(top_class) %>% 
        summarise(grp.mean=mean(amount))
    
    plot_distribution1 <- ggplot(df_grouped, aes(x=amount, color=top_class, fill=top_class)) +
        geom_density(alpha=0.1)+
        geom_vline(data=mu, aes(xintercept=grp.mean, color=top_class), linetype="dashed")+
        labs(title="Amount",x="Amount in log scale", y = "Density")+
        theme_classic() +
        scale_x_log10()

    plot_boxplot1 <- ggplot(df_grouped, aes(x=amount, y=top_class, fill=top_class)) +
        geom_boxplot(alpha=0.1) +
        scale_x_log10() +
        theme(legend.position = "none")
    
    return(list(plot_distribution1, plot_boxplot1))
}

plot_distribution_country_category <- function(class, top) {
    df_mod <- df %>% 
        rename(`class`= !!class,
               `group`= !!(if(class=='category') 'customer_country' else 'category'))
    
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
    
    mu <- df_grouped %>% 
        group_by(top_class) %>% 
        summarise(grp.mean=mean(amount))
    
    gg <- ggplot(df_grouped, aes(x=amount, color=top_class, fill=top_class)) +
        facet_wrap(vars(top_group)) +
        geom_density(alpha=0.1)+
        geom_vline(data=mu, aes(xintercept=grp.mean, color=top_class), linetype="dashed")+
        labs(title="Amount",x="Amount", y = "Density") +
        coord_cartesian(xlim = c(0, 200), ylim = c(0, 0.02)) +
        theme(legend.position = "none")

    return(gg)
}

map_location <- function(class) {
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
    
    if (class=='category') {
        gg <- gg + facet_wrap(vars(category))
    }
    
    return(gg)
}

plot_distribution_hourofday <- function(class) {
    df_mod <- df %>% 
        rename(`class`= !!class)
    
    df_grouped <- df_mod %>% 
        mutate(day_hour_=floor_date(tx_date_proc, "hour"),
               hour_=make_datetime(
                   hour = hour(tx_date_proc),
                   min = minute(tx_date_proc),
                   sec = second(tx_date_proc)))
    
    gg <- ggplot(df_grouped, aes(x=hour_, color=weekday, fill=weekday)) +
        facet_wrap(vars(class)) +
        geom_density(alpha=0.1) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        coord_cartesian(ylim = c(0, 0.00005))
    
    return(gg)
}


ui <- fluidPage(

    # Application title
    titlePanel("Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('class', 'Class', choices=c('customer_country', 'category'), selected = 'customer_country'),
            sliderInput("top", "Top countries:", min = 1, max = 50, value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            verbatimTextOutput('skim'),
            plotOutput('plot_distribution1'),
            plotOutput('plot_distribution2'),
            plotOutput('plot_distribution_country_category'),
            plotOutput('map_location'),
            plotOutput('plot_distribution_hourofday')
        )
    )
)

server <- function(input, output) {
    
    # Skim
    output$skim <- renderPrint(text_skim)
    
    # Distribution
    observeEvent(c(input$class, input$top), ignoreNULL = FALSE, ignoreInit = FALSE, {
        
        plot_temp <- plot_distribution1(input$class, input$top)
        output$plot_distribution1 <- renderPlot(plot_temp[1])  
        output$plot_distribution2 <- renderPlot(plot_temp[2])
        
        output$plot_distribution_country_category <- renderPlot(plot_distribution_country_category(input$class, input$top))

        output$map_location <- renderPlot(map_location(input$class))
        
        output$plot_distribution_hourofday <- renderPlot(plot_distribution_hourofday(input$class))
    })

}

shinyApp(ui = ui, server = server)

# Libraries
library(shiny)
library(tidyverse)
library(skimr)
library(ggpmisc)
library(lubridate)
library(ggridges)
library(scales)
library(ggrepel)
library(ggalluvial)
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

df_mod_class_top <- function(df, class, group, top, other) {
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
        top_n(top, Total) %>%
        arrange(desc(Total))
    
    daytime_sorted <- df_mod %>% 
        group_by(`daytime`) %>% 
        summarise(hour=mean(`hour`)) %>% 
        arrange((hour)) %>% 
        .[['daytime']]

    df_grouped <- df_mod %>% 
        mutate(top_class=ifelse(`class` %in% df_top_class[['class']], `class`, 'Other')) %>% 
        mutate(top_class=factor(top_class, levels=c(df_top_class[['class']], 'Other'))) %>% 
        mutate(top_class=fct_rev(top_class)) %>% 
        mutate(top_group=ifelse(`group` %in% df_top_group[['group']], `group`, 'Other')) %>% 
        mutate(top_group=factor(top_group, levels=c(df_top_group[['group']], 'Other'))) %>% 
        mutate(top_group=fct_rev(top_group)) %>% 
        mutate(daytime=factor(daytime, levels=daytime_sorted))
    
    if (is.null(other)) {
        df_grouped <- df_grouped %>%
            filter(`top_class` != 'Other') %>% 
            filter(`top_group` != 'Other')
    }

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

map_location <- function(df, class, top) {
    # https://rpubs.com/Lluis_Ramon/Prestantacion_ggplot2_ggmap
    # https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
    data(wrld_simpl)
    world_ggmap <- fortify(wrld_simpl, region = "ISO2")
    
    df_grouped <- df %>% 
        group_by(`customer_country`, `category`) %>% 
        summarise(n_=n(),
                  amount_=mean(amount)) %>% 
        mutate(row_=row_number())
    
    df_top_country <- df %>% 
        group_by(`customer_country`) %>% 
        summarise(Total=mean(`amount`)) %>% 
        top_n(top, Total) %>%
        arrange(desc(Total)) %>% 
        .[['customer_country']]
    
    world_ggmap_mean <- world_ggmap %>%
        group_by(id) %>%
        summarise(long = mean(long), lat = mean(lat)) %>% 
        filter(id %in% df_top_country)
    
    gg <- ggplot(df_grouped) +
        geom_map(aes(map_id = customer_country, fill = amount_), map = world_ggmap) +
        expand_limits(x = world_ggmap$long, y = world_ggmap$lat) +
        geom_text(aes(x=long, y=lat, label = id), data=world_ggmap_mean, size = 3, hjust = 0.5, color='red')+
        scale_fill_viridis_c(name='Mean amount')+
        theme_void()
        # theme(legend.position = "none")
    
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
             x = "Hour of the day",
             fill='Weekday', color='Weekday')
    
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
        scale_fill_viridis_c(name = "Expenditure") +
        labs(title = "Spendings",
             subtitle = "Total expenditure",
             y = class,
             x = "Hour of the day")
    
    return(gg)
}

plot_category_country <- function(df, class, group) {
    df_grouped <- df %>% 
        group_by(top_class, top_group) %>% 
        summarise(sum_=sum(amount)/1e3)
    
    gg <- ggplot(df_grouped, aes(x = top_group, y = sum_, fill = top_class, label = paste(top_class, "\n", round(sum_, 0)))) +  # Create stacked bar chart
        geom_bar(stat = "identity", aes(alpha=0.8)) +
        geom_text_repel(size = 3, position = position_stack(vjust = 0.5)) + #, angle = 45
         theme(axis.text.x=element_text(angle=90, hjust=1)) +
        coord_flip() +
        labs(title = paste("Spendings per", group, 'and', class),
             subtitle = "Total expenditure",
             x = group,
             y = "Amount in thousands") +
        scale_fill_viridis_d(name=class) +
        scale_alpha_continuous(guide=FALSE)
    
    return(gg)  
}

plot_sankey <- function(df, class) {
    # https://rkabacoff.github.io/datavis/Other.html
    
    df_grouped <- df %>% 
        group_by(top_class, daytime) %>% 
        summarise(amount_=sum(amount))
    
    dim(df_grouped)
    
    gg <- ggplot(df_grouped, aes(axis1 = top_class, axis3 = daytime, y = amount_)) +
        geom_alluvium(aes(fill=top_class)) +
        geom_stratum() +
        geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
        scale_x_discrete(limits = c(class, 'Daytime'), expand = c(.1, .1)) +
        labs(title = "Flow and allocation", subtitle = paste0('Stratified by ', class, ' and daytime'), y = "Proportional to total amount") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.y=element_blank()) +
        scale_fill_viridis_d()
    
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
                                  checkboxGroupInput('other', "Show 'Other'", choices = c('Show'), selected=c('Show')),
                                  br(),
                                  span("Select the top rules based on the proposed sorting criteria. It's recommended the 'category'.")
                              ),
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Map", 
                                                       br(),
                                                       plotOutput('map_location', height='800px'),
                                                       br(),
                                                       br(),
                                                       span('Here we drill down into the top 5 categories with spending where Fashion & Shoes highly attracts the Chinese tourists. An international campaign targeted the Asian market can be done as there is an appetite from Japan & China’s tourists into the fashion industry. Also, US’s tourists show powerful appetite for Bars & Restaurants category, this can be a sign for having US’s themed restaurants & bars can be furthered invested in to attract more US tourists.'),
                                                       br(),
                                                       plotOutput('plot_category_country')
                                              ),
                                              tabPanel("Flow",
                                                       br(),
                                                       plotOutput('plot_sankey', height='1000px')
                                              ),
                                              tabPanel("Distribution", 
                                                       br(),
                                                       plotOutput('plot_distribution1_1'),
                                                       br(),
                                                       br(),
                                                       span('This ridgelines chart explains the spending distribution by category. The top 3 categories with high variation is Fashion & Shoes, Accommodation, Bars & Restaurants. These 3 categories account for 83% of all spending. Therefore, the focus of promoting places for tourists should be allocated for these categories.'),
                                                       br(),
                                                       br(),
                                                       plotOutput('plot_distribution1_2'),
                                                       br(),
                                                       br(),
                                                       plotOutput('plot_distribution2'),
                                                       br(),
                                                       br(),
                                                       plotOutput('plot_distribution_country_category')
                                              ),
                                              tabPanel("Hour of the day", 
                                                       br(),
                                                       span('We can see similar pattern in purchase behavior between Fridays & Thursdays. However, some nationalities are morning spending more than the others. In these top 5 nationalities spenders, US & JP nationalities exhibited early spending pattern and that can be an opportunity to target these tourists with customized offers.'),
                                                       br(),
                                                       plotOutput('plot_distribution_hourofday'),
                                                       br(),
                                                       br(),
                                                       span('This visualization is a heatmap that explains the purchase behavior of top 5 tourists countries by the hour of the day. Here we can see some variation based on the purchase category, for example the peak hours of the category Fashion & Shoes begins afternoon (16) and starts to decline after 18. Our recommendation is to activate marketing campaigns during the off-hours in specific categories (i.e. Bars & Restaurants) to encourage tourists visiting these shops with tempting discounts or offers.'),
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
                                  h3("Dataset summary"),
                                  verbatimTextOutput('skim')
                              )
                          )
                 )
)

server <- function(input, output) {
    
    # Skim
    output$skim <- renderPrint(text_skim)
    
    # Distribution
    observeEvent(c(input$class, input$top, input$other), ignoreNULL = FALSE, ignoreInit = FALSE, {
        # Selection
        class <- if(input$class=='Category') 'category' else 'customer_country'
        group <- if(input$class=='Category') 'customer_country' else 'category'
        group2 <- if(input$class=='Category') 'Country' else 'Category'
        # other <- if(is.null(input$other)) 'Hide' else 'Show'
        # Dataset
        df_mod <- df_mod_class_top(df, class, group, input$top, input$other)

        plot_temp <- plot_distribution1(df_mod, input$class)
        output$plot_distribution1_1 <- renderPlot(plot_temp[1])
        output$plot_distribution1_2 <- renderPlot(plot_temp[2])

        output$plot_distribution2 <- renderPlot(plot_distribution2(df_mod, input$class))
        output$plot_distribution_country_category <- renderPlot(plot_distribution_country_category(df_mod, input$class, group2))
        output$map_location <- renderPlot(map_location(df, input$class, input$top))
        output$plot_distribution_hourofday <- renderPlot(plot_distribution_hourofday(df_mod, input$class))
        output$plot_heatmap_hourofday <- renderPlot(plot_heatmap_hourofday(df_mod, input$class))
        output$plot_category_country <- renderPlot(plot_category_country(df_mod, input$class, group2))
        output$plot_sankey <- renderPlot(plot_sankey(df_mod, input$class))
        
    })
    
}

shinyApp(ui = ui, server = server)

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
        summarise(Total=sum(`amount`),
                  Mean=mean(`amount`),
                  N=n()) %>% 
        top_n(top, Total) %>%
        arrange(desc(Total))
    
    df_top_group <- df_mod %>% 
        group_by(`group`) %>% 
        summarise(Total=sum(`amount`),
                  Mean=mean(`amount`),
                  N=n()) %>% 
        top_n(top, Total) %>%
        arrange(desc(Total))
    
    daytime_sorted <- df_mod %>% 
        group_by(`daytime`) %>% 
        summarise(hour=sum(`hour`)) %>% 
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
    
    # plot_distribution1 <- ggplot(df, aes(x=amount, color=top_class, fill=top_class)) +
    #     geom_density(alpha=0.1)+
    #     geom_vline(data=mu, aes(xintercept=grp.mean, color=top_class), linetype="dashed")+
    #     labs(title="Amount", 
    #          subtitle=paste('Amount distribution per', class), 
    #          x="Amount in log scale", 
    #          y = "Density", 
    #          fill=class, color=class)+
    #     scale_x_log10() +
    #     scale_fill_viridis_d(name=class) +
    #     theme(axis.text.y=element_blank())
    
    plot_boxplot1 <- ggplot(df, aes(x=amount, y=top_class, fill=top_class)) +
        geom_boxplot(alpha=0.1) +
        stat_summary(fun.x=mean, geom="point", shape=20, size=4, color="black", fill="black", alpha=0.5) +
        scale_x_log10() +
        theme(legend.position = "none") +
        scale_fill_viridis_d(name=class) +
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
        theme(axis.text.y=element_blank()) +
        scale_fill_viridis_d(name=class) +
        scale_fill_discrete(name=class) +
        scale_color_discrete(name=class)
    
    return(gg)
}

map_location <- function(df, class, top, counter) {
    # https://rpubs.com/Lluis_Ramon/Prestantacion_ggplot2_ggmap
    # https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
    data(wrld_simpl)
    world_ggmap <- fortify(wrld_simpl, region = "ISO2")
    
    df_grouped <- df %>% 
        group_by(`customer_country`, `category`) %>% 
        summarise(sum_=sum(amount)/1e3,
                  mean_=mean(amount),
                  n_=n()) %>% 
        mutate(row_=row_number())
    
    df_grouped <- counter_convert(df_grouped, counter)
    
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
        geom_map(aes(map_id = customer_country, fill = counter_), map = world_ggmap) +
        expand_limits(x = world_ggmap$long, y = world_ggmap$lat) +
        geom_text(aes(x=long, y=lat, label = id), data=world_ggmap_mean, size = 3, hjust = 0.5, color='red')+
        scale_fill_viridis_c(name=counter)+
        theme_void()
    
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

counter_convert <- function(df, counter) {
    if (counter == 'Total sum of the operations') {
        df <- rename(df, `counter_`= sum_)
    } else if (counter == 'Mean of the operations') {
        df <- rename(df, `counter_`= mean_)
    } else if (counter == 'Number of operations') {
        df <- rename(df, `counter_`= n_)
    } else {
        df
    }
    
    return(df)
}

plot_category_country <- function(df, class, group, counter) {
    df_grouped <- df %>% 
        group_by(top_class, top_group) %>% 
        summarise(sum_=sum(amount)/1e3,
                  mean_=mean(amount),
                  n_=n())
    
    df_grouped <- counter_convert(df_grouped, counter)
    
    gg <- ggplot(df_grouped, aes(x = top_group, y = counter_, fill = top_class, label = paste(top_class, "\n", round(counter_, 0)))) +  # Create stacked bar chart
        geom_bar(stat = "identity", aes(alpha=0.8)) +
        geom_text_repel(size = 3, position = position_stack(vjust = 0.5)) + #, angle = 45
         theme(axis.text.x=element_text(angle=90, hjust=1)) +
        coord_flip() +
        labs(title = paste("Spendings per", group, 'and', class),
             subtitle = "Total expenditure",
             x = group,
             y = counter) +
        scale_fill_viridis_d(name=class) +
        scale_alpha_continuous(guide=FALSE)
    
    return(gg)  
}

plot_sankey <- function(df, class, counter) {
    # https://rkabacoff.github.io/datavis/Other.html
    
    df_grouped <- df %>% 
        group_by(top_class, daytime) %>% 
        summarise(sum_=sum(amount),
                  mean_=mean(amount),
                  n_=n())
    
    df_grouped <- counter_convert(df_grouped, counter)
    
    gg <- ggplot(df_grouped, aes(axis1 = top_class, axis3 = daytime, y = counter_)) +
        geom_alluvium(aes(fill=top_class)) +
        geom_stratum() +
        geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
        scale_x_discrete(limits = c(class, 'Daytime'), expand = c(.1, .1)) +
        labs(title = "Flow and allocation", subtitle = paste0('Stratified by ', class, ' and daytime'), y = paste("Proportional to ", tolower(counter))) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.y=element_blank()) +
        scale_fill_viridis_d()
    
    return(gg)
}

ui <- navbarPage(title = "Citibank",
                 tabPanel("Top entities",
                          sidebarLayout(
                              sidebarPanel(
                                  div(img(src="logo.png",height=194/2,width=300/2), style="text-align: center;"),
                                  br(),
                                  span("Select the top countries or categories for sorting criteria."),
                                  br(),
                                  br(),
                                  selectInput('class', 'Class', choices=c('Country', 'Category'), selected = 'Country'),
                                  sliderInput('top', 'Top', min = 1, max = 50, value = 5),
                                  checkboxGroupInput('other', "Show 'Other'", choices = c('Show'), selected=c('Show')),
                              ),
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Map", 
                                                       br(),
                                                       selectInput('counter_map', 'Counter', choices=c('Total sum of the operations', 'Mean of the operations', 'Number of operations'), selected = 'Total sum of the operations'),
                                                       br(),
                                                       span("When 'Country' is selected for class grouping, we can confirm the information covers the whole world."),
                                                       br(),
                                                       span("When 'Category' is selected, also can confirm the classified information covers almost most of the countries"),
                                                       br(),
                                                       plotOutput('map_location', height='800px'),
                                                       br(),
                                                       br(),
                                                       span("When 'Category' is selected and 'Number of operations'"),
                                                       br(),
                                                       span("Top 5 countries in terms of number of transactions are: US, FR, GB, IT and BR."),
                                                       br(),
                                                       span("Top 5 countries in terms of total sales amount are: US, GB, CN, FR and JP."),
                                                       br(),
                                                       span("Top 5 countries in terms of average ticket are: VN, TH, SA, ID and AO."),
                                                       br(),
                                                       br(),
                                                       span("Notice the different top countries based on the 3 KPIs (Total Count, Total Value, Average Ticket)."),
                                                       br(),
                                                       span("If we consider those countries with the highest average ticket, they just have relatively fewer number of transactions (less than 50). But when they transact, they spend more compared to the other countries."),
                                                       br(),
                                                       br(),
                                                       span("When 'Country' is selected, we can drill down into the top 5 categories with spending where Fashion & Shoes highly attracts the US, GB and Chinese buyers. An international campaign targeted the Asian market can be done as there is an appetite from Japan & China’s tourists into the fashion industry. Also, US’s tourists show powerful appetite for Bars & Restaurants category, this can be a sign for having US’s themed restaurants & bars can be furthered invested in to attract more US tourists."),
                                                       br(),
                                                       plotOutput('plot_category_country')
                                              ),
                                              tabPanel("Flow",
                                                       br(),
                                                       selectInput('counter_flow', 'Counter', choices=c('Total sum of the operations', 'Mean of the operations', 'Number of operations'), selected = 'Total sum of the operations'),
                                                       span("When 'Category' is selected and 'Sum', we can confirm most of the Fashion & Shoes purchase happens in the Afternon and Evening. Also some of the operations happens ad mid morning. Lunch and after office time for workers is a great target for ad campains in Social Media and Mall centers."),
                                                       br(),
                                                       br(),
                                                       plotOutput('plot_sankey', height='1000px')
                                              ),
                                              tabPanel("Distribution", 
                                                       br(),
                                                       span("When 'Country' is selected, the following two plots shows in a logarithmic scale that from the highest expenders (total sum of amount), the highest mean per transaction came from China."),
                                                       br(),
                                                       span("When 'Category' is selected, it can be read that from the highest cumulative amount, the highest mean expese is the 'Accomodation'."),
                                                       br(),
                                                       span("These 'Distribution' plots could help the marketing team to define an ad targeted campain."),
                                                       br(),
                                                       plotOutput('plot_distribution1_2'),
                                                       br(),
                                                       br(),
                                                       span("When 'Category' is selected. This ridgelines chart explains the spending distribution by category. The top 3 categories with high variation is Fashion & Shoes, Accommodation, Bars & Restaurants. These 3 categories account for 83% of all spending. Therefore, the focus of promoting places for tourists should be allocated for these categories."),
                                                       br(),
                                                       plotOutput('plot_distribution2'),
                                                       br(),
                                                       br(),
                                                       span("When 'Category' is selected. Is noticed the huge spike in low transactions in Japan in Transportations."),
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
                                                       span("When 'Category' is selected. This visualization is a heatmap that explains the purchase behavior of top 5 tourists countries by the hour of the day. Here we can see some variation based on the purchase category, for example the peak hours of the category Fashion & Shoes begins afternoon (16) and starts to decline after 18. Our recommendation is to activate marketing campaigns during the off-hours in specific categories (i.e. Bars & Restaurants) to encourage tourists visiting these shops with tempting discounts or offers."),
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
                                  span("General descriptive information of the transactions")
                              ),
                              mainPanel(
                                  h3("Dataset summary"),
                                  br(),
                                  span("Simple statistics of the dataset"),
                                  br(),
                                  br(),
                                  verbatimTextOutput('skim')
                              )
                          )
                 )
)

server <- function(input, output) {
    
    # Skim
    output$skim <- renderPrint(text_skim)
    
    # Distribution
    observeEvent(c(input$class, input$top, input$other, input$counter_map, input$counter_flow), ignoreNULL = FALSE, ignoreInit = FALSE, {
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
        output$map_location <- renderPlot(map_location(df, input$class, input$top, input$counter_map))
        output$plot_distribution_hourofday <- renderPlot(plot_distribution_hourofday(df_mod, input$class))
        output$plot_heatmap_hourofday <- renderPlot(plot_heatmap_hourofday(df_mod, input$class))
        output$plot_category_country <- renderPlot(plot_category_country(df_mod, input$class, group2, input$counter_map))
        output$plot_sankey <- renderPlot(plot_sankey(df_mod, input$class, input$counter_flow))
        
    })
    
}

shinyApp(ui = ui, server = server)

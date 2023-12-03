#---------------
#setwd("STAT628-Module3-Group6/")
#load packages
library(shiny)
library(shinydashboard)
library(DT)
library(fmsb)
library(ggplot2)
library(dplyr)
library(sf)
library(leaflet)
library(cowplot)
diytheme <- theme(plot.title=element_text(face="bold.italic",
                                          size="14", color="brown"),
                  axis.title=element_text(face="bold.italic",
                                          size=10, color="brown"),
                  axis.text=element_text(face="bold", size=9,
                                         color="darkblue"),
                  panel.background=element_rect(fill="white",
                                                color="darkblue"),
                  panel.grid.major.y=element_line(color="grey",
                                                  linetype=1),
                  panel.grid.minor.y=element_line(color="grey",
                                                  linetype=2),
                  panel.grid.minor.x=element_blank(),
                  legend.position="top") 


#-----
#define functions

calculateMeanVarPlot <- function(dataset) {
  # 假设输入的数据集中最后一列不需要
  da <- dataset[,-12] 
  
  # 计算平均值
  mean_dish <- sum(da[,1]*da[,-1])/sum(da[,-1])
  
  # 定义计算均值和标准差的函数
  mean_var <- function(f) {
    res <- c()
    for (i in 1:length(f)) {
      res <- c(res, rep(i,f[i]))
    }
    mu <- mean(res)
    sd <- sd(res)
    return(c(mu, sd))
  }
  
  # 应用mean_var函数到数据集的每一列
  results <- lapply(da[-1], mean_var) 
  results <- do.call(cbind, results)
  
  # 转换结果为数据框并设置列名
  results_df <- as.data.frame(t(results))
  names(results_df) <- c("mu", "sd")
  results_df$category <- rownames(results_df)
  
  # 创建ggplot对象
  p <- ggplot(data = results_df, aes(x = category, y = mu, group = 1)) +
    geom_point(aes(colour = category, size = sd)) + 
    geom_line() +
    scale_colour_manual(values = rainbow(n = length(unique(results_df$category)))) + 
    scale_size_continuous(name = "Standard Deviation") + 
    ggtitle("Mean and Standard Deviation of Food Categories") +
    geom_hline(yintercept = mean_dish, color = "red") +
    xlab(" ") +
    ylab("Mean Value") +
    diytheme +
    theme(legend.position = "bottom") +
    guides(size = FALSE) +
    guides(colour = FALSE)
  
  return(p)
}




createRadarChart <- function(dataset) {
  PA_ch <- dataset
  
  PA_ch_presentage <- PA_ch %>%
    filter(stars != 3.0) %>%
    mutate(star_category = case_when(
      stars %in% c(1.0, 2.0) ~ "low",
      stars %in% c(4.0, 5.0) ~ "high"
    )) %>%
    group_by(star_category) %>%
    summarise_all(sum) %>%
    mutate(across(-star_category, ~./review_count)) %>%
    select(-stars, -review_count)
  
  df <- rbind(
    max = c(1, 1, 1, 1, 1, 1),
    min = c(0, 0, 0, 0, 0, 0),
    PA_ch_presentage[,-1]
  )
  
  # Create the radar map without the legend argument
  radarchart(
    df,
    axistype = 1,
    pcol = c("blue", "red"),
    pfcol = c(rgb(0, 0, 1, 0.3), rgb(1, 0, 0, 0.3)),
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = c("0%", "25%", "50%", "75%", "100%"),
    cglwd = 0.8,
    vlcex = 0.8,
    title = "Radar map of high and low stars"
  )
  
  # Add the legend function with the legend argument
  legend(
    "topright",
    legend = c("high", "low"),
    fill = c("blue", "red"),
    bty = "n",
    cex = 0.8
  )
}

generateBarPlot <- function(data, variable) {
  # 将字符串变量转换为符号
  variable_sym <- rlang::sym(variable)
  
  plot_data1 <- data[data$RestaurantsPriceRange2%in% c(1,2),]
  plot_data2 <- data[data$RestaurantsPriceRange2%in% c(3,4),]
  # 使用符号进行分组和汇总
  
  
  plot_data1 <- plot_data1 %>%
    group_by(!!variable_sym) %>%
    filter((!!variable_sym != "") & (!is.na(!!variable_sym)) & (!!variable_sym != "None")) %>%
    summarise(avg_star = mean(star, na.rm = TRUE), .groups = "drop")
  
  # 打印 plot_data 来调试
  
  
  # 创建条形图
  plot1 <- ggplot(plot_data1, aes_string(x = variable, y = "avg_star")) +
    geom_col(fill = "tan1", color = "black", alpha = 0.9) +
    geom_text(aes(label = sprintf("%.2f", avg_star)), vjust = -0.5, size = 5) +
    labs(x = paste(variable, "Category"), y = "Average Stars") +
    ggtitle("Low Price") +
    diytheme
  
  # 使用符号进行分组和汇总
  plot_data2 <- plot_data2 %>%
    group_by(!!variable_sym) %>%
    filter((!!variable_sym != "") & (!is.na(!!variable_sym)) & (!!variable_sym != "None")) %>%
    summarise(avg_star = mean(star, na.rm = TRUE), .groups = "drop")
  
  
  # 创建条形图
  plot2 <- ggplot(plot_data2, aes_string(x = variable, y = "avg_star")) +
    geom_col(fill = "purple", color = "black", alpha = 0.9) +
    geom_text(aes(label = sprintf("%.2f", avg_star)), vjust = -0.5, size = 5) +
    labs(x = paste(variable, "Category"), y = "Average Stars") +
    ggtitle("High Price") +
    diytheme
  
  plot <- plot_grid(plot1,plot2)
  
  return(plot)
}

#定义UI
#--------------

ui <- dashboardPage(
  dashboardHeader(title = "Yelp Analysis for Italian Restaurants in Philadelphia"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Atrribute visualize", tabName = "barPlot", icon = icon("table")),
      menuItem("Suggestions and plots", tabName = "cuisine", icon = icon("cutlery"))
    )
  ),
  dashboardBody(
    
    
    tabItems(
      tabItem(tabName = "home",
              h2("Introduction"),
              p("The app uses Yelp data to analyze Italian restaurants in Philadelphia, and restaurant operators can use the app to seek recommendations to improve their restaurant performance."),
              p("Contact information:"),
              p("Please reach out to any of our group members if there is any question:"),
              p("Baiheng Chen: bchen342@wisc.edu"),
              p("Ruizhen Jing: rjing5@wisc.edu"),
              p("Ziang Zeng: zeng86@wisc.edu")
      ),
      tabItem(
        tabName = "map",
        h2("Philadelphia Italian Restaurants Map"),
        fluidRow(
          column(6, leafletOutput("map")),
          #column(6, verbatimTextOutput("restaurantInfo")),
          column(6, DTOutput("restaurantTable"))# 显示餐厅信息的位置
        )
      ),
      tabItem(tabName = "barPlot",
              h2("Bar Plot"),
              selectInput("barPlotVariable", "Select Variable", choices = c("OutdoorSeating", "WiFi", "RestaurantsAttire","Alcohol","NoiseLevel","RestaurantsDelivery","GoodForKids"), selected = "OutdoorSeating"),
              plotOutput("barPlot"),
              fluidRow(
                column(12, verbatimTextOutput("selectedDescription"), style = "height: 1200px; overflow-y: auto;")
              )
      ),
      tabItem(tabName = "cuisine",
              h2("Different types of Italian restaurants"),
              sidebarLayout(
                sidebarPanel(
                  radioButtons("price", "Price", choices = c("High" = "high", "Low" = "low")),
                  radioButtons("location", "Location", choices = c("Downtown" = "center", "Other" = "not"))
                ),
                
                mainPanel(
                  fluidRow(
                    column(12, plotOutput("radarChart")) # 雷达图输出，占满整行
                  ),
                  h3("According to the analysis of indicators"),
                  conditionalPanel(
                    condition = "input.price == 'high' && input.location == 'center'",
                    h4("We evaluated each category of restaurant based on the five indicators and compared the high and low scores. This helped us identify the key factors for success for each category. For high-priced downtown Italian restaurants, customers value environment, service, and Price & Value. High-rated restaurants have better environment than low-rated ones, while low-rated restaurants have more complaints about Price & Value. High-priced restaurants need to provide an excellent dining ambiance to earn customers’ praise. A poor dining experience can make customers feel that the restaurant is not worth the money.")
                  ),
                  conditionalPanel(
                    condition = "input.price == 'low' && input.location == 'center'",
                    h4("We evaluated each category of restaurant based on the five indicators and compared the high and low scores. This helped us identify the key factors for success for each category. For low-priced downtown Italian restaurants, customers care about Service, Environment, and Price & Value, but only environment differs significantly between high and low ratings. These restaurants can boost their ratings by enhancing their dining ambiance, while keeping their service and Price & Value at the same level.")
                  ),
                  conditionalPanel(
                    condition = "input.price == 'low' && input.location == 'not'",
                    h4("We evaluated each category of restaurant based on the five indicators and compared the high and low scores. This helped us identify the key factors for success for each category. For low-priced Italian restaurants outside the city center, the main factors that distinguish high and low scores are environment and Price & Value. These restaurants can increase their star ratings by offering better value for money and enhancing their ambiance.")
                  ),
                  conditionalPanel(
                    condition = "input.price == 'high' && input.location == 'not'",
                    h4("We evaluated each category of restaurant based on the five indicators and compared the high and low scores. This helped us identify the key factors for success for each category. For high-priced Italian restaurants outside the city center, Environment and Service are the key factors that affect the ratings. High-rated restaurants have much better environment and service than low-rated ones. However, customers care less about Price & Value for high-rated restaurants than low-rated ones, which suggests that when the dining experience is bad, they will criticize the value of the expensive restaurant.")
                  ),
                  fluidRow(
                    column(12, plotOutput("meanVarPlot")) # 均值和标准差图输出，占满整行
                  ),
                  h3("According to the analysis of foods"),
                  conditionalPanel(
                    condition = "input.price == 'high' && input.location == 'center'",
                    h4("Food quality is a vital factor for a restaurant’s success. We identified the most frequently mentioned dishes in the reviews, such as Pizza, Cheese, Dessert, Chicken, Salad, Pasta, Wine, Soup, Seafood and Beef. We analyzed how these dishes influenced the ratings of the four types of restaurants and plotted a line chart. The x-axis of the chart shows the dish name, the y-axis shows the rating, the position of the dot for each dish shows the average rating of the reviews that mention the dish, and the size of the dot shows the rating variance of the reviews that mention the dish. Generally, we assume that a dish with a higher average rating helps a restaurant improve its overall rating, and a dish with a lower rating variance reduces the risk of a restaurant disappointing its customers.
                       For high-priced downtown Italian restaurants, cheese and wine are effective choices to enhance the restaurant ratings, as they are often praised by the customers. However, beef and soup have a large variation in quality among different restaurants, and they may even be the worst dishes in some cases.")
                  ),
                  conditionalPanel(
                    condition = "input.price == 'low' && input.location == 'center'",
                    h4("Food quality is a vital factor for a restaurant’s success. We identified the most frequently mentioned dishes in the reviews, such as Pizza, Cheese, Dessert, Chicken, Salad, Pasta, Wine, Soup, Seafood and Beef. We analyzed how these dishes influenced the ratings of the four types of restaurants and plotted a line chart. The x-axis of the chart shows the dish name, the y-axis shows the rating, the position of the dot for each dish shows the average rating of the reviews that mention the dish, and the size of the dot shows the rating variance of the reviews that mention the dish. Generally, we assume that a dish with a higher average rating helps a restaurant improve its overall rating, and a dish with a lower rating variance reduces the risk of a restaurant disappointing its customers.
                       For cheap Italian restaurants downtown, dessert is one of the most adventurous dishes. Seafood, although not as frequently ordered as dessert, is generally well-received by the customers. Soup, however, is the least favored dish, hard to prepare well, and often gets lower ratings than other dishes.")
                  ),
                  conditionalPanel(
                    condition = "input.price == 'low' && input.location == 'not'",
                    h4("Food quality is a vital factor for a restaurant’s success. We identified the most frequently mentioned dishes in the reviews, such as Pizza, Cheese, Dessert, Chicken, Salad, Pasta, Wine, Soup, Seafood and Beef. We analyzed how these dishes influenced the ratings of the four types of restaurants and plotted a line chart. The x-axis of the chart shows the dish name, the y-axis shows the rating, the position of the dot for each dish shows the average rating of the reviews that mention the dish, and the size of the dot shows the rating variance of the reviews that mention the dish. Generally, we assume that a dish with a higher average rating helps a restaurant improve its overall rating, and a dish with a lower rating variance reduces the risk of a restaurant disappointing its customers.
                       For low-priced Italian restaurants outside the city center, seafood is the most favored dish, and desserts and wine are also great choices for the restaurants. Cheese and salad, however, are less appealing to the customers, and they are challenging for the restaurants to prepare well.")
                  ),
                  conditionalPanel(
                    condition = "input.price == 'high' && input.location == 'not'",
                    h4("Food quality is a vital factor for a restaurant’s success. We identified the most frequently mentioned dishes in the reviews, such as Pizza, Cheese, Dessert, Chicken, Salad, Pasta, Wine, Soup, Seafood and Beef. We analyzed how these dishes influenced the ratings of the four types of restaurants and plotted a line chart. The x-axis of the chart shows the dish name, the y-axis shows the rating, the position of the dot for each dish shows the average rating of the reviews that mention the dish, and the size of the dot shows the rating variance of the reviews that mention the dish. Generally, we assume that a dish with a higher average rating helps a restaurant improve its overall rating, and a dish with a lower rating variance reduces the risk of a restaurant disappointing its customers.
                       For high-priced Italian restaurants outside the city center, pasta and pizza are low-risk choices that can boost the restaurant ratings, as they are usually well-reviewed by the customers. Soup and beef, on the other hand, are high-risk dishes that can lower the ratings, as they are often criticized by the customers.")
                  )
                  
                )
              )
      )
      
      
      
    )
  )
)

# 定义服务器逻辑
#----------------------

server <- function(input, output, session) {
  
  selectedDataForMeanVar <- reactive({
    switch(
      paste(input$price, input$location),
      "high center" = read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/dishes/PA_ch_dishes.csv"),
      "low center" = read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/dishes/PA_cl_dishes.csv"),
      "high not" = read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/dishes/PA_nh_dishes.csv"),
      "low not" = read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/dishes/PA_nl_dishes.csv")
    )
  })
  
  selectedDataForRadar <- reactive({
    switch(
      paste(input$price, input$location),
      "high center" = read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_ch_df.csv"),
      "low center" = read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_cl_df.csv"),
      "high not" = read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_nh_df.csv"),
      "low not" = read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/index/PA_nl_df.csv")
    )
  })
  
  
  #print(head(joined_data()))
  # 生成雷达图输出
  output$radarChart <- renderPlot({
    data <- req(selectedDataForRadar())
    createRadarChart(data)
  })
  
  # 生成均值和标准差图输出
  output$meanVarPlot <- renderPlot({
    data <- req(selectedDataForMeanVar())
    calculateMeanVarPlot(data)
  })
  
  joined_data <- reactive({
    read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/attr_data.csv")
  })
  
  italian_restaurants <- reactive({
    url <- "https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/restaurant_data.csv"
    read.csv(url)
  })
  
  zipcodes_sf <- reactive({
    # st_read("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/PhillyStreets_Zipcodes_Poly.zip") %>%
    #   st_transform(4326)
    
    url <- "https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/PhillyStreets_Zipcodes_Poly.zip"
    local_dir <- tempdir()
    temp_zip <- tempfile(fileext = ".zip")
    download.file(url, temp_zip)
    
    # 解压缩到指定目录
    unzip(temp_zip, exdir = local_dir)
    st_read(paste(local_dir,"/PhillyStreets_Zipcodes_Poly.shp",sep="")) %>% st_transform(4326)
  })
  
  
  
  selected_zipcode_data <- reactiveVal(NULL)
  #selected_restaurant_data <- reactiveVal(data.frame(name = "Test", stars = 5))
  selected_restaurant_data <- reactiveVal()
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -75.163789, lat = 39.952583, zoom = 12) %>%
      addPolygons(
        data = zipcodes_sf(),
        fillColor = "transparent",
        color = "skyblue1",
        weight = 2,
        fillOpacity = 0
      ) %>%
      addCircleMarkers(
        data = italian_restaurants(),
        lng = ~longitude, lat = ~latitude,
        #popup = ~paste("Name: ", name, "<br>Stars: ", stars,"<br>RestaurantsDelivery: ",RestaurantsDelivery),
        popup = ~paste("Name: ", name, "<br>Latitude: ", latitude, "<br>Longitude: ", longitude),
        label = ~as.character(stars),
        labelOptions = labelOptions(noHide = TRUE),
        #id = ~name,
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = FALSE,
          removeOutsideVisibleBounds = TRUE,
          animateAddingMarkers = TRUE
        ),
        #
        color = "#800080", # 紫色
        fillOpacity = 0.5, # 半透明
        radius = 6 # 圆形大小
      )
  })
  
  
  # 在 observeEvent 中处理点击 Zipcode 区域事件
  # 在 server 函数中添加一个观察者来处理地图点击事件
  # 继续在 observeEvent 中添加代码来更新地图上的餐厅位置
  # observeEvent(input$map_shape_click, {
  #   click <- input$map_shape_click
  #   print(click)
  #   if (!is.null(click$properties$ZIPCODE)) {
  #     selected_zipcode <- click$properties$ZIPCODE
  #     
  #     # 筛选出该 Zipcode 下的意大利餐厅
  #     restaurants_in_zipcode <- italian_restaurants[italian_restaurants$zipcode == selected_zipcode, ]
  #     print(restaurants_in_zipcode)
  #     # 在地图上添加紫色半透明点
  #     leafletProxy("map") %>%
  #       clearMarkers() %>% # 清除之前的标记
  #       addCircleMarkers(data = restaurants_in_zipcode, 
  #                        lng = ~longitude, lat = ~latitude,
  #                        id = ~X, popup = ~paste("Name: ", name, "<br>Stars: ", stars),
  #                        
  #                        color = "#800080", # 紫色
  #                        fillOpacity = 0.5, radius = 6)
  #     
  #   }
  #   
  #   
  # })
  # 
  # observeEvent(input$map_marker_click, {
  #   click_marker <- input$map_marker_click
  #   #print(click_marker)
  #   if (!is.null(click_marker$id)) {
  #     selected_restaurant <- italian_restaurants() %>%
  #       filter(name == click_marker$id)
  #     selected_restaurant_data(selected_restaurant)
  #     print(selected_restaurant)
  #   }
  # })
  
  observeEvent(input$map_marker_click, {
    click_info <- input$map_marker_click
    if (!is.null(click_info)) {
      # 根据经纬度在数据集中查找餐馆
      selected_restaurant <- italian_restaurants() %>%
        filter(latitude == click_info$lat, longitude == click_info$lng)
      selected_restaurant_data(selected_restaurant)
    }
  })
  
  output$restaurantTable <- renderDataTable({
    selected_data <- selected_restaurant_data()
    
    # 确保 selected_data 不是 NULL 或空
    if (is.null(selected_data) || nrow(selected_data) == 0) {
      return(NULL)
    }
    
    # 使用 DT::datatable() 直接显示数据表
    datatable(t(selected_data), options = list(
      pageLength = 10,
      autoWidth = TRUE
    ))
  })
  #c('name', 'stars', 'review_count', 'is_open','RestaurantsDelivery','NoiseLevel','RestaurantsGoodForGroups')
  
  output$barPlot <- renderPlot({
    req(input$barPlotVariable)
    data <- req(joined_data())
    generateBarPlot(data, input$barPlotVariable)
  })
  
  
  output$selectedDescription <- renderText({
    descriptions <- list(
      OutdoorSeating = "Restuarant offer outdoor seating has higher means \n in average star rating for both high price and low price",
      WiFi = "Paid WiFi only occur in low price restaurant and badly \n influences the star rating",
      RestaurantsAttire = "Require for dressy can decrease your star",
      Alcohol = "restaurant has full bar lower mean stars than others",
      NoiseLevel = "There is no 'very loud' high price restaurant, \n if you has a low price italian restaurant, avoid being too loud",
      RestaurantsDelivery = "Significant difference in Delivery \n are seen in High price restaurant",
      GoodForKids = "Tests shows no significant difference in \n star rating whether they are good for kids or not"
    )
    
    selected_variable <- input$barPlotVariable
    selected_description <- descriptions[[selected_variable]]
    
    paste("Selected Description: ", selected_description)
  })
}

# 运行应用
shinyApp(ui, server)


#----------


# library(rsconnect)
# 
# rsconnect::setAccountInfo(name='hqtwaw-sirius-zeng',
#                           token='62F241EC921C27B166C93325F7A45AC5',
#                           secret='n63M0YsAXhpON6FSni8yN5qw0Att03dkG+mcPsEF')
# rsconnect::deployApp("code/app.R")





source("global.R", local = TRUE)



ui <- dashboardPage(
                    
                    # Header
                    dashboardHeader(title = "Sales-Dashboard",titleWidth = 200),
                    
                    # Side bar of the Dashboard
                    dashboardSidebar(
                        selectInput(
                            inputId = "sale",
                            label = "Name of Item:",
                            choices = item_list,
                            selected = "AB20",
                      
                            selectize = FALSE),
                        
                        # Side menu of the Dashboard  
                        sidebarMenu(
                            selectInput(
                                inputId = "month",
                                label = "Month",
                                choices = month_list,
                                selected = 99,
                                size = 13,
                                selectize = FALSE),
                            actionLink("remove", icon = icon("sync-alt"),"Remove detail tabs"),
                            menuItem("Source Code", icon = icon("github"), href = "https://github.com/Bakti-Siregar/Flights-Dashboard"),
                            menuItem("About Me", icon = icon("linkedin"), href = "https://www.linkedin.com/in/bakti-siregar-15955480/"),
                            menuItem("Video", icon = icon("youtube"), href = "https://www.youtube.com/watch?v=vZR_2hhvsu4")
                        )
                    ),
                    
                    # The body of the dashboard
                    dashboardBody(
                        tabsetPanel(id = "tabs",
                                    tabPanel(title = "Main Dashboard",
                                             value = "page1",
                                             fluidRow(valueBoxOutput("total_sales"),
                                                      valueBoxOutput("per_day"),
                                                      valueBoxOutput("percent_profit")),
                                             fluidRow(column(width = 6,d3Output("group_totals")),
                                                      column(width = 6,d3Output("top_customers")),
                                                      
                                                      
                                                      )
                                    )
                        )
                    )
)


#defaults,
x <- as.character(1990:2018)
d_vintages <- c("All years", x)
d_regions <- unique(ca_data_wdesc$wm_region)

shinyUI(
  
  navbarPage("Wine Reviews",
             
             theme = shinythemes::shinytheme("simplex"),
             
             tabPanel("CA Competition",

                        mainPanel(
                              plotlyOutput("ratingprice_fig",
                                         width = 725,
                                         height = 525)
                                  )
                      ),

             tabPanel("Wine Enthusiast",

                        mainPanel(
                            plotOutput("qualityprice_fig",
                                       width = 925,
                                       height = 525)
                            )
                      ),

             tabPanel("SF Competition",

                        mainPanel(

                          plotOutput("rating_price_sf",
                                     width = 800,
                                     height = 650))
              ),

             tabPanel("Review Analysis",

                      sidebarLayout(

                        sidebarPanel(
                          width = 3,

                          selectInput("review_region",
                                      "Region:",
                                      choices = d_regions,
                                      selected = d_regions[1]),

                          selectInput("review_comp",
                                      "Compare to:",
                                      choices = list("Gold",
                                                     "Silver"),
                                      selected = "Gold"),

                          selectInput("review_attribute",
                                      "Wine Attribute:",
                                      choices = sort(unique(attribute_data$category)),
                                      selected = sort(unique(attribute_data$category))[1])),

                        mainPanel(

                          column(width = 4,
                                 plotOutput("wordcloud",
                                            width = 450,
                                            height = 375)),

                          column(width = 4,
                                 offset = 2,
                                 plotOutput("wordcloud2",
                                            width = 450,
                                            height = 375))
                          )
                        )
                      ),
             
             tabPanel("Review Summary",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          width = 3,
                          
                          selectInput("reviewsummary_region",
                                      "Region:",
                                      choices = c("Napa", "Sonoma"),
                                      selected = "Sonoma")),
                        
                        mainPanel(
                          
                          plotlyOutput("barchart",
                                       width = 900,
                                       height = 600))
                        )),    
             
             navbarMenu("More",
                        
                        tabPanel("Data", "Raw data"),
                        tabPanel("Summary", "Summary of data")
             )
             
             
  )
)
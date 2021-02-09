#install.packages(c("shiny", "shinydashboard", "shinydashboardPlus", "shinyjs", "shinybusy", "shinyBS", "tidyverse", "DT"))

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)    #For show/ hide/ toggle
library(shinybusy)  #For loading spinner (add_busy_bar, add_busy_spinner)
library(shinyBS)
library(shinyalert)
library(tidyverse)
library(DT)
#library(rdrop2)

#Preload the data
source("functions.R")

product_library<-
  read_csv("./Data/product_library.csv", na="") %>%
  mutate(
    ewg_risk = factor(ewg_risk, levels = c("Unknown", "C", "B", "A", "Perfect"))
  )
ingredient_library<- 
  read_csv("./Data/ingredient_library.csv")

#Create input options
product_list<- 
  list(Product = product_library$product_name)

famous_brand<- 
  product_library %>%
  filter(n_rating>0) %>%
  .$brand %>% 
  table() %>% 
  .[.>21] %>% 
  sort(decreasing = T) %>% names()

brand_list<- list(
  #"All Brands" = list("All Brands"),
  "Top brands" = product_library[,"brand"] %>% unlist() %>% unique() %>% .[. %in% famous_brand] %>% sort(),
  "Other brands" = product_library[,"brand"] %>% unlist() %>% unique() %>% .[!(. %in% famous_brand)] %>% sort()
)

#Customized javascript code
#Cookie management
# jsCode <- '
#   shinyjs.getcookie = function(params) {
#     var cookie = Cookies.get("id");
#     if (typeof cookie !== "undefined") {
#       Shiny.onInputChange("jscookie", cookie);
#     } else {
#       var cookie = "";
#       Shiny.onInputChange("jscookie", cookie);
#     }
#   }
#   shinyjs.setcookie = function(params) {
#     Cookies.set("id", escape(params), { expires: 0.5 });  
#     Shiny.onInputChange("jscookie", params);
#   }
#   shinyjs.rmcookie = function(params) {
#     Cookies.remove("id");
#     Shiny.onInputChange("jscookie", "");
#   }
# '

# UI
ui <- dashboardPage(
  dashboardHeader(title = "SkinCarer"
  ),
  dashboardSidebar(
    collapsed = T,
    sidebarMenu(
      id = "menuItems",
      menuItem(text = "Skin Product Scanner", tabName = "main_page"),
      menuItem(text = "My Skincare Products", tabName = "my_skincare_products"),
      menuItem(text = "Custom Search", tabName = "custom_search"),
      menuItem(text = "How To Start", tabName = "how_to_start"),
      menuItem(text = "Disclaimer", tabName = "disclaimer"),
      menuItem(text = "Contact Us", tabName = "contact_us")
    )
  ),
  dashboardBody(
    tags$head(
      useShinyjs(),
      #tags$script(src = "js/js.cookie.min.js"), #Import cookie management package
      #extendShinyjs(text = jsCode),  #For cookie management
      useShinyalert(),
      add_busy_spinner(spin = "fading-circle", height = "30px", width = "30px"),
      tags$meta(name = "og:image", content = "Img/logo.jpg"),
      tags$style(        #CSS
        HTML('
             .skin-blue .main-header .logo {background-color: #39cccc;}
             .skin-blue .main-header .logo:hover {background-color: #39cccc;}
             .skin-blue .main-header .navbar {background-color: #39cccc;}
             .skin-blue .main-header .navbar:hover {background-color: #39cccc;}
             .skin-blue .main-header .navbar .sidebar-toggle:hover {background-color: #7adddd;}
             span.irs-bar {background: #39cccc; border-top-color: #39cccc; border-bottom-color:#39cccc}
             span.irs-from {background: #39cccc}
             span.irs-to {background: #39cccc}
             .main-header .logo {text-align: left}
             div.modal-footer {display:none}
             .sharethis-inline-share-buttons .st-btn {display: inline-block! important}
             ')
      ),
      #Google Analytics
      tags$script(src = "https://www.googletagmanager.com/gtag/js?id=UA-175905320-1", async = "async"),
      tags$script(HTML(
        "
          window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag('js', new Date());
          
            gtag('config', 'UA-175905320-1');
        "
      )),
      #Sharethis - Share button
      #tags$script(src = "https://platform-api.sharethis.com/js/sharethis.js#property=5f3a63c52e993000125e5af2&product=inline-share-buttons", async = "async"),
      
      #hotjar
      tags$script(HTML("
                           (function(h,o,t,j,a,r){
        h.hj=h.hj||function(){(h.hj.q=h.hj.q||[]).push(arguments)};
        h._hjSettings={hjid:1953795,hjsv:6};
        a=o.getElementsByTagName('head')[0];
        r=o.createElement('script');r.async=1;
        r.src=t+h._hjSettings.hjid+j+h._hjSettings.hjsv;
        a.appendChild(r);
    })(window,document,'https://static.hotjar.com/c/hotjar-','.js?sv=');
                       "))
    ),
    
    #####################UI Start######################
    tabItems(
      tabItem(tabName = "main_page",
              #div(class="sharethis-inline-share-buttons", style = "position:absolute; right:50px; top: 5px"), #Add the share buttons
              fluidRow(
                gradientBox(
                  title = strong("About this tool"),
                  width = 12,
                  gradientColor = "teal",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  closable = TRUE,
                  footer = 
                    tagList(
                      # p(
                      #   "We have pre-selected ",
                      #   span("Perfect ", style = "color:#39cccc ; font-weight: bold"),
                      #   "products for you below! ", 
                      #   span("(Perfect: All ingredients are safe based on EWG safety rating)", style = "color:#39cccc ; font-weight: bold"),
                      #   style = "font-size:18px"
                      # ),
                      p(
                        "You can explore more skin products that match your skin type and needs using below filters!",  
                        style = "font-size:18px"
                      ),
                      p(
                        'Please visit ',
                        span("How To Start ",
                             style = "color:#39cccc ; font-weight: bold", 
                             onclick = 
                               #Trigger refresh by assigning blank value
                               "
                               Shiny.setInputValue('navigate_to_tab', '');
                               Shiny.setInputValue('navigate_to_tab', 'how_to_start');
                               "
                             ),
                        'for more detail.',
                        style = "font-size:18px"
                      ),
                      p("Go to ",
                        span("Custom Search ",
                             style = "color:#39cccc ; font-weight: bold", 
                             onclick = 
                               #Trigger refresh by assigning blank value
                               "
                               Shiny.setInputValue('navigate_to_tab', '');
                               Shiny.setInputValue('navigate_to_tab', 'custom_search');
                               "
                             ),
                        "if you cannot find your product in the scanner. You can input the ingredients to find out more about the product and its ingredients.",
                        style = "font-size:18px"
                        ),
                      p(
                        "Data source: Environmental Working Group (EWG), SkinCarisma.com",
                        style = "font-size:18px; text-decoration: underline"
                      )
                    )
                  
                )
              ),
              fluidRow(
                id = "scanner",
                box(width = 12,
                    title = strong("Skin Product Scanner"),
                    fluidRow(
                      column(width = 4,
                             selectInput("Brand_Control",
                                         label = "Brand",
                                         width = "100%",
                                         choices = brand_list,
                                         multiple = TRUE,
                                         selected =  NULL,
                                         selectize= TRUE
                             )
                      ),
                      column(width = 4,
                             selectizeInput("Product_Control",
                                            label = "Product",
                                            choices = product_list,
                                            multiple = TRUE,
                                            selected = NULL,
                                            options = list(create = TRUE)
                             )
                      ),
                      column(width = 4
                             
                      )
                    ),
                    fluidRow(
                      column(width = 4,
                             selectInput("Category_Control",
                                         label = "Category",
                                         width = "100%",
                                         choices = c( "Cleansers",
                                                      "Toners",                           
                                                      "Exfoliating Scrubs & Peeling Gel",
                                                      "Serums, Essence & Ampoules",     
                                                      "Moisturizers",
                                                      "Sheet Masks",
                                                      "Wash-Off Masks",
                                                      "Leave-On & Sleeping Masks/Packs",
                                                      "Facial Sunscreen",
                                                      "Acne & Blemish Treatments",
                                                      "Facial Oils",
                                                      "Eye Cream & Treatments"),
                                         multiple = TRUE,
                                         selected  = NULL,
                                         selectize = TRUE
                             )
                      ),
                      column(width = 4,
                             selectInput("EWG_Risk_Control", 
                                         label = "Ingredient Safety Level",
                                         width = "100%",
                                         choices = c(
                                           "Perfect" = "Perfect",
                                           "A" = "A" ,
                                           "B" = "B",
                                           "C" = "C",
                                           "Unknown" = "Unknown"
                                         ),
                                         multiple=TRUE,
                                         selected= NULL,
                                         selectize= TRUE
                             )
                      ),
                      column(width = 4,
                             sliderInput("EWG_Average_Rating_Control", 
                                         label = "Safety Rating （1 Good - 10 Bad)",
                                         width = "100%",
                                         min = 1, 
                                         max = 10,
                                         step = 0.5,
                                         value = c(1, 10))
                             
                      )
                    ),
                    div(
                      id="advanced_filter",
                      style = "display: none;",
                      fluidRow(
                        column(width = 4,
                               selectInput("Skin_Type_Control", 
                                           label = "Skin Type",
                                           width = "100%",
                                           choices = c(
                                             "Dry Skin" = "dry_skin" ,
                                             "Oily Acne Prone Skin" = "oily_acne_prone_skin",
                                             "Sensitive Skin" = "sensitive_skin"
                                           ),
                                           multiple=TRUE,
                                           selected= NULL,
                                           selectize= TRUE
                               )
                        ),
                        column(width = 4,
                               selectInput("Harm_Free_Control", 
                                           label = "Harm Free",
                                           width = "100%",
                                           choices = c(
                                             "Paraben Free" = "Paraben.Free",
                                             "Sulfate Free" = "Sulfate.Free",
                                             "Alcohol Free" = "Alcohol.Free",
                                             "Silicone Free" = "Silicone.Free",
                                             "Allergen Free" = "EU.Allergen.Free",
                                             "Cruelty Free" = "Cruelty.Free",
                                             "Fungal Acne Safe" = "Fungal.Acne.Malassezia.Safe"
                                           ),
                                           multiple=TRUE,
                                           selected= NULL,
                                           selectize= TRUE
                               )
                        ),
                        column(width = 4,
                               selectInput("Function_Control", 
                                           label = "Extra Function",
                                           width = "100%",
                                           choices = c(
                                             "Brightening" = "Brightening" ,
                                             "Wound Healing" = "Promotes.Wound.Healing",
                                             "Acne Fighting" = "Acne.Fighting",
                                             "Anti Aging" = "Anti.Aging",
                                             "UV Protection" = "UV.Protection"
                                           ),
                                           multiple=TRUE,
                                           selected= NULL,
                                           selectize= TRUE
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 4,
                               sliderInput("n_Unknown_Ingredient_Control", 
                                           label = "Number of Unknown Ingredients",
                                           width = "100%",
                                           min = 0, 
                                           max = 60,
                                           step = 1,
                                           value = c(0, 60))
                        ),
                        column(width = 4,
                               sliderInput("n_Ingredient_Control", 
                                           label = "Number of Ingredients",
                                           width = "100%",
                                           min = 1, 
                                           max = 170,
                                           step = 10,
                                           value = c(1, 170))
                        ),
                        column(width = 4,
                               sliderInput("Rating_Control", 
                                           label = "Customer Rating",
                                           width = "100%",
                                           min = 0, 
                                           max = 5, 
                                           step = 0.5,
                                           value = c(0, 5))
                        )
                      )
                    ), #End - div(id="advanced filter")
                    fluidRow(
                      column(
                        width=12,
                        actionButton(
                          inputId = "Button_Advanced_Filter", 
                          label = "Show/ Hide Advanced Filters", 
                          style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none"
                        ),
                        actionButton(
                          "Button_Filter",
                          "Apply Filter", 
                          icon = icon("search"),
                          style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none"
                        )
                      )
                      
                    )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput("Sorting_Control",
                              label = span("Sort By", style = "font-size:16px"),
                              width = "100%",
                              choices = c("Popularity" = "n_rating",
                                          "Ingredient Safety Level (Safest to Unsafest)" = "ewg_risk",
                                          "Safety Rating (Lowest to Highest)" = "ewg_average_rating",
                                          "Customer Rating (Highest to Lowest)" = "rating",
                                          "Number of Unknown Ingredients (Lowest to Highest)" = "ewg_n_unknown",
                                          "Number of Ingredients (Lowest to Highest)" = "n_ingredient"),
                              selected  = "ewg_risk"
                  )
                ),
                column(
                  width = 4,
                  selectInput("n_Item_Control",
                              label = span("Number of Products Per Page", style = "font-size:16px"),
                              width = "100%",
                              choices = c(30,60,90),
                              selected  = 30
                  )
                )
              ),
              div(style = "float: right; margin-right:15px",
                  span(strong(textOutput("page_number_top", inline = T)), style = "font-size:16px;")),
              div(style = "float: right; margin-right:15px",
                  span(strong(textOutput("nrow", inline = T)), style = "font-size:16px;")),
              p(
                actionButton("prevBtn_top", 
                             "",
                             icon = icon("arrow-left"),
                             style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none"),
                actionButton("nextBtn_top", 
                             "",
                             icon = icon("arrow-right"),
                             style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none")
              ),
              uiOutput("UI_Product_Cards"),
              div(style = "float: right; margin-right:15px",
                  span(strong(textOutput("page_number_bottom", inline = T)), style = "font-size:16px;")),
              p(
                actionButton("prevBtn_bottom", 
                             "",
                             icon = icon("arrow-left"),
                             style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none"),
                actionButton("nextBtn_bottom", 
                             "",
                             icon = icon("arrow-right"),
                             style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none")
              )
              
              
      ),
      tabItem(tabName = "my_skincare_products",
              fluidRow(
                box(
                  width = 12,
                  h3("My Skincare Products"),
                  actionButton("Button_My_Products_Delete",
                               "Remove Selected Products",
                               icon = icon("trash-alt"),
                               style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none"
                  ),
                  #p(strong(textOutput("n_my_skincare_products",  inline = T)), style = "font-size:16px"),
                  div(
                    class = "table-responsive",
                    style = "border-style:none;",
                    DTOutput("my_skincare_products_table")
                  )
                )
                
              )
              
      ),
      tabItem(tabName = "custom_search",
              fluidRow(
                box(
                  width = 12,
                  h3("Custom Search"),
                  p("Cannot find your product in the scanner? You can input the ingredients below and get the analytic result."),
                  selectizeInput("Custom_Search_Category",
                                 label = "Category",
                                 choices = list( "",
                                                 "Cleansers",
                                                 "Toners",
                                                 "Exfoliating Scrubs & Peeling Gel",
                                                 "Serums, Essence & Ampoules",
                                                 "Moisturizers",
                                                 "Sheet Masks",
                                                 "Wash-Off Masks",
                                                 "Leave-On & Sleeping Masks/Packs",
                                                 "Facial Sunscreen",
                                                 "Acne & Blemish Treatments",
                                                 "Facial Oils",
                                                 "Eye Cream & Treatments"), 
                                 multiple = FALSE,
                                 selected  = "",
                  ),
                  selectizeInput("Custom_Search_Brand",
                                 label = "Brand",
                                 choices = c(list(""),brand_list),
                                 multiple = FALSE,
                                 selected = "",
                                 options = list(create = TRUE)
                                 
                  ),
                  textInput("Custom_Search_Product_Name", 
                            label = "Product Name"),
                  selectizeInput("Custom_Search_Ingredients",
                                 label = "Ingredients",
                                 choices = list(Ingredients = ingredient_library$common_name %>% sort()),
                                 multiple = TRUE,
                                 selected = NULL,
                                 options = list(create = TRUE)
                  ),
                  fluidRow(
                    column(width = 6,
                           fileInput("Custom_Search_Product_Picture",
                                     label = "Product Picture",
                                     width = "100%",
                                     accept = c('image/png', 'image/jpeg')
                           )
                    ),
                    column(width = 6,
                           fileInput("Custom_Search_Ingredient_Picture",
                                     label = "Ingredient List Picture",
                                     width = "100%",
                                     accept = c('image/png', 'image/jpeg')
                           )
                    )
                    
                  ),
                  actionButton("Button_Custom_Search_Analyze",
                               'Analyze',
                               icon = icon("microscope"),
                               style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none"
                               ),
                  p(),
                  uiOutput("UI_Custom_Search_Product_Card")
                  
                )
                
              )
      ),
      tabItem(tabName = "how_to_start",
              fluidRow(
                tags$img(src = "Img/how_to_start_1.jpg",
                         width = "630px",
                         height = "8400px",
                         style = "max-width:100%; height:auto"
                ),
                tags$img(src = "Img/how_to_start_2.jpg",
                         width = "630px",
                         height = "840px",
                         style = "max-width:100%; height:auto"
                )
              )
      ),
      tabItem(tabName = "contact_us",
              fluidRow(
                box(title = "Contact Us",
                    status = "primary",
                    boxProfile(title = "SkinCarer",
                               src = "Img/profile_user.png",
                               boxProfileItemList(
                                 bordered = T,
                                 boxProfileItem(title = "Email",
                                                description = a("skincarerhk@gmail.com", href = "mailto:slincarerhk@gmail.com", class="pull-right")
                                 ),
                                 boxProfileItem(title = "Instagram",
                                                description = a("@skincarerhk", href = "https://www.instagram.com/skincarerhk/", class="pull-right")
                                 )
                               ))
                )
              )
              
      ),
      tabItem(tabName = "disclaimer",
              fluidRow(
                gradientBox(
                  title = "Disclaimer",
                  gradientColor = "teal",
                  collapsible = TRUE,
                  closable = TRUE,
                  width = 12,
                  footer = 
                    div(
                      p(
                        'Your Use of this Website and Any Materials, Products, Samples, information Derived from this Website is at Your Own Risk.'
                      ),
                      p(
                        'We provide all of our Website content for general entertainment and information purposes only. The content constitutes the opinions of its author only. Unless otherwise specified, authors are not medical professionals or certified experts in their field. You should never substitute information from this Website for information obtained from a licensed professional.'
                      ),
                      p(
                        'Neither SkinCarer, any of its agents, nor any other party involved in creating, producing, or delivering this Website is liable for any damages whatsoever arising out of your access to, or use of, this Website, any material from this Website, or any products offered on this Website. Without limiting the foregoing, this Website, the material provided on this Website, and the products offered on this Website, are provided "AS IS" WITHOUT GUARANTEE OR WARRANTY OF ANY KIND EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NONINFRINGEMENT.'
                      ),
                      p(
                        '
                        Any products and/or services described on the website are offered in jurisdictions where they may be legally offered for sale. The information on the website is not an offer or solicitation by anyone in any jurisdiction in which an offer or solicitation cannot legally be made, or to any person to whom it is unlawful to make a solicitation.
                        '
                      ),
                      p(
                        '
                        SkinCarer will not under any circumstances be liable to you or any other person or entity for any damages whatsoever arising from, connected with, or relating to the website or services. You expressly acknowledge that SkinCarer has entered into this agreement with you and makes the website and services available to you, in reliance upon the limitations and exclusions of liability and the disclaimers set forth herein, and that the same form an essential basis of the bargain between you and SkinCarer. You expressly agree that the limitations and exclusions of liability and the disclaimers set forth herein will survive, and continue to apply in the case of a fundamental breach or breaches, the failure of essential purpose of contract, the failure of any exclusive remedy, or termination of this agreement.
                        '
                      )
                    )
                ),
                gradientBox(
                  title = "Information and Product Descriptions",
                  gradientColor = "teal",
                  collapsible = TRUE,
                  closable = TRUE,
                  width = 12,
                  footer = 
                    div(
                      p(
                        'The Website may make available certain data, news, research, statistics, stories, photographs and opinions or other information (collectively "Information") that it has prepared itself or that has been independently obtained by other services and individuals, including news wires, statistics providers, journalists, authors and other providers (collectively the "Information Providers"). We do not guarantee or certify the accuracy, completeness, timeliness or correct sequencing of the Information made available through the Website or Services, the Information Providers or any other third party transmitting the Information (the "Information Transmitters"). You agree that neither SkinCarer, the Information Providers nor the Information Transmitters shall be liable in any way for the accuracy, completeness, timeliness or correct sequencing of the Information, or for any decision made or action taken by you relying upon the Information. You understand that none of the Information available through the Website constitutes a recommendation or solicitation to take or not take any particular action.'
                      ),
                      p(
                        'Further, SkinCarer does not warrant that product descriptions on the Website are accurate, complete, reliable, current or error-free. If a product itself is not as described, your sole remedy is to return it promptly to the manufacturer.'
                      )
                    )
                ),
                gradientBox(
                  title = "Links from the Website",
                  gradientColor = "teal",
                  collapsible = TRUE,
                  closable = TRUE,
                  width = 12,
                  footer = 
                    div(
                      p(
                        '
                        The Website may display advertisements and other information adjacent to or included with your content. We reserve the right to display advertisements with user submissions and to use user submissions for advertising and promotional purposes without any compensation to you. Advertisements and sponsorships are clearly indicated.
                        '
                      ),
                      p(
                        '
                        Any links to other sites from the Website are provided for your convenience only. This includes links to third parties contained in advertisements, including banner advertisements, affiliate and sponsored links. We have no control over the contents of those sites or resources, and accept no responsibility for them or for any loss or damage that may arise from your use of them, including purchases made in such sites. If you decide to access any of the third party websites linked to this Website, you do so entirely at your own risk and subject to the terms and conditions of use for such websites. Please review carefully the third-party’s policies and practices and make sure you understand them before you engage in any transaction. Complaints, claims, concerns, or questions regarding third-party products should be directed to the third-party.
                        '
                      ),
                      p(
                        '
                        The purchase, payment, warranty, guarantee, delivery, maintenance, and all other matters concerning the merchandise, services or information, opinion or advice ordered or received from such businesses are solely between you and such businesses. We do not endorse, warrant, or guarantee such products, information, or services. We will not be a party to or in any way responsible for monitoring any transaction between you and third-party providers of such products, services, or information, or for ensuring the confidentiality of your transactions.
                        '
                      ),
                      p(
                        '
                        We are an informational website that receives a commission, fee, and/or other compensation on some purchases made through or linked from the Website. Specifically, as a participant in the Amazon.com Services LLC Associates Program, we earn fees by linking to amazon.com and affiliated sites on qualifying purchases.
                        '
                      )
                    )
                )
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  #Initialization
  initial_result <- reactiveVal(TRUE)
  data_filtered<- reactiveVal()
  
  #Get query parameter
  observe({
    url_query <- parseQueryString(session$clientData$url_search)
    if (!is.null(url_query[["product"]])) {
      updateSelectizeInput(
        session,
        "Product_Control",
        selected = url_query[["product"]]
      )
    }
  })
  
  #Display/ Hide Advanced Filter
  observeEvent(input$Button_Advanced_Filter, {
    shinyjs::toggle("advanced_filter")
  })
  
  #####################################################################################
  #Apply filter/ sorting
  observeEvent(
    c(input$Button_Filter,input$Sorting_Control, input$Product_Control), 
    {
        
        data<- product_library %>%
          filter(ewg_average_rating >= input$EWG_Average_Rating_Control[1], ewg_average_rating <= input$EWG_Average_Rating_Control[2]) %>%
          filter(n_ingredient>=input$n_Ingredient_Control[1], n_ingredient<=input$n_Ingredient_Control[2]) %>%
          filter(ewg_n_unknown >= input$n_Unknown_Ingredient_Control[1], ewg_n_unknown <= input$n_Unknown_Ingredient_Control[2]) %>%
          filter(rating>=input$Rating_Control[1], rating<=input$Rating_Control[2]) 
        
        if(!is.null(input$Product_Control) ) {
          data <- data %>%
            filter(
              product_name %>% tolower() %>% str_remove_all("[^\\w]") %>% str_detect(paste(input$Product_Control%>% tolower() %>% str_remove_all("[^\\w]"), collapse = "|"))
              |
              brand %>% tolower() %>% str_detect(paste(input$Product_Control%>% tolower(), collapse = "|"))
              )
        }
        if(!is.null(input$Category_Control)) {
          data<- data %>%
            filter(category %in% input$Category_Control)
        }
        
        if( !is.null(input$EWG_Risk_Control)) {
          data<- data %>%
            filter(ewg_risk %in% input$EWG_Risk_Control)
        }
        
        if(!is.null(input$Harm_Free_Control)) {
          data<- data %>% 
            filter_at(.vars=vars(input$Harm_Free_Control), ~.==TRUE)
        }
        
        if(!is.null(input$Skin_Type_Control)) {
          data<- data %>% 
            filter_at(.vars=vars(input$Skin_Type_Control), ~.>0)
        }
        
        if(!is.null(input$Function_Control)) {
          data<- data %>% 
            filter_at(.vars=vars(input$Function_Control), ~.>0)
        }
        
        if(!is.null(input$Brand_Control)) {
          data<- data %>% 
            filter(brand %in% input$Brand_Control)
        }
        
        #Only show products with images when people just arrive the website
        if (initial_result() == T) {
          data<- data %>%
            filter(image != "Img/image_blank.png") 
        }
        
        # #Sorting
        if(input$Sorting_Control == "n_ingredient" | 
           input$Sorting_Control == "ewg_n_unknown" |
           input$Sorting_Control == "ewg_average_rating") {
          data<- data %>%
            arrange(!!as.name(input$Sorting_Control))
        } else {
          data<- data %>%
            arrange(desc(!!as.name(input$Sorting_Control)))
        }
      
      #Deduplicate same products in different categories
      data<- data %>% distinct(ref,.keep_all = TRUE)
      
      #Show products without images from now on
      initial_result(FALSE) 
      
      #Update data_filtered
      data_filtered(data)
      
      #page_handling
      page$current<- 1
      page$max<- ceiling(nrow(data)/ page$n_item)
    
    })
  
  output$nrow<- renderText({paste("Number of result:", nrow(data_filtered()))})
  
  #Page handling
  page<- reactiveValues(
    current = 1,
    max = 1,
    n_item = 60
  )
  
  #Number of product per page
  observe({
    page$n_item <- input$n_Item_Control %>% as.integer()
  })
  
  #Enable/ Disable buttons by page values
  observe({
    toggleState(id = "prevBtn_top", condition = page$current > 1)
    toggleState(id = "nextBtn_top", condition = page$current < page$max)
    
    toggleState(id = "prevBtn_bottom", condition = page$current > 1)
    toggleState(id = "nextBtn_bottom", condition = page$current < page$max)
  })
  
  #Trigger previous/ next page
  observeEvent(
    c(input$prevBtn_top, input$prevBtn_bottom ), { 
    page$current <- page$current -1
    shinyjs::runjs("document.getElementById('scanner').scrollIntoView();")
    })
  observeEvent(
    c(input$nextBtn_top, input$nextBtn_bottom), { 
    page$current <- page$current +1 
    shinyjs::runjs("document.getElementById('scanner').scrollIntoView();")
    })
  
  #Display current_page/ max_page
  output$page_number_top<-
    renderText({paste("Page:", page$current, "/", page$max)   })
  
  output$page_number_bottom<-
    renderText({paste("Page:", page$current, "/", page$max)   })
  
  #Render product cards
  product_cards<- reactiveValues(cards=list())
  observeEvent(
    c(data_filtered(), page$current, page$n_item),
    {
      product_cards$cards <- NULL  #Clear the previous product_cards result first
      
      if(nrow(data_filtered())>0) {
        for (i in ((page$current-1) * page$n_item + 1): min(nrow(data_filtered()),((page$current-1) * page$n_item + page$n_item) )   ) {
          product_cards$cards[[i]] <- create_product_card(data_filtered()[i,])
        }  #End - For loop
      } 
      
    }
  )
  
  output$UI_Product_Cards<- renderUI({
    tagList(
      lapply(1:ceiling(length(product_cards[["cards"]])/3),
             function(i){
               fluidRow(
                 product_cards[["cards"]][(i-1)*3+1],
                 product_cards[["cards"]][(i-1)*3+2],
                 product_cards[["cards"]][(i-1)*3+3]
               )
             })
    )
  })
  
  #####################################################################################
  #Add product to My Skincare Product
  my_skincare_products_ref <- reactiveVal(c())
  
  data_my_skincare_products<- reactiveVal(
    data.frame(
      Category = NA,
      Brand = NA,
      Product = NA,
      Image = NA,
      "Customer Rating" = NA,
      "Number of Unknown Ingredients" = NA,
      "Ingredient Safety Level" = NA,
      "Safety Rating" = NA,
      "Dry Skin" = NA,
      "Oily Acne Prone Skin" = NA,
      "Sensitive Skin" = NA,
      check.names = FALSE
    )[-1,]
  )
  
  observeEvent(
    input$add_to_my_skincare_products, {
      
      #Display a success message
      shinyalert(
        title = "Added to My Skincare Product",
        text = "",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#39CCCC",
        timer = 1000,
        imageUrl = "",
        animation = TRUE
      )
      
      #Update my skincare product list
      my_skincare_products_ref(c(
        my_skincare_products_ref(),
        input$add_to_my_skincare_products
      ))
      
      #Update the dataframe for my skincare products
      data_my_skincare_products(
        data_filtered() %>% 
          filter(ref %in% my_skincare_products_ref()) %>%
          mutate(
            image = paste0('<img src=', image,' height = "150px" width = "150px"></img>'),
            rating = ifelse(rating == 0, "N/A", rating),
            ewg_n_unknown = ewg_n_unknown %>% as.integer(),
            ewg_average_rating = round(ewg_average_rating %>% as.numeric(), digits = 1)
          ) %>%
          select(
            Category = category,
            Brand = brand,
            Product = product_name,
            Image = "image",
            "Customer Rating" = rating,
            "Number of Unknown Ingredients" = ewg_n_unknown,
            "Ingredient Safety Level" = ewg_risk,
            "Safety Rating" = ewg_average_rating,
            "Dry Skin" = dry_skin,
            "Oily Acne Prone Skin" = oily_acne_prone_skin,
            "Sensitive Skin" = sensitive_skin
          ) %>%
          arrange(
            Category
          )
      )
    }
  )
  
  output$my_skincare_products_table<- renderDT({
    data_my_skincare_products() %>%
      datatable(
        style = 'bootstrap',
        class = "table",
        escape = FALSE,
        rownames = FALSE
      ) %>%
      formatStyle(columns = "Number of Unknown Ingredients",
                  color = styleInterval(c(0, 5, 10), c('green', 'yellow', 'orange', 'red'))
      )%>%
      formatStyle(columns = "Ingredient Safety Level",
                  color = styleEqual(c("Perfect", "A", "B", "C", "Unknown"), c('green', 'lightgreen', 'orange', 'red', 'gray'))
      )%>%
      formatStyle(columns = "Safety Rating",
                  color = styleInterval(c(2, 6), c('green', 'orange', 'red'))
      )%>%
      formatStyle(columns = "Dry Skin",
                  color = styleEqual(c("Good", "Neutral", "Bad"), c('green', 'orange', 'red'))
      )%>%
      formatStyle(columns = "Oily Acne Prone Skin",
                  color = styleEqual(c("Good", "Neutral", "Bad"), c('green', 'orange', 'red'))
      )%>%
      formatStyle(columns = "Sensitive Skin",
                  color = styleEqual(c("Good", "Neutral", "Bad"), c('green', 'orange', 'red'))
      )
    
  })
  
  observeEvent(
    input$Button_My_Products_Delete, {
      if (nrow(data_my_skincare_products())!=0) {
        data_my_skincare_products(data_my_skincare_products()[-input$my_skincare_products_table_rows_selected,]  )
      }
    }
  )

  output$n_my_skincare_products<- renderText({
    paste(nrow(data_my_skincare_products()), " Products are added in My Skincare Products")
  })
  
  #####################################################################################
  #Custom Search
  cs_result_UI<- reactiveVal()
  
  observeEvent(
    input$Button_Custom_Search_Analyze, {
      
      if(input$Custom_Search_Category ==""|
         is.null(input$Custom_Search_Product_Name)| 
         input$Custom_Search_Brand=="" | 
         is.null(input$Custom_Search_Ingredients)) {
        #Check whether mandatory information is filled
        #If not
        shinyalert(
          title = "Information Missing",
          text = "<span>Please make sure that <strong>category</strong>, <strong>ingredients</strong>, <strong>product name</strong> and <strong>brand</strong> are filled</span>",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = TRUE,
          type = "error",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#39CCCC",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        
        
      } else {
        #Mandatory information filled
        #Render product card for custom search
        
        timestamp<- format(Sys.time(), "%Y%m%d_%H%M%OS")
        
        # drop_auth(rdstoken = "dropbox_token.RDS")
        # drop_dir_path<- "/r/Projects/SkinCarer/"
        
        dir.create(file.path("Custom_Search_Submission/Pictures/"), showWarnings = FALSE)
        
        product_pic_filepath<- "Img/image_blank.png"
        ingredient_pic_filepath<- "NA"
        
        if (!is.null(input$Custom_Search_Product_Picture)) {
          product_pic<- input$Custom_Search_Product_Picture
          product_pic_filepath<- paste0("www/Img/Product_Img/",timestamp,"_" ,product_pic$name)
          file.copy(product_pic$datapath, file.path(product_pic_filepath))
          #drop_upload(product_pic_filepath, path = paste0(drop_dir_path,"www/Img/Product_Img/"))
        }
        
        if (!is.null(input$Custom_Search_Ingredient_Picture)) {
          ingredient_pic<- input$Custom_Search_Ingredient_Picture
          ingredient_pic_filepath<- paste0("Custom_Search_Submission/Pictures/",timestamp,"_" ,ingredient_pic$name)
          file.copy(ingredient_pic$datapath, file.path(ingredient_pic_filepath))
          #drop_upload(ingredient_pic_filepath, path = paste0(drop_dir_path,"Custom_Search_Submission/Pictures/"))
        }
        
        submission<- 
          data.frame(
            timestamp = timestamp,
            category = input$Custom_Search_Category,
            product_name = input$Custom_Search_Product_Name,
            brand = input$Custom_Search_Brand,
            ingredients = paste0(input$Custom_Search_Ingredients, collapse = ";"),
            image = product_pic_filepath %>% str_remove_all("www/"),
            ingredient_image = ingredient_pic_filepath
          )
        
        #Update the custom search result UI
        cs_result_UI(
          submission %>%
            create_product_library(ingredient_library = ingredient_library) %>%
            create_product_card()
        )
        
        #Save the submission
        submission_old<- read_csv("Custom_Search_Submission/submission.csv")
        #submission_old<- drop_read_csv(file = paste0(drop_dir_path, "Custom_Search_Submission/submission.csv"))
        
        submission<- bind_rows(submission_old, submission)
        write_csv(submission, "Custom_Search_Submission/submission.csv")
        #drop_upload("Custom_Search_Submission/submission.csv", path = paste0(drop_dir_path,"Custom_Search_Submission"))
        
        
        #Reset the inputs
        reset("Custom_Search_Category")
        reset("Custom_Search_Product_Name")
        reset("Custom_Search_Brand")
        reset("Custom_Search_Ingredients")
        reset("Custom_Search_Product_Name")
        reset("Custom_Search_Product_Picture")
        reset("Custom_Search_Ingredient_Picture")
        
        #Print record
        print(paste(Sys.time(), "New product submission is received"))
      }
    }
  )
  
  output$UI_Custom_Search_Product_Card<- renderUI({
    cs_result_UI()
  })
  
  #####################################################################################
  #Modal window - Ingredient Table
  output$ingredient_table_modal<- renderDT({
    create_ingredient_table_full(
      ingredients_string = input$modal_ingredient,
      ingredient_library
    ) %>%
      create_ingredient_table_display(
        ingredient_table_full = .
      ) %>%
      datatable(rownames=F, 
                style = 'bootstrap',
                class = "table table-striped",
                escape = F,
                options = list(pageLength = 30
                               #columnDefs = list(list(targets = 0, visible = FALSE))  #Hide the EWG (column 0)
                )
      ) %>%
      formatStyle(columns = "Safety Rating",
                  color = styleInterval(
                    cuts = c(2, 6),
                    values = c('green', rgb(156,101,0,maxColorValue = 255), 'red'))
      ) %>%
      formatStyle(columns = "Acne Triggering",
                  color = styleInterval(
                    cuts = c(3),
                    values = c('orange', 'red'))
      )
  })
  
  observeEvent(
    input$modal_ingredient, {
      showModal(modalDialog(
        title = div(strong("Ingredient Table"), 
                    actionButton("modal_close", 
                                 icon = icon("times"),
                                 label = "", 
                                 style="position:absolute; right:15px; top:10px; background-color:#39cccc; color:white; font-weight:bold; border-style: none;", 
                                 "data-dismiss"="modal") #Add this attribute to close modal
        ),
        size = "l",
        easyClose = T,
        div(
          class = "table-responsive",
          DTOutput("ingredient_table_modal"),
          modalButton("Close")
        )
        
        
      ))
    })
  
  #####################################################################################
  #Modal window - Enlarged image
  observeEvent(
    input$modal_image, {
      showModal(modalDialog(
        size = "m",
        easyClose = T,
        fade = T,
        img(src=input$modal_image, height="100%", width="100%")
      ))
    })
  
  #####################################################################################
  #Navigate to How To Start tab when navigate_to_tab is triggered by Shiny.setInputValue
  observeEvent(
    input$navigate_to_tab, {
      if (input$navigate_to_tab != "") {
        updateTabItems(session, "menuItems", input$navigate_to_tab)
      }
    }
  )
  
}

shinyApp(ui, server)
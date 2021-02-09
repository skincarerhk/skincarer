library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

#ingredient strings (Separated by ";") -> ingredient full table
create_ingredient_table_full <- 
  function(ingredients_string, ingredient_library) {
    ingredients_vector<- ingredients_string %>% str_split(";") %>% unlist()
    
    output_df<-
      data.frame(
        ingredients = ingredients_vector
        ) %>% 
      mutate(
        join_helper = ingredients %>% str_remove_all("[[:punct:] ]+") %>% str_remove_all(" ")  %>% tolower
        ) %>% 
      left_join(ingredient_library, by = "join_helper", keep = FALSE) %>%
      distinct(join_helper, .keep_all = T) %>%
      select(-join_helper)
    
    return(output_df)
  }

#ingredient full table -> ingredient table display
create_ingredient_table_display <- 
  function(ingredient_table_full) {
    
    effect_convertor<-
      list(
        "sensitive_skin_bad" = "Bad for Sensitive Skin",             
        "sensitive_skin_good" = "Good for Sensitive Skin",
        "dry_skin_bad" = "Bad for Dry Skin",
        "dry_skin_good" = "Good for Dry Skin",
        "oily_skin_bad" = "Bad for Oily Skin",
        "oily_slin_good" = "Good for Oily Skin",
        "harmful_sub_alcohol" = "Alcohol",
        "harmful_sub_allergens" = "Allergens",
        "harmful_sub_paraben" = "Paraben",
        "harmful_sub_sulfate" = "Sulfate",
        "harmful_sub_silicone" = "Silicone",
        "harmful_sub_fungal_acen_trigger" = "Acne Triggering",
        "effect_uv_protection" = "UV Protection",
        "effect_promotes_wound_healing" = "Promote Wound Healing",
        "effect_acne_fighting" = "Acne Fighting",
        "effect_brightening" = "Brightening",
        "effect_anti_aging" = "Anti Aging"
      )
    
    ingredient_table_full$effect<- 
      apply(
        ingredient_table_full %>% select(sensitive_skin_bad:effect_anti_aging),
        1,
        function(x) {
          names(x)[(x==TRUE & !is.na(x))] %>%
            effect_convertor[.] %>%      #Convert column names to readable descriptions
            paste0(collapse = "<br/>")
          }
        )
    
    ingredient_table_full<-
      ingredient_table_full %>%
      select(
        Ingredients = ingredients,
        "Safety Rating" = ewg,
        #"Data Availability" = data_availability,
        "Acne Triggering" = comedogenic_rating,
        Remark = remark,
        Effect = effect
      )
    
    return(ingredient_table_full)
  }

create_product_library<-
  function(submission, ingredient_library) {  
    #submission: data.frame
    #mandated columns: "category", "brand", "product_name", "ingredients"
    #optional columns: "image", "rating", "n_rating", "price", "product_url", "Cruelty.Free", "Minimal.Ingredients"
    
    submission<- 
      submission %>%
      #tibble() %>%
      rowwise() %>%
      mutate(ingredient_table = list(create_ingredient_table_full(ingredients, ingredient_library)))
    
    submission<-
      submission %>%
      transmute(
        "category" = category,
        "brand" = brand,
        "product_name" = product_name,
        "image" = ifelse(!"image" %in% colnames(.), "Img/image_blank.png",image),
        "rating" = ifelse(!"rating" %in% colnames(.), 0, rating),
        "n_rating" = ifelse(!"n_rating" %in% colnames(.), 0, n_rating),
        "price" = ifelse(!"price" %in% colnames(.), "----", price),
        "product_url" = ifelse(!"product_url" %in% colnames(.),"N/A", product_url),
        "ingredients" = ingredients,
        "Paraben.Free" = ingredient_table$harmful_sub_paraben %>% sum(na.rm = T)==0,                 
        "Sulfate.Free" = ingredient_table$harmful_sub_sulfate %>% sum(na.rm = T)==0,
        "Alcohol.Free" = ingredient_table$harmful_sub_alcohol %>% sum(na.rm = T)==0,                  
        "Silicone.Free" = ingredient_table$harmful_sub_silicone%>% sum(na.rm = T)==0 ,                
        "EU.Allergen.Free" = ingredient_table$harmful_sub_allergens %>% sum(na.rm = T)==0,             
        "Fungal.Acne.Malassezia.Safe" = ingredient_table$harmful_sub_fungal_acen_trigger %>% sum(na.rm = T)==0,
        "Cruelty.Free" = ifelse(!"Cruelty.Free" %in% colnames(.), FALSE, Cruelty.Free),
        "Minimal.Ingredients" = ifelse(!"Minimal.Ingredients" %in% colnames(.), FALSE, Minimal.Ingredients),
        "Brightening" = ingredient_table$effect_brightening %>% sum(na.rm = T)>0,
        "Promotes.Wound.Healing" = ingredient_table$effect_promotes_wound_healing %>% sum(na.rm = T)>0,
        "Acne.Fighting" = ingredient_table$effect_acne_fighting %>% sum(na.rm = T)>0,
        "Anti.Aging" = ingredient_table$effect_anti_aging %>% sum(na.rm = T)>0,
        "UV.Protection" = ingredient_table$effect_uv_protection%>% sum(na.rm = T)>0,
        "Dry.Skin.Good" = ingredient_table$dry_skin_good%>% sum(na.rm = T),                 
        "Dry.Skin.Bad" = ingredient_table$dry_skin_bad%>% sum(na.rm = T),                  
        "Oily.Acne.Prone.Skin.Good" = ingredient_table$oily_skin_good%>% sum(na.rm = T),      
        "Oily.Acne.Prone.Skin.Bad" = ingredient_table$oily_skin_bad %>% sum(na.rm = T), 
        "Sensitive.Skin.Good" = ingredient_table$sensitive_skin_good %>% sum(na.rm = T),         
        "Sensitive.Skin.Bad" = ingredient_table$sensitive_skin_bad %>% sum(na.rm = T),            
        "ewg_safe" = round(sum(ingredient_table$ewg<=2, na.rm = T)/ length(ingredient_table$ewg)*100, digits = 0),                      
        "ewg_moderate" = round(sum(ingredient_table$ewg>2 & ingredient_table$ewg<=6, na.rm = T)/ length(ingredient_table$ewg)*100, digits = 0),
        "ewg_hazard" = round(sum(ingredient_table$ewg>6, na.rm = T)/ length(ingredient_table$ewg)*100, digits = 0),                    
        "ewg_unknown" = round(sum(is.na(ingredient_table$ewg))/ length(ingredient_table$ewg)*100, digits = 0),                   
        "cir_safe" = round(sum(ingredient_table$cir == "A", na.rm = T)/ length(ingredient_table$cir)*100, digits = 0),                      
        "cir_moderate" = round(sum(ingredient_table$cir == "B", na.rm = T)/ length(ingredient_table$cir)*100, digits = 0),
        "cir_hazard" = round(sum(ingredient_table$cir == "C", na.rm = T)/ length(ingredient_table$cir)*100, digits = 0),                    
        "cir_unknown" = round(sum(is.na(ingredient_table$cir))/ length(ingredient_table$cir)*100, digits = 0),
        "ref"= paste(brand,product_name, sep="#_"),
        "dry_skin" = ifelse(Dry.Skin.Bad>0 ,"Bad", ifelse(Dry.Skin.Good>0, "Good", "Neutral")),
        "oily_acne_prone_skin" = ifelse(Oily.Acne.Prone.Skin.Bad>0 ,"Bad", ifelse(Oily.Acne.Prone.Skin.Good>0, "Good", "Neutral")),
        "sensitive_skin" = ifelse(Sensitive.Skin.Bad>0 ,"Bad", ifelse(Sensitive.Skin.Good>0, "Good", "Neutral")),
        "n_ingredient" = nrow(ingredient_table),
        "ewg_n_unknown" = round(ewg_unknown * n_ingredient /100, digits = 0),
        "ewg_average_rating" = ingredient_table$ewg %>% as.numeric() %>% mean(na.rm=TRUE),
        "ewg_risk" = ifelse(n_ingredient ==0 | ewg_unknown >=20, "Unknown", ifelse(ewg_hazard>0, "C", ifelse((ewg_moderate*n_ingredient/100)>=5,"B",ifelse((ewg_moderate*n_ingredient/100) == 0, "Perfect", "A")))),
        "cir_risk" = ifelse(n_ingredient ==0 | cir_unknown >=20, "Unknown", ifelse(cir_hazard>0, "C", ifelse((cir_moderate*n_ingredient/100)>=5,"B",ifelse((cir_moderate*n_ingredient/100) == 0, "Perfect", "A"))))
      ) 
    
    return(submission)
  }


#Create UI - product card
create_product_card<- function(product_data) {
  
  functions <- 
    product_data[c(
      "Brightening",
      "Promotes.Wound.Healing",
      "Acne.Fighting",
      "Anti.Aging",
      "UV.Protection")] %>%
    unlist() %>%
    .[. > 0] %>%
    names() %>%
    str_replace_all(
      pattern = "\\.", " ") %>%
    str_replace_all("Promotes Wound Healing", "Wound Healing") %>%
    lapply(dashboardLabel, status = "info")
  
  if(length(functions)==0){
    functions<- dashboardLabel("N/A", status = "warning")
  }
  
  harm_free_features<- product_data[c(
    "Paraben.Free",
    "Sulfate.Free",
    "Alcohol.Free",
    "Silicone.Free",
    "EU.Allergen.Free",
    "Cruelty.Free",
    "Fungal.Acne.Malassezia.Safe"
  )] %>%
    unlist() %>%
    .[.==T] %>%
    names %>%
    str_replace_all(
      pattern = 
        c(
          "Paraben.Free" = "Paraben Free",
          "Sulfate.Free" = "Sulfate Free",
          "Alcohol.Free" = "Alcohol Free",
          "Silicone.Free" = "Silicone Free",
          "EU.Allergen.Free" = "Allergen Free",
          "Cruelty.Free" = "Cruelty Free",
          "Fungal.Acne.Malassezia.Safe" = "Fungal Acne Safe"
        )
    ) %>%
    lapply(dashboardLabel, status = "success")
  
  product_card<-
    gradientBox(width = 4,
                #height = "500px",
                gradientColor = "teal",
                collapsible = F,
                closable = F,
                footer_padding = T,
                title = div(
                  h3(product_data["brand"], style = "display:inline; font-weight:bold"),
                  div(
                    style = "position:absolute; right:5px; top: 5px",
                    actionButton("Button_add_to_my_skincare_products",
                                 icon = icon("plus"),
                                 "",
                                 style = "background-color:#39cccc; color:white; font-weight:bold; border-style: none;",
                                 ref = product_data["ref"],
                                 onclick = "Shiny.setInputValue('add_to_my_skincare_products', this.getAttribute('ref'));"
                    ),
                    a(icon("whatsapp"),
                      "",
                      class = "btn",
                      style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none",
                      href = paste0("https://api.whatsapp.com/send?text=http://skincarehk.com/?product=", 
                                    product_data["product_name"] %>% as.character() %>% URLencode() %>%URLencode(repeated = T) ) 
                    )
                    
                    # ,actionButton("Button_Close_Box",
                    #              icon = icon("close"),
                    #              "",
                    #              #onclick = "this.parentNode.parentNode.parentNode.parentNode.parentNode.remove();", #Close and remove the box
                    #              "data-widget" ="remove",  #Close the box
                    #              style = "background-color:#39cccc; color:white; font-weight:bold; border-style: none;"
                    # )
                  )
                ),
                footer = 
                  tagList(
                    fluidRow(
                      column(width=4,
                             tags$img(src = product_data["image"], 
                                      width = "200px", 
                                      height = "200px", 
                                      style = "max-width:100%; height:auto",
                                      onclick = 
                                        #Trigger refresh by assigning blank value
                                        "
                                        Shiny.setInputValue('modal_image', '');
                                        Shiny.setInputValue('modal_image', this.getAttribute('src'));
                                        "
                             )
                      ),
                      column(width=8,
                             p(strong("Product: "), product_data["product_name"], style = "font-size:18px"),
                             p(strong("Ingredient Safety Level: "), product_data["ewg_risk"], style = "font-size:18px"),
                             p(strong("Safety Rating (1 Good - 10 Bad): "), round(product_data["ewg_average_rating"] %>% as.numeric(), digits = 1) , style = "font-size:18px"),
                             p(strong("Number of Unknown Ingredients: "), product_data["ewg_n_unknown"], style = "font-size:18px"),
                             p(strong("Number of Ingredients: "), product_data["n_ingredient"], style = "font-size:18px"),
                             p(strong("Customer Rating: "), ifelse(product_data["rating"]==0,"N/A", round(product_data["rating"] %>% as.numeric(), digits = 1)), style = "font-size:18px"),
                             #Ingredient Table
                             p(
                               actionButton("Button_Ingredient_Table", 
                                            "Ingredient Table",
                                            icon = icon("list"),
                                            style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none;",
                                            ingredients = product_data["ingredients"], 
                                            onclick = 
                                            #Trigger refresh by assigning blank value
                                            "
                                            Shiny.setInputValue('modal_ingredient',''); 
                                            Shiny.setInputValue('modal_ingredient', this.getAttribute('ingredients'));
                                            "
                               )
                             ),
                             #Amazon Purchase Link
                             p(a(icon("amazon"),
                                 "Find it on Amazon",
                                 class = "btn",
                                 style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none",
                                 #icon = icon("amazon"),
                                 href = paste0("https://www.amazon.com/s?i=beauty&camp=1789&creative=9325&linkCode=ur2&linkId=c5189870b013859030028977de86a850&tag=skincarer0f-20&k=",
                                               product_data["product_name"])
                             )
                             ),
                             
                             #iHerb Link
                             p(a(icon("leaf"),
                                 "Find it on iHerb (5% Off)",
                                 class = "btn",
                                 style = "background-color:#39cccc; color:white; font-weight:bold;  border-style: none",
                                 href = paste0("https://hk.iherb.com/search?rcode=BBD3886&kw=", 
                                               product_data["product_name"])
                             )
                             )
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(width=4,
                             p(strong("Skin Type:"), style = "font-size:18px"),
                             dashboardLabel("Dry Skin", 
                                            status = ifelse(product_data["dry_skin"]=="Good","success",
                                                            ifelse(product_data["dry_skin"]=="Bad","danger","info"))
                             ),
                             dashboardLabel("Oily Acne Skin", 
                                            status = ifelse(product_data["oily_acne_prone_skin"]=="Good","success",
                                                            ifelse(product_data["oily_acne_prone_skin"]=="Bad","danger","info"))
                             ),
                             dashboardLabel("Sensitive Skin", 
                                            status = ifelse(product_data["sensitive_skin"]=="Good","success",
                                                            ifelse(product_data["sensitive_skin"]=="Bad","danger","info"))
                             )
                      ),
                      column(width=4,
                             p(strong("Harm Free:"), style = "font-size:18px"),
                             harm_free_features
                      ),
                      column(width=4,
                             p(strong("Extra Functions:"), style = "font-size:18px"),
                             functions
                      )
                      
                    )
                  )
    )
  
  return(product_card)
}

# Example
# ingredient_library<- read_csv("Data/ingredient_library.csv")
# product_library<- read_csv("Data/product_library.csv")
# ingredient_table_full<- create_ingredient_table_full(product_library$ingredients[2], ingredient_library)
# ingredient_table_display<- create_ingredient_table_display(ingredient_table_full)
# product_library_test<-create_product_library(product_library[,], ingredient_library)

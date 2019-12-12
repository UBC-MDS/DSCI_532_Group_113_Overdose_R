library(tidyverse)
library(viridis)
library(plotly)
library(gridExtra)
# dash
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(dash)
library(readxl)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# load dataset
pivoted_data <- read_csv("data/2012-2018_lab4_data_drug-overdose-deaths-connecticut-wrangled-pivot.csv")
drug_description <- readxl::read_excel("data/lab4_drug-description.xlsx")

drug_name <- "Heroin"

set_graph_race <- function(drug = drug_name){
# some wrangling for race
drug = sym(drug)
if (drug == sym("Everything")){
    top_race <- pivoted_data %>% 
        count(Race)
} else{
    top_race <- pivoted_data %>% 
    group_by(Race) %>% 
    summarise(n = sum(!!drug))
}
top_race <- top_race %>% 
    arrange(desc(n)) %>% 
    head(3)

race <- top_race %>% 
    ggplot(aes(reorder(Race, -n), n)) + 
    geom_bar(aes(fill = Race), stat = "identity", show.legend = FALSE) + 
    scale_fill_viridis_d() + 
    labs(x = "Race", y = "count", title = paste("Top 3 Races with the most deaths in", drug))

    return(race)
}

set_graph_gender <- function(drug = drug_name){

  drug = sym(drug)
if (drug == sym("Everything")){
    pivoted_data <- pivoted_data 
} else{
    pivoted_data <- pivoted_data %>% 
        filter(!!drug == 1)
}

gender <- pivoted_data %>% 
    filter(Sex == "Male" | Sex == "Female") %>% 
    ggplot(aes(Sex, fill = Sex)) + 
    geom_bar() + 
    scale_fill_viridis_d() + 
    labs(x = "", y = "Gender", title = paste("Gender distribution for the deaths in", drug))

    return(gender)
}


set_graph_age <- function(drug = drug_name){

    drug = sym(drug)
if (drug == sym("Everything")){
    pivoted_data <- pivoted_data 
} else{
    pivoted_data <- pivoted_data %>% 
        filter(!!drug == 1)
}
   age <- pivoted_data %>% 
    ggplot(aes(Age)) + 
    geom_density(alpha = 0.8, show.legend = FALSE, fill = "#21908C") + 
    scale_fill_viridis_d() + 
    labs(x = "Age", y = "count", title = paste("Age distribution for the deaths in", drug)) 

    return(age)
}


app <- Dash$new(external_stylesheets = list("https://cdnjs.cloudflare.com/ajax/libs/normalize/7.0.0/normalize.min.css",
                                            "https://cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css",
                                            "https://codepen.io/bcd/pen/KQrXdb.css", 
                                            "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"))

DrugsDD <- dccDropdown(
  id = 'drugs_dd',
  options = lapply(
    unique(drug_description$Drug), function(x){
      list(label=x, value=x)
    }),
    value = 'Heroin'
)

set_description <- function(drug = drug_name){
    
    filtered <- drug_description %>% filter(Drug == drug)

    return(filtered[["Description"]])
}
   
set_image <- function(drug = drug_name){
  
    filtered <- drug_description %>% filter(Drug == drug)

    return(filtered[["Link"]])

}

set_reference<- function(drug = drug_name){

    filtered <- drug_description %>% filter(Drug == drug)

    return(filtered[["Reference"]])
}



app$layout(
  htmlDiv(
    list(
      htmlDiv(htmlH1('Overdose')),
      htmlDiv(
        list(
           dccTabs(id="tabs", children = list(
                dccTab(label = 'The Killer'),
                dccTab(label = 'The Victims', children = list(
                    htmlDiv(list(
                        DrugsDD,
                        htmlImg(
                          id='drug_img',
                          src = set_image(),
                          height = '150',
                          width = '200'
                          ),
                        htmlP(children = set_description(), id="drug_desc"),
                        htmlA(
                          children = 'This info was retrieved from drugbank.ca',
                          id = "drug_ref",
                          href = set_reference(),
                          target="_blank")
                        ),  style = list('display' = "block", 'float' = "left", 'margin-left' = "100px",
                              'margin-right' = "1px", 'width' = "500px", "font-size" = "15px"),                              
                    ),
                    htmlDiv(
                      list(
                        htmlDiv(list(
                            dccGraph(
                              id='vic-age_0',
                              figure = ggplotly(set_graph_age(), width = 400, height = 300)
                              )
                            ), style = list('display' = "table-row", "margin-bottom" = "1px") 
                          ),
                          htmlDiv(list(
                              htmlDiv(list(
                              dccGraph(
                                id='vic-gender_0',
                                figure = ggplotly(set_graph_gender(), width = 400, height = 300)
                                )
                              ), style = list('display' = "block", 'float' = "left", 'margin-left' = "1px",
                              'margin-right' = "1px")
                              ),
                              htmlDiv(list(
                              dccGraph(
                                id='vic-race_0',
                                figure = ggplotly(set_graph_race(), width = 400, height = 300)
                                )
                              ), style = list('display' = "block", 'float' = "left",  'margin-left' = "1px",
                              'margin-right' = "1px")
                              )          
                            ) , style = list('display' = "table-row", "margin-top" = "1px", 'float' = "left") 
                          ) 
                      ), style = list('float' = "right")
                    )
                  )
                )
              )   
           )
        )
      )
    ), style = list('background-color' = "#ffffff")
  )
)

#Callbacks
 app$callback(
   output=list(id = 'drug_img', property='src'),
   
   params=list(input(id = 'drugs_dd', property='value')),

   function(drug_input) {
     result <- set_image(drug = drug_input) 
     return(result)
   })

 app$callback(
   output=list(id = 'vic-race_0', property='figure'),
   
   params=list(input(id = 'drugs_dd', property='value')),

   function(drug_input) {
     result <- ggplotly(set_graph_race(drug = drug_input) ,width = 400, height = 300)

     return(result)
   })

   app$callback(
   output=list(id = 'vic-gender_0', property='figure'),
   
   params=list(input(id = 'drugs_dd', property='value')),

   function(drug_input) {
     result <- ggplotly(set_graph_gender(drug = drug_input) ,width = 400, height = 300)

     return(result)
   })

 app$callback(
   output=list(id = 'vic-age_0', property='figure'),
   
   params=list(input(id = 'drugs_dd', property='value')),

   function(drug_input) {
     result <- ggplotly(set_graph_age(drug = drug_input) ,width = 400, height = 300)

     return(result)
   })

  app$callback(
   output=list(id = 'drug_desc', property='children'),
   
   params=list(input(id = 'drugs_dd', property='value')),

   function(drug_input) {
     result <- set_description(drug = drug_input) 
     return(result)
   })

app$run_server(showcase = TRUE)


           
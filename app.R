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
url <- "https://github.com/UBC-MDS/DSCI_532_Group_113_Overdose_R/blob/master/data/2012-2018_lab4_data_drug-overdose-deaths-connecticut-wrangled-pivot.csv?raw=true"
pivoted_data <- read_csv(url)

url_2 <- "https://github.com/UBC-MDS/DSCI_532_Group_113_Overdose_R/blob/master/data/lab4_drug-description.csv?raw=true"
drug_description <- read_csv(url_2)
url_3 <- "https://github.com/UBC-MDS/DSCI_532_Group_113_Overdose_R/blob/master/data/2012-2018_lab4_data_drug-overdose-counts.csv?raw=true"
combination_count <- read_csv(url_3) %>% 
                        rename(second_drug =  `Second drug`) %>%
                        mutate(index = factor(index),
                               second_drug = factor(second_drug))

combination_count$index <- combination_count$index %>% 
                                fct_relevel('Heroin', 'Fentanyl', 'Cocaine', 'Benzodiazepine', 'Ethanol', 'Oxycodone',
                                 'Methadone', 'Other', 'Fentanyl Analogue', 'Amphet', 'Tramad', 'Hydrocodone',
                                  'Oxymorphone','OpiateNOS', 'Morphine', 'Hydromorphone')
                         

combination_count$second_drug <- combination_count$second_drug %>% 
                                 fct_relevel('Hydromorphone','Morphine','OpiateNOS','Oxymorphone','Hydrocodone','Tramad','Amphet','Fentanyl Analogue',
                                           'Other','Methadone','Oxycodone', 'Ethanol', 'Benzodiazepine',  'Cocaine','Fentanyl', 'Heroin')

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

drugs_heatmap <- combination_count %>% ggplot(aes(index, second_drug)) +
                                          geom_tile(aes(fill = Count)) +
                                          geom_text(aes(label = round(Count, 1)), color = 'white') +
                                          labs(x = "Second drug", y = "First drug")+
                                          scale_fill_viridis() +
                                          theme_minimal() +
                                          theme(
                                              axis.text = element_text(angle = 45)
                                          )

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
                dccTab(label = 'The Killer', children =list(
                  htmlDiv(list(
                   htmlP("Here goes the other graph")
                  ), style = list('display' = "block", 'float' = "left", 'margin-left' = "10px",
                              'margin-right' = "1px", 'width' = "10px", "font-size" = "15px") ),
                    htmlDiv(list(
                   dccGraph(
                              id='vic-heatmap-0',
                              figure = ggplotly(drugs_heatmap, width = 800, height = 600)
                              )
                  ), style = list('display' = "block", 'float' = "right", 'margin-left' = "10px",
                              'margin-right' = "500px", 'width' = "500px", "font-size" = "15px") )
                  )
                ),
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
                              'margin-right' = "1px", 'width' = "300px", "font-size" = "15px"),                              
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

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))


           
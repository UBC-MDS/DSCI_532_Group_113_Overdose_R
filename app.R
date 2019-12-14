library(tidyverse)
library(viridis)
library(plotly)
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
url_1 <- "https://github.com/UBC-MDS/DSCI_532_Group_113_Overdose_R/blob/master/data/2012-2018_lab4_data_drug-overdose-deaths-connecticut-wrangled-melted.csv?raw=true"
drug_overdose_wrangled_m = read_csv(url_1) 
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

header_colors <- function(){
  list(
    bg_color = "#0D76BF",
    font_color = "#fff",
    "light_logo" = FALSE
  )
}

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
    labs(x = "Race", y = "count", title = paste("Top 3 Races \nwith the most deaths in", drug)) + 
    theme(
      plot.title = element_text(size = 10),
      axis.text = element_text(angle = 45),
      axis.text.x=element_blank()
    )
  
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
    geom_bar(show.legend = FALSE) + 
    scale_fill_viridis_d() + 
    labs(x = "Gender", title = paste("Gender distribution \nfor the deaths in", drug)) + 
    theme(
      plot.title = element_text(size = 10),
      axis.text = element_text(angle = 45),
      axis.text.x=element_blank()
    )
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
    labs(x = "Age", y = "count", title = paste("Age distribution \nfor the deaths in", drug)) + 
    theme(
      plot.title = element_text(size = 10),
      axis.text = element_text(angle = 45)
      )
  
  return(age)
}

drugs_heatmap <- combination_count %>% 
ggplot(aes(index, second_drug, text = paste('First Drug:', index, '<br>Second Drug: ', second_drug))) +
  geom_tile(aes(fill = Count)) +
  geom_text(aes(label = round(Count, 1)), color = 'white', size = 3) +
  labs(title = "Count of overdose victims with a combination of 2 drugs", x = "First drug", y = "Second drug") +
  scale_fill_viridis() +
  theme_minimal() +
  theme(
    axis.text = element_text(angle = 45)
  )
drugs_heatmap <- ggplotly(drugs_heatmap, width = 650, height = 600, tooltip = "text")

df <- drug_overdose_wrangled_m %>%   
  group_by(Drug) %>%
  summarize(times_tested_positive = sum(Toxicity_test, na.rm = TRUE))%>%
  arrange(desc(times_tested_positive))

h_bar_plot <- df %>% ggplot(aes(x=reorder(Drug, times_tested_positive), y=times_tested_positive)) +
  geom_bar(stat='identity',fill="cyan4") +
  coord_flip()+
  labs(title = "Ranking of drugs by the times tested positive",x ="Drug ", y = "Times a drug tested positive")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=10))
  



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
  htmlDiv(htmlBr(),
          children = list(
            htmlDiv(
              id = "app-page-header",
              style = list(
                width = "100%",
                background = header_colors()[["bg_color"]],
                color = "#fff"
                
              ),
              children = list(
                htmlA(
                  id = "dashbio-logo",
                  href = "/Portal"
                ),
                htmlH1("Overdose"),
                htmlA(
                  id = "gh-link",
                  children = list(paste0(
                    "How drug overdose is stealing lives from us!"
                  )), 
                  href = "https://github.com/UBC-MDS/DSCI_532_Group_113_Overdose_R",
                  style = list(color = "white",'margin-left' = "10px","font-size" = "20px"),
                  
                  htmlImg(
                    src = "assets/git.png"
                  )
                )
              )
              
              
            ),
            
            
            
            htmlDiv(
              style = list('margin-left' = "10px"),
              children = list(htmlH5(paste0(
                                "Overdose app allows you to visualize ",
                                "different factors associated with ",
                                "accidental death by overdose in Connecticut, US, from 2012 - 2018"
                              )
                              ))),
            
            htmlDiv(style = list('margin-left' = "10px"),
                    children = list(
                      htmlH5(style = list(color = "grey",'margin-left' = "10px","font-size" = "20px"),paste0(
                        "You ",
                        "can interactively explore this issue ",
                        "using (The Killers tab) or ",
                        "the  (The Victims tab)"
                      )
                      ))),
            
            htmlDiv(
              list(
                dccTabs(id="tabs", children = list(
                  dccTab(label = 'The Killer', children =list(
                    htmlDiv(list( 
                      htmlP("This section, named 'the killers', focuses on the effect of drugs. Two static graphs are displayed; one is the prevalence ranking of drugs found in the deceased people. Another one is the correlation map of two drugs from this dataset, which counts and compares the occurrences of two-drug combinations in the deaths."
                    )), style = list("margin-left" = "300px", "margin-right" = "300px", "font-size"= "16px")),
                    htmlDiv(list(
                      dccGraph(
                        id='vic-drugs',
                        figure = ggplotly(h_bar_plot, width = 550, height = 600)
                      )
                    ), style = list('display' = "block", 'float' = "left", 'margin-left' = "10px",
                                    'margin-right' = "1px", 'width' = "500px", "font-size" = "15px", "margin-bottom" = "3px") ),
                    htmlDiv(list(
                      dccGraph(
                        id='vic-heatmap-0',
                        figure = drugs_heatmap
                      )
                    ), style = list('display' = "block", 'float' = "right", 'margin-left' = "10px",
                                    'margin-right' = "10px", 'width' = "650px", "font-size" = "15px", "margin-bottom" = "3px") )
                  )
                  ),
                  dccTab(label = 'The Victims', children = list(
                    htmlDiv(list(
                      htmlP("Please select one drug to see the affected demographic group by age, race and gender"),
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
                            figure = ggplotly(set_graph_age(), width = 700, height = 300)
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
                                          'margin-right' = "50px")
                          )          
                        ) , style = list('display' = "table-row", "margin-top" = "1px", 'float' = "left") 
                        ) 
                      ), style = list('float' = "right")
                    )
                  )
                  )
                ), style = list("font-size"= "16px", "font-weight" = "bold")   
                )
              )
            ), 
              htmlDiv(style = list('margin-left' = "10px"),
                    children = list(
                      htmlA(children = "Data retrieved from the data.ct.gov", href = "https://catalog.data.gov/dataset/accidental-drug-related-deaths-january-2012-sept-2015"
                      )))
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
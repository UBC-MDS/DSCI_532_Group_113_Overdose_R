library(tidyverse)
library(viridis)
library(plotly)
# dash
library(dashCoreComponents)
library(dashHtmlComponents)
library(dash)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# load dataset
pivoted_data <- read_csv("data/2012-2018_lab4_data_drug-overdose-deaths-connecticut-wrangled-pivot.csv")
drug_name <- "Heroin"
drug_name <- sym(drug_name)

# some wrangling for race
if (drug_name == sym("Everything")){
    top_race <- pivoted_data %>% 
        count(Race)
} else{
    top_race <- pivoted_data %>% 
    group_by(Race) %>% 
    summarise(n = sum(!!drug_name))
}
top_race <- top_race %>% 
    arrange(desc(n)) %>% 
    head(3)

# race plot
race <- top_race %>% 
    ggplot(aes(reorder(Race, -n), n)) + 
    geom_bar(aes(fill = Race), stat = "identity", show.legend = FALSE) + 
    scale_fill_viridis_d() + 
    labs(x = "Race", y = "count", title = paste("Top 3 Races with the most deaths in", drug_name))

# age plot
age <- pivoted_data %>% 
    ggplot(aes(Age)) + 
    geom_density(alpha = 0.8, show.legend = FALSE, fill = "#21908C") + 
    scale_fill_viridis_d() + 
    labs(x = "Age", y = "count", title = paste("Age distribution for the deaths in", drug_name)) 

#gender plot
gender <- pivoted_data %>% 
    filter(Sex == "Male" | Sex == "Female") %>% 
    ggplot(aes(Sex, fill = Sex)) + 
    geom_bar() + 
    scale_fill_viridis_d() + 
    labs(x = "", y = "Gender", title = paste("Gender distribution for the deaths in", drug_name))

app = Dash$new()

app$layout(htmlDiv(list(
  htmlH1('Overdose'),
  dccTabs(id="tabs", value='tab_1', children=list(
    dccTab(label='The Killer', value='tab_1'),
    dccTab(label='The Victim', value='tab_2')
  )),
  htmlDiv(id='tabs_content')
)))

app$layout(htmlDiv(list(
  htmlH1('Overdose'),
  dccTabs(id="tabs", children=list(
    dccTab(label='The Killer', children=list(
      dccGraph(
        id='vic-gender_0',
        figure = ggplotly(gender, width = 600, height = 400)
      )
      )),
    dccTab(label='The Victim', children=list(
      dccGraph(
        id='vic-age',
        figure = ggplotly(age, width = 600, height = 400)
      ), 
      dccGraph(
        id='vic-gender',
        figure = ggplotly(gender, width = 600, height = 400)
      ), 
      dccGraph(
        id='vic-race',
        figure = ggplotly(race, width = 600, height = 400)
      )
      ))
  ))
)))

app$run_server()

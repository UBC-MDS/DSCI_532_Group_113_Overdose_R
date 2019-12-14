# Reflection

## visualization interactivity 

Overdash is a visualization app that is designed to provide insights into the rising deaths by accidental overdose in Connecticut from 2012 to 2019. It assists health professional, law enforcement professionals and policymaker by providing a visual representation of this crisis. This is the R version of the app. 

During the implementation of the R app, we took into account the feedback given from UBC-MDS teaching team and feedback received during DSCI 532 peer review session and we made changes accordingly. The changes have enhanced visualization interactivity of the app in many ways. First, the trend line plot was removed as it caused confusion  to some users. Instead, we focused on enhancing the interactivity of other plots. For example, two tabs were designed so that the users can move from one section to another in a smoother way. Furthermore, descriptions were added to each plot to assist users to understand the usage of each plot. Also,the colours of all plots were set to be colour-blind friendly. To compensate for the lack of data in smaller demographic groups, we selected only the top three more frequent groups to be displayed on the graphs. Finally, links to the source of the data and the drug information website were added.  

# Limitations

## Data

The data used to generate this app was limited to one city in the US. We think the scope of the app can be expanded to wider populations to include more demographic groups other than Connecticut. In the future version of the app, we plan to allow the user to upload the data that require analysis.

## Visualization

One of the limitations of the app is that it shows only a combination of up to 2 drugs in the heatmap. Additional plot has to be included to display the effect of a combination of more than 3 drugs at the same times. For the effect of drugs by race,  I  the top 3 groups are selected for visualization purposes. More interactivity can be added to the plots by allowing the user to select the number of groups interactively. 

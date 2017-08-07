# Analyze Boston Data Explorer

This application utilizes the public data sets on Analyze Boston to provide a web interface for creating interactive map data visualizations based off of any of the geospatial data sets on Analyze Boston. It includes basic functionality such as plotting point, line, or polygon data on a map, and choosing a color for each data layer. Advanced features include clustering point data or scaling point or polygon data based on a data parameter. It includes a data viewer which shows the data in a table view. Finally, it has a few data showcases which present premade interactive maps that highlight some of the possibilities of the Data Explorer. They also connect users with Boston resources such as the Imagine Boston plan. 

## Getting Started

In order to run this project, you must install [R](https://cran.r-project.org/) on your machine. You should also install [RStudio](https://www.rstudio.com/products/rstudio/download/), as it is the best IDE for presenting R apps, especially R Shiny applications.

Clone this repository into a chosen folder, and open the project in RStudio. Before running the application, the required libraries must be installed. See the top of the main files (`ui.r`, `global.r`, `server.r`) to see which libraries must be installed. With any of these files open, you can click "Run App" in RStudio. This will start the application on your local machine.

## Project Structure

The application has the basic Shiny application structure: `ui.r` contains the front-end structure of the application. From the navigation bar to the social media buttons, elements shown on the web page are (for the most part) described in this file. `server.r` contains the logic that controls the application. Data layers are added, or inputs are made reactive, all through this file. Note that the website has three main views: Interactive Map, Data Viewer, and Data Showcases. Because of this, both `ui.r` and `server.r` are segmented between these three views. Finally, there is `global.r`, which represents the data and utility functions that might be required in both UI and server, or might be required across multiple Shiny sessions.

## Deployment

Currently, this application is live on a free [shinyapps website](https://rupayan.shinyapps.io/dataExplorer/). Luckily, RStudio makes deploying Shiny applications very simple. Just a click of the blue deploy button in the application lets a user deploy any application to their linked account. Of course, the account that the current application is on will probably soon be deprecated.

## Authors

**GE's Boston Civic Fellows** created this application in the summer of 2017. The application was created in a collaboration between the City of Boston and General Electric, as a part of the company's move of its headquarters to Boston.

## License



## Acknowledgments


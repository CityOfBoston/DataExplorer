# Analyze Boston Data Explorer

This application utilizes the public data sets on Analyze Boston to provide a web interface for creating interactive map data visualizations based off of any of the geospatial data sets on Analyze Boston. It includes basic functionality such as plotting point, line, or polygon data on a map, and choosing a color for each data layer. Advanced features include clustering point data or scaling point or polygon data based on a data parameter. It includes a data viewer which shows the data in a table view. Finally, it has a few data showcases which present premade interactive maps that highlight some of the possibilities of the Data Explorer. They also connect users with Boston resources such as the Imagine Boston plan. 

## Getting Started

In order to run this project, you must install [R](https://cran.r-project.org/) on your machine. You should also install [RStudio](https://www.rstudio.com/products/rstudio/download/), as it is the best IDE for presenting R apps, especially R Shiny applications.

Clone this repository into a chosen folder, and open the project in RStudio. Before running the application, the required libraries must be installed. See the top of the main files (`ui.r`, `global.r`, `server.r`) to see which libraries must be installed. With any of these files open, you can click "Run App" in RStudio. This will start the application on your local machine.

Here is a quick [Intro Video](https://youtu.be/rxOF4i_9AkQ) that was created for the application. Watch it to get a general idea of the features and possibilities of the application.

## Project Structure

The application has the basic Shiny application structure: `ui.r` contains the front-end structure of the application. From the navigation bar to the social media buttons, elements shown on the web page are (for the most part) described in this file. `server.r` contains the logic that controls the application. Data layers are added, or inputs are made reactive, all through this file. Note that the website has three main views: Interactive Map, Data Viewer, and Data Showcases. Because of this, both `ui.r` and `server.r` are segmented between these three views. Finally, there is `global.r`, which represents the data and utility functions that might be required in both UI and server, or might be required across multiple Shiny sessions.

## Deployment

Currently, this application is live on a free [shinyapps website](https://samanthayoung.shinyapps.io/DataExplorer/). Luckily, RStudio makes deploying Shiny applications very simple. Just a click of the blue deploy button in the application lets a user deploy any application to their linked account. Of course, the account that the current application is on will probably soon be deprecated.

**IMPORTANT: When deploying the application, make sure to change the _baseURL_ variable in global.R to whatever the new base URL will be wherever the application is located. This URL is used for social sharing buttons.**

## Design/Development Mindset

This application was designed with a heavy focus on UI and UX. Consider conducting user testing and feedback whenever developing new views and designs. Note that a good mindset when presenting data to the user is to keep it simple, which is why the basic functionality of the Data Explorer is kept to selecting a data set and a color. Advanced features should be under the advanced options panel.

## Potential Future Features

The application has a lot of potential in terms of future updates. Here is a list of some features that could be added, in no particular order:
* Allow for MassGIS, MBTA, and other public data sets to be selected as well.
* Create a legend for when polygons are colored based on parameters.
* Implement a loading symbol/sign when the data explorer is loading large data sets
* Identify and notify users of large data sets
  * Note that Analyze Boston data sets have a parameter called **size**, but unfortunately that parameter is left blank on all the data sets currently. Possibly if this is updated on the Analyze Boston API, the Data Explorer can show a simple notification on larger data sets.
* Style/customize the Data Viewer tab better
* Better descriptions of data sets/make descriptions more accessible
  * Currently, the description of the data sets are only shown in the advanced options panel. Ideally people would be able to see it somehow while selecting it from the dropdown, while avoiding making the UI too cluttered. One potential solution is making a small icon with a tooltip that pops up on hover.
* Fix bug with Facebook social button that causes popups to be blocked.
  * This is because the social popup is not directly opened by the "click" action.
* Add more social features
  * Shorten URLs when sharing so that social links are not overwhelming, better for Twitter.
  * Make the share preview (meta data tags) look nicer.
  * Have a dynamic image preview showcasing the interactive map when sharing it.
* Incorporate the [Intro Video](https://youtu.be/rxOF4i_9AkQ) into the application somehow.
* Provide a on-first-visit tutorial for the application.
  * Could be the above intro video
  * Could be a new video that is more of a guide of the features.
  * Could be an interactive guide that people can click through to create a map.
* Add more/improve the data showcases. Create narratives utilizing new data sets and new advanced features.
* Create a simple advanced feature that puts a data parameter on the label of a data point/polygon.
  * Similar to what the current data parameter scale feature does, but instead of having a data parameter, simply have it be a "data label" where uses can click on a point/polygon in order to view that data point's parameter.
  * Would allow for users to inspect non-numeric parameters such as addresses or string-based qualities of the data.

## Authors

**GE's Boston Civic Fellows** created this application in the summer of 2017. The application was created in a collaboration between the City of Boston and General Electric, as a part of the company's move of its headquarters to Boston.

## License



## Acknowledgments

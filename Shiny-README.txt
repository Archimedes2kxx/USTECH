This README file is for reference when I have been away from Shiny apps for a while and need a quick recovery tutorial

A. Structure
-- Shiny apps can have two files: ui.R and server.R ... but both code chunks can be put into a single app.R file
-- The ui.R code is eventually converted to the HTML interface for users
-- The server.R code runs on the server and accesses any data files the app needs

B. Creation
-- Write the ui.R and server.R files
-- If either is loaded, the R Studio IDE displays a Run App button just above the edit window.
-- Click the Run App button to run the app

C. Publish
-- Shiny apps usually run on the R Studio server
-- When either ui.R or server.R is loaded, a publish button will be displayed above the edit window. It looks like an "eye" with eyeliner above the eyeball and below
-- Click the eyeball and select where to publish the Shiny app to the R Studio server ... includes all files in the app's project director
-- Copy the URL of the published server URL to access the app from  any browser

D. Review the Shiny tutorial videos and older notes to remember how to code
1 -- Simple template for Shiny apps
    library(shiny)
    
    ui <- fluidPage()
    
    server <- function(input, output) {
    
    }
    
    shinyApp(ui = ui, server = server)

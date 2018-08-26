# How to develop a shiny app?

## Resources: Coursera Course - Developing Data Products 

## Users

  - Web app for your prediction algorithm: you want to create a web site so that users can input the relevant predictors and obtain their prediction.
  
## What is Shiny?

  - Shiny is a platform for creating interactive R programs embedded into a web page.  
  
## Three components in web pogamming

  - html: gives a webpage structure and sectioning as well as markup instructions
  
  - css: gives the style of the web page
  
  - java scripts (js): for interactivity
  
## Introduction to Shiny

- Step 1: Install shiny

```r
install.packages("shiny")
library(shiny)
```

- How to create a shiny project?

In order to create a shiny project you need two things:
        
        1. ui.R : For use interface, controls how it looks.
        
        2. server.R: Controls what it does.
        
  The two files need to be in the same directory (folder). 
  
### Example 1

  1. Following is a minimal example of ui.R

```r
library(shiny)
shinyUI(pageWithSidebar(
#----- title of my shiny app -----------
  headerPanel("Data science FTWI"),
#----- display of the sidebar panel-----
  sidebarPanel(
  h3('Sidebar text')
  ),
#----- display of the main panel--------
  mainPanel(
  h3('Main Panel text') # h3: 3rd level html heading
  )
#---------------------------------------
))

```
  2.  server.R
  
    Following server.R function is not going to do anything.
  
```r
library(shiny)
shinyServer(
## Even though this function is not doing anything we need the following function to be included
  function(input, output){ 
  }
#............................
)

```
  3. runApp
  
  In R, change to the directories with these files and type runApp()
  
![Example 1 output](figures/ex1.png)  



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

############### body ################

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
##########################################

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

## Example 2: R function for HTML markups

Note: WHen creating a shiny app you should be careful of the the commas.

1. ui.R function

```r
library(shiny)
shinyUI(pageWithSidebar(

############### body ################

#----- title of my shiny app -----------
  headerPanel("Illustating markup"),
#----- display of the sidebar panel-----
  sidebarPanel(
  h1('Sidebar text'), # comma after every h label
  h1('H1 text'), 
  h2('H2 text'), 
  h3('H3 text'), 
  h4('H4 text') # no comma here
  ),
#----- display of the main panel--------
  mainPanel(
  h3('Main Panel text') ,
  code('some code'),
  p('some ordinary text')
  )
#---------------------------------------
##########################################

))
```

2. server. R

This is same as example 1

```r
library(shiny)
shinyServer(
 we need the following function to be included
  function(input, output){ 
  }
)
```
 3. Example 2: output

![Example 2:output](figures/ex2.png)  

library(ggplot2)
library(shiny)
library(plotly)
library(stringr)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(randomForest)
library(pROC)
library(ggmosaic)
library(devtools)
library(PPtreeViz)
library(devtools)
library(graph)
library(PairViz)
library(PPforest)
source("shinyplots.R")

data.sources = list.files(pattern="*.Rdata")
for(i in 1:length(data.sources)){
  load( data.sources[i] )
}

ppf <- ppf_fish
imp <- impo_fish
rf <- rf_fish
impoaver <- impoaver_fish
impoinfo <- impoinfo_fish


#############################
#         SERVER            #
#############################
shinyServer( function(input, output){
  
  #Define reactive values for MDS plot and vote matrix plots
  rv <- reactiveValues( data = data.frame(
    MDS1 = rf.mds$points[,1], MDS2 = rf.mds$points[,2],
    Class = ppf$train[, colcl],ids = 1:nrow(rf.mds$points),
    fill = logical(nrow(ppf$train ) ),proj.vote =
      prvote,
    vote = ppf$votes, pred = ppf$prediction.oob, scale.dat ) )
  
  #function to update selected elements in each plot
  updateRV <- function(selected) {
    fill <- logical(length(rv$data$fill))
    fill[selected] <- TRUE
    rv$data$fill <- fill
  }
  
  #custom events when a user interacts directly with a graph, use event_data to access the data in a selection and
  #update the selection, id will be the key for the selections
  observeEvent(event_data("plotly_selected"),{
    selected <- rv$data$ids %in% event_data("plotly_selected")$key
    updateRV(selected)
  })
  
  
  #custom events when a user interacts directly with a graph, use event_data to access the data in a click and
  #update the click selection, id will be the key for the selections.
  observeEvent(event_data("plotly_click"),{
    k <- event_data("plotly_click")$key
    if (any(k %in% unique(rv$data$ids))){
      selected <- rv$data$ids %in% k
    }
    
    updateRV(selected)
  })
  
 

  rv3 <- reactiveValues(bestnode = data.frame(ids = 1:(nrow(bestnode) / sum(length(
    unique( bestnode$node )))), bestnode %>% 
      dplyr::filter(node == 1), ooberr = ppf$oob.error.tree), fill = FALSE)
  
  
  updateRV3 <- function(selectedbest) {
    fill <- logical(length(rv3$bestnode$fill))
    fill[selectedbest] <- TRUE
    rv3$bestnode$fill <- fill
  }
  
  observeEvent(event_data("plotly_click", source = "dibu"), {
    k <- event_data("plotly_click", source = "dibu")$key
    if (any(k %in% unique(rv3$bestnode$ids))) {
      selectedbest <- rv3$bestnode$ids %in% k
    }
    updateRV3(selectedbest)
  })
  

  
  selectedparopt <- reactive({
    input$paropt
  })
  
  
  
  
  ############################
  #           TAB1           #
  ############################
  #proximity and vote matrix individual level comparison

  
  
  ##Parallel plot with standarized data

  output$parallel <- renderPlotly({
    yy <- rv$data$ids[rv$data$fill]
    
    if(selectedparopt()%in%"Parallel"){
    p <- ggplot(scale.dat.melt, aes(x = Variables, y = Value,
                                    group = ids, key = ids, colour = Class, var = var)) +
      geom_line(alpha = 0.3) + scale_x_discrete(limits = levels(as.factor(scale.dat.melt$var)), expand = c(0.01,0.01)) +
      ggtitle("Data parallel plot ") + theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5), aspect.ratio = 1) +
      scale_colour_brewer(type = "qual", palette = "Dark2")

    if (length(yy) > 0) {
      dat <-   scale.dat.melt %>% dplyr::filter(ids %in% yy)
      p <- ggplot(scale.dat.melt, aes(x = Variables, y = Value, 
                                      group = ids, key = ids, color = Class, var = var)) +
        geom_line(alpha = 0.1) + scale_x_discrete(limits = levels(as.factor(scale.dat.melt$var)), expand = c(0.01,0.01)) + 
        ggtitle("Data parallel plot") + theme(legend.position = "none",axis.text.x  = element_text(angle = 90, vjust = 0.5), aspect.ratio = 1) + 
        scale_colour_brewer(type = "qual",palette = "Dark2")
      
      p <- p + geom_line(data = dat) 
      }
    }else{
      p <-  scale.dat.melt2 %>% arrange(Variables) %>%ggplot( aes(x = Variables, y = Value,
                                                            group = ids, key = ids, colour = Class, var = var)) +
        geom_line(alpha = 0.3) + scale_x_discrete(limits = levels(as.factor(scale.dat.melt2$var))[sample(length(levels(as.factor(scale.dat.melt2$var))))], expand = c(0.01,0.01)) +
        ggtitle("Data parallel plot ") + theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5)) +
        scale_colour_brewer(type = "qual", palette = "Dark2")
    
      
      if (length(yy) > 0) {
        dat <-   scale.dat.melt2 %>% arrange(Variables)%>% dplyr::filter(ids %in% yy)
        p <- scale.dat.melt2 %>% arrange(Variables)%>%ggplot( aes(x = Variables, y = Value,
                                                                  group = ids, key = ids, colour = Class, var = var)) +
          geom_line(alpha = 0.3) + scale_x_discrete(limits = levels(as.factor(scale.dat.melt2$var))[sample(length(levels(as.factor(scale.dat.melt2$var))))], expand = c(0.01,0.01)) +
          ggtitle("Data parallel plot ") + theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5)) +
          scale_colour_brewer(type = "qual", palette = "Dark2")
        
        
        p <- p + geom_line(data = dat) 
      }
    }
    ggplotly(p,tooltip = c("var","colour","y","key")) %>% layout(dragmode = "select")
  })
  
  #MDS plot
  output$mdsplot <- renderPlotly({
    yy <- rv$data$ids[rv$data$fill]
    
    p <- ggplot(data = rv$data, aes(x = MDS1, y = MDS2, 
                                    colour = Class, key = ids)) + 
      geom_point(size = I(3),alpha = .5)  + theme(legend.position = "none", legend.text = element_text(angle = 90), legend.key = element_blank(), aspect.ratio =
                                                    1)  + labs(y = "MDS 2", x = "MDS 1", title = "Multidimensional Scaling") +
      scale_colour_brewer(type = "qual",palette = "Dark2")
    
    if (length(yy) > 0) {
      dat <- rv$data %>% dplyr::filter(ids %in% yy)
      p <- ggplot(data = rv$data, aes(x = MDS1, y = MDS2, color = Class, key = ids)) + 
        geom_point( size = I(3), alpha = .1) + theme(legend.position = "none", legend.text = element_text(angle = 90), legend.key = element_blank(), aspect.ratio =1) +
        labs(y = "MDS 2", x = "MDS 1", title = "Multidimensional Scaling")  + 
        scale_colour_brewer(type =   "qual",palette = "Dark2")
      
      p <- p + geom_point(data = dat, size =  I(3))
      
    }
    ggplotly(p,tooltip = c("colour","x","y","key")) %>% layout(dragmode = "select")
    
  })
  
  #Jittered Side-by-side probability plot
  output$sideplot <- renderPlotly({
    
    yy <- rv$data$ids[rv$data$fill]
    
    reddat <- rv$data %>% 
      select(ids, Class, starts_with("vote"), pred) 
    colnames(reddat) <- colnames( reddat) %>% stringr::str_replace("vote.","")
    
    sidepl <- reddat %>% gather(classpred, Probability, -pred, -ids, -Class)
    
    
    p <- ggplot(data = sidepl, aes(classpred, Probability, colour = Class, key = ids)) + 
      geom_jitter(height = 0, size = I(3), alpha = .5) +
      theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
      labs(x = "Class", title = "Side by side plot", y = "Proportion") + scale_colour_brewer(type = "qual", palette = "Dark2")
    
    if (length(yy) > 0) {
      dat <- sidepl %>% dplyr::filter(!ids %in% yy)
      dat_fil <- sidepl %>% dplyr::filter(ids %in% yy)
      p <- ggplot(data = dat, aes(classpred, Probability, colour = Class, key = ids)) + 
        geom_jitter(height = 0, size = I(3), alpha = .1) +
        theme(legend.position = "none",axis.text.x  = element_text(angle = 45, vjust = 0.5) , aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot",  y = "Proportion") + scale_colour_brewer(type = "qual", palette = "Dark2")
      
      p <- p + geom_jitter( height = 0, data = dat_fil, size =I(3)) 
    }
    
    ggplotly(p,tooltip = c("colour","y","key")) %>% layout(dragmode = "select")
    
    
  })
  
  
  output$ternaryplot <- renderPlotly({
    yy <- rv$data$ids[rv$data$fill]
    
    
    if(n.class == 3){
      s <- simplex(2)
      pts <- data.frame(s$points)
      
      edg <- data.frame(x1=pts[,1][s$edges[,1]], x2=pts[,1][s$edg[,2]],
                        y1=pts[,2][s$edg[,1]], y2=pts[,2][s$edg[,2]])
      
      
      p <- ggplot(data = dat3.empt, aes(
        x = proj.vote.x, y = proj.vote.x.1, colour = Class, key = ids
      )) +  geom_blank() + geom_point(data = filter(dat3.empt, rep == 2), size = I(3), alpha = .5) + ylab("") +
        xlab("") + geom_segment(data = edg, aes(x = x1, xend = x2, y = y1, yend = y2, key = NULL), color = "black") +
      theme(legend.position = "none" , aspect.ratio = 1) + ggtitle("Vote matrix ") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +labs(x = "T1", y ="T2") 
        
      if (length(yy) > 0) {
        dat33 <- dat3.empt %>% dplyr::filter(ids %in% yy)
        s <- simplex(2)
        pts <- data.frame(s$points)
        
        edg <- data.frame(x1=pts[,1][s$edges[,1]], x2=pts[,1][s$edg[,2]],
                          y1=pts[,2][s$edg[,1]], y2=pts[,2][s$edg[,2]])
        
        
        p <- ggplot(data = dat3.empt, aes( x = proj.vote.x, y = proj.vote.x.1, 
                                           colour = Class, key = ids)) + 
          geom_blank()  + geom_point(data = filter(dat3.empt, rep == 2), size = I(3), alpha = .1) + ylab("") + xlab("") + 
          geom_segment(data = edg, aes(x = x1, xend = x2, y = y1, yend = y2, key = NULL), color = "black") +
          theme(legend.position = "none", aspect.ratio = 1) + ggtitle("Vote matrix ternary plot") +
          scale_colour_brewer(type = "qual",palette = "Dark2")
        p <- p + geom_point(data = filter(dat33, rep == 2), size = I(3)) +labs(x = "T1", y ="T2") 
      }
      ggplotly(p,tooltip = c("colour","x","y","key")) %>% layout(dragmode = "select")
      
    }else{
      
      t1 <-ternaryshell(gg1, ppf, length(unique(ppf$train[,ppf$class.var]))-1, 1, 2)
      t2 <- ternaryshell(gg1, ppf, length(unique(ppf$train[,ppf$class.var]))-1, 1, 3)
      t3 <- ternaryshell(gg1, ppf, length(unique(ppf$train[,ppf$class.var]))-1, 2, 3)
      
      if (length(yy) > 0) {
        dat33 <- dat3 %>% dplyr::filter(ids %in% yy)
        #gg1 <- ternarydata(ppf, 1, 2, 3)
        gg2 <-  gg1 %>%  dplyr::filter(ids %in%yy)
        
        t1 <- ternaryshell2(gg1, gg2,ppf, length(unique(ppf$train[,ppf$class.var]))-1, 1, 2)
        t2 <- ternaryshell2(gg1, gg2, ppf, length(unique(ppf$train[,ppf$class.var]))-1, 1, 3)
        t3 <- ternaryshell2(gg1, gg2, ppf, length(unique(ppf$train[,ppf$class.var]))-1, 2, 3)
      
      }
      subplot(ggplotly(t1,tooltip = c("colour","x","y","key")), ggplotly(t2,tooltip = c("colour","x","y","key"))
              ,ggplotly(t3,tooltip = c("colour","x","y","key")))
    }
    
    
  
  })


  ################################
  #             Tab 2            #
  ################################
  
  selectedDatanode <- reactive({
    input$goButton2
    isolate(input$xnode)
  
    })

  #Importance
  output$importancetree <- renderPlotly({
    
 
    if(length(unique(impo.pl$node)) > 3){
tr<- 494
       # impo.pl <- impo.pl %>% filter(node == selectedDatanode()) 
      impo.pl <- impo.pl %>% filter(node == node[1:3]) 
      
    p <- ggplot(filter(impo.pl,!ids %in% tr), aes( x = Variables, y = Abs.importance, group = ids,
                                                  key = ids, var = var)) +
      geom_jitter(height = 0, size = I(2), alpha = 0.3) + facet_grid(node ~ .) +
      scale_x_discrete(limits = levels(as.factor(impo.pl$var) ) ) + ggtitle("Importance variable for each tree") +
      theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5 ) )

    p <- p + geom_jitter( data = filter(impo.pl, ids %in% tr), color = "red",height = 0) +
      facet_grid(node ~ .) + scale_x_discrete(limits = levels(as.factor(impo.pl$var) ) ) +
      ggtitle("Importance variable for each tree") +
      theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5), aspect.ratio = 1)

    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]
   
    if (length(yy2) > 0) {
      
      node <- ppf$output.trees[[yy2]]$Tree.Struct[ppf$output.trees[[yy2]]$Tree.Struct[,"Index"]!=0, "id"]
      bnf <- function(x) {
        bn <- abs(x$projbest.node)
        bn[bn == 0] <- NA
        data.frame(node = node, bn)
      }
      bestnode <- ppf[["output.trees"]] %>%  lapply(bnf) %>% bind_rows()
      
      
      colnames(bestnode)[-1] <- colnames(ppf$train[ , -which(colnames(ppf$train)==ppf$class.var)])
      bestnode$node <- as.factor(bestnode$node)
      
      impo.pl <- bestnode %>% 
        mutate(ids = rep(1:ppf$n.tree,each = nrow(ppf[[8]][[yy2]]$projbest.node) ) ) %>% 
        gather(var, value, -ids, -node) 
      impo.pl$Variables <- as.numeric(as.factor(impo.pl$var))
      impo.pl$Abs.importance <- round(impo.pl$value,2)
      
      dat <-   impo.pl %>% dplyr::filter(!ids %in% yy2)
      dat2 <-   impo.pl %>% dplyr::filter(ids %in% yy2)
      p <- ggplot(dat, aes(x = Variables , y = Abs.importance, key = ids,var.= var)) +
        geom_jitter(height = 0, size = I(2),alpha = 0.3) + facet_grid(node[1:3] ~ .) +
        scale_x_discrete(limits = levels(as.factor(impo.pl$var) ) ) + ggtitle("Importance variable for each tree") +
        theme(legend.position = "none", axis.text.x  = element_text(angle = 90, vjust = 0.5), aspect.ratio = 1 )

      p <-  p  + facet_grid(node[1:3] ~ .) + geom_jitter(height = 0, data = dat2,  color = "red" )

    }
  }
    ggplotly(p,tooltip = c("var","y","key"), source = "dibu")
  })

  #Density
  output$plotdensity <- renderPlotly({
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]

    if (length(yy2) > 0) {

      PPtree_dens(ppf, yy2) %>% layout(dragmode = "select")
    }else{
      PPtree_dens(ppf, tr) %>% layout(dragmode = "select")

    }

  })

  #Tree structure with PPtreeViz
  output$plottree <- renderPlot({
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]

    if (length(yy2) > 0) {
      plot(ppf[[8]][[yy2]])
    }else{
      plot(ppf[[8]][[tr]])

    }
  })

  #Boxplot error tree
  output$boxtreeerror <- renderPlotly({
  
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]

    if (length(yy2) > 0) {
      dat2 <-   rv3$bestnode %>% dplyr::filter(ids %in% yy2)
    
      error <- round(dat2$ooberr, 3)
      p <-
        ggplot(error.tree, aes(
          x = trees, y = OOB.error.tree, fill = trees, key = ids
        )) + geom_boxplot() + scale_fill_manual(values = "#ffffff") +
        guides(fill = FALSE) + labs(x = "", y = "OOB error trees") +
        coord_flip()  +   geom_point(
          aes(y = error),key=yy2,alpha = 0.1,size = I(3),color = I("red")
        ) + geom_point(data = error.tree, aes(y = OOB.error.tree)
                       , alpha = 0.8, size = I(1), color = I("black"))    
     
      ggplotly(p,tooltip =  c("y"),source = "dibu") %>% layout(dragmode = "select")
    }else{
      error <- round(ppf$oob.error.tree[tr], 3)
      p <- ggplot(error.tree, aes(x = trees, y = OOB.error.tree,fill = trees,  key = ids)) + geom_boxplot() +
        scale_fill_manual(values = "#ffffff") +
        guides(fill = FALSE) +labs(x = "", y = "OOB error trees") +
        coord_flip()  +   geom_point(
          aes(y = error), alpha = 0.1, size = I(3), color = I("red") ) + geom_point(aes(y = OOB.error.tree)
          , alpha = 0.8, size = I(1),color = I("black"))    

      ggplotly(p,tooltip = c("y"),source = "dibu") %>% layout(dragmode = "select")

    }

  })

  #Mosaic plot
  output$plotmosaic <- renderPlotly({
    yy1 <- as.numeric(rv3$bestnode$ids[rv3$bestnode$fill])
    yy2 <- yy1[!is.na(yy1)]

    if (length(yy2) > 0) {
      PPtree_mosaic(ppf, yy2) %>% layout(dragmode = "select")
    }else{
      PPtree_mosaic(ppf, tr) %>% layout(dragmode = "select")

    }

  })

  ################################
  #             Tab 3            #
  ################################
  #

  selectedData <- reactive({
    input$goButton
    isolate(input$xcol)
  })
  

  selectedDataimpo <- reactive({
   input$importance
  })

  # Side by side PPRF
  output$siderfpp <- renderPlotly({

    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {
      p <- ggplot(data = dat.sidepp.pl, aes(x = Classvote, y = Probability, colour = Class, key = ids) ) +
        geom_jitter(height = 0, size = I(3), alpha = .5) +
        theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot PPforest",  y = "Proportion") + scale_colour_brewer(type = "qual",palette =
                                                                                        "Dark2")
    }
    if (length(selectedData( ) ) != length(unique(ppf$train[,ppf$class.var] ))) {
      dat <- dat.sidepp.pl %>% dplyr::filter(!Class %in% selectedData())
      dat_fil <- dat.sidepp.pl %>% dplyr::filter(Class %in% selectedData())

      p <- ggplot(data = dat, aes(Classvote, Probability, colour = Class, key = ids)) +
        geom_jitter(height = 0, size = I(3), alpha = .1) +
        theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot random forest",  y = "Proportion") + scale_colour_brewer(type = "qual",palette =
                                                                                             "Dark2")
      p <- p + geom_jitter( height = 0, data = dat_fil, alpha = .5, size = I(3))

    }
    ggplotly(p,tooltip = c("colour", "y", "key"))

  })


  # Side by side RF
  output$siderf <- renderPlotly({

    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {
      p <- ggplot(data = dat.side.pl, aes(Classvote, Probability, colour = Class, key = ids) ) +
        geom_jitter(height = 0, size = I(3), alpha = .5) +
        theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot random forest",  y = "Proportion") + scale_colour_brewer(type = "qual",palette =
                                                                                             "Dark2")

    }
    if (length(selectedData()) != length(unique(ppf$train[,ppf$class.var]))) {
      dat <- dat.side.pl %>% dplyr::filter(!Class %in% selectedData())
      dat_fil <- dat.side.pl %>% dplyr::filter(Class %in% selectedData())
      p <-
        ggplot(data = dat, aes(Classvote, Probability, colour = Class, key = ids)) +
        geom_jitter(height = 0, size = I(3), alpha = .1) +
        theme(legend.position = "none", axis.text.x  = element_text(angle = 45, vjust = 0.5), aspect.ratio = 1) +
        labs(x = "Class", title = "Side by side plot random forest", y = "Proportion")  + scale_colour_brewer(type = "qual",palette =
                                                                                              "Dark2")
      p <-
        p + geom_jitter( height = 0,data = dat_fil, size = I(3), alpha = .5
        )
    }

    ggplotly(p,tooltip = c("colour","y","key"))


  })

  # ROC RF and PP

  output$plot_rocpp <- renderPlotly({

    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {

      p <- ggplot(data = dat.rocpprf, aes(x = specificities, y = sensitivities,colour = Classvote) ) +
        geom_path(alpha = 0.5, size = 1.2)+ scale_x_reverse() +
        geom_abline(intercept = 1, slope = 1, color = 'grey') +
        labs(x = 'Specificity', y = 'Sensitivity', title ="ROC curve PPforest") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +  theme(legend.position = "none", aspect.ratio = 1)
    }

    if (length(selectedData()) != length(unique(ppf$train[,ppf$class.var]))) {
      dat <- dat.rocpprf %>% dplyr::filter(!Classvote %in% selectedData())
      dat_fil <- dat.rocpprf %>% dplyr::filter(Classvote %in% selectedData())

      p <- dat %>% ggplot( aes(y = sensitivities, x = specificities, colour = Classvote)) +
        geom_path( alpha = 0.1, size = 1.2 ) + scale_x_reverse() + labs(x = 'Specificity', y = 'Sensitivity', title ="ROC curve PPforest") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +  theme(legend.position = "none", aspect.ratio = 1)

      p <- p +  geom_path(data = dat_fil,size = 1.2 )
    }
    ggplotly(p, tooltip = c("colour","x","y"))

  })


  output$plot_rocrf <- renderPlotly({

    if (length(selectedData()) ==  length(unique(ppf$train[,ppf$class.var]))) {
      p <- dat.rocrf %>% ggplot(aes( y = sensitivities,x = specificities, colour = Classvote
      )) + geom_path(alpha = 0.5, size=1.2)  + scale_x_reverse() +
        geom_abline(intercept = 1, slope = 1, color = 'grey')+
        labs(x = 'Specificity', y = 'Sensitivity', title ="ROC curve random forest") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +  theme(legend.position = "none", aspect.ratio = 1)
    }

    if (length(selectedData()) !=  length(unique(ppf$train[,ppf$class.var]))) {
      dat <- dat.rocrf %>% dplyr::filter(!Classvote %in% selectedData())
      dat_fil <- dat.rocrf %>% dplyr::filter(Classvote %in% selectedData())

      p <- dat %>% ggplot(aes(y = sensitivities, x = specificities, colour = Classvote)) +
        geom_path(alpha = 0.1, size = 1.2)  + scale_x_reverse() +
        geom_abline(intercept = 1, slope = 1, color='grey')+
        labs(x = 'Specificity', y = 'Sensitivity', title ="ROC curve random forest") +
        scale_colour_brewer(type = "qual",palette = "Dark2") +  theme(legend.position = "none", aspect.ratio = 1)

      p <- p +  geom_path(data = dat_fil,size = 1.2 )
    }
    ggplotly(p, tooltip = c("colour","x","y"))

  })

  #Plot OOB error
  output$plot_oobpp <- renderPlotly({

    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {


      p1 <- oob.pl %>% ggplot(aes( x = tree.id, y = OOB.error , colour = Class)) +
        geom_point(alpha = .5) + geom_line(size = I(0.5), alpha = .5) + labs(y = "OOB error rate",
                                                                             x = "Number of trees", title = "Cumulative OOB error") + ylim( c(0,1) ) +
        theme(legend.position = "none", aspect.ratio = 1) + scale_color_manual(values = myColors)
    }

    if (length(selectedData()) != length(unique(ppf$train[,ppf$class.var]))) {
      dat <- oob.pl %>% dplyr::filter(!Class %in% selectedData())
      dat_fil <-oob.pl %>% dplyr::filter(Class %in% selectedData())

      p1 <- dat %>% ggplot(aes( x = tree.id, y = OOB.error , colour = Class) ) +
        geom_point(alpha = .1) + geom_line(size = I(0.1),alpha = .1) +
        labs(y = "OOB error rate", x = "Number of trees", title = "Cumulative OOB error") + ylim(c(0,1)) +
        theme(legend.position = "none", aspect.ratio = 1) + scale_color_manual(values = myColors)

      p1 <- p1 + geom_point(data = dat_fil, alpha = .5, aes(x = tree.id, y = OOB.error, colour = Class)) + geom_line(data = dat_fil,alpha = .5)

    }

    plotly::ggplotly(p1,tooltip = c("colour","y","x"))

  })


  output$plot_oobrf <- renderPlotly({


    if (length(selectedData()) == length(unique(ppf$train[,ppf$class.var]))) {
      p <- dat_rf %>% mutate(Class = as.factor(Class)) %>%
        ggplot(aes(x = trees, y = OOB,colour = Class)) +
        geom_point(alpha = .5) + geom_line(alpha = .5) +  scale_color_manual(values = myColors) +
        labs(y = "OOB error rate", x = "Number of trees", title = "Cumulative OOB error") + ylim(c(0,1)) +
        theme(legend.position = "none", aspect.ratio = 1) + scale_x_continuous(name = "Number of trees")

    }
    if (length(selectedData()) != length(unique(ppf$train[,ppf$class.var]))) {
      dat <- dat_rf %>% dplyr::filter(!Class %in% selectedData())
      dat_fil <- dat_rf %>% dplyr::filter(Class %in% selectedData())

      p <- dat %>% mutate(Class = as.factor(Class)) %>%
        ggplot(aes(x = trees, y = OOB,colour = Class)) +
        geom_point(alpha = .1) + geom_line(alpha = .1) +  scale_color_manual(values = myColors) +
        labs(y = "OOB error rate", x = "Number of trees", title = "Cumulative OOB error") + ylim(c(0,1)) + theme(legend.position = "none", aspect.ratio = 1)

      p <- p + geom_point(data = dat_fil, aes(x = trees, y = OOB, colour = Class), alpha = .5) +
        geom_line(data = dat_fil, alpha = .5 )
    }

    ggplotly(p, tooltip = c("colour","y","x"))

  })



  #Confussion matrix tables PP and RF

  output$tablepp <- renderTable({
    ppf$confusion
  }, caption = "Confusion Matrix PPforest",caption.placement = getOption("xtable.caption.placement", "top"))


  output$tablerf <- renderTable({
    rf$confusion
  }, caption = "Confusion Matrix randomForest",caption.placement = getOption("xtable.caption.placement", "top"))


  output$plot_impopp <- renderPlotly({
  
    
    if(selectedDataimpo()%in%"PPforest impo"){
    
      if(nrow(impoinfo ) > 20){
      impoinfo <- impoinfo[1:20,]
        }
    p <- ggplot2::ggplot(impoinfo, ggplot2::aes(x = mean, y = variable)) +
      ggplot2::geom_point() + ggplot2::theme(aspect.ratio=1) + labs(x = "Importance",
                                                                    y = "")
    }
        if(selectedDataimpo()%in%"PPforest avgtr"){
          
         if(nrow(impoaver ) > 20){
         impoaver <- impoaver[1:20,]
         }
         
           p <- ggplot2::ggplot(impoaver, ggplot2::aes(x = mean, y = variable)) +
             ggplot2::geom_point() + ggplot2::theme(aspect.ratio=1) + labs(x = "Importance",
                                                                           y = "")
      }else{

    if(nrow(imp ) > 20){
    imp <- imp[1:20,]
      }
     p <- ggplot(data = imp, aes(imp2,nm) ) + geom_point() + labs(x = "Importance",
                                                                         y = "")
    }
    
                                                            
ggplotly(p)

  })


  output$plot_imporf <- renderPlotly({
    if(nrow(imp )>20){
      p <- ggplot(data = imp.pl[1:20,], aes(imp,nm) ) + geom_point() +
        labs(x = "Importance", y = "")
    }else{
      p <- ggplot(data = imp.pl, aes(imp,nm) ) + geom_point() +
        labs(x = "Importance", y = "") }
    ggplotly(p)

  })

 }

)
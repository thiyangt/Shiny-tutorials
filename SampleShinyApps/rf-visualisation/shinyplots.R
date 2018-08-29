

#Data transformation density and mosaic
dat_dens <- function(PPclassOBJ, node.id, Rule, legend = TRUE, std = TRUE, 
                     image = FALSE, diff.prop = 0.2,c1=FALSE) {
  
  searchGroup <- function(node.id, TS, gName) {
    flag <- TRUE
    sel.id <- TS[node.id, 2:3]
    LR.id <- c(TRUE, FALSE)
    sel.group <- NULL
    i <- 1
    while ((sel.id[i] != 0) && (i < length(sel.id))) {
      if (TS[sel.id[i], 2] != 0) {
        sel.id <- c(sel.id, TS[sel.id[i], 2:3])
        if (LR.id[i]) 
          LR.id <- c(LR.id, c(TRUE, TRUE))
        else LR.id <- c(LR.id, c(FALSE, FALSE))
      }
      if (TS[sel.id[i + 1], 2] != 0) {
        sel.id <- c(sel.id, TS[sel.id[i + 1], 2:3])
        if (LR.id[i + 1]) 
          LR.id <- c(LR.id, c(TRUE, TRUE))
        else LR.id <- c(LR.id, c(FALSE, FALSE))
      }
      i <- i + 2
    }
    sel.Name <- TS[sel.id[which(TS[sel.id, 2] == 0)], 3]
    selName <- sort(gName[sel.Name])
    L.list <- sort(gName[sel.Name[LR.id[which(TS[sel.id, 
                                                 2] == 0)]]])
    R.list <- sort(gName[sel.Name[!LR.id[which(TS[sel.id, 
                                                  2] == 0)]]])
    return(list(selName = selName, Llist = L.list, Rlist = R.list))
  }
  
  TS <- PPclassOBJ$Tree.Struct
  Alpha <- PPclassOBJ$projbest.node
  cut.off <- PPclassOBJ$splitCutoff.node
  origdata <- PPclassOBJ$origdata
  origclass <- PPclassOBJ$origclass
  p <- ncol(origdata)
  gName <- names(table(origclass))
  
  if (TS[node.id, 2] != 0) {
    SG.result <- searchGroup(node.id, TS, gName)
    selG <- SG.result$selName
    selL <- SG.result$Llist
    selR <- SG.result$Rlist
    sel.id <- NULL
    LR.class <- NULL
    for (i in 1:length(selG)) {
      sel.id <- c(sel.id, which(origclass == selG[i]))
      LR.class <- c(LR.class, rep(ifelse(sum(selL == selG[i]) != 
                                           0, "L", "R"), length(which(origclass == selG[i]))))
    }
    proj.data <- c(as.matrix(origdata) %*% as.matrix(Alpha[TS[node.id, 
                                                              4], ]))[sel.id]
    
    
    proj.class <- origclass[sel.id]
    plot.data <- data.frame(proj.data = proj.data, origclass = proj.class,
                            cut = cut.off[TS[node.id, 4], Rule], node.id =node.id, LR.class,Dir=as.factor(proj.data>cut.off[TS[node.id, 4], Rule]))
    colnames(plot.data)[2]<-"Class"
    
    
    plot.data
    
  }
}

PPtree_dens <- function(ppf, tr, nodes = NULL) {
  
  if(length(ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1])<3){
    ns =length(ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1])
  }else{
    ns=3
  }
  if(is.null(nodes)){
    nodes <- ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1][1:ns]
  }
  nn <- data.frame(nn = nodes)
  densf <- function(x) {
    dat_dens(PPclassOBJ = ppf[["output.trees"]][[tr]],
             node.id = x,
             Rule = 1)
    
  }
  
  dat_pl <- apply(nn, 1, densf)  %>%  lapply(data.frame) %>% bind_rows()
  
  myColors <- brewer.pal(dim(unique(ppf$train[ppf$class.var]))[1], "Dark2")
  names(myColors) <- levels(ppf$train[ppf$class.var][, 1])
  dat_pl$Class <- as.factor(dat_pl$Class)
  
  if(is.factor(ppf$train[ppf$class.var][, 1])){
    levels(dat_pl$Class) <-  levels(ppf$train[ppf$class.var][, 1])
    
  }else{
    levels(dat_pl$Class) <-  levels(as.factor(ppf$train[ppf$class.var][, 1]))
  }
  
  p1 <- dat_pl %>% filter(node.id %in%nodes) %>%
    ggplot( aes(  x = proj.data, group = Class, fill = Class ) ) + 
    geom_density(alpha = .5) + facet_grid(~ node.id, scales = 'free') + 
    scale_fill_manual(values = myColors) + geom_vline(aes(xintercept = cut),
                                                      linetype = "dashed",
                                                      color = 2) + xlab("")
  
  
  
  p1 <-  p1 + theme(legend.position = "none",aspect.ratio = 1)
  ggplotly(p1, tooltip = c("fill", "x"))
  
}

#Mosaic plot Tab 2
PPtree_mosaic <- function(ppf,tr, nodes = NULL){
  if(length(ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1])<3){
    ns =length(ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1])
  }else{
    ns=3
  }
  
  if(is.null(nodes)){
    nodes <- ppf[[8]][[tr]]$Tree.Struct[ppf[[8]][[tr]]$Tree.Struct[,4]!=0,1][1:ns]
  }
  nn <- data.frame(nn = nodes)
  
  densf <- function(x){
    dat_dens(PPclassOBJ=ppf[[8]][[tr]],node.id=x,Rule=1)
  }
  
  dat_pl<- apply(nn, 1, densf)  %>%  lapply(data.frame) %>%bind_rows()
  
  levels(dat_pl$Dir)<-c("Left", "Right")
  myColors <- brewer.pal(length( unique( ppf[[8]][[tr]]$origclass ) ), "Dark2")
  names(myColors) <- levels(dat_pl$Class)
  
  dat_mosaic <- data.frame( with(dat_pl, table(Class, Dir,node.id) ) )
  
  
  p1 <- dat_mosaic %>% filter(node.id %in%nodes) %>% ggplot() + 
    geom_mosaic( aes(weight = Freq, x = product(Class,Dir) ,fill = Class))+facet_grid(~node.id)+
    scale_fill_manual(values = myColors) + theme(legend.position="none",axis.text.x  = element_text(angle=90, vjust=0.5),aspect.ratio = 1) +xlab("Class")
  
  ggplotly(p1)
  
}

##ROC curve tab 3
rocky <- function(response, predictor){
  aux <- roc(response, predictor)
  sensi <- aux$sensitivities
  speci <- aux$specificities
  tresh <- aux$thresholds
  auc <- aux$auc
  data.frame(
    sensitivities = sensi, specificities = speci, auc = rep(auc, length(sensi))
  )
}

#plot oob error
ppf_oob_error <- function(ppf, nsplit1) {
  ntree <- NULL
  value <- NULL
  variable <- NULL
  error.cum <- function(ppf, m) {
    l.train <- 1:nrow(ppf$train)
    index2 <- lapply(as.numeric(attributes(ppf$boot.samp)$names[1:m]), function(x)
      x + 1)
    
    
    oob.obs <- index2 %>%  lapply(function(x)
      data.frame(obs=!l.train %in% x)) %>% bind_cols() %>%t()
    pred.mtree <- ppf$vote.mat[1:m,]
    
    
    
    oob.pred <-
      sapply(
        X = 1:nrow(ppf$train), FUN = function(i) {
          t1 <- table(pred.mtree[oob.obs[, i] == TRUE, i])
          names(t1)[which.max(t1)]
        }
      )
    
    
    oob.mat <- sapply(
      X = 1:nrow(ppf$train), FUN = function(i) {
        table(pred.mtree[oob.obs[, i] == TRUE, i])
      }
    )
    
    aux <- unlist(lapply(oob.pred, is.null))
    oob.all <-
      1 - sum(diag(table(unlist(oob.pred[!aux]), ppf$train[!aux, 1]))) / length(ppf$train[!aux, 1])
    tab.err <- table(unlist(oob.pred[!aux]), ppf$train[!aux, 1])
    oob.class <- 1 - diag(tab.err) / apply(tab.err, 2, sum)
    c(oob.all, oob.class)
  }
  
  
  mm <- data.frame(m = round(seq(
    2, round(ppf$n.tree),nsplit1)))
  
  errcfun <- function(x){
    error.cum(ppf,x)
  }
  
  oob.err.sp <- data.frame(mm, apply(mm, 1,errcfun)  %>% t() )
  
  names(oob.err.sp)[1:2] <- c("tree.id", "All")
  
  oob.pl <- oob.err.sp %>% gather(variable, value, -tree.id)
  
  
  colnames(oob.pl)[2:3] <- c("Class", "OOB.error")
  oob.pl
}


load("impo_fish.Rdata")
load("ppf_fish.Rdata")
load("rf_fish.Rdata")

# cosas que tuve que agregar porque no encuentra el objeto
tr <- 494
ppf <- ppf_fish
rf <- rf_fish# rf() es una funcion que existe, para obtener muestras de x ~ F
num = FALSE
imp<-impo_fish

# daba errror en el server, class no esta definido, lo puse yo a prepo
# colcl se necesita para que corra shinyplots, no puede estar definido en server supongo
class <- colnames(ppf$train)[1]
colcl <- which(colnames(ppf$train) %in% class)


# el projt se usaba antes de ser creado
f.helmert <- function(d)
{
  helmert <- rep(1 / sqrt(d), d)
  for (i in 1:(d - 1))
  {
    x <- rep(1 / sqrt(i * (i + 1)), i)
    x <- c(x,-i / sqrt(i * (i + 1)))
    x <- c(x, rep(0, d - i - 1))
    helmert <- rbind(helmert, x)
  }
  return(helmert)
}

node <- ppf$output.trees[[tr]]$Tree.Struct[ppf$output.trees[[tr]]$Tree.Struct[,"Index"]!=0, "id"]
bnf <- function(x) {
  bn <- abs(x$projbest.node)
  bn[bn == 0] <- NA
  data.frame(node = node, bn)
}

#projected data
projct <- t(f.helmert(length(unique(ppf$train[ , ppf$class.var] )))[-1, ])



##########data str#########

if(length(levels(ppf$train[, colcl]))==2){
  prvote <-  as.matrix(ppf$votes) 
}else{
  prvote <-  as.matrix(ppf$votes) %*% projct
}

if(!is.factor(ppf$train[,colcl] )){
  ppf$train[,colcl] <- as.factor(ppf$train[,colcl])
}

n.class <- ppf$train %>% select_(ppf$class.var) %>% unique() %>% nrow()
lev <- ppf$train %>% select_(ppf$class.var) %>%   sapply(levels) %>% as.factor() 

k = 2
id <- diag(dim(ppf$train)[1])
id <- id + 1 - ppf$proximity
rf.mds <- stats::cmdscale(d = stats::as.dist(id), eig = TRUE,  k = k)
colnames(rf.mds$points) <- paste("MDS", 1:k, sep = "")
nlevs <- nlevels(ppf$train[, 1])

df <- data.frame(Class = ppf$train[, 1], rf.mds$points)


bestnode <- ppf[["output.trees"]] %>%  lapply(bnf) %>% bind_rows()


colnames(bestnode)[-1] <- colnames(ppf$train[ , -which(colnames(ppf$train)==ppf$class.var)])
bestnode$node <- as.factor(bestnode$node)

#scale data for the parallel plot tab 1 option 1
myscale <- function(x) (x - mean(x)) / sd(x)

scale.dat <- ppf$train %>% mutate_each(funs(myscale),-matches(ppf$class.var)) 
scale.dat.melt <- scale.dat %>%  mutate(ids = 1:nrow(ppf$train)) %>% gather(var,Value,- colcl,-ids, convert=TRUE)
scale.dat.melt$Variables <- as.numeric(as.factor(scale.dat.melt$var))
colnames(scale.dat.melt)[1] <- "Class"

#parallel option 2

eu <- eulerian( ncol(ppf$train) - 1)
dat.aux <- scale.dat[ , -1]
scale.dat2 <- data.frame(Type = scale.dat [ , 1], dat.aux[ , eu])

scale.dat.melt2 <- scale.dat2 %>%  mutate(ids = 1:nrow(ppf$train)) %>% gather(var,Value,-Type,-ids)
scale.dat.melt2$Variables <- as.numeric(as.factor(scale.dat.melt2$var))
colnames(scale.dat.melt2)[1] <- "Class"



###importance
#impo <- c("Permuted", "PPforest importance")
makePairs <- function(dat, id = c(a, b, c)) {
  aux <- dat[,-c(1, 2)]
  
  d <- aux[, id]
  
  grid <- expand.grid(x = id, y = id)
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(
      Class = dat[, 1],
      ids = dat[, 2],
      x = dat[, xcol+2],
      y = dat[, ycol+2],
      pair = paste(grid[i, ], collapse = '-')
      
    )
  }))
  
  all
}

#ppf PPforest object
#V1,V2,V3 select the 3 proj directions
ternarydata <- function(ppf, v1, v2, v3) {
  n.class <- ppf$train %>% select_(ppf$class.var) %>% unique() %>% nrow()
  projct <- t(f.helmert( length(unique(ppf$train[, ppf$class.var]) ) )[-1,])
  
  dat3 <-
    data.frame(
      Class = ppf$train[, ppf$class.var],
      ids = 1:nrow(ppf$train),
      proj.vote = as.matrix(ppf$votes) %*% projct
    )
  
  ##with 3 or less classes
  empt <- rep(1:nrow(dat3), 3)
  #dat3.empt <- dat3[empt, ] %>% mutate(rep = rep(1:3, each = nrow(dat3)))
  if (n.class > 3) {
    gg1 <-  makePairs(dat3, c(v1,  v2, v3))  %>% mutate(idspr =paste(ids,pair, sep=""))
  }
  
  gg1 <-  makePairs(dat3, id = c(v1, v2, v3)) %>% mutate(idspr =paste(ids,pair, sep=""))
  
  return(gg1)
}


#ternary
if(length( levels( ppf$train[, colcl] ) ) == 2){
  dat3 <- data.frame(Class = ppf$train[, colcl], ids = 1:nrow(rf.mds$points),
                     proj.vote = as.matrix(ppf$votes) )
  colnames(  dat3)[3:4] <- c( "proj.vote.x", "proj.vote.x.1")
}else{
  dat3 <- data.frame(Class = ppf$train[, colcl], ids = 1:nrow(rf.mds$points),
                     proj.vote = as.matrix(ppf$votes) %*% projct)
}



f_composition <- function(data) {
  d <- dim(data)[2]
  hm <- f.helmert(d)
  x <- data - matrix(1 / d, dim(data)[1], d)
  return((x %*% t(hm))[,-1])
}

simplex <- function(p = 3) {
  vert <- f_composition(diag(p + 1))
  colnames(vert) <- paste0("d", 1:ncol(vert))
  
  wires <-
    do.call(expand.grid, list(c(1:nrow(vert)), c(1:nrow(vert))))
  
  structure(list(points = vert,
                 edges = wires[!(wires[, 1] == wires[, 2]),]))
}


##with 3 or less classes
empt <- rep(1:nrow(dat3), 3)
dat3.empt <- dat3[empt, ] %>% mutate(rep = rep(1:3, each = nrow(dat3)))
if(n.class>3){
  gg1 <- ternarydata(ppf, v1=1, v2=2, v3=3) 
}



ternaryshell <- function(gg1, ppf, sp = length(unique(ppf$train[,ppf$class.var]))-1, dx = 1, dy = 2, v1 = 1, v2 = 2, v3 = 3){
  s <- simplex(sp)
  pts <- data.frame(s$points)
  
  edg <- data.frame(x1=pts[,dx][s$edges[,1]], x2=pts[,dx][s$edg[,2]],
                    y1=pts[,dy][s$edg[,1]], y2=pts[,dy][s$edg[,2]])
  
  p1  <- gg1 %>% filter(pair %in% paste(dx,dy, sep="-") ) %>%
    ggplot(aes(x, y, color = Class, key = ids)) +
    geom_segment(data = edg, aes(x = x1, xend = x2,
                                 y = y1, yend = y2, key = NULL), color = "black") +
    geom_point(size = I(3), alpha = .5) +
    labs(y = "",  x = "") +
    theme(legend.position = "none", aspect.ratio = 1) +
    scale_colour_brewer(type = "qual", palette = "Dark2") +
    labs(x = paste0("T",dx,""), y = paste0("T",dy,"")) +
    theme(aspect.ratio=1)
  
  p1
}

ternaryshell2 <- function(gg1, gg2, ppf, sp = length(unique(ppf$train[,ppf$class.var]))-1, dx = 1, dy = 2, v1=1, v2=2, v3=3){
  s <- simplex(sp)
  pts <- data.frame(s$points)
  
  edg <- data.frame(x1 = pts[,dx][s$edges[,1]], x2 = pts[,dx][s$edg[,2]],
                    y1 = pts[,dy][s$edg[,1]], y2 = pts[,dy][s$edg[,2]])
  
  p1  <- gg1 %>% filter(pair %in% paste(dx,dy, sep="-") ) %>%
    ggplot(aes(x, y, color = Class, key =ids)) + 
    geom_segment(data = edg, aes(x = x1, xend = x2,
                                 y = y1, yend = y2, key = NULL ), color = "black") +
    geom_point(size = I(3), alpha = .1) +
    labs(y = "",  x = "") +
    theme(legend.position = "none", aspect.ratio = 1) +
    scale_colour_brewer(type = "qual", palette = "Dark2") +
    labs(x = paste0("T",dx,""), y = paste0("T",dy,"")) +
    theme(aspect.ratio = 1)
  
  p1 +  geom_point(data = gg2 %>% filter(pair %in% paste(dx,dy, sep = "-") ), aes(x, y, color = Class, key =ids), size = I(3)) 
}



###Tab 2 data
##Importance tree

impo.pl <- bestnode %>% 
  mutate(ids = rep(1:ppf$n.tree,each = nrow(ppf[[8]][[tr]]$projbest.node) ) ) %>% 
  gather(var, value, -ids, -node) 
impo.pl$Variables <- as.numeric(as.factor(impo.pl$var))
impo.pl$Abs.importance <- round(impo.pl$value,2)


#eror tree
error.tree <- data_frame(ids = 1:ppf$n.tree, trees = "trees", OOB.error.tree = ppf$oob.error.tree[,1])


n.class <- ppf$train %>% select_(ppf$class.var) %>% unique() %>% nrow()
lev <- ppf$train %>% select_(ppf$class.var) %>%   sapply(levels) %>% as.factor() 


###Tab 3
#rf
dat.side <- data.frame(ids = 1:nrow(ppf$train), 
                       Type = ppf$train[, ppf$class.var], 
                       rf$votes, pred = rf$predicted )

if(num){
  colnames(dat.side)[(as.numeric( unique(as.factor(rf$classes) )) + 2)] <- unique(rf$classes)
}
dat.side.pl <- dat.side %>% gather(Classvote, Probability, -pred, -ids, -Type)
colnames(dat.side.pl)[2] <- "Class"

#rfpp
dat.sidepp <- data.frame( ids = 1:nrow(ppf$train), Type = ppf$train[,ppf$class.var], ppf$votes, pred = ppf$prediction.oob)
if(num){
  colnames(dat.sidepp)[(as.numeric(unique(ppf$train[,ppf$class.var]))+2)] <-levels(ppf$train[,ppf$class.var])
}
dat.sidepp.pl <- dat.sidepp %>% gather(Classvote,Probability,-pred,-ids,-Type)
colnames(dat.sidepp.pl )[2] <- "Class"

#ROC
rocf <- function(d){
  rocky(d$cond,d$Probability)
}
if(num){
dat.rocpprf <- dat.sidepp.pl %>% group_by(Classvote) %>%
  mutate(cond = Class == Classvote)  %>% do( rocf(.) )
}else{
  dat.rocpprf <- dat.sidepp.pl %>% group_by(Classvote) %>%
    mutate(cond = Class %in% Classvote)  %>% do( rocf(.) )
} 


dat.rocpprf$Classvote <- as.factor(dat.rocpprf$Classvote)


if(num){
dat.rocrf <- dat.side.pl %>%group_by(Classvote) %>%
  mutate(cond = Class == Classvote) %>% do(rocf(.))
}else{
  dat.rocrf <- dat.side.pl %>%group_by(Classvote) %>%
    mutate(cond = Class %in% Classvote) %>% do(rocf(.))
}
dat.rocrf$Classvote <- as.factor(dat.rocrf$Classvote)


 # predictions <- trees_pred(ppf_fish, xnew = ppf_fish$train[ ,-1])
 # mroc <- multiclass.roc(fishcatch$Type, predictions[[2]])


#oob error data rf
nsplit1 <- round(nrow(ppf$train)/13)
sel.tr <- round(seq(2, round(rf$ntree),nsplit1))
err <- rf$err.rate
err <- cbind(err, rf$test$err.rate)
colnames(err)[1] <- "All"

dat_rf_aux <-data.frame(err) 
colnames(dat_rf_aux) <- colnames(err)

dat_rf <- dat_rf_aux %>% mutate(trees = 1:rf$ntree) %>% gather(Class,OOB,-trees) %>%
  dplyr::filter(trees %in% sel.tr)

oob.pl <- ppf_oob_error(ppf, nsplit1 = round(nrow(ppf$train)/13) )

myColors <- c("#000000", brewer.pal(length(unique(ppf$train[,ppf$class.var])),"Dark2"))
names(myColors) <- levels(dat_rf$Class)


#imporf
aux <- data.frame(rf$importance)
imp.pl <- data.frame(nm = rownames(rf$importance),imp = aux[,"MeanDecreaseAccuracy"]) %>% arrange(imp)
imp.pl$nm <-  factor(imp.pl$nm, levels = imp.pl[order( imp.pl$imp), "nm"])









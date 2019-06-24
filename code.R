# OPENING FILE
d1<-read.csv(file.choose())
head(d1)
View(d1)

# CREATING COPY OF DATA
d1.c = d1
View(d1.c)

# REMOVING NON-NUMERIC DATA
d1.c$Policy.Number<-NULL
d1.c$Age.Band<-NULL
d1.c$Gender<-NULL
d1.c$Married<-NULL
d1.c$Vehicle.Age.Band<-NULL
d1.c$Fuel.Type<-NULL
d1.c$Count<-NULL
d1.c$Capped.Losses<-NULL

View(d1.c)

# SCALING THE DATA
d1.stand <- scale(d1.c[-1])

View(d1.stand)

# WSS PLOT ---

wssplot <- function(d1, nc=15, seed=222){
  wss <- (nrow(d1)-1)*sum(apply(d1,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(d1, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(d1.stand)

library(cluster)
clusplot(d1, results$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=0, lines=0)

# K MEANS CLUSTERING ----

results <- kmeans(d1.stand,4)
attributes(results)

results$cluster

View(aggregate(d1.c, by=list(cluster=results$cluster), mean))

table(d1$Gender.Dummy, results$cluster)




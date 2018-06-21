# Copyright statement:
# This shiny apllication is developed by Duco Veen to be used for educational purposes.
# Is is part of a program sponsered by the Education Incentive Funds of Utrecht University. 
# The lay-out for the shiny applications for this program is developed by Kimberley Lek. 
# The application is licensed under the GNU General Public License V3.0 

# Author Comment:
# I have tried to code this according to the Google R Style Guide to improve readibility:
# https://google.github.io/styleguide/Rguide.xml
# For any quenstions or comments you can contact me at d.veen@uu.nl.

# File description:
# This file contains the server for the application related to correlation.



# Loading library's 
library(shiny)
library(MASS) 

# server design
server <- function(input, output) {
  
  # First we will create data points with 0 correlation
  N <- 1000 # population size
  population <- mvrnorm(n=N,mu=c(0,0),Sigma =matrix(c(1,0,0,1),ncol = 2),empirical = T)
  # draw samples from bivariate standard normal distribution with 0 covariance.


  #n.sample <- 20
  #sample <- sample(x=(1:N), size = n.sample, replace = F)
  
  # empty vectors to store samples, corerlation and p values in.
  sample <- reactiveValues(
  correlations = NULL, 
  p.values     = NULL,
  sample       = NULL
  )
  
# function to sample with certain sample size and store correlation and p.value  
observeEvent(input$samplebutton,{
  sample$sample       <- sample(x=(1:N), size = 20, replace = F)
    sample$correlations <- c(sample$correlations, as.numeric(cor.test(x = population[sample$sample, 1], 
                                                                      y = population[sample$sample, 2])$estimate))
    sample$p.values     <- c(sample$p.values, as.numeric(cor.test(x = population[sample$sample, 1], 
                                                                  y = population[sample$sample, 2])$p.value))
  }
  )
  
# reset sample history
observeEvent(input$reset, {
  sample$correlations = NULL 
  sample$p.values     = NULL
  sample$sample       = NULL
})


  
  # plot for correlation tab. 
  output$corplot <- renderPlot({
    # create a scatterplot for population
    plot(population[,1],population[,2],bty="n", ylab = "y", xlab = "x", pch=19)
    # color the sampled points red
    if(length(sample$correlations) > 0){
    points(population[sample$sample,], col = "red", pch =19, cex=1.7)
    abline(lm(population[sample$sample,2] ~ 1 + population[sample$sample,1]), col= "red")
    legend("topright", bty = "n", c(paste0("Cor = ",round(cor.test(x = population[sample$sample, 1], 
                                                                   y = population[sample$sample, 2])$estimate,2)),
                                    paste0("p = ",round(cor.test(x = population[sample$sample, 1], 
                                                                 y = population[sample$sample, 2])$p.value,2))))
    }
  }
  )
  
  output$corhist <- renderPlot({
    # create a histogram for all sampled correlations
    if(length(sample$correlations) > 0){
    hist(sample$correlations, xlim = c(-1,1),breaks = seq(-1,1,by=.1), xlab = "Histogram of correlations of the drawn samples",
         main = "")
    
    xx <- c(seq(from = seq(-1,1,by=.1)[which(hist(plot=F,sample$correlations[length(sample$correlations)], 
                                                  breaks = seq(-1,1,by=.1))$counts == 1)],
                to = seq(-1,1,by=.1)[which(hist(plot=F,sample$correlations[length(sample$correlations)], 
                                                breaks = seq(-1,1,by=.1))$counts == 1)+1],by=.1),
            seq(from = seq(-1,1,by=.1)[which(hist(plot=F,sample$correlations[length(sample$correlations)], 
                                                  breaks = seq(-1,1,by=.1))$counts == 1)],
                to = seq(-1,1,by=.1)[which(hist(plot=F,sample$correlations[length(sample$correlations)], 
                                                breaks = seq(-1,1,by=.1))$counts == 1)+1],by=0.1)[c(2,1)])
    yy <-  c(hist(plot=F,sample$correlations, 
                  breaks = seq(-1,1,by=.1))$counts[which(hist(plot=F, sample$correlations[length(sample$correlations)], breaks = seq(-1,1,by=.1))$counts==1)],
             hist(plot=F,sample$correlations, 
                  breaks = seq(-1,1,by=.1))$counts[which(hist(plot=F, sample$correlations[length(sample$correlations)], breaks = seq(-1,1,by=.1))$counts==1)],
             hist(plot=F,sample$correlations, 
                  breaks = seq(-1,1,by=.1))$counts[which(hist(plot=F, sample$correlations[length(sample$correlations)], breaks = seq(-1,1,by=.1))$counts==1)]-1,
             hist(plot=F,sample$correlations, 
                  breaks = seq(-1,1,by=.1))$counts[which(hist(plot=F, sample$correlations[length(sample$correlations)], breaks = seq(-1,1,by=.1))$counts==1)]-1)
    polygon(x = xx, y = yy, col="red", density=100, angle = 0, border = "black")
    }
  }
  )
  
  output$phist <- renderPlot({
    # create a histogram for all sampled p.values
    if(length(sample$correlations) > 0){
      hist(sample$p.values, xlim=c(0,1),breaks = seq(0,1,by=.05),xlab = "Histogram of p values of the drawn samples", main  = "")
      xxx <- c(seq(from = seq(0,1,by=.05)[which(hist(plot=F,sample$p.values[length(sample$p.values)], breaks = seq(0,1,by=.05))$counts == 1)],
                   to = seq(0,1,by=.05)[which(hist(plot=F,sample$p.values[length(sample$p.values)], breaks = seq(0,1,by=.05))$counts == 1)+1],by=.05),
               seq(from = seq(0,1,by=.05)[which(hist(plot=F,sample$p.values[length(sample$p.values)], breaks = seq(0,1,by=.05))$counts == 1)],
                   to = seq(0,1,by=.05)[which(hist(plot=F,sample$p.values[length(sample$p.values)], breaks = seq(0,1,by=.05))$counts == 1)+1],by=0.05)[c(2,1)])
      yyy <-  c(hist(plot=F,sample$p.values, breaks = seq(0,1,by=.05))$counts[which(hist(plot=F, sample$p.values[length(sample$p.values)], breaks = seq(0,1,by=.05))$counts==1)],
                hist(plot=F,sample$p.values, breaks = seq(0,1,by=.05))$counts[which(hist(plot=F, sample$p.values[length(sample$p.values)], breaks = seq(0,1,by=.05))$counts==1)],
                hist(plot=F,sample$p.values, breaks = seq(0,1,by=.05))$counts[which(hist(plot=F, sample$p.values[length(sample$p.values)], breaks = seq(0,1,by=.05))$counts==1)]-1,
                hist(plot=F,sample$p.values, breaks = seq(0,1,by=.05))$counts[which(hist(plot=F, sample$p.values[length(sample$p.values)], breaks = seq(0,1,by=.05))$counts==1)]-1)
      polygon(x = xxx, y = yyy, col="red", density=100, angle = 0, border = "black")
    }
  }
  )
  
  

}
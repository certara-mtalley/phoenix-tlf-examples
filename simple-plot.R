library(ggplot2)
library(dplyr)

data <- readxl::read_xls("C:/Users/mtalley/Downloads/Simple Plot.xls")
data <- data[-1, ] |>
    transform(X = Time,
              Y = Conc,
              Group = Subject) 

data$X <- as.numeric(data$X)
data$Group <- as.factor(data$Group)
data$Y <- as.numeric(data$Y)

### User Controlled Options 

log.y <- TRUE
rm.zero <- FALSE
point.size <- 2
x.label<- "Time"
y.label<-"Concentration"
x.units <- "h"
y.units <- "ug/L"
X.min <- 0
X.max <- 50
X.small.tic <- 5
X.big.tic <- 10
Y.min <- 0
Y.max <- 50
Y.small.tic <- 5
Y.big.tic <- 10

###

label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")

p <- ggplot(data, aes(x = X, y = Y, group = Group, color = Group)) +
    geom_line() +  
    geom_point() +
    scale_x_continuous(breaks = seq(X.min, X.max, by = X.small.tic),
                       minor_breaks = seq(X.min, X.max, by = X.small.tic),
                       limits = c(X.min, X.max),
                       labels = label_at(X.big.tic)) +
    scale_y_continuous(breaks = seq(Y.min, Y.max, by = Y.small.tic),
                       minor_breaks = seq(Y.min, Y.max, by = Y.small.tic),
                       limits = c(Y.min, Y.max),
                       labels = label_at(Y.big.tic)) +
    theme_minimal() +  
    labs(x = paste(x.label," (", x.units, ")", sep = ""), 
         y = paste(y.label," (", y.units, ")", sep = ""), 
         color = "Subject") +
    theme(legend.position = "right",
          axis.line = element_line(linewidth = 0.5, color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.5),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 

# Print the plot
print(p)

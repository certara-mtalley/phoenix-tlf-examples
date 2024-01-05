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
add.err.bars <- TRUE
x.label<- "Time"
y.label<-"Mean"
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

mean_data <- data %>%
    group_by(X) %>%
    summarise(MeanY = mean(Y, na.rm = TRUE),
              SDY = sd(Y, na.rm = TRUE))

label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")

p <- ggplot(mean_data, aes(x = X, y = MeanY)) +
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
         y = paste(y.label," (", y.units, ")", sep = "")) +
    theme(legend.position = "right",
          axis.line = element_line(linewidth = 0.5, color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.5),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 

if (add.err.bars == TRUE) {
    p <- p +
        geom_errorbar(aes(ymin = MeanY - SDY, ymax = MeanY + SDY), width = 0.5) 
} 

# Print the plot
print(p)

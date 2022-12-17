
# Libraries
library('dplyr')
library('ggplot2')
library('cowplot')
library('DescTools') 
library('stargazer')

# Cleaning Utilities
cleanData <- function(df) {
  if (any(is.na(df))) {
    cat('Se omitieron:', sum(is.na(df)), 'Filas de Datos' )
    df <- na.omit(df)
  }
  return(df)
}

# Gets Stats from all Columns in a Dataframe
statsList <- function(x) {
  x <- mean(x)
  y <- median(x)
  z <- Mode(x)
  s <- sd(x)
  stats_list <- list('Media' = x, 'Mediana' = y, 'Moda' = z, 'SD' = s)
  return(stats_list)
}

# Prints a Descriptive Table of a Dataframe
printDescriptiveTable <- function(df) {
  stargazer(df, type = 'text', 
            min.max = TRUE, 
            mean.sd = TRUE, 
            nobs = FALSE, 
            median = TRUE, 
            iqr = TRUE,
            digits = 2, 
            align = T,
            title = 'Tabla Descriptiva - Variables Cuantitativas') 
  
# Tabla Descriptiva - Variables Categóricas
x <- model.matrix(nse5f ~ area + refin + sexojef + IA, data = df)
x.df <- data.frame(x) 
names(x) <- colnames(x)
stargazer(x.df, type = "text", 
          iqr = TRUE, 
          title = 'Tabla Descriptiva - Variables Cualitativas')
}

# Creates a Frequency Table
calculate_rel_freqs <- function(df) {
  for (col in names(df)) {
    freq <- table(df[,col])
    rel_freq <- prop.table(freq)
    print(rel_freq)
  }
}

# Function to plot histogram - bar 
plot_bar <- function(df, column, title, x_lab, y_lab) {
  ggplot(df, aes(x = factor(column))) +
    geom_bar(show.legend = FALSE,
             fill = "#1F1949",
             alpha = 0.75) +
    labs(title = title,
         x = x_lab, 
         y = y_lab) + 
    theme_minimal()
}


plot_bar2 <- function(df, ncols, printPlot) {
  columns <- names(df)

  plots <- lapply(columns, function(column) {
      ggplot(df, aes(x = factor(df[[column]]))) +
      geom_bar(show.legend = FALSE,
               fill = "#1F1949",
               alpha = 0.75) +
      labs(title = column,
           x = column, 
           y = 'Freq') + 
      theme_minimal()
  })
  plot_grid(plotlist = plots, nrow = NULL, ncol = ncols)
  
  if (printPlot == TRUE) {
    ggsave(filename = 'Variables Cualitativas.jpg', plot = last_plot(), dpi = 400)
  } else {
    plot_grid(plotlist = plots, nrow = NULL, ncol = ncols)  
  }
}

plot_histogram <- function(df, bins, ncols, printPlot) {
  columns <- names(df)
  
  plots <- lapply(columns, function(column) {
      ggplot(df, aes(df[[column]])) +
      geom_histogram(position = 'identity',
                     bins = bins,
                     show.legend = FALSE,
                     fill = "#1F1949",
                     alpha = 0.75) +
      geom_vline(aes(xintercept = mean(df[[column]])), 
                 color = "#f93565", 
                 linetype = "dotted", linewidth = .5) + 
      labs(title = column,
           x = column, 
           y = 'Freq') + 
      theme_minimal()
  })
  plot_grid(plotlist = plots, nrow = NULL, ncol = ncols)
  if (printPlot == TRUE) {
    ggsave(filename = 'Variables Cuantitativas.jpg', plot = last_plot(), dpi = 400)
  } else {
    plot_grid(plotlist = plots, nrow = NULL, ncol = ncols)  
  }
}


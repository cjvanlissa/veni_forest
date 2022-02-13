df_plot <- data.frame(Variable = names(VI$variable.importance), Value = VI$variable.importance)
df_plot$Variable <- gsub("^\\d+\\. ", "", df_plot$Variable)
df_plot$Level <- ordered(ren$V4, levels = c("Individual", "Microsystem", "Mesosystem", "Macrosystem"))
df_plot$x <- runif(nrow(df_plot))
df_plot$y <- df_plot$Level
levels(df_plot$y) <- c(1, 7, 9, 10)
df_plot$y <- as.numeric(as.character(df_plot$y))
df_plot$y[df_plot$y == 1] <- runif(length(df_plot$y[df_plot$y == 1]), 2, 6)
ggplot(df_plot, aes(x=x, y = y, size = Value, color = Level, label = Variable)) + #geom_point()+
  geom_text()+
  coord_polar()+
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())+
  scale_size(guide = 'none')
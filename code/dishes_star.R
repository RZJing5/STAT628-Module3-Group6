# change the path to PA_ch_dishes.csv, PA_cl_dishes.csv, PA_nh_dishes.csv can get all the images
da <- read.csv("https://raw.githubusercontent.com/RZJing5/STAT628-Module3-Group6/main/data/dishes/PA_nl_dishes.csv")
da <- da[,-12]

mean_dish <- sum(da[,1]*da[,-1])/sum(da[,-1])


mean_var <- function(f){
  res <- c()
  for (i in 1:length(f)){
    res <- c(res, rep(i,f[i]))
  }
  mu <- mean(res)
  sd <- sd(res)
  return(c(mu, sd))
}


results <- lapply(da[-1], mean_var) 
results <- do.call(cbind, results)


results_df <- as.data.frame(t(results))
names(results_df) <- c("mu", "sd")
results_df$category <- rownames(results_df)
library(ggplot2)


ggplot(data = results_df, aes(x = category, y = mu, group = 1)) +
  geom_point(aes(colour = category, size = sd)) + 
  geom_line() +
  scale_colour_manual(values = rainbow(n = length(unique(results_df$category)))) + 
  scale_size_continuous(name = "Standard Deviation") + 
  ggtitle("Mean and Standard Deviation of Food Categories") +
  geom_hline(yintercept = mean_dish, color = "red")+
  xlab(" ") +
  ylab("Mean Value") +
  theme_minimal()+
  theme(legend.position = "bottom") +
  guides(size = FALSE)+
  guides(colour = FALSE)




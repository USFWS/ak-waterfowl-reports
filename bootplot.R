#make an example figure from GAM bootsprapping similations

bootplot <- function(repdf = NULL, what = "Population Index"){
  #Now use results to find mean and SD at each time point
  sumdf <- repdf |> group_by(Year) |>
    summarise(mean = mean(results), sd = sd(results)) |>
    mutate(index = what, upper = mean + 2*sd, lower = mean - 2*sd)
  p1 <- ggplot() + 
    geom_ribbon(data = sumdf, aes(x=Year, ymin=pmax(0, lower), ymax = upper), 
                                  fill = "orange", show.legend = FALSE) + 
    geom_line(data = repdf, aes(x = Year, y = results, group = Rep), alpha = 0.25) + 
    geom_line(data = sumdf, aes(x = Year, y = mean), col = "darkgrey", lwd = 1) + 
    geom_pointrange(data = sppdf, aes(x = Year, y = index, ymin = lower, ymax = upper, 
                                      col = Observer),
                    position = position_dodge(width = 0.5), show.legend = FALSE) + 
    labs(x = "Year", y = "Population Index") +
    #scale_shape_manual(values = 1:7) + 
    scale_x_continuous(breaks = seq(min(repdf$Year), max(repdf$Year), by = 4)) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() + 
    theme(axis.title=element_text(size=13,face="bold")) +
    theme(axis.text.x = element_text(size=11), axis.text.y = element_text(size=11))
  return(p1)
}
print(bootplot(repdf))
p1 <- bootplot(repdf)
png(filename = paste0(path, "plot_trends/figures/", spp, "_index_2.png"))
print(p1)
dev.off()

geom_ribbon(aes(x=Year, ymin=pmax(0, lower), ymax = upper), fill = "lightgray", 
              alpha = 0.5) + 
  geom_line(aes(x=Year, y=mean), lwd = 1) + 
  geom_pointrange(data = df, aes(x=Year-0.1, y=index, ymin=pmax(0,lower), ymax = upper, 
                  col = "Total")) + 
  geom_pointrange(data = df, aes(x=Year+0.1, y=index2, ymin=pmax(0,lower2), ymax = upper2, 
                  col = "Breeding")) +
  labs(x = "Year", y = "Population Index") +
  scale_x_continuous(breaks = seq(2007, 2024, by = 2)) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values=c("Total"= "black", "Breeding" = "darkgray")) +  
  theme_bw() + 
  theme(legend.position = "top", legend.title=element_blank(), legend.text=element_text(size=11)) +
  theme(text=element_text(family="Times")) +
  theme(axis.title=element_text(size=13,face="bold")) +
  theme(axis.text.x = element_text(size=11), axis.text.y = element_text(size=11))
library(ggplot2)

ggplot(ToothGrowth, aes(x=as.factor(dose), y=len, fill= as.factor(dose) )) +
  geom_bar(stat = "identity") +
  facet_wrap(~supp, scales="free_y", labeller = label_both)+
  ylab ("deletion")+
  theme_classic()+
  theme(legend.position = "bottom")

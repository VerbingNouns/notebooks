# Script written by Lauren M Ackerman
# 26 February 2019
# Contact: l.m.ackerman AT gmail DOT com
# Adapted from Jalal Al-Tamimi

clmm.max <- clmm(as.factor(rating) ~ interaction(freq,gram) + (1 | subj) + (1 | item),
                 data = data[data$region == 1,]) # ordinal regression with INTERACTION BUILT INTO MAIN EFFECT

vlines <- c(0, # intercept, interaction(freq, gram)high.no
            clmm.max$beta[[1]], # interaction(freq, gram)low.no
            clmm.max$beta[[2]], # interaction(freq, gram)high.yes
            clmm.max$beta[[3]]) # interaction(freq, gram)low.yes
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("one" = "red",
            "two" = "orange",
            "three" = "green",
            "four" = "blue",
            "five" = "purple") # establish the colour-rating level correspondence

tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(one=  plogis(clmm.max$Theta[1] - xaxis), # add column for rating levels (must be adapted for larger scales)
         two=  plogis(clmm.max$Theta[2] - xaxis) - plogis(clmm.max$Theta[1] - xaxis),
         three=plogis(clmm.max$Theta[3] - xaxis) - plogis(clmm.max$Theta[2] - xaxis),
         four= plogis(clmm.max$Theta[4] - xaxis) - plogis(clmm.max$Theta[3] - xaxis),
         five= 1 - (plogis(clmm.max$Theta[4] - xaxis))) %>%
  gather(rating,probability,3:7) %>% # make long data
  mutate(rating=factor(rating,levels=c("one","two","three","four","five"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=rating),lwd=1) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("high-no","low-no","high-yes","low-yes"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background

#### example code ####
library(ggplot2)
library(dabestr)
library (Rmisc)
library(dplyr)
library(multcomp) 
setwd('C:/Huiying data/3rd Chapter/2026May')

df<- read.csv('whc_df.csv')
df$X<- NULL
# check if you have NA #
df<- na.omit(df)

#delete undesired cols #
df <- df[,-c(2:4)]
df <- df[,-c(3)]

# group #
df_ct<- filter(df,df$group=='0')
df_all<- filter(df,df$group=='all')

# when dabestr works on you computer #
# bootstarp 5000 times #

multi.two.group.unpaired <- 
  df_ct %>%
  dabest(treatment,WHC, 
         idx = list(c('all','AMF','Compost','Silicate','Vermiculate','Bentonite','Biochar')),
         paired = FALSE)
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)
multi.two.group.unpaired.meandiff %>% plot()
dat<-multi.two.group.unpaired.meandiff$result 

Overview <- dat[c(1,2,8,10,11)] 
#write.csv(Overview,'WHC_stats.csv')

## in case dabestr package doesn't work on your computer (to get statsitic) ##
sta<- summarySE(df_ct, measurevar="WHC", groupvars=c("treatment"))
sta_all<- summarySE(df_all, measurevar="WHC", groupvars=c("treatment"))
colnames(sta)[3]<- c('mean')
colnames(sta_all)[3]<- c('mean')

# anova #
df_ct$treatment <- relevel(
  factor(df_ct$treatment),
  ref = "CT"
)
ano_test<- aov(WHC~treatment,data=df_ct)

# 2 ways #
whc_p<- as.matrix(TukeyHSD(ano_test)[["treatment"]])
dunnett_test <- glht(ano_test, linfct = mcp(treatment = "Dunnett"))
summary(dunnett_test) #view in Console

# figures #

# Morandi colors- personal preference
morandi_col <- c(
  "CT"      = "#6F8F9C",   # control: muted blue
  
  "Biochar"      = "#C98276",  
  "Vermiculate"     = "#C9A66B",  
  "AMF" = "#8FA98A", 
  "Silicate"     = "#A889A8" ,  
  "Compost"      = "#B58B66",  
  "Bentonite"      = "#7F9A8A"    
  
)


# option1
p_0_whc<- ggplot(
  df_ct,
  aes(x = treatment, y = WHC, fill = treatment)
) +
  
  # raw data background
  geom_jitter(
    aes(color = treatment),
    width = 0.15,
    size = 2,
    alpha = 0.2
  ) +
  
  # boxplot
  geom_boxplot(
    width = 0.55,
    alpha = 0.1,
    outlier.shape = NA
  ) +
  
  scale_fill_manual(values = morandi_col) +
  scale_color_manual(values = morandi_col) +
  
  theme_classic() +
  
  labs(
    x = "",
    y = "Water holding capacity (%)",
    title = 'a'
  ) +
  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# option2
p_0_whc_2<- ggplot() +
  geom_jitter(
    data = df_ct,
    aes(
      x = treatment,
      y = WHC,
      color = treatment
    ),
    width = 0.15,
    size = 2.5,
    alpha = 0.2
  ) +
  
  # error bar
  geom_errorbar(
    data = sta,
    aes(
      x = treatment,
      ymin = mean - se,
      ymax = mean + se,
      color = treatment
    ),
    width = 0.15,
    linewidth = 0.8
  ) +
  
  # mean point
  geom_point(
    data = sta,
    aes(
      x = treatment,
      y = mean,
      fill = treatment
    ),
    shape = 21,
    size = 4,
    color = "black",
    stroke = 0.5
  ) +
  
  scale_color_manual(values = morandi_col) +
  scale_fill_manual(values = morandi_col) +
  
  theme_classic() +
  
  labs(
    x = "",
    y = "Water holding capacity (%)"
  ) +
  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# all - group, with example of boxplot
p_all_whc<- ggplot(
  df_all,
  aes(x = treatment, y = WHC, fill = treatment)
) +
  
  # raw data background
  geom_jitter(
    aes(color = treatment),
    width = 0.15,
    size = 2,
    alpha = 0.2
  ) +
  
  # boxplot
  geom_boxplot(
    width = 0.55,
    alpha = 0.1,
    outlier.shape = NA
  ) +
  
  scale_fill_manual(values = morandi_col) +
  scale_color_manual(values = morandi_col) +
  
  theme_classic() +
  
  labs(
    x = "",
    y = "",
    title = 'b'
  ) +
  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
library(patchwork)
p_0_whc + p_all_whc + plot_layout(ncol = 2, widths = c(1, 1), guides = "collect")

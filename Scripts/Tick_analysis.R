# Tick manuscript: Tick frequency by sex; P/A by size; 25cm & 2m sprint speed
pacman::p_load("dplyr", "car", "ggplot2", "ggdist", "performance", "magick", "cowplot")
tick_data <- read.csv("Data/Ticks_data_final.csv")

########
# Logistic regression for presence/abcense and body size in males & females
########
logit_male_dat <- tick_data %>% 
  filter(Sex == "M")

# Logistic regression: model summary 
logit_male_model<- glm(Ticks_1Y_0N ~ SVL, data=logit_male_dat, family=binomial(link="logit"))
summary(logit_male_model)
saveRDS(logit_male_model, file = "Models/logit_male_model.RDS")

# Logistic regression: create a new data frame for predictions
newdata <- data.frame(SVL = seq(min(logit_male_dat$SVL), 
                                max(logit_male_dat$SVL), 
                                length.out = 100))

# Logistic regression: compute the fitted lines and SE's
predicitions_male <-predict(logit_male_model,
                       newdata = newdata,
                       type = "link",
                       se.fit = TRUE) %>% 
  data.frame() %>% 
  mutate(ll = fit - 1.96 * se.fit,
         ul = fit + 1.96 * se.fit) %>% 
  select(-residual.scale, -se.fit) %>% 
  mutate_all(plogis) %>%
  bind_cols(newdata)

# Logistic regression: plotting model
logistic_regression_males <- predicitions_male %>% 
  ggplot(aes(x = SVL)) +
  #geom_ribbon(aes(ymin = ll, ymax = ul),
              #alpha = 1/2) +
  geom_line(aes(y = fit), size = 1.5) +
  stat_dots(data = logit_male_dat %>% 
              mutate(Ticks = factor(Ticks_1Y_0N, 
                                    levels = c("Yes", "No"))),
            aes(y = Ticks_1Y_0N, 
                side = ifelse(Ticks_1Y_0N == 0, "top", "bottom"),
                color = Ticks_Y_N),
            scale = 0.07, shape = 19) +
  annotate("text", x = 45.0, y = .98, label = "A: Male", fontface = "bold", size = 5)+
  scale_color_manual("Tick Presence", values = c("grey", "#D55E00")) +
  scale_x_continuous("SVL (mm)") +
  scale_y_continuous("Probability of infection",
                     expand = c(0, 0)) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none", 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# bring Legend image
Tick_image <- magick::image_read("Final_figures/Legend_LR.png") %>% 
  magick::image_background("none")
image <- image_fill(Tick_image, 'none')
tick_raster <- as.raster(image)

# Regression final plot
Logit_final_male <- ggdraw() +
  draw_plot(logistic_regression_males) +
  draw_image(tick_raster, scale = .185, x = -.32, y= 0.38) 


##########
# Female Logistic regression:
##########
# female model summary 
logit_female_dat <- tick_data %>% 
  filter(Sex == "F")
logit_female_model<- glm(Ticks_1Y_0N ~ SVL, data=logit_female_dat, family=binomial(link="logit"))
summary(logit_female_model)
saveRDS(logit_female_model, file = "Models/logit_female_model.RDS")

# Logistic regression: create a new data frame for predictions
newdata <- data.frame(SVL = seq(min(logit_female_dat$SVL), 
                                max(logit_female_dat$SVL), 
                                length.out = 100))

# Logistic regression: compute the fitted lines and SE's
predicitions_female <-predict(logit_female_model,
                       newdata = newdata,
                       type = "link",
                       se.fit = TRUE) %>% 
  data.frame() %>% 
  mutate(ll = fit - 1.96 * se.fit,
         ul = fit + 1.96 * se.fit) %>% 
  select(-residual.scale, -se.fit) %>% 
  mutate_all(plogis) %>%
  bind_cols(newdata)

# Logistic regression: plotting model
logistic_regression_females <- predicitions_female %>% 
  ggplot(aes(x = SVL)) +
  #geom_ribbon(aes(ymin = ll, ymax = ul),
             # alpha = 1/2) +
  geom_line(aes(y = fit), size = 1.5) +
  stat_dots(data = logit_female_dat %>% 
              mutate(Ticks = factor(Ticks_1Y_0N, 
                                    levels = c("Yes", "No"))),
            aes(y = Ticks_1Y_0N, 
                side = ifelse(Ticks_1Y_0N == 0, "top", "bottom"),
                color = Ticks_Y_N),
            scale = 0.1, shape = 19) +
  annotate("text", x = 53.5, y = .98, label = "B: Female", fontface = "bold", size = 5)+
  scale_color_manual("Tick Presence", values = c("grey", "#D55E00")) +
  scale_x_continuous("SVL (mm)")  +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none", 
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))


# bring Legend image
Tick_image <- magick::image_read("Final_figures/Legend_LR.png") %>% 
  magick::image_background("none")
image <- image_fill(Tick_image, 'none')
tick_raster <- as.raster(image)

# Regression final plot
Logit_final_female <- ggdraw() +
  draw_plot(logistic_regression_females) +
  draw_image(tick_raster, scale = .185, x = -.32, y= 0.38) 


### Final figure for males and females
Final_logit <- plot_grid(Logit_final_male, Logit_final_female)


########
# Sex differences in tick presence- chi-square test with 
# Yates' correction
########
# Tick precense abcence by sex
male_female <- tick_data %>% 
  select(Sex, Ticks_Y_N) %>% 
  filter(Ticks_Y_N == "Yes")

# Contingency table
contingency_table <- table(male_female$Sex, male_female$Ticks_Y_N)

# Run chi-square test with Yates' correction
sex_yates <- chisq.test(contingency_table, correct = TRUE)
saveRDS(sex_yates, file = "Models/sex_yates.rds")



########
# ANCOVA : 25CM sprint speed 
# MALES
########
sprint_25cm_data <- tick_data %>% 
  filter(Sex == "M") %>% 
  select(HLL, Mass, SVL, Ticks_Y_N, Max_25cm)

# model
sprint_25cm_mod <- lm(Max_25cm ~ HLL + Ticks_Y_N, data=sprint_25cm_data)
anova(sprint_25cm_mod) # differences in sprint speed between treatments
check_model(sprint_25cm_mod) # residuals & normality look fine; save mod
saveRDS(sprint_25cm_mod, "Models/sprint_25CM.rds")

# plot
sprint_25cm_plot <- ggplot(sprint_25cm_mod, 
                           aes(x=HLL, y=Max_25cm, 
                               color=Ticks_Y_N, shape=Ticks_Y_N)) +
  geom_point(size = 4) +
  stat_smooth(method=lm, se = FALSE) +
  scale_color_manual("Tick Presence", values = c("grey", "#D55E00")) +
  scale_shape_manual("Tick Presence", values = c(16, 17)) +
  scale_y_continuous(limits=c(1.3,4.01), breaks = seq(1.5, 4, by = .5)) +
  ylab(expression("Speed"~(~ms^{"-1"}))) +
  xlab("Hind limb length (mm)") + 
  annotate("text", x = 35, y = 4.0, label = "Maximum sprint speed", fontface = "bold", size = 5)+
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# bring Legend image
Tick_image <- magick::image_read("Final_figures/Treatment_leg.png") %>% 
  magick::image_background("none")
image <- image_fill(Tick_image, 'none')
tick_raster <- as.raster(image)

#  final plot
Sprint_25cm_final <- ggdraw() +
  draw_plot(sprint_25cm_plot) +
  draw_image(tick_raster, scale = .185, x = -.32, y= 0.38) 



########
# ANCOVA : 2m sprint speed 
# MALES
########
sprint_2m_data <- tick_data %>% 
  filter(Sex == "M") %>% 
  select(HLL, Mass, SVL, Ticks_Y_N, Max_2m)

# model
sprint_2m_mod <- lm(Max_2m ~ HLL + Ticks_Y_N, data=sprint_2m_data)
anova(sprint_2m_mod) # differences in sprint speed between treatments
check_model(sprint_2m_mod)# residuals & normality look fine; save mod
saveRDS(sprint_2m_mod, "Models/sprint_2M.rds")

# plot
sprint_2m_plot <- ggplot(sprint_2m_mod, 
                           aes(x=HLL, y=Max_2m, 
                               color=Ticks_Y_N, shape=Ticks_Y_N)) +
  geom_point(size = 4) +
  stat_smooth(method=lm,se = FALSE) +
  scale_color_manual("Tick Presence", values = c("grey", "#D55E00")) +
  scale_shape_manual("Tick Presence", values = c(16, 17)) +
  scale_y_continuous(limits=c(.3,3), breaks = seq(.5, 3, by = .5)) +
  ylab(expression("Speed"~(~ms^{"-1"}))) +
  xlab("Hind limb length (mm)") + 
  annotate("text", x = 33.3, y = 3.0, label = "2 Meter run", fontface = "bold", size = 5)+
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# add legend
Sprint_2m_final <- ggdraw() +
  draw_plot(sprint_2m_plot) +
  draw_image(tick_raster, scale = .185, x = -.32, y= 0.38) 

# final plot with both
Final_sprint <- plot_grid(Sprint_25cm_final, Sprint_2m_final, labels = c('A', 'B'))


########
# body condition
########
# calcualting residuals from Regression of mass and SVL
tick_data_BCI <- tick_data[complete.cases(tick_data$Mass, tick_data$SVL),] # note one individual is missing mass
tick_data_BCI <- tick_data_BCI %>% filter(Sex == "M")
fit <- lm(Mass ~ SVL, data = tick_data_BCI)
tick_data_BCI$residuals <- residuals(fit)
bci_mod <- lm(residuals ~ Ticks_Y_N, data = tick_data_BCI)
anova(bci_mod) # no differences between treatments
check_model(fit) # residuals & normality look fine; save mod
saveRDS(bci_mod, file = "Models/BCI_ticks.rds")

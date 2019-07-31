library(here); library(stargazer); library(caret); library(tidyverse);
library(car); library(corrplot); library(RColorBrewer); library(moments)
theme_set(theme_minimal())
dv <- read.csv(here("outputs", "act_peds.csv")) %>%
  dplyr::select(GEOID, e_wlk_den, a_wlk_den)
iv <- read.csv(here("outputs", "ind_vars.csv"))
all_vars <- left_join(dv, iv, by = "GEOID") %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  dplyr::select(-a_wlk_den, -pct_wlk, -trans_scr)
# sub = suburbs
sub <- all_vars %>%
  filter(stcty %in% c("42017", "42029", "42045", "42091")) %>%
  dplyr::select(-stcty, -litter_scr, -ppl_dens, -ppl_interaction) %>%
  drop_na(.)
# phila = Philadelphia
phila <- all_vars %>%
  filter(stcty == "42101") %>%
  dplyr::select(-stcty, -pop_dens, -job_dens) %>%
  drop_na(.)

# Find and remove outliers
ggplot(sub, aes(x = pop_dens, y = e_wlk_den)) + geom_point() +
  geom_hline(yintercept = 30000, color = brewer.pal(8, "YlOrRd")[5]) +
  labs(title = "Outliers, four suburban counties",
       x = "Population density",
       y = "Estimated pedestrian density")
ggsave(here("figures", "sub_outliers.png"), width = 7, height = 5, units = "in")
drop_vars <- all_vars %>% 
  filter(stcty %in% c("42017", "42029", "42045", "42091")) %>%
  filter(e_wlk_den > 30000) %>%
  drop_na() %>%
  pull(GEOID)
sub %<>% filter(!(GEOID %in% drop_vars)) %>%
  dplyr::select(-GEOID)

ggplot(phila, aes(x = ppl_dens, y = e_wlk_den)) + geom_point() +
  geom_hline(yintercept = 300000, color = brewer.pal(8, "YlOrRd")[5]) +
  labs(title = "Outliers, Philadelphia",
       x = "Population + job density",
       y = "Estimated pedestrian density")
ggsave(here("figures", "phila_outliers.png"), width = 7, height = 5, units = "in")
drop_vars <- all_vars %>% 
  filter(stcty == "42101") %>%
  filter(e_wlk_den > 300000) %>%
  drop_na() %>%
  pull(GEOID)
phila %<>% filter(!(GEOID %in% drop_vars)) %>%
  dplyr::select(-GEOID)

# Multicollinearity
png(here("figures", "sub_corr.png"), width = 10, height = 10, units = "in", res = 500)
corrplot(cor(sub), method = "number", type = "upper", col = brewer.pal(n = 8, name = "PuOr"))
dev.off()
png(here("figures", "phila_corr.png"), width = 10, height = 10, units = "in", res = 500)
corrplot(cor(phila), method = "number", type = "upper", col = brewer.pal(n = 8, name = "PuOr"))
dev.off()

# Suburban counties
# Skewness of variables
# Ugly code but works for now
dat <- sub
# If skewness > 1.5, log variable
new_dat <- NULL
skewed_vars <- NULL
for (i in 1:length(dat)){
  if(skewness(dat[i]) > 1.5){
    safe <- dat[i] # Prevent overwriting dat
    safe[safe == 0] <- 0.001
    new_col <- unlist(log(safe))
    new_dat <- cbind(new_dat, new_col)
    skewed_vars <- c(skewed_vars, names(dat)[i])
  } else {
    new_col <- dat[i]
    new_dat <- cbind(new_dat, new_col)
  }
}
names(new_dat) <- names(dat)

fm <- lm(e_wlk_den~., data = new_dat)
sm <- train(e_wlk_den ~., data = new_dat,
            method = "leapSeq",
            tuneGrid = data.frame(nvmax = 1:10),
            trControl = trainControl(method = "cv", number = 10)
)
sm$results; sm$bestTune; summary(sm$finalModel)
evThree <- names(coef(sm$finalModel, 3))[2:4]
evThree # "pop_dens"  "pct_coll"  "road_dens"

fm <- lm(e_wlk_den ~ pop_dens + pct_coll + road_dens, data = new_dat)
summary(fm); vif(fm)
spm(~e_wlk_den + pop_dens + pct_coll + road_dens, data = new_dat)

skewed_vars # Note when ln of var is used in regression
stargazer(fm, style = "qje", dep.var.labels = "ln(Estimated Pedestrian Density by Tract)",
          covariate.labels = c("ln(Population density)",
                               "ln(Percentage college students)",
                               "Road density"),
          title = "Estimated Pedestrian Density by Tract", out = here("regressions", "sub_fm.txt"))

png(here("figures", "sub_model.png"), width = 10, height = 7.5, units = "in", res = 500)
spm(~e_wlk_den + pop_dens + pct_coll + road_dens, data = new_dat,
    main = "", col = brewer.pal(8, "YlOrRd")[5])
dev.off()

# Philadelphia
# Skewness of variables
# We remove job_dens and pop_dens because we have two variables that combine these:
# ppl_dens and ppl_interaction
# I've also removed university and urban because Philadelphia is practically 99% urban
# And we don't really want to use the university Boolean for sampling purposes
dat <- phila
# If skewness > 1.5, log variable
new_dat <- NULL
skewed_vars <- NULL
for (i in 1:length(dat)){
  if(skewness(dat[i]) > 1.5){
    safe <- dat[i] # Prevent overwriting dat
    safe[safe == 0] <- 0.001
    new_col <- unlist(log(safe))
    new_dat <- cbind(new_dat, new_col)
    skewed_vars <- c(skewed_vars, names(dat)[i])
  } else {
    new_col <- dat[i]
    new_dat <- cbind(new_dat, new_col)
  }
}
names(new_dat) <- names(dat)

fm <- lm(e_wlk_den~., data = new_dat)
sm <- train(e_wlk_den ~., data = new_dat,
            method = "leapSeq",
            tuneGrid = data.frame(nvmax = 1:10),
            trControl = trainControl(method = "cv", number = 10)
)
sm$results; sm$bestTune; summary(sm$finalModel)
evThree <- names(coef(sm$finalModel, 3))[2:4]
evThree # "pct_coll" "bl_pov" "ta_dens"

fm <- lm(e_wlk_den ~ pct_coll + bl_pov + ta_dens, data = new_dat)
summary(fm); vif(fm)
spm(~e_wlk_den + pct_coll + bl_pov + ta_dens, data = new_dat)

skewed_vars # Note when ln of var is used in regression
stargazer(fm, style = "qje", dep.var.labels = "ln(Estimated Pedestrian Density by Tract)",
          covariate.labels = c("ln(Percentage college students)",
                               "Percentage residents below FPL",
                               "ln(Transit activity density)"),
          title = "Estimated Pedestrian Density by Tract", out = here("regressions", "phila_fm.txt"))

png(here("figures", "phila_model.png"), width = 10, height = 7.5, units = "in", res = 500)
spm(~e_wlk_den + pct_coll + bl_pov + ta_dens, data = new_dat,
    main = "", col = brewer.pal(8, "YlOrRd")[5])
dev.off()

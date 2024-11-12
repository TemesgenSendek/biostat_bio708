# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))

#COUNT DATA
df_vpart <- read_csv("data_raw/data_vpart.csv")
print(df_vpart)
summary(df_vpart)
var(df_vpart$n_sp) > mean(df_vpart$n_sp)

v_pois <- glm(n_sp ~ distance + cat_area + hull_area, 
               data = df_vpart, family = "poisson")
summary(v_pois)


df_pred <- df_vpart %>% 
  reframe(distance = seq(min(distance),
                         max(distance),
                         length=100),
          cat_area=mean(cat_area),
          hull_area=mean(hull_area)) %>% 
            mutate(log_y_pred=predict(v_pois, newdata=.),
                   y_pred= exp(log_y_pred))

#visualize fit
df_vpart %>% ggplot(aes(x=distance, y=n_sp))+
  geom_point() +
  geom_line(data=df_pred, aes(y=y_pred)) +
labs(x="Dist to the sea", y="spp richness") +
  theme_bw()


# Effect size
glm(n_sp ~ scale(distance) + scale(cat_area)+ scale(hull_area),
    data =  df_vpart, family = "poisson")



#END 


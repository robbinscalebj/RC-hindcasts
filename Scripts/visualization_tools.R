#visualization tools to test trained models

#visualize predictions
start_date <- as_datetime("2021-05-20 00:00:00")
end_date <- as_datetime("2021-06-30 00:00:00")
final_preds|>
  filter(Depth_m %in% c(10))|>
  ggplot()+
  geom_line(aes(y = .pred, x = Datetime))+
  geom_point(aes(y = WaterTemp_C, x = Datetime))+
  #facet_grid(.~Depth_m)+
  scale_x_datetime(limits = c(start_date, end_date))

pred_sums<- final_preds|>
  group_by(Datetime, Depth_m)|>
  summarize(mean_do = mean(.pred),
            ci = 1.96*sqrt(sd(.pred)/n()),
            pi = 1.96*sqrt(sd(.pred)))


predictions|>
  bind_rows(RC_df)|>
  filter(Depth_m %in% c(0))|>
  ggplot()+
  geom_line(aes(y = .pred, x = Datetime, group = ensemble))+
  #facet_grid(.~Depth_m)+
  scale_x_datetime(limits = c(forecast_start, last(predictions$Datetime)-days(10)))+
  scale_y_continuous(limits = c(7.5,10))


pred_sums|>
  filter(Depth_m == 0)|>
  ggplot()+
  geom_line(aes(y = mean_do, x = Datetime))+
  #geom_line(aes(y = mean_do+ci, x = Datetime), color = "red", linetype= "dashed")+
  #geom_line(aes(y = mean_do-ci, x = Datetime), color = "red", linetype= "dashed")+
  geom_line(aes(y = mean_do+pi, x = Datetime), color = "blue", linetype= "dashed")+
  geom_line(aes(y = mean_do-pi, x = Datetime), color = "blue", linetype= "dashed")+
  #facet_grid(.~Depth_m)+
  scale_x_datetime(limits = c(forecast_start, last(predictions$Datetime)-days(5)))+
  theme_bw()+
  scale_y_continuous(limits = c(7,11))+
  ylab("DO (mg.L)")+
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 14))
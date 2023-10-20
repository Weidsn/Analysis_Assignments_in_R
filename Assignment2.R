csvTitanic <- read_csv("titanic.csv") %>% 
  view()

summary(csvTitanic)

# fix "alive" feature
csvTitanic <- csvTitanic |> 
  mutate(alive = recode(alive, yes = TRUE, no = FALSE)) |> 
  glimpse()

# fix "embark_town" feature
csvTitanic <- csvTitanic |> 
  mutate(embark_town = factor(embark_town))

# Check if embarked and embark_town have matching values
all.equal(
csvTitanic |>
  mutate(embarked = recode(embarked, 
                      "S" = "Southampton", 
                      "C"="Cherbourg", 
                      "Q"="Queenstown")) |> 
  select(embarked)
,
  csvTitanic |> 
  select(embark_town)
,
check.attributes= FALSE)
# complete check!

unique(csvTitanic$who)
unique(csvTitanic$sex)

# making factors
csvTitanic$sex <- factor(csvTitanic$sex)
csvTitanic$who <- factor(csvTitanic$who)

# renaming values in "survived"
csvTitanic <- csvTitanic |> 
  mutate(survived = recode(survived, `1` ="Survivor", `0` = "Victim"))

# Bar chart, average age of each age group
# csvTitanic |> 
#   drop_na(age) |> 
#   group_by(who) |> 
#   summarize(avg_age = mean(age)) |> 
#   ggplot(aes(who,avg_age, fill=who)) +
#   geom_col() +
#   theme_bw()

# Histogram, age by suvivor and victim
csvTitanic |> 
  drop_na(age) |> 
  ggplot(aes(age)) +
  geom_histogram(fill="blue", alpha=0.5, bins=27) +
  facet_wrap(~survived) +
  labs(title="Age of survivors and victims",
       y="Number of people") +
  theme_bw()

# # scatterplot, fare & age, unfiltered
# csvTitanic |> 
#   drop_na(age) |> 
#   ggplot(aes(age,fare)) +
#   facet_wrap(~class)+
#   geom_point()

# facet labeller for pclass
pclass_lab <- c("First Class", "Second Class","Third Class")
names(pclass_lab) <- c(1,2,3)
glimpse(pclass_lab)

# scatterplot, fare & age, filtered
csvTitanic |> 
  drop_na(age) |> 
  filter(fare<500 & 0 < fare & who %in% c("woman","man")) |> 
  ggplot(aes(x=fare, y=age)) +
  facet_wrap(~pclass, scale="free_x",
             labeller= labeller(pclass = pclass_lab))+
  geom_point(aes(color=sex)) +
  scale_color_manual(values= c("firebrick2","blue2"))+
  labs(title = "Plot of fare price and age, divided by cabin class", 
       x="Fare Price", y="Age")

# Bar chart, average fare by sibsp using labeller
csvTitanic |> 
  drop_na(age) |> 
  filter(fare<500 & 0 < fare) |>  # & who %in% c("woman","man")) |> 
  # filter(pclass > 2) |> 
  group_by(sibsp, pclass) |> 
  summarize(avg_fare = mean(fare)) |> 
  ggplot(aes(sibsp,avg_fare)) +
  facet_wrap(~pclass, labeller= labeller(pclass = pclass_lab),
             scales = "free_x") +
  geom_col(aes(fill=sibsp)) +
  labs(title = "Average fare grouped by the number of siblings or spouse onboard",
       y= "Average Fare", x = "Number of siblings or spouse onboard") +
  theme_bw()


# average fare by parch
# csvTitanic |> 
#   drop_na(age) |> 
#   filter(fare<500 & 0 < fare & who %in% c("woman","man")) |> 
#   group_by(parch, pclass) |> 
#   summarize(avg_fare = mean(fare)) |> 
#   ggplot(aes(parch,avg_fare)) +
#   facet_wrap(~pclass, labeller= labeller(pclass = pclass_lab)) +
#   geom_col(aes(fill=parch))


# scatter plot, sibsp & parch
# csvTitanic |> 
#   ggplot(aes(sibsp,parch)) +
#   geom_point() +
#   geom_smooth()

# Box plot, fare by number of Parents or Children onboard
csvTitanic |> 
  filter(fare<500 & 0 < fare & who %in% c("woman","man")) |> 
  ggplot(aes(x=factor(parch),y=fare)) +
  geom_boxplot(fill="lightskyblue", color="steelblue4") +
  # geom_smooth(method="lm")+
  facet_wrap(~pclass, 
             labeller= labeller(pclass = pclass_lab), 
             scales = "free") +
  labs(title = "Fare by number of Parents or Children onboard",
       y= "Fare", x="Number of Parents or Children onboard")+
  theme_bw()
  
  
  

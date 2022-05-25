library(hexSticker)
library(wesanderson)

# Setting parameters for model1
pars <- default_pars("model1")

binwidth_age <- 1
binwidth_size <- 0.1

# Simulate population
data <- simulate_population(pars = pars)

names(wes_palettes)
wes_palette("Darjeeling2") %>% str()
wes_palette("Chevalier1") %>% str()

#Plot the data
p <- data %>%
  ggplot(aes(size)) +
  geom_histogram(binwidth = binwidth_size,
                 fill = "#046C9A",
                 colour = "white") +
  stat_function(fun = function(x) size_dist_model(x, pars) * binwidth_size,
                colour = "#FDD262",
                size = 1.2,
                alpha = 0.7)

p <- p + theme_void() + theme_transparent()

sticker(p,
        package="sizedist",
        p_size=20, p_y = 1.45, p_color = ,
        h_fill = "#046C9A", h_color = "#ABDDDE",
        s_x=1., s_y=0.90, s_width=1.2, s_height=1,
        filename="inst/figures/sizedist.png")

sticker(p,
        package="sizedist",
        p_size=23, p_x = 1.21, p_y = 1.18,
        h_fill = "#046C9A", h_color = "#FDD262",
        s_x=1.02, s_y=1, s_width=1.3, s_height=1.2,
        filename="inst/figures/sizedist_2.png")

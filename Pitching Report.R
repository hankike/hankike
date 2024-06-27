library(baseballr)
library(ggridges)

# Download data from baseball savant. Apply here.
P <- read.csv("~/Downloads/665871_data.csv") #change player code

columns = c("pitch_name", "pitch_type", "release_speed", "effective_speed", "release_spin_rate", "vx0", "vy0", "vz0",
            "ax", "ay", "az", "pfx_x", "pfx_z", "plate_x", "plate_z", "zone", "description")
Pitches = P[, columns]
Pitches = Pitches %>% mutate(pfx_x_in = 12*pfx_x, pfx_z_in = 12*pfx_z)

# First graph is RPM vs pitch velocity
ggplot(Pitches, aes(release_speed, release_spin_rate)) + 
  geom_point(colour="black", shape = 21, size = 2,
             aes(fill = factor(pitch_type))) + 
  theme_classic() + 
  labs(x = "Pitch Speed (mph)", y = "Spin Rate (RPM)", title = "RPM vs Pitch Velocity", 
       subtitle = "Javier Assad, 2024 season") + #change player name
  scale_fill_discrete(name = "pitch_type")

# Second graph is pitch movement
ggplot(Pitches, aes(pfx_x_in, pfx_z_in)) + 
  geom_point(colour="black", shape = 21, size = 2,
             aes(fill = factor(pitch_type))) + 
  theme_classic() + 
  xlim(c(-25,25)) + #if pitcher a lefty use (25,-25). right use (-25,25)
  ylim(c(-25,25)) + 
  labs(x = "Horizontal Break (in)", y = "Vertical Break (in)", title = "Pitch Movement, Catcher Perspective", 
       subtitle = "Javier Assad, 2024 Season") +
  scale_fill_discrete(name = "pitch_type") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0), lty = 2, col = "red", alpha = 0.5) 

# Third graph is a velocity distribution
ggplot(Pitches, aes(x = release_speed, y = pitch_type, fill = pitch_type)) +
  geom_density_ridges(scale = 0.9, alpha = 0.75) +
  theme_ridges(center_axis_labels = TRUE) + 
  scale_y_discrete(expand = expand_scale(add = c(0.2, 1))) +
  labs(x = "Velocity (mph)", y = NULL , title = "
       Javier Assad Velocity Distribution") +
  theme(legend.position = "none")

# Generates the noisecanceling hex sticker (man/figures/logo.png).
# The artwork tells the noise-cancelling story: a clean trait "signal" plus a
# pair of mirror-image "noise" / "anti-noise" waves that cancel each other out.

library(ggplot2)
library(hexSticker)
library(sysfonts)
library(showtext)

font_add_google("Quicksand", "quicksand")
showtext_auto()

# Ocean blues + teal palette.
navy        <- "#0A1E38"
teal_signal <- "#27E0C8"
blue_noise  <- "#3C7CB8"
blue_anti   <- "#7FB5DD"
faint       <- "#16406A"

x <- seq(0, 4 * pi, length.out = 600)

waves <- rbind(
  data.frame(x = x, y =  0.30 * sin(5 * x + 0.6), grp = "h2a"),
  data.frame(x = x, y = -0.30 * sin(5 * x + 0.6), grp = "h2b"),
  data.frame(x = x, y =  0.55 * sin(3 * x),       grp = "noise"),
  data.frame(x = x, y = -0.55 * sin(3 * x),       grp = "anti"),
  data.frame(x = x, y =  0.85 * sin(x),           grp = "signal")
)
waves$grp <- factor(
  waves$grp,
  levels = c("h2a", "h2b", "noise", "anti", "signal")
)

p <- ggplot(waves, aes(x = x, y = y, group = grp, color = grp)) +
  geom_hline(yintercept = 0, color = teal_signal, linewidth = 0.4,
             alpha = 0.6) +
  geom_line(aes(linewidth = grp, alpha = grp), lineend = "round") +
  scale_color_manual(values = c(
    h2a = faint, h2b = faint,
    noise = blue_noise, anti = blue_anti, signal = teal_signal
  )) +
  scale_linewidth_manual(values = c(
    h2a = 0.6, h2b = 0.6, noise = 0.9, anti = 0.9, signal = 1.5
  )) +
  scale_alpha_manual(values = c(
    h2a = 0.5, h2b = 0.5, noise = 0.9, anti = 0.9, signal = 1
  )) +
  theme_void() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(-1.25, 1.25))

sticker(
  subplot   = p,
  package   = "noisecanceling",
  p_family  = "quicksand",
  p_size    = 15,
  p_y       = 1.5,
  p_color   = "#EAF7F5",
  s_x       = 1,
  s_y       = 0.82,
  s_width   = 1.35,
  s_height  = 0.95,
  h_fill    = navy,
  h_color   = teal_signal,
  h_size    = 1.4,
  url       = "rprimi.github.io/noisecanceling",
  u_color   = "#7FB5DD",
  u_size    = 4.2,
  dpi       = 320,
  filename  = "man/figures/logo.png"
)

# Embed Font
# C. McIntyre
# April 25, 2022

packages <- c("datasets", "ggplot2", "dplyr")

install.packages(setdiff(packages, rownames(installed.packages()))) 

library(datasets)
library(ggplot2)
library(dplyr)

data("iris")

iris2 <- iris %>%
  mutate(LongName = paste("Iris",Species,sep=" ")) %>%
  mutate(ShortName = paste("I.",Species,sep=" "))

# Colors & symbols
# make your own color palette
# there are a variety of built-in color palettes, including many that are color-blind friendly
# but sometimes you want to make your own. We'll define one based on ShortName
scale_color_shortname <- function(...){
  ggplot2:::manual_scale('color', 
                         values = setNames(c("#238443", "#c2e699", "#78c679"),
                                           c("I. virginica", "I. versicolor", "I. setosa")), limits=force, 
                         ...)
}

# use Frutiger font - NPS standard - when possible
# NPS employees can download Fruiter to their NPS computer from https://www.nps.gov/subjects/hfc/nps-typefaces.htm
# must be on the vpn to do the download
# after font is downloaded, install it on your computer, then run this line 
windowsFonts("Frutiger LT Std 55 Roman" = windowsFont("Frutiger LT Std 55 Roman"))

Frutiger <- iris2 %>%
  ggplot(aes(x=Sepal.Width,y=Sepal.Length)) + 
  geom_point(aes(color=ShortName,shape=ShortName), position=position_jitter(width = 0.02, height = 0.02,seed=37)) + 
  labs(title="Length and width of Iris sepals, by species",color = "Species",shape = "Species") + 
  scale_x_continuous(name="Sepal width (mm)") + 
  scale_y_continuous(name="Sepal length (mm)") +
  theme_classic() + 
  theme(legend.title = element_text(size=10, color="black")) +
  theme(legend.text = element_text(size=9,face="italic",color="black")) +
  theme(axis.title = element_text(size=10,color="black")) + 
  theme(axis.text = element_text(size=9,color="black")) +
  theme(plot.title = element_text(size=12, color="black")) +
  theme(axis.ticks = element_line(color='black')) + 
  theme(axis.line = element_line(color='black')) +
  scale_color_shortname() + 
  theme(text = element_text(family = "Frutiger LT Std 55 Roman", face="plain"))
Frutiger

# save your charts as PDF
# use of custom fonts requires using the "cairo_pdf" device in the ggsave function
# you can/should also define the dpi (dots per inch), 300 is good for printing
# you can/should define the size of your figure

# save the Frutiger font individually as a 3x4in rectangle
ggsave(Frutiger, file = "Frutiger_Jitter.pdf", device = cairo_pdf, dpi=300, width = 4, height = 3, units="in")

# here's what happens if you don't use the cairo_pdf device
ggsave(Frutiger, file = "Frutiger_Jitter2.pdf", dpi=300, width = 4, height = 3, units="in")
# you get a warning message about an invalid font type - open the pdf - a bunch of stuff is missing


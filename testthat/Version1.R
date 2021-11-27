rm(list = ls())
library(mergePlot)
library(magick)

dir.fig = 'Figure'
dir.create(dir.fig, showWarnings = FALSE, recursive = TRUE)

# --- Make six plots and save into a folder ---
nf = 6
sub = MakePic(n=nf, width=400, height=300, path=dir.fig)

# --- load the png file in folder  ---
fns = list.files(dir.fig, pattern = glob2rx('Fig*.png'), full.names = TRUE)
sub = fns %>% image_read()

# --- trim the empty margins  ---
figs= sub %>% image_trim()

# --- put the subfigures into a canvas ---
fig.com = mergePlot(figs, canvas=plotCanvas(TEST=T, col.bg = 'pink'))

# --- save the figure ---
image_write(fig.com, path='Figure/test2.png')

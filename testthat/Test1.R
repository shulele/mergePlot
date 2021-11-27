rm(list = ls())
library(mergePlot)

# --- Make six plots ---
nf = 6
sub = MakePic(n=nf, width=400, height=300)

# --- trim the empty margins  ---
figs= sub %>% image_trim()

# --- put the subfigures into a canvas ---
fig.com = mergePlot(figs, canvas=plotCanvas(TEST=T, col.bg = 'pink'))

# --- save the figure ---
image_write(fig.com, path='test1.png')

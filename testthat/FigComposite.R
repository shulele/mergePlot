rm(list = ls())
source('R/MakePic.R')
source('R/PlotCanvas.R')

nf =6

sub = MakePic(n=nf, width=400, height=300)
figs= sub %>% image_trim()
fig.com = composite(figs, bg=plot.canvas(TEST=T, col.bg = 'pink'))
image_write(fig.com, path='test.png')

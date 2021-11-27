library(magick)
rm(list=ls())

r.text = c('RCP 8.1', 'RCP 4.5', 'RCP 2.6', '')
fns = list.files('Figure/', pattern = glob2rx('Fig*.png'), full.names = T)
idx=c(10:12, 7:9, 4:6, 1:3)
idx=1:12
fn=fns[1]
tt.text=basename(fns)
nf = length(fns)

#'
image.processing <-
  function(x,
           border.color ='white',
           title.size = 24,
           title.location = c(0,0),
           title.font = '',
           title.color = 'black',
           ylab.size = 24,
           ylab.location = c(0, 0),
           ylab.font = '',
           ylab.color='black'

  ){
    x.trim = image_trim(x, fuzz = 0)
    x.border = image_border(x.trim, color = border.color, geometry = '50x50')
    x1 = image_annotate(x.border, text = tt.text[i], gravity = 'north', location = '+0+20',
                        size=24,  color = title.color, font=title.font)
    x2 = image_annotate(x1, text = tt.text[i], gravity = 'west', location = '+20+00', degrees = -90,
                        size=24,  color = ylab.color, font='Time New Roman')
    # x.trans = image_transparent(x2, color='white', fuzz = 0)
    xx = image_trim(x2, fuzz = 0)
    xx
  }
nf=5
xlist=NULL
for(i in 1:nf){
  fn=fns[idx[i]]
  x = image_read(fn, strip = TRUE)
  x.ready = image.processing(x)
  xlist = c(xlist, x.ready)
}
img = do.call(c, xlist)

nr = 4
nc = 3
tile = paste(nc, 'x', nr)
iout <- image_montage(img, tile = tile, bg = 'pink', gravity = 'southwest',
                      geometry = 'x500+1+5')
image_border
# image_draw()
magick::image_write(image=iout, path = 'test.png', format = 'png')

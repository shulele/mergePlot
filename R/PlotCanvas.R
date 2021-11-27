#' Plot canvas for picture montage.
#'
#' The canvas is defined as rectangle, where xlim=c(0, 1) and ylim=c(0, 1).
#' Resolution, width and height are defined with function magick::image_graph().
#'
#' @param width Width of the picture, see magick::image_graph()
#' @param height Height of the picture, see magick::image_graph()
#' @param res 'res' of the picture, see magick::image_graph()
#' @param col.bg Color of the background
#' @param TEST Whether in test mode.
#' @param dev.off Whethere call dev.off(). If FALSE, you can add more options, such as title/drawing, into the canvas.
#' @importFrom graphics abline axis rect
#' @export
#' @examples
#'
#' x=plotCanvas(TEST=TRUE, col.bg = 'pink')
#' print(x)
#'
#' # --- plot canvas with title or background plot.
#' fig=plotCanvas(TEST=TRUE, col.bg = 'pink', dev.off = FALSE);
#' x=seq(0, 1, length.out=50); y = sin(x*12)/4 +0.25
#' lines(x,  y, col='red')
#' mtext(side=3, 'Sine plot', line=-5, cex=3, col='darkblue')
#' dev.off()
#' plot(fig)
#'
#'
plotCanvas <- function(width=1200, height=900, res=96,
                        col.bg='white', TEST=FALSE,
                        dev.off=TRUE){
  mygrid<-function(x= (0:10)/10, y =x * hw){
    abline(v=x, col='gray', lty=2)
    abline(h=y, col='gray', lty=2)
  }
  xaxs = yaxs = 'i'
  hw = height/width
  xlim = c(0, 1)
  ylim = xlim*hw

  bg <-magick::image_graph(width=width,
                           height=height,
                           res=res)

  par(mar=rep(0, 4))
  plot(xlim, ylim, xlab='', ylab='', type='n',
       axes=FALSE, xlim=xlim, ylim=ylim, asp=1, yaxs=yaxs, xaxs=xaxs)
  # rect(0, 0, xlim[2], ylim[2], col=col.bg, border = col.bg)
  if(TEST){
    xt= (0:10) * 0.1; yt =xt * hw
    rect(0, 0, xlim[2], ylim[2],
         col=col.bg, border = col.bg)
    mygrid()
    axis(side=1, at = xt, labels=xt, line=-4, col='blue', col.axis = 'blue')
    axis(side=3, at = xt, labels=xt, line=-4, col='blue', col.axis = 'blue')
    axis(side=2, at = yt, labels=rev(xt), line=-4, col='blue', col.axis = 'blue')
    axis(side=4, at = yt, labels=rev(xt), line=-4, col='blue', col.axis = 'blue')
  }
  if(dev.off){
    dev.off()
  }
  bg
}



#' Make dummy pictures
#'
#' @param n Number of pictures to plot
#' @param path Directory where the file wil be saved. If path=NULL, return a "magick-image" type.
#' @param axes Whether show the axes, same as axes in plot()
#' @param ... more options, same as png/jpeg when path is NOT null. OR options in magick::image_graph(...) WHEN path=NULL
#' @importFrom grDevices dev.off png terrain.colors
#' @importFrom graphics box grid par plot text
#' @export
#' @examples
#'
#' # width, height and res pass to magick::image_graph()
#' figs=MakePic(axes = TRUE, res=72, width=400, height=400)
#' print(figs)
#'
#' # save pictures in to path 'Figure'
#' figs=MakePic(n=6, axes = TRUE, path ='Figure' )
#' list.files('Figure')
MakePic <- function(n=6,
                    path=NULL,
                    axes=FALSE,
                    ...){
  plotfun <- function(xlim=c(0, 100)){
    col=terrain.colors(n=n)
    par(mfrow=c(1,1), mar=rep(3,4))
    plot(xlim, xlim, xlab='', ylab='', col=i, type='n', axes=axes)
    box()
    text(x=diff(xlim)*0.5, y=diff(xlim)*0.5, cex=6, paste(i), col = col[i])
    grid()
  }
  if(is.null(path)){
    fig = list()
  }else{
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
  for(i in 1:n){
    if(is.null(path)){
      fig[[i]] <- magick::image_graph(...)
      plotfun()
      dev.off()
    } else{
      fn = file.path(path, paste0('Fig.', formatC(i, width=2, flag=0), '.png'))
      message(i, '/', n, '\t', fn)
      png(fn, ...)
      plotfun()
      dev.off()
    }
  }
  if(is.null(path)){
    return( do.call(c, fig) )
  }else{
    # void
  }
}


#' Plot figs onto canvas
#'
#' @param figs  The figures put on the canvas.
#' @param canvas The cavas, which could be from plotCanvas or magick::image_graph()
#' @param location The coordicates (between 0 and 1) in canvas that define the northwest corner of sub-figures. y-axis is DOWNWARD
#' @param TEST Whether in test mode in the plotCanvas. It is invalid when using user-defined canvas.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' library(magrittr)
#' library(magick)
#' sub = MakePic(n=6, width=400, height=300)
#' figs= sub %>% magick::image_trim()
#' bg = plotCanvas(TEST=TRUE, col.bg = 'pink')
#' fig.com = mergePlot(figs,  canvas = bg)
#' plot(fig.com)
#'
#' # ---- More complex examples ----
#' library(magick)
#' sub = MakePic(n=6, width=400, height=300)
#' location = rbind(c( 0.0, 0.0),
#'                  c( 0.3, 0.1),
#'                  c( 0.6, 0.05),
#'                  c( 0.1, 0.4),
#'                  c( 0.3, 0.6),
#'                  c( 0.6, 0.4))
#' resize = seq(0.8, 1.3, 0.1) *100
#' subtitle = paste(resize, '%')
#' figs=list()
#' for(i in 1:length(sub)){
#'   figs[[i]] <- sub[i] %>%
#'     magick::image_trim() %>%
#'     magick::image_annotate(text=subtitle[i], gravity = 'north', size = 40) %>%
#'     magick::image_resize(geometry_size_percent(resize[i]))
#' }
#' figs = do.call(c, figs)
#' fig.com = mergePlot(figs,  canvas = bg, location=location)
#' plot(fig.com)
#' # ---- save files ----
#' #image_write(image=fig.com, format = 'pdf', path='output.pdf')
#'
#' # ---- more examples ----
#' library(magick)
#' sub = MakePic(n=7, width=200, height=300)
#' subtitle = paste('Subfigure', 1:length(sub))
#' figs=list()
#' for(i in 1:length(sub)){
#'   figs[[i]] <- sub[i] %>%
#'     magick::image_annotate(text=subtitle[i], gravity = 'north', size = 20, color = 'black') %>%
#'     magick::image_annotate(text=subtitle[i], gravity = 'west', size = 16, location = '+30+30',
#'                            color = 'darkblue', degrees = -90) %>%
#'     magick::image_trim()
#' }
#' figs = do.call(c, figs)
#' bg=plotCanvas(height = 600, width=600, TEST=TRUE)
#' fig.com = mergePlot(figs, canvas = bg)
#' plot(fig.com)
#' # ---- save files ----
#' #image_write(image=fig.com, format = 'pdf', path='output.pdf')
#'
#'
mergePlot <- function(figs, canvas=plotCanvas(TEST=TEST), location=NULL, TEST=FALSE){
  nf = length(figs)
  if(is.null(location)){
    nr = 2
    nc = ceiling(nf/nr)
    location = expand.grid( (1:nc - 1) /nc,
                            (1:nr - 1) /nr )
  }
  bginfo <- magick::image_info(canvas)
  width =bginfo$width
  height=bginfo$height
  fig.com = canvas
  for(i in 1:nf){
    ofs = paste0('+', location[i, 1] * width,
                 '+', location[i, 2] * height)
    fig.com = magick::image_composite(fig.com,
                                      figs[i],
                                      offset = ofs)
  }
  fig.com
}



rm(list=ls())
library(png)
library(tools)

ArrangePictures <-
  function(fns,
           res = 100,
           inch = 5,
           idx=1:length(fns),
           fn.out = 'Merge.png', #  结果文件名称
           ny = 4,  # 结果图片的行数
           TEST = TRUE, # T 标注位置号码，文件名
           title.text = basename(fns),

           r.text = paste('Row', 1:ny),
           r.cex = 3,  # y轴字体放大倍数
           r.family = 'Times New Roman',  # y轴字体名称
           r.srt = 90,  # y轴字体旋转角度
           r.move = c(0.0, 0.2), # y轴字体水平/垂直移动距离。

           tt.cex = 3,  # 小标题放大倍数
           tt.family = 'Times New Roman',  # 小标题字体名称
           tt.move = c(0.0, -0.05), # 小标题水平/垂直移动距离。

           #' === 小图片的起始点位置比例，或者绝对值 ==== 。
           #' ===假设分辨率500， 左右边框宽度81, 41, 上下宽度81， 101。 ======
           xr = c(0, 1), yr = c(0, 1),

           #' =======图片间隔，x方向和y方向==============
           gap = c(0, 0.1),

           #' =======全图边缘宽度（左右，上下）=============
           mar = c(0, 0, 0,0),
           xaxs = 'r', yaxs = 'r' # see the xaxs/yaxs value in par()
  )
  {
    nf = length(fns)

    nx = ceiling(nf/ny)
    xlim=c(0, nx) + c(-1, 1)*mar[1:2] + c(0, (nx-1) * gap[1])
    ylim=c(0, ny) + c(-1, 1)*mar[3:4] + c(0, (ny-1) * gap[2])

    fns = fns[idx]

    cr = expand.grid(1:nx, 1:ny);
    colnames(cr)  = c('icol', 'irow')
    ll.y = (cr$irow) + (cr$irow-1) * gap[2] -1
    tr.y = ll.y+1
    ll.x = (cr$icol) + (cr$icol-1) * gap[1] -1
    tr.x = ll.x +1
    cc.x = ll.x + (tr.x - ll.x) * 0.5
    cc.y = ll.y + (tr.y - ll.y) * 0.5

    r.x = -0.5 * mar[1]
    r.y = sort(unique(ll.y)) + 0.5
    dim=matrix(0, nrow=nf, ncol=2)
    img=list()
    for(i in 1:nf){
      fn=fns[i]
      img[[i]] = readPNG(source = fn)
      dim[, 1:2] = dim(img[[i]])[1:2]
    }

    df = data.frame(
      id = 1:(ny*nx),
      cr,
      ll.x, ll.y, tr.x, tr.y,
      cc.x, cc.y,
      tt.x = cc.x,
      tt.y = tr.y + gap[2] * 0.5,
      tt.text = title.text,
      fn = fns,
      c1 = round(xr[1] * dim[2]),
      c2 = round(xr[2] * dim[2]),
      r1 = round(yr[1] * dim[1]),
      r2 = round(yr[2] * dim[1])
    )
    df



    fn.ext = tools::file_ext(fn.out)
    if(grepl('pdf', tolower(fn.ext) ) ){
      pdf(fn.out, res=res, unit='in', width = diff(xlim)*inch,  height= diff(ylim)*inch)
    }else if(grepl('png', tolower(fn.ext) ) ){
      png(fn.out, res=res, unit='in', width = diff(xlim)*inch,  height= diff(ylim)*inch)
    }else{
      jpeg(fn.out, res=res, unit='in', width = diff(xlim)*inch,  height= diff(ylim)*inch)
    }
    if(TEST){
      par(mar=rep(2, 4))
      plot(0, 0, xlab='', ylab='', type='n', axes=TRUE, xlim=xlim, ylim=ylim, asp=1, yaxs=yaxs, xaxs=xaxs)
    }else{
      par(mar=rep(0, 4))
      plot(0, 0, xlab='', ylab='', type='n', axes=FALSE, xlim=xlim, ylim=ylim, asp=1, yaxs=yaxs, xaxs=xaxs)
    }
    for(i in 1:nf){
      fn=fns[i]
      message(i, '/', nf,  '(', df[i, 'icol'], ', ', df[i, 'irow'],   ')',    '\t', fn)
      pos = df[i, c('ll.x', 'll.y', 'tr.x', 'tr.y')]
      ic = round(xr[1] * dim[i,2]):round(xr[2] * dim[i,2])
      ir = round(yr[1] * dim[i,1]):round(yr[2] * dim[i,1])
      rasterImage(img[[i]][ir, ic,], pos[1], pos[2], pos[3], pos[4])
      text(df[i, 'tt.x'] + tt.move[1], df[i, 'tt.y'] + tt.move[2],
           df[i, 'tt.text'],  family=tt.family, cex=tt.cex, font=2)
    }
    text(x=r.x+ r.move[1], y=r.y+ r.move[2], label = r.text, pos=2, font=2,
         cex=r.cex, srt=r.srt, family=r.family)
    if(TEST){
      text(cc.x, cc.y, paste(1:nf, '-', basename(fns) ), cex=2, col=2)
      grid()
    }
    dev.off()
    # print(df)
    df

    message('\nOUTPUT FILE = ', fn.out)

  }


r.text = c('RCP 8.1', 'RCP 4.5', 'RCP 2.6', '')

fns = list.files('Figure/', pattern = glob2rx('Fig*.png'), full.names = T)

idx=c(10:12, 7:9, 4:6, 1:3)

#' === 小图片的起始点位置比例，或者绝对值 ==== 。
#' ===假设分辨率500， 左右边框宽度81, 41, 上下宽度81， 101。 ======
xr = c(81, 500-41) / 500
yr = c(81, 500-101) /500


r.text = c('RCP 8.1', 'RCP 4.5', 'RCP 2.6', '')
fns = list.files('Figure/', pattern = glob2rx('Fig*.png'), full.names = T)
idx=c(10:12, 7:9, 4:6, 1:3)

#' === 小图片的起始点位置比例，或者绝对值 ==== 。
#' ===假设分辨率500， 左右边框宽度81, 41, 上下宽度81， 101。 ======
xr = c(81, 500-41) / 500
yr = c(81, 500-101) /500

x=ArrangePictures(fns,
                  idx=idx,
                  xr=xr, yr=yr,
                  gap=c(0.1, 0.3),
                  TEST=F,
                  mar = c(0.1, 0, 0, 0) # 左右，下上
)




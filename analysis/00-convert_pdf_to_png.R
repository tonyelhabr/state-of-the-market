

dir <- 'data-raw'
basename <- '2018-State-of-the-Market-Report'
path_report <- file.path(dir, paste0(basename, '.pdf'))
pdftools::pdf_convert(path_report, verbose = TRUE)

n_pages <- 181

# Dimensions of 1 page.
imgwidth <- 612
imgheight <- 792

# Grid dimensions.
gridwidth <- 15
gridheight <- 13

spacing <- 1
totalwidth <- (imgwidth + spacing) * (gridwidth)
totalheight <- (imgheight + spacing) * gridheight

setwd(dir) # Usually bad practive, but just do it.
png(
  paste0(basename, '.png'),
  width = round((imgwidth + spacing) * gridwidth / 7),
  height = round((imgheight + spacing) * gridheight / 7)
)
par(mar = c(0, 0, 0, 0))
plot(
  0,
  0,
  type = 'n',
  xlim = c(0, totalwidth),
  ylim = c(0, totalheight),
  asp = 1,
  bty = 'n',
  axes = FALSE
)
for (i in 1:n_pages) {
  cat(sprintf('i: %03d', i), sep = '\n')
  fname <- paste(basename , '_', i, '.png', sep = '')
  img <- png::readPNG(fname)

  x <- (i %% gridwidth) * (imgwidth + spacing)
  y <- totalheight - (floor(i / gridwidth)) * (imgheight + spacing)

  rasterImage(
    img,
    xleft = x,
    ybottom = y - imgheight,
    xright = x + imgwidth,
    ytop = y
  )
}
dev.off()


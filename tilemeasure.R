23-5/8
11 13/16

23.625
11.8125

min.cut.desired <- 4 #Inches

room.length <- 66 #Always the longer side, unless square
room.height <- 60
tile.long.side <- 23.625 #Again, longer unless square
tile.short.side <- 11.8125

offset.values <- tile.long.side/3 
toprow.values <- seq(4,6,0.5)
row1.startwidth.values <- seq(3,tile.long.side,0.125)


layout.dt <- CJ(offset = offset.values,
                toprow.height = toprow.values,
                row1.startwidth = row1.startwidth.values)
layout.dt[, bottomrow.height := 12 - toprow.height]
layout.dt <- layout.dt %>% dplyr::relocate(row1.startwidth, .after = last_col()) %>% as.data.table()

allnames <- c("toprow.height",
              "bottomrow.height",
              paste0("row", 1:6, ".startwidth"),
              paste0("row", 1:6, ".fullpieces"),
              paste0("row", 1:6, ".lastwidth"))


# layout.dt <- data.table(matrix(data = as.numeric(1:length(allnames)), nrow = 1, ncol = length(allnames)))
# setnames(layout.dt, allnames)
# layout.dt <- layout.dt[toprow.height == 1e6]

for (i in 1:nrow(layout.dt)) {

  row1.startwidth <- layout.dt[i, row1.startwidth]
  offset <- layout.dt[i, offset]
  toprow.height <- layout.dt[i, toprow.height]
  
  row1.remain <- room.length - row1.startwidth
  row1.full <- floor(row1.remain/tile.long.side)
  row1.remain <- row1.remain %% tile.long.side

  set(layout.dt, i = i, j = "row1.fullpieces", value = row1.full)
  set(layout.dt, i = i, j = "row1.lastwidth", value = row1.remain)

  for (row in 2:6) {
    
    if (row == 2) {
      nextrow.x <- ifelse(row1.startwidth + offset > tile.long.side, row1.startwidth + offset - tile.long.side, row1.startwidth + offset)
    } else {
      nextrow.x <- ifelse(nextrow.x + offset > tile.long.side, nextrow.x + offset - tile.long.side, nextrow.x + offset)
    }
    
    nextrow.remain <- room.length - nextrow.x
    nextrow.full <- floor(nextrow.remain/tile.long.side)
    nextrow.remain <- nextrow.remain %% tile.long.side
    # full.nextrow <- room.length - (nextrow.full*tile.long.side) - nextrow.remain - nextrow.x
    
    set(layout.dt, i = i, j = paste0("row", row, ".startwidth"), value = nextrow.x)
    set(layout.dt, i = i, j = paste0("row", row, ".fullpieces"), value = nextrow.full)
    set(layout.dt, i = i, j = paste0("row", row, ".lastwidth"), value = nextrow.remain)
    
  }# End loop throw tile rows
  
}# End loop through input rows (aaaaaaaaahahaha)

# backup <- copy(layout.dt)

layout.dt[, id := .I]
orig.cols <- ncol(layout.dt) - 1 #to account for id var
layout.dt <- melt(layout.dt, id.vars = "id")
layout.dt[, vartype := str_remove(variable, "row\\d\\.")]
layout.dt[(vartype == "startwidth" & value < 4) |
          (vartype == "lastwidth" & value < 4),
          cutrow := 1]
layout.dt[variable %in% paste0("row", 4:6, ".lastwidth") & value > 20,
          cutrow := 1]
layout.dt <- layout.dt[is.na(cutrow), -"cutrow"]
fulltiles <- layout.dt[vartype == "fullpieces" & variable %nin% c("row1.fullpieces", "row6.fullpieces"), .(fulltiles = sum(value)), by = id]

layout.dt[, N := .N, id]
layout.dt <- layout.dt[N == orig.cols]

layout.dt <- dcast(layout.dt, id ~ variable, value.var = "value")

layout.dt <- fulltiles[layout.dt, on = .(id)]


for (plot.iter in layout.dt[, unique(id)]) {

  aset <- layout.dt[id == plot.iter]
  aset <- melt(aset)
  
  plot.title <- paste0("Offset: ", aset[variable == "offset", value], " inches; Top Row Height: ",
                       aset[variable == "toprow.height", value], " inches; Full Tiles: ", aset[variable == "fulltiles", value])
  
  pieces.per.row <- aset[str_detect(variable, "fullpieces")]
  pieces.per.row[, variable := str_remove_all(variable, "\\.fullpieces")]
  pieces.per.row[, value := value + 2]
  setnames(pieces.per.row, c("row", "rowtiles"))
  
  plot.dt <- data.table(row = paste0("row", rep(1:6, each = 4)), rowmark = 1:4)
  plot.dt <- pieces.per.row[plot.dt, on = .(row)]
  plot.dt <- plot.dt[rowmark <= rowtiles, -"rowtiles"]
  
  plot.toprow <- aset[variable == "toprow.height", value]
  
  row.ymax <- room.height
  for (rowct in 1:6) {
    
    rowheight <- case_when(rowct == 1 ~ plot.toprow,
                           rowct == 6 ~ 12 - plot.toprow,
                           TRUE ~ 12)
    
    plot.dt[row == paste0("row", rowct), ymax := row.ymax]
    plot.dt[row == paste0("row", rowct), ymin := ymax - rowheight]
    
    row.ymax <- row.ymax - rowheight
    
  }
  
  plot.dt[rowmark == 1, xmin := 0]
  plot.dt[, maxrowmark := max(rowmark), by = row]
  plot.dt[rowmark == maxrowmark, xmax := room.length]
  plot.dt[, maxrowmark := NULL]
  
  startwidths <- aset[str_detect(variable, "startwidth")]
  startwidths[, variable := str_remove(variable, "\\.startwidth")]
  setnames(startwidths, c("row", "new.xmin"))
  
  
  #TODO - This section needs to be improved to automatically handle the row values
  
  plot.dt <- startwidths[plot.dt, on = .(row)]
  plot.dt[rowmark == 2, xmin := new.xmin]
  plot.dt[rowmark == 3, xmin := new.xmin + tile.long.side]
  plot.dt[rowmark == 4, xmin := new.xmin + (tile.long.side*2)]
  plot.dt[rowmark == 1, xmax := new.xmin]
  plot.dt[rowmark == 2, xmax := new.xmin + tile.long.side]        
  plot.dt[rowmark == 3, xmax := new.xmin + (tile.long.side*2)]
  plot.dt[xmax > room.length, xmax := room.length]
  
  # plot.dt[, color := rep(c("tan2", "wheat3"), nrow(plot.dt)/2)]
  plot.dt[, color := "wheat3"]
  
  plot.dt[, text.y := mean(c(max(ymax),min(ymax))), by = row]
  plot.dt[, text.x := mean(c(max(xmax),min(xmin))), by = .(row, rowmark)]
  plot.dt[, rowheight := ymax-ymin]
  plot.dt[, rowwidth := xmax-xmin]
  plot.dt[, plottext := paste0("(",rowwidth, ",", rowheight, ")")]
  plot.dt[, text.y := text.y - (rowheight/2)]
  
  saveplot <- ggplot(data = plot.dt, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color)) +
    geom_rect(color = "grey50") +
    geom_text(aes(x = text.x, y = text.y, label = plottext)) +
    scale_x_continuous(breaks = seq(0, floor(room.length/10)*10, 12)) +
    scale_y_continuous(breaks = seq(0, floor(room.height/10)*10, 12)) +
    scale_fill_manual(values = plot.dt$color) +
    labs(title = plot.title, subtitle = "Labels are (width, height)") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank())
  
  ggsave(paste0("output/coletiles/plot", plot.iter, ".png"), plot = saveplot, width = 9.9, height = 9)

}# End plot creation

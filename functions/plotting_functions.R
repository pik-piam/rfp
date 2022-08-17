# This file contains the functions used to plot RFPs and heatmaps

draw_single_rfp_heatmap <- function(data_rfp, x0, y0, r=1, N=100, SUM="TOTAL") {
  
  calculate_circle_coordinates <- function(x0, y0, r, theta_start, theta_end, N=100) {
    # Calculate circle coordinates
    theta <- seq(from = theta_start, to = theta_end, length.out = N)
    circleCoordX <- r * cos(theta) + x0  # calculate x coords
    circleCoordY <- r * sin(theta) + y0  # calculate y coords
    
    # Save as data frame
    if (theta_end - theta_start < pi) {
      circleData <- data.frame(
        X = c(x0, circleCoordX, x0), 
        Y = c(y0, circleCoordY, y0))
    } else {
      circleData <- data.frame(
        X = c(circleCoordX), 
        Y = c(circleCoordY))
    }
    
    return(circleData)
  }
  
  # native R plot functions
  plot_circle <- function(x0, y0, r, theta_start, theta_end, colour, CONTOUR=NULL, N=100) {
    circleData <- calculate_circle_coordinates(x0, y0, r, theta_start, theta_end, N=100) 
    
    if(!is.null(CONTOUR)) {
      polygon(circleData$X, circleData$Y, col=colour, border = CONTOUR)
      #lines(circleData$X, circleData$Y, col=CONTOUR)
    } else {
      polygon(circleData$X, circleData$Y, col=colour, border = NA)
    }
  }
  
  if (!is.na(data_rfp$value[which(data_rfp$rfp == "Direct emissions cost")])) {
    plot_circle(x0, y0, r, pi/2, pi,      colour=data_rfp$colour[which(data_rfp$rfp == "Direct emissions cost")],    CONTOUR="white")
  }
  if (!is.na(data_rfp$value[which(data_rfp$rfp == "Indirect cost")])) {
    plot_circle(x0, y0, r, 0, pi/2,     colour=data_rfp$colour[which(data_rfp$rfp == "Indirect cost")],   CONTOUR="white")
  }
  if (!is.na(data_rfp$value[which(data_rfp$rfp == "Revenue")])) {
    plot_circle(x0, y0, r, pi, pi*3/2, colour=data_rfp$colour[which(data_rfp$rfp == "Revenue")], CONTOUR="white")
  }
  if (!is.na(data_rfp$value[which(data_rfp$rfp == "Low-carbon capital expenditure")])) {
    plot_circle(x0, y0, r, pi*3/2, 2*pi,   colour=data_rfp$colour[which(data_rfp$rfp == "Low-carbon capital expenditure")],  CONTOUR="white")
  }
  
  plot_circle(x0, y0, r*0.8, 0, 2*pi, colour="white", CONTOUR="white")
  
  if (SUM == "TOTAL") {
    plot_circle(x0, y0, r*0.7, 0, 2*pi, colour=data_rfp$colour[which(data_rfp$rfp == "Total")])
    text(x0, y0, labels=paste(round(data_rfp$value[which(data_rfp$rfp == "Total")], digits=0)), col="black", cex=)
  } else {
    plot_circle(x0, y0, r*0.7, 0, 2*pi, colour=data_rfp$colour[which(data_rfp$rfp == "Overall")])
    text(x0, y0, labels=paste(round(data_rfp$value[which(data_rfp$rfp == "Overall")], digits=0)), col="black", cex=)
  }
}

draw_single_rfp_heatmap_diff_abs <- function(data_rfp, x0, y0, r=1, N=100, SUM="TOTAL") {
  
  calculate_circle_coordinates <- function(x0, y0, r, theta_start, theta_end, N=100) {
    # Calculate circle coordinates
    theta <- seq(from = theta_start, to = theta_end, length.out = N)
    circleCoordX <- r * cos(theta) + x0  # calculate x coords
    circleCoordY <- r * sin(theta) + y0  # calculate y coords
    
    # Save as data frame
    if (theta_end - theta_start < pi) {
      circleData <- data.frame(
        X = c(x0, circleCoordX, x0), 
        Y = c(y0, circleCoordY, y0))
    } else {
      circleData <- data.frame(
        X = c(circleCoordX), 
        Y = c(circleCoordY))
    }
    
    return(circleData)
  }
  
  # native R plot functions
  plot_circle <- function(x0, y0, r, theta_start, theta_end, colour, CONTOUR=NULL, N=100) {
    circleData <- calculate_circle_coordinates(x0, y0, r, theta_start, theta_end, N=100) 
    
    if(!is.null(CONTOUR)) {
      polygon(circleData$X, circleData$Y, col=colour, border = CONTOUR)
      #lines(circleData$X, circleData$Y, col=CONTOUR)
    } else {
      polygon(circleData$X, circleData$Y, col=colour, border = NA)
    }
  }
  
  if (!is.na(data_rfp$value[which(data_rfp$rfp == "Direct emissions cost")])) {
    plot_circle(x0, y0, r, pi/2, pi,      colour=data_rfp$colour[which(data_rfp$rfp == "Direct emissions cost")],    CONTOUR="white")
  }
  if (!is.na(data_rfp$value[which(data_rfp$rfp == "Indirect cost")])) {
    plot_circle(x0, y0, r, 0, pi/2,     colour=data_rfp$colour[which(data_rfp$rfp == "Indirect cost")],   CONTOUR="white")
  }
  # if (!is.na(data_rfp$value[which(data_rfp$rfp == "Revenue")])) {
  #   plot_circle(x0, y0, r, pi, pi*3/2, colour=data_rfp$colour[which(data_rfp$rfp == "Revenue")], CONTOUR="white")
  # }
  if (!is.na(data_rfp$value[which(data_rfp$rfp == "Low-carbon capital expenditure")])) {
    plot_circle(x0, y0, r, pi*3/2, 2*pi,   colour=data_rfp$colour[which(data_rfp$rfp == "Low-carbon capital expenditure")],  CONTOUR="white")
  }
  
  plot_circle(x0, y0, r*0.8, 0, 2*pi, colour="white", CONTOUR="white")
  
  if (SUM == "TOTAL") {
    plot_circle(x0, y0, r*0.7, 0, 2*pi, colour=data_rfp$colour[which(data_rfp$rfp == "Total")])
    text(x0, y0, labels=paste(round(data_rfp$value[which(data_rfp$rfp == "Total")], digits=0)), col="black", cex=)
  } else {
    plot_circle(x0, y0, r*0.7, 0, 2*pi, colour=data_rfp$colour[which(data_rfp$rfp == "Overall")])
    text(x0, y0, labels=paste(round(data_rfp$value[which(data_rfp$rfp == "Overall")], digits=0)), col="black", cex=)
  }
}

draw_rfp_heatmap_world <- function(i_data, i_sector, i_scenario, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_scenario <- length(i_scenario)
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      sector %in% i_sector,
      scenario %in% i_scenario) %>% 
    rename(value = diff_rel) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours     <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values <- c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_scenario-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kscen in i_scenario) {
    print(kscen)
    j <- 0
    for (ksec in i_sector) {
      print(ksec)
      draw_single_rfp_heatmap(v_dataplot %>% filter(region == "World", sector == ksec, scenario == kscen), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=ksec, 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_scenario-1)*2, 2), labels = i_scenario, tick = FALSE)
  
}

draw_rfp_heatmap_world_byModel <- function(i_data, i_sector, i_model, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_model  <- length(i_model)
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      region == "World",
      sector %in% i_sector,
      model %in% i_model) %>% 
    rename(value = diff_rel) 
  
  print(v_data)
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours     <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values <- c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_model-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kmod in i_model) {
    print(kmod)
    j <- 0
    for (ksec in i_sector) {
      print(ksec)
      draw_single_rfp_heatmap(v_dataplot %>% filter(region == "World", sector == ksec, model == kmod), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=ksec, 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_model-1)*2, 2), labels = i_model, tick = FALSE)
  
}

draw_rfp_heatmap_world_diff_abs <- function(i_data, i_sector, i_scenario, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_scenario <- length(i_scenario)
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      sector %in% i_sector,
      scenario %in% i_scenario) %>% 
    rename(value = diff_abs) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours    <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values  <- c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_scenario-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kscen in i_scenario) {
    print(kscen)
    j <- 0
    for (ksec in i_sector) {
      print(ksec)
      draw_single_rfp_heatmap_diff_abs(v_dataplot %>% filter(region == "World", sector == ksec, scenario == kscen), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=ksec, 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_scenario-1)*2, 2), labels = i_scenario, tick = FALSE)
  
}

draw_rfp_heatmap_world_diff_abs_byModel <- function(i_data, i_sector, i_model, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_model  <- length(i_model)
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      sector %in% i_sector,
      model %in% i_model) %>% 
    rename(value = diff_abs) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours    <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  #values  <- c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  values  <- c(-1000000, -50, -25, -10, -5, -1, 0, 1, 5, 10, 25, 50, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_model-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kmod in i_model) {
    print(kmod)
    j <- 0
    for (ksec in i_sector) {
      print(ksec)
      draw_single_rfp_heatmap_diff_abs(v_dataplot %>% filter(region == "World", sector == ksec, model == kmod), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=ksec, 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_model-1)*2, 2), labels = i_model, tick = FALSE)
  
}

draw_rfp_heatmap_region_byModel <- function(i_data, i_sector, i_region, i_model, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_model  <- length(i_model)
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      region == i_region,
      sector %in% i_sector,
      model %in% i_model) %>% 
    rename(value = diff_rel) 
  
  print(v_data)
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours     <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values <- c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_model-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kmod in i_model) {
    print(kmod)
    j <- 0
    for (ksec in i_sector) {
      print(ksec)
      draw_single_rfp_heatmap(v_dataplot %>% filter(region == i_region, sector == ksec, model == kmod), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=ksec, 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_model-1)*2, 2), labels = i_model, tick = FALSE)
  
}

draw_rfp_heatmap_region_diff_abs_byModel <- function(i_data, i_sector, i_region, i_model, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_model  <- length(i_model)
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      sector %in% i_sector,
      model %in% i_model) %>% 
    rename(value = diff_abs) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours    <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values  <- c(-1000000, -20, -10, -5, -2.5, -1, 0, 1, 2.5, 5, 10, 20, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_model-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kmod in i_model) {
    print(kmod)
    j <- 0
    for (ksec in i_sector) {
      print(ksec)
      draw_single_rfp_heatmap_diff_abs(v_dataplot %>% filter(region == i_region, sector == ksec, model == kmod), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=ksec, 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_model-1)*2, 2), labels = i_model, tick = FALSE)
  
}

plot_rfp_pathways <- function(i_data, i_sector, i_scenario, i_region, HORIZON=2050, DISCOUNT=0.0, PRINT=TRUE) {
  
  tmp <- i_data %>% 
    filter(period >= 2020, period <= HORIZON, 
           sector == i_sector,
           scenario == i_scenario,
           region == i_region) %>% 
    mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
    mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
    mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))
  
  ymin <- min(tmp$diff_abs, na.rm = TRUE)
  ymax <- max(tmp$diff_abs, na.rm = TRUE)
  
  x_ref <- 2020 + (HORIZON - 2020)*seq(0.85, 1.0, length.out=4)
  y_ref <- ymin + (ymax - ymin)*1.0
  
  p <- ggplot() +
    geom_segment(aes(x=2020, xend=HORIZON, y=0, yend=0), colour="black")
  
  if (all(is.na(tmp$diff_abs[which(grepl("Direct", tmp$rfp))]))) {
    p <- p + 
      geom_point(aes(x=x_ref[1], y=y_ref), pch=21, size=5, color="grey", fill="white") +
      geom_text(aes(x=x_ref[1], y=y_ref, label="D"), color="grey")
  } else {
    p <- p + 
      geom_point(aes(x=x_ref[1], y=y_ref), pch=21, size=5, color="black", fill=RColorBrewer::brewer.pal(4, "Set1")[1]) +
      geom_text(aes(x=x_ref[1], y=y_ref, label="D"), color="white")
  }
  if (all(is.na(tmp$diff_abs[which(grepl("Indirect", tmp$rfp))]))) {
    p <- p + 
      geom_point(aes(x=x_ref[2], y=y_ref), pch=21, size=5, color="grey", fill="white") +
      geom_text(aes(x=x_ref[2], y=y_ref, label="I"), color="grey")
  } else {
    p <- p + 
      geom_point(aes(x=x_ref[2], y=y_ref), pch=21, size=5, color="black", fill=RColorBrewer::brewer.pal(4, "Set1")[2]) +
      geom_text(aes(x=x_ref[2], y=y_ref, label="I"), color="white")
  }
  if (all(is.na(tmp$diff_abs[which(grepl("Low-carbon", tmp$rfp))]))) {
    p <- p + 
      geom_point(aes(x=x_ref[3], y=y_ref), pch=21, size=5, color="grey", fill="white") +
      geom_text(aes(x=x_ref[3], y=y_ref, label="L"), color="grey")
  } else {
    p <- p + 
      geom_point(aes(x=x_ref[3], y=y_ref), pch=21, size=5, color="black", fill=RColorBrewer::brewer.pal(4, "Set1")[3]) +
      geom_text(aes(x=x_ref[3], y=y_ref, label="L"), color="white")
  }
  if (all(is.na(tmp$diff_abs[which(grepl("Revenue", tmp$rfp))]))) {
    p <- p + 
      geom_point(aes(x=x_ref[4], y=y_ref), pch=21, size=5, color="grey", fill="white") +
      geom_text(aes(x=x_ref[4], y=y_ref, label="R"), color="grey")
  } else {
    p <- p + 
      geom_point(aes(x=x_ref[4], y=y_ref), pch=21, size=5, color="black", fill=RColorBrewer::brewer.pal(4, "Set1")[4]) +
      geom_text(aes(x=x_ref[4], y=y_ref, label="R"), color="white")
  }
  
  
  p <- p +
    geom_line(aes(x=period, y=diff_abs, color=rfp), 
              lwd=1.25,
              data=i_data %>% 
                filter(period >= 2020, period <= HORIZON, 
                       grepl("Direct|Indirect|Low-carbon|Revenue", rfp),
                       sector == i_sector,
                       scenario == i_scenario,
                       region == i_region) %>% 
                mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    geom_line(aes(x=period, y=diff_abs, color=rfp), 
              lwd=1.5,
              lty=3,
              data=i_data %>% 
                filter(period >= 2020, period <= HORIZON, 
                       rfp == "Diagnostics|RFP|Total",
                       sector == i_sector,
                       scenario == i_scenario,
                       region == i_region) %>% 
                mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    geom_point(aes(x=period, y=diff_abs),
               color = "white",
               pch=16, size=3,
               data=i_data %>% 
                 filter(period >= 2020, period <= HORIZON, 
                        rfp == "Diagnostics|RFP|Total",
                        sector == i_sector,
                        scenario == i_scenario,
                        region == i_region) %>% 
                 mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                 mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                 mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    geom_point(aes(x=period, y=diff_abs),
               color = "black",
               pch=16, size=2,
               data=i_data %>% 
                 filter(period >= 2020, period <= HORIZON, 
                        rfp == "Diagnostics|RFP|Total",
                        sector == i_sector,
                        scenario == i_scenario,
                        region == i_region) %>% 
                 mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                 mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                 mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    geom_line(aes(x=period, y=diff_abs, color=rfp), 
              lwd=1.4,
              lty=2,
              data=i_data %>% 
                filter(period >= 2020, period <= HORIZON, 
                       rfp == "Diagnostics|RFP|Overall",
                       sector == i_sector,
                       scenario == i_scenario,
                       region == i_region) %>% 
                mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    #facet_wrap(sector~scenario, scales = "free_y") +
    xlab("") + ylab("[billion $US]") +
    scale_color_manual(name = "RFP", 
                       values = c(
                         "Direct emissions cost" = RColorBrewer::brewer.pal(4, "Set1")[1],
                         "Indirect cost" = RColorBrewer::brewer.pal(4, "Set1")[2],
                         "Low-carbon capital expenditure" = RColorBrewer::brewer.pal(4, "Set1")[3],
                         "Revenue" = RColorBrewer::brewer.pal(4, "Set1")[4],
                         "Total" = "#000000",
                         "Overall" = "#666666")) +
    coord_cartesian(ylim = c(ymin, ymax), expand = FALSE, clip = "off") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(1,1,1,1), units = "cm"),
      panel.grid = element_blank(),
      axis.ticks = element_line(),
      axis.line = element_line(size = 0.2),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.position = "bottom")
  
  if (PRINT) print(p)
  
  return(p)
}

plot_rfp_pathways_wtotal <- function(i_data, i_sector, i_scenario, i_region, HORIZON=2050, DISCOUNT=0.0, PRINT=TRUE, TOTAL=TRUE) {
  
  tmp <- i_data %>% 
    filter(period >= 2020, period <= HORIZON, 
           sector == i_sector,
           scenario == i_scenario,
           region == i_region) %>% 
    mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
    mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
    mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))
  
  ymin <- min(tmp$diff_abs, na.rm = TRUE)
  ymax <- max(tmp$diff_abs, na.rm = TRUE)
  
  x_ref <- 2020 + (HORIZON - 2020)*seq(0.85, 1.0, length.out=4)
  y_ref <- ymin + (ymax - ymin)*1.0
  
  p <- ggplot() +
    geom_segment(aes(x=2020, xend=HORIZON, y=0, yend=0), colour="black")
  
  if (all(is.na(tmp$diff_abs[which(grepl("Direct", tmp$rfp))]))) {
    p <- p + 
      geom_point(aes(x=x_ref[1], y=y_ref), pch=21, size=5, color="grey", fill="white") +
      geom_text(aes(x=x_ref[1], y=y_ref, label="D"), color="grey")
  } else {
    p <- p + 
      geom_point(aes(x=x_ref[1], y=y_ref), pch=21, size=5, color="black", fill=RColorBrewer::brewer.pal(4, "Set1")[1]) +
      geom_text(aes(x=x_ref[1], y=y_ref, label="D"), color="white")
  }
  if (all(is.na(tmp$diff_abs[which(grepl("Indirect", tmp$rfp))]))) {
    p <- p + 
      geom_point(aes(x=x_ref[2], y=y_ref), pch=21, size=5, color="grey", fill="white") +
      geom_text(aes(x=x_ref[2], y=y_ref, label="I"), color="grey")
  } else {
    p <- p + 
      geom_point(aes(x=x_ref[2], y=y_ref), pch=21, size=5, color="black", fill=RColorBrewer::brewer.pal(4, "Set1")[2]) +
      geom_text(aes(x=x_ref[2], y=y_ref, label="I"), color="white")
  }
  if (all(is.na(tmp$diff_abs[which(grepl("Low-carbon", tmp$rfp))]))) {
    p <- p + 
      geom_point(aes(x=x_ref[3], y=y_ref), pch=21, size=5, color="grey", fill="white") +
      geom_text(aes(x=x_ref[3], y=y_ref, label="L"), color="grey")
  } else {
    p <- p + 
      geom_point(aes(x=x_ref[3], y=y_ref), pch=21, size=5, color="black", fill=RColorBrewer::brewer.pal(4, "Set1")[3]) +
      geom_text(aes(x=x_ref[3], y=y_ref, label="L"), color="white")
  }
  if (all(is.na(tmp$diff_abs[which(grepl("Revenue", tmp$rfp))]))) {
    p <- p + 
      geom_point(aes(x=x_ref[4], y=y_ref), pch=21, size=5, color="grey", fill="white") +
      geom_text(aes(x=x_ref[4], y=y_ref, label="R"), color="grey")
  } else {
    p <- p + 
      geom_point(aes(x=x_ref[4], y=y_ref), pch=21, size=5, color="black", fill=RColorBrewer::brewer.pal(4, "Set1")[4]) +
      geom_text(aes(x=x_ref[4], y=y_ref, label="R"), color="white")
  }
  
  
  p <- p +
    geom_line(aes(x=period, y=diff_abs, color=rfp), 
              lwd=1.25,
              data=i_data %>% 
                filter(period >= 2020, period <= HORIZON, 
                       grepl("Direct|Indirect|Low-carbon|Revenue", rfp),
                       sector == i_sector,
                       scenario == i_scenario,
                       region == i_region) %>% 
                mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020)))
  if (TOTAL) { 
    p <- p + 
      geom_line(aes(x=period, y=diff_abs, color=rfp), 
                lwd=1.5,
                lty=3,
                data=i_data %>% 
                  filter(period >= 2020, period <= HORIZON, 
                         rfp == "Diagnostics|RFP|Total",
                         sector == i_sector,
                         scenario == i_scenario,
                         region == i_region) %>% 
                  mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                  mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                  mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
      geom_point(aes(x=period, y=diff_abs),
                 color = "white",
                 pch=16, size=3,
                 data=i_data %>% 
                   filter(period >= 2020, period <= HORIZON, 
                          rfp == "Diagnostics|RFP|Total",
                          sector == i_sector,
                          scenario == i_scenario,
                          region == i_region) %>% 
                   mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                   mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                   mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
      geom_point(aes(x=period, y=diff_abs),
                 color = "black",
                 pch=16, size=2,
                 data=i_data %>% 
                   filter(period >= 2020, period <= HORIZON, 
                          rfp == "Diagnostics|RFP|Total",
                          sector == i_sector,
                          scenario == i_scenario,
                          region == i_region) %>% 
                   mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                   mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                   mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020)))
  }
  
  p <- p +
    geom_line(aes(x=period, y=diff_abs, color=rfp), 
              lwd=1.4,
              lty=2,
              data=i_data %>% 
                filter(period >= 2020, period <= HORIZON, 
                       rfp == "Diagnostics|RFP|Overall",
                       sector == i_sector,
                       scenario == i_scenario,
                       region == i_region) %>% 
                mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    #facet_wrap(sector~scenario, scales = "free_y") +
    xlab("") + ylab("[billion $US]") +
    scale_color_manual(name = "RFP", 
                       values = c(
                         "Direct emissions cost" = RColorBrewer::brewer.pal(4, "Set1")[1],
                         "Indirect cost" = RColorBrewer::brewer.pal(4, "Set1")[2],
                         "Low-carbon capital expenditure" = RColorBrewer::brewer.pal(4, "Set1")[3],
                         "Revenue" = RColorBrewer::brewer.pal(4, "Set1")[4],
                         "Total" = "#000000",
                         "Overall" = "#666666")) +
    coord_cartesian(ylim = c(ymin, ymax), expand = FALSE, clip = "off") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(1,1,1,1), units = "cm"),
      panel.grid = element_blank(),
      axis.ticks = element_line(),
      axis.line = element_line(size = 0.2),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.position = "bottom")
  
  if (PRINT) print(p)
  
  return(p)
}


# Doesn't work for some reason...
plot_rfp_pathways_grid <- function(i_data, i_region, HORIZON=2050, DISCOUNT=0.0, PRINT=TRUE) {
  p <- ggplot() +
    geom_segment(aes(x=2020, xend=HORIZON, y=0, yend=0), colour="black") +
    geom_line(aes(x=period, y=diff_abs, color=rfp), 
              lwd=1.25,
              data=i_data %>% 
                filter(period >= 2020, period <= HORIZON, 
                       grepl("Direct|Indirect|Low-carbon|Revenue", rfp),
                       #sector == i_sector,
                       #scenario == i_scenario,
                       region == i_region) %>% 
                mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    geom_line(aes(x=period, y=diff_abs, color=rfp), 
              lwd=1.5,
              lty=3,
              data=i_data %>% 
                filter(period >= 2020, period <= HORIZON, 
                       rfp == "Diagnostics|RFP|Total",
                       #sector == i_sector,
                       #scenario == i_scenario,
                       region == i_region) %>% 
                mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    geom_point(aes(x=period, y=diff_abs),
               color = "white",
               pch=16, size=3,
               data=i_data %>% 
                 filter(period >= 2020, period <= HORIZON, 
                        rfp == "Diagnostics|RFP|Total",
                        #sector == i_sector,
                        #scenario == i_scenario,
                        region == i_region) %>% 
                 mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                 mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                 mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    geom_point(aes(x=period, y=diff_abs),
               color = "black",
               pch=16, size=2,
               data=i_data %>% 
                 filter(period >= 2020, period <= HORIZON, 
                        rfp == "Diagnostics|RFP|Total",
                        #sector == i_sector,
                        #scenario == i_scenario,
                        region == i_region) %>% 
                 mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                 mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                 mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    geom_line(aes(x=period, y=diff_abs, color=rfp), 
              lwd=1.4,
              lty=2,
              data=i_data %>% 
                filter(period >= 2020, period <= HORIZON, 
                       rfp == "Diagnostics|RFP|Overall",
                       #sector == i_sector,
                       #scenario == i_scenario,
                       region == i_region) %>% 
                mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp)) %>% 
                mutate(sector = gsub("Energy Supply\\|", "", sector)) %>% 
                mutate(diff_abs = diff_abs*1/(1+DISCOUNT)^(period - 2020))) +
    #facet_wrap(sector~scenario, scales = "free_y") +
    xlab("") + ylab("[billion $US]") +
    scale_color_manual(name = "RFP", 
                       values = c(
                         "Direct emissions cost" = RColorBrewer::brewer.pal(4, "Set1")[1],
                         "Indirect cost" = RColorBrewer::brewer.pal(4, "Set1")[2],
                         "Low-carbon capital expenditure" = RColorBrewer::brewer.pal(4, "Set1")[3],
                         "Revenue" = RColorBrewer::brewer.pal(4, "Set1")[4],
                         "Total" = "#000000",
                         "Overall" = "#666666")) +
    facet_grid(scenario~sector) +
    coord_cartesian(expand = FALSE) +
    theme_minimal() +
    theme(
      plot.margin = unit(c(1,1,1,1), units = "cm"),
      panel.grid = element_blank(),
      axis.ticks = element_line(),
      axis.line = element_line(size = 0.2),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),,
      legend.title = element_text(size = 14),
      legend.position = "bottom")
  
  if (PRINT) print(p)
  
  return(p)
}

draw_rfp_heatmap_regions <- function(i_data, i_sector, i_scenario, SUM=sum_type) {
  
  n_sector <- length(i_sector)
  n_scenario <- length(i_scenario)
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      sector %in% i_sector,
      scenario %in% i_scenario) %>% 
    rename(value = diff_rel) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours     <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values <- c(-5000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 5000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_scenario-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kreg in c("People's Republic of China" , "European Union (28 member countries)", "Republic of India" , "United States of America")) {
    j <- 0
    for (ksec in i_sector) {
      draw_single_rfp_heatmap(v_dataplot %>% filter(region == kreg, sector == ksec, scenario == i_scenario), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=ksec, 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(4-1)*2, 2), labels = c("CHN", "EUR", "IND", "USA"), tick = FALSE)
  
}

draw_rfp_heatmap_legend <- function(i_values, i_colours) {
  
  plot(0,0, type="n", 
       xlab = "", ylab = "",
       xlim=c(i_values[2], i_values[length(i_values)-1]), axes=FALSE)
  
  for (kc in 2:(length(i_colours)-1)) {
    rect(i_values[kc], -0.1, i_values[kc+1], 0.1, col = i_colours[kc])
  }
  
  axis(1, at = i_values[2:length(i_values)-1])
  
}


draw_rfp_heatmap_world_byModel_forReport <- function(i_data, i_sector, i_model, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_model  <- length(i_model)
  
  label_sector_common <- LCSn(i_sector)
  label_model <- i_model
  label_model[which(grepl("GCAM", label_model))]    <- "G"
  label_model[which(grepl("MESSAGE", label_model))] <- "MG"
  label_model[which(grepl("REMIND", label_model))]  <- "RM"
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      region == "World",
      sector %in% i_sector,
      model %in% i_model) %>% 
    rename(value = diff_rel) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours     <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values <- c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_model-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kmod in i_model) {
    j <- 0
    for (ksec in i_sector) {
      draw_single_rfp_heatmap(v_dataplot %>% filter(region == "World", sector == ksec, model == kmod), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=substr(ksec, nchar(label_sector_common)+1, nchar(ksec)), 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_model-1)*2, 2), labels = label_model, tick = FALSE)
  
}

draw_rfp_heatmap_region_byModel_forReport <- function(i_data, i_sector, i_model, i_region, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_model  <- length(i_model)
  
  label_sector_common <- LCSn(i_sector)
  label_model <- i_model
  label_model[which(grepl("GCAM", label_model))]    <- "G"
  label_model[which(grepl("MESSAGE", label_model))] <- "MG"
  label_model[which(grepl("REMIND", label_model))]  <- "RM"
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      region %in% i_region,
      sector %in% i_sector,
      model %in% i_model) %>% 
    rename(value = diff_rel) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours     <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values <- c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_model-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kmod in i_model) {
    j <- 0
    kregion <- grep(kmod, i_region, value=TRUE)
    for (ksec in i_sector) {
      draw_single_rfp_heatmap(v_dataplot %>% filter(region == kregion, sector == ksec, model == kmod), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=substr(ksec, nchar(label_sector_common)+1, nchar(ksec)), 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_model-1)*2, 2), labels = label_model, tick = FALSE)
  
}


draw_rfp_heatmap_world_diff_abs_byModel_forReport <- function(i_data, i_sector, i_model, SUM="TOTAL") {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_model  <- length(i_model)
  
  label_sector_common <- LCSn(i_sector)
  label_model <- i_model
  label_model[which(grepl("GCAM", label_model))]    <- "G"
  label_model[which(grepl("MESSAGE", label_model))] <- "MG"
  label_model[which(grepl("REMIND", label_model))]  <- "RM"
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      sector %in% i_sector,
      model %in% i_model) %>% 
    rename(value = diff_abs) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours    <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values  <- c(-1000000, -20, -10, -5, -2.5, -1, 0, 1, 2.5, 5, 10, 20, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_model-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kmod in i_model) {
    #print(kmod)
    j <- 0
    for (ksec in i_sector) {
      #print(ksec)
      draw_single_rfp_heatmap_diff_abs(v_dataplot %>% filter(region == "World", sector == ksec, model == kmod), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=substr(ksec, nchar(label_sector_common)+1, nchar(ksec)), 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_model-1)*2, 2), labels = label_model, tick = FALSE)
  
}


draw_rfp_heatmap_region_diff_abs_byModel_forReport <- function(i_data, i_sector, i_model, i_region, SUM="TOTAL",
                                                               SCALE=c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)) {
  
  sum_type <- SUM
  
  n_sector <- length(i_sector)
  n_model  <- length(i_model)
  
  label_sector_common <- LCSn(i_sector)
  label_model <- i_model
  label_model[which(grepl("GCAM", label_model))]    <- "G"
  label_model[which(grepl("MESSAGE", label_model))] <- "MG"
  label_model[which(grepl("REMIND", label_model))]  <- "RM"
  
  v_data <- i_data %>% 
    rfp_separate(REMOVE_PREFIX = TRUE) %>% 
    filter(
      sector %in% i_sector,
      model %in% i_model) %>% 
    rename(value = diff_abs) 
  
  # Compute color scale based on input rfp data
  ncol <- 11
  colfunc_rw <- colorRampPalette(c("#e41a1c", "#ffffff"))
  colfunc_wg <- colorRampPalette(c("#ffffff", "#4daf4a"))
  colours    <- c(colfunc_rw(ncol), colfunc_wg(ncol))
  
  abs_max <- 100 #max(abs(v_data$value), na.rm = TRUE)
  #values  <- c(seq(-abs_max, 0, length.out = ncol+1), seq(0, abs_max, length.out = ncol+1)[2:(ncol+1)]) 
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- c(-1000000, -500, -100, -50, -25, -10, 0, 10, 25, 50, 100, 500, 1000000)
  # colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  # values  <- SCALE #c(-1000000, -100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100, 1000000)
  colours <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  values  <- c(-1000000, -20, -10, -5, -2.5, -1, 0, 1, 2.5, 5, 10, 20, 1000000)
  
  v_dataplot <- v_data %>% 
    mutate(colour = paste(cut(value, 
                              breaks = values, 
                              labels = colours)))
  
  
  
  # Initialise heat map
  par(las = 1)
  plot(0,0, type="n", xlim=c(-1, 1+(n_model-1)*2), ylim = c(-1, 1+(n_sector-1)*2), asp = 1, las = 1, xlab = "", ylab = "", axes = FALSE)
  
  # Plot individual element (sector x scenario)
  i <- 0
  for (kmod in i_model) {
    #print(kmod)
    j <- 0
    kregion <- grep(kmod, i_region, value=TRUE)
    for (ksec in i_sector) {
      #print(ksec)
      draw_single_rfp_heatmap_diff_abs(v_dataplot %>% filter(region == kregion, sector == ksec, model == kmod), i, j, SUM=SUM)
      if (i == 0) text(i-1.5, j, labels=substr(ksec, nchar(label_sector_common)+1, nchar(ksec)), 1, xpd=NA)
      j <- j + 2
    }
    i <- i + 2
  }
  
  axis(3, at = seq(0, 1+(n_model-1)*2, 2), labels = label_model, tick = FALSE)
  
}


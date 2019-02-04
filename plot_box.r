setwd("/Users/ylao/repo/r_visulize")

plot_significance <- function(src_session, dst_session, significance, session_names) {
    # Only plot for significance == 0, 1, 2, 3, 4
    significance_str = c("n.s.", "*", "**", "***", "****")[significance + 1]
    
    # Get x cooredinate
    src_x = match(src_session, session_names)
    dst_x = match(dst_session, session_names)
    mid_x = (src_x + dst_x) / 2
    
    # Get session type
    # E.g. "E1-T" -> "T", "E3-R" -> "R"
    src_type = substr(src_session, nchar(src_session), nchar(src_session))
    dst_type = substr(dst_session, nchar(src_session), nchar(src_session))
    
    y_top_base = 1.13
    vertical_gap = 0.0125
    if ((src_type == "T" && dst_type == "R") || (src_type == "R" && dst_type == "T")) {
        y_base = y_top_base
    } else if ((src_type == "R" && dst_type == "N") || (src_type == "N" && dst_type == "R")) {
        y_base = y_top_base - 2.8 * vertical_gap
    } else if ((src_type == "T" && dst_type == "N") || (src_type == "N" && dst_type == "T")) {
        y_base = y_top_base - 5.6 * vertical_gap
    } else {
        print(src_type)
        print(dst_type)
    }
    
    # Plot line
    lines(c(src_x, dst_x), c(y_base, y_base), xpd = NA) 
    lines(c(src_x, src_x), c(y_base, y_base - vertical_gap), xpd = NA)
    lines(c(dst_x, dst_x), c(y_base, y_base - vertical_gap), xpd = NA)
    
    # Plot text
    text(mid_x, y_base + vertical_gap, significance_str, xpd = NA)
}

plot_nine_session_vs_value <- function(session_value_list, metric_name){
    # Inputs:
    # - session_value_list:
    #     c("factor_name_1": c(value_1, value_2, value_3, ...),
    #       "factor_name_2": c(value_1, value_2, value_3, ...),
    #       ...)
    # - metric_name:
    #     e.g. "Percentage Time Active"
    
    # Prepare metric data frame
    session_names = names(session_value_list)
    sessions = c()
    for (session_name in session_names) {
        sessions = c(sessions, rep(session_name, length(session_value_list[[session_name]])))
    }
    values = c()
    for (session_name in session_names) {
        values = c(values, session_value_list[[session_name]])
    }
    
    metric = data.frame(factor=sessions, value=values)
    colnames(metric) = c("Session", "Value")
    metric$Session = factor(metric$Session, levels=session_names)

    y_range = c(0, 1)
    
    col.pool = c(
        "indianred3",
        "light blue",
        "grey80",
        "indianred3",
        "light blue",
        "grey80",
        "indianred3",
        "light blue",
        "grey80"
    )
    
    # Main plots
    boxplot(
        Value ~ Session,
        data = metric,
        ylim = y_range,
        outline = F,
        notch = F,
        lwd = 1,
        ylab = metric_name,
        xlab = "",
        medlwd = 1,
        # boxwex = 1,
        xaxt = "n"
        # axes = F
    )
    # axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
    stripchart(
        Value ~ Session,
        vertical = TRUE,
        data = metric,
        method = "jitter",
        add = TRUE,
        pch = 15,
        cex = 0.5,
        col =  col.pool
    )
    
    # Plot # of flies on top
    num_flies_per_session = as.data.frame(table(metric[!is.na(metric$Value),]$Session))$Freq
    text(x = (1:length(num_flies_per_session)) - 0.2,
         y = 1.17,
         num_flies_per_session,
         xpd = TRUE,
         srt = 0,
         adj = 0
    )
    
    # Plot session name on buttom
    num_flies_per_session = as.data.frame(table(metric[!is.na(metric$Value),]$Session))$Freq
    text(x = (1:length(num_flies_per_session)) - 0.2,
         y = -0.1,
         session_names,
         xpd = NA,
         srt = 0,
         adj = 0
    )
    
    # Plot vertical separators
    for (j in c(3, 6)) {
        lines(c(j, j) + 0.5,
              c(y_range[1] - 1e3, y_range[1] + 1e3),
              col = "light grey",
              lty = 1)
    }
    
    # New: significance plot
    plot_significance("E1-T", "E1-R", 0, session_names)
    plot_significance("E1-R", "E1-N", 1, session_names)
    plot_significance("E1-T", "E1-N", 2, session_names)
}

session_value_list = readRDS(file="arrays.rds")
metric_name = "Percentage Time Active"
plot_nine_session_vs_value(session_value_list, metric_name)

# pdf(paste0("my_plot.pdf"), onefile = T, width = 8)
# dev.off()


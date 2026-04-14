# Standalone helpers for monthly passenger change plots.
# This file keeps the legacy Rmd untouched and provides a version that can
# work with both the old `df_bus` schema and `E:/0408俊典/115期中.RData`.

# libraries used in this file: dplyr, tidyr, ggplot2, openxlsx, lubridate, scales
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)

normalize_month <- function(month_str) {
    month_num <- suppressWarnings(as.integer(month_str))

    if (is.na(month_num)) {
        stop(sprintf("Invalid month input: %s", month_str))
    }

    if (month_num < 1 || month_num > 12) {
        stop(sprintf("Invalid month input: %s", month_str))
    }

    sprintf("%02d", month_num)
}

check_path <- function(path) {
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
}

resolve_0414_base_dir <- local({
    cached_base_dir <- NULL

    function() {
        if (!is.null(cached_base_dir)) {
            return(cached_base_dir)
        }

        frame_ofiles <- vapply(
            sys.frames(),
            function(env) {
                ofile <- env$ofile
                if (is.null(ofile)) "" else as.character(ofile)
            },
            character(1)
        )
        frame_ofiles <- frame_ofiles[nzchar(frame_ofiles)]

        if (length(frame_ofiles) > 0) {
            cached_base_dir <<- dirname(normalizePath(frame_ofiles[length(frame_ofiles)], winslash = "/", mustWork = FALSE))
            return(cached_base_dir)
        }

        fallback_path <- "D:/Github/115_Midterm-Report/TPASS/0414.r"
        cached_base_dir <<- dirname(normalizePath(fallback_path, winslash = "/", mustWork = FALSE))
        cached_base_dir
    }
})

get_chinese_plot_family <- local({
    font_initialized <- FALSE
    font_family <- "Kai"

    function() {
        if (!font_initialized) {
            font_initialized <<- TRUE

            if (.Platform$OS.type == "windows") {
                try(grDevices::windowsFonts(Kai = grDevices::windowsFont("Microsoft JhengHei")), silent = TRUE)
            }
        }

        font_family
    }
})

ensure_extension <- function(filename, extension) {
    extension <- paste0(".", extension)

    if (is.null(filename) || !nzchar(filename)) {
        return(filename)
    }

    if (grepl(paste0("\\", extension, "$"), filename, ignore.case = TRUE)) {
        return(filename)
    }

    paste0(filename, extension)
}

strip_extension <- function(filename, extension) {
    if (is.null(filename) || !nzchar(filename)) {
        return(filename)
    }

    sub(paste0("\\.", extension, "$"), "", filename, ignore.case = TRUE)
}

resolve_title_and_filename <- function(title = NULL, filename = NULL, default_title, extension) {
    normalized_title <- strip_extension(title, extension)
    normalized_filename <- strip_extension(filename, extension)
    label <- default_title

    if (!is.null(normalized_title) && nzchar(normalized_title)) {
        label <- normalized_title
    } else if (!is.null(normalized_filename) && nzchar(normalized_filename)) {
        label <- normalized_filename
    }

    list(
        title = label,
        filename = ensure_extension(label, extension)
    )
}

build_monthly_passenger_changes_period_label <- function(year.from, month.from, year.to, month.to) {
    paste0(
        year.from - 1911, "年", as.integer(month.from), "月至",
        year.to - 1911, "年", as.integer(month.to), "月"
    )
}

build_monthly_passenger_changes_xlsx_title <- function(year.from, month.from, year.to, month.to, region_name = "花蓮縣") {
    paste0(
        build_monthly_passenger_changes_period_label(year.from, month.from, year.to, month.to),
        region_name, "乘客各搭乘次數區間月人數變化"
    )
}

build_monthly_passenger_changes_png_title <- function(year.from, month.from, year.to, month.to, region_name = "花蓮縣") {
    paste0(
        build_monthly_passenger_changes_period_label(year.from, month.from, year.to, month.to),
        region_name, "乘客各搭乘次數區間月人數變化折線圖"
    )
}

prepare_bus_monthly_passenger_changes_data <- function(
    year.from,
    month.from,
    year.to,
    month.to,
    data = NULL) {
    year.from <- as.integer(ifelse(as.integer(year.from) < 1000, as.integer(year.from) + 1911, as.integer(year.from)))
    year.to <- as.integer(ifelse(as.integer(year.to) < 1000, as.integer(year.to) + 1911, as.integer(year.to)))
    month.from <- normalize_month(month.from)
    month.to <- normalize_month(month.to)

    if (is.null(data)) {
        if (exists("df_bus", inherits = TRUE)) {
            data <- get("df_bus", inherits = TRUE)
        } else if (exists("Hualien", inherits = TRUE)) {
            data <- get("Hualien", inherits = TRUE)
        } else {
            stop("No default data found. Supply `data =` or load `df_bus` / `Hualien`.")
        }
    }

    legacy_cols <- c("上車交易日期", "卡號")
    midterm_cols <- c("資料代表日期(yyyy-MM-dd)", "卡號")

    normalized_data <- if (all(legacy_cols %in% names(data))) {
        transmute(
            data,
            日期 = as.Date(.data[["上車交易日期"]]),
            卡號 = as.character(.data[["卡號"]])
        )
    } else if (all(midterm_cols %in% names(data))) {
        transmute(
            data,
            日期 = as.Date(.data[["資料代表日期(yyyy-MM-dd)"]]),
            卡號 = as.character(.data[["卡號"]])
        )
    } else {
        stop("Unsupported data schema. Expected legacy df_bus columns or 115期中.RData columns.")
    }

    start_date <- as.Date(sprintf("%04d-%s-01", year.from, month.from))
    end_month_start <- as.Date(sprintf("%04d-%s-01", year.to, month.to))
    end_date_exclusive <- seq(end_month_start, by = "month", length.out = 2)[2]

    normalized_data <- filter(
        normalized_data,
        .data[["日期"]] >= start_date,
        .data[["日期"]] < end_date_exclusive,
        !is.na(.data[["卡號"]]),
        .data[["卡號"]] != ""
    )

    if (nrow(normalized_data) == 0) {
        stop("No data available after filtering. Check `data` and date range.")
    }

    data_long <- normalized_data %>%
        mutate(
            年月 = paste0(year(.data[["日期"]]) - 1911, sprintf("%02d", month(.data[["日期"]])))
        ) %>%
        count(年月, 卡號, name = "搭乘次數") %>%
        mutate(
            次數區間 = cut(
                搭乘次數,
                breaks = c(-Inf, 1, 4, Inf),
                labels = c("僅一次", "二至四次", "五次以上")
            )
        ) %>%
        count(年月, 次數區間, name = "數量") %>%
        complete(
            年月,
            次數區間 = c("僅一次", "二至四次", "五次以上"),
            fill = list(數量 = 0)
        ) %>%
        mutate(
            次數區間 = factor(次數區間, levels = c("僅一次", "二至四次", "五次以上"))
        ) %>%
        arrange(年月, 次數區間)

    df_real <- data_long %>%
        mutate(次數區間 = as.character(次數區間)) %>%
        pivot_wider(
            names_from = 次數區間,
            values_from = 數量,
            values_fill = 0
        ) %>%
        select(年月, 僅一次, `二至四次`, `五次以上`)

    df_prop <- df_real %>%
        mutate(合計 = rowSums(across(-年月))) %>%
        transmute(
            年月 = 年月,
            僅一次 = if_else(合計 == 0, "0%", paste0(round(僅一次 / 合計 * 100, 2), "%")),
            `二至四次` = if_else(合計 == 0, "0%", paste0(round(`二至四次` / 合計 * 100, 2), "%")),
            `五次以上` = if_else(合計 == 0, "0%", paste0(round(`五次以上` / 合計 * 100, 2), "%"))
        )

    list(
        data_long = data_long,
        df_real = df_real,
        df_prop = df_prop,
        year.from = year.from,
        month.from = month.from,
        year.to = year.to,
        month.to = month.to
    )
}

write_bus_monthly_passenger_changes_xlsx <- function(
    df_real,
    df_prop,
    year.from,
    month.from,
    year.to,
    month.to,
    region_name = "花蓮縣",
    xlsx_root = file.path(resolve_0414_base_dir(), "tables"),
    xlsx_title = NULL,
    xlsx_filename = NULL) {
    if (is.null(xlsx_title)) {
        xlsx_title <- NULL
    }

    resolved_xlsx <- resolve_title_and_filename(
        title = xlsx_title,
        filename = xlsx_filename,
        default_title = build_monthly_passenger_changes_xlsx_title(
            year.from = year.from,
            month.from = month.from,
            year.to = year.to,
            month.to = month.to,
            region_name = region_name
        ),
        extension = "xlsx"
    )
    xlsx_title <- resolved_xlsx$title
    xlsx_filename <- resolved_xlsx$filename

    xlsx_path <- file.path(
        xlsx_root,
        paste0(year.from - 1911, normalize_month(month.from), "-", year.to - 1911, normalize_month(month.to)),
        "乘客各搭乘次數區間月人數變化"
    )
    check_path(xlsx_path)

    wb <- createWorkbook()
    title_style <- createStyle(textDecoration = "bold", fontSize = 14)

    addWorksheet(wb, "數量")
    addWorksheet(wb, "比例")
    writeData(wb, sheet = "數量", x = xlsx_title, startRow = 1, startCol = 1, colNames = FALSE)
    addStyle(wb, sheet = "數量", style = title_style, rows = 1, cols = 1, gridExpand = FALSE)
    writeData(wb, sheet = "數量", x = df_real, startRow = 3)

    writeData(wb, sheet = "比例", x = xlsx_title, startRow = 1, startCol = 1, colNames = FALSE)
    addStyle(wb, sheet = "比例", style = title_style, rows = 1, cols = 1, gridExpand = FALSE)
    writeData(wb, sheet = "比例", x = df_prop, startRow = 3)

    output_path <- file.path(xlsx_path, xlsx_filename)
    saveWorkbook(wb, file = output_path, overwrite = TRUE)

    output_path
}

bus_monthly_passenger_changes_plot <- function(
    year.from,
    month.from,
    year.to,
    month.to,
    data = NULL,
    region_name = "花蓮縣",
    write_xlsx = TRUE,
    xlsx_root = file.path(resolve_0414_base_dir(), "tables"),
    png_title = NULL,
    xlsx_title = NULL,
    xlsx_filename = NULL) {
    prepared <- prepare_bus_monthly_passenger_changes_data(
        year.from = year.from,
        month.from = month.from,
        year.to = year.to,
        month.to = month.to,
        data = data
    )

    data_long <- prepared$data_long
    df_real <- prepared$df_real
    df_prop <- prepared$df_prop
    year.from <- prepared$year.from
    month.from <- prepared$month.from
    year.to <- prepared$year.to
    month.to <- prepared$month.to

    if (is.null(png_title)) {
        png_title <- build_monthly_passenger_changes_png_title(
            year.from = year.from,
            month.from = month.from,
            year.to = year.to,
            month.to = month.to,
            region_name = region_name
        )
    }

    xlsx_output_path <- NULL
    if (isTRUE(write_xlsx)) {
        xlsx_output_path <- write_bus_monthly_passenger_changes_xlsx(
            df_real = df_real,
            df_prop = df_prop,
            year.from = year.from,
            month.from = month.from,
            year.to = year.to,
            month.to = month.to,
            region_name = region_name,
            xlsx_root = xlsx_root,
            xlsx_title = xlsx_title,
            xlsx_filename = xlsx_filename
        )
    }

    plot_family <- get_chinese_plot_family()
    plot_colors <- grDevices::gray.colors(4)

    plot_obj <- ggplot(
        data_long,
        aes(
            x = as.character(年月),
            y = 數量,
            color = 次數區間,
            group = 次數區間
        )
    ) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = plot_colors) +
        scale_y_continuous(
            trans = "log10",
            breaks = scales::pretty_breaks(n = 6),
            labels = scales::comma
        ) +
        labs(
            x = "月份",
            y = "人數",
            color = "搭乘次數",
            title = png_title
        ) +
        theme_minimal() +
        theme(
            text = element_text(family = plot_family),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 20),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            panel.grid.major.y = element_line(color = "gray80"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA)
        )

    attr(plot_obj, "xlsx_output_path") <- xlsx_output_path
    plot_obj
}

generate_bus_monthly_passenger_changes_plot <- function(
    year.from,
    month.from,
    year.to,
    month.to,
    data = NULL,
    region_name = "花蓮縣",
    write_xlsx = TRUE,
    xlsx_root = file.path(resolve_0414_base_dir(), "tables"),
    image_root = file.path(resolve_0414_base_dir(), "images"),
    png_title = NULL,
    png_filename = NULL,
    xlsx_title = NULL,
    xlsx_filename = NULL) {
    if (is.null(png_title)) {
        png_title <- NULL
    }

    resolved_png <- resolve_title_and_filename(
        title = png_title,
        filename = png_filename,
        default_title = build_monthly_passenger_changes_png_title(
            year.from = year.from,
            month.from = month.from,
            year.to = year.to,
            month.to = month.to,
            region_name = region_name
        ),
        extension = "png"
    )
    png_title <- resolved_png$title
    png_filename <- resolved_png$filename

    path <- file.path(
        image_root,
        paste0(year.from - 1911, normalize_month(month.from), "-", year.to - 1911, normalize_month(month.to)),
        "乘客各搭乘次數區間月人數變化"
    )
    check_path(path)

    plot_obj <- bus_monthly_passenger_changes_plot(
        year.from = year.from,
        month.from = month.from,
        year.to = year.to,
        month.to = month.to,
        data = data,
        region_name = region_name,
        write_xlsx = write_xlsx,
        xlsx_root = xlsx_root,
        png_title = png_title,
        xlsx_title = xlsx_title,
        xlsx_filename = xlsx_filename
    )

    png_output_path <- file.path(path, png_filename)

    ggsave(
        filename = png_output_path,
        plot = plot_obj,
        width = 15,
        height = 5,
        units = "in",
        dpi = 300,
        bg = "white"
    )

    invisible(list(
        png_output_path = png_output_path,
        xlsx_output_path = attr(plot_obj, "xlsx_output_path")
    ))
}

# Recommended usage with `E:/0408俊典/115期中.RData`
# load("E:/0408俊典/115期中.RData")
# source("D:/Github/115_Midterm-Report/TPASS/0414.r")
#
# 花蓮縣市區客運:
# generate_bus_monthly_passenger_changes_plot(
#     2024, 7, 2025, 6,
#     data = bus_hualien,
#     png_title = "113年7月至114年6月花蓮縣市區客運乘客各搭乘次數區間月人數變化折線圖",
#     xlsx_title = "113年7月至114年6月花蓮縣市區客運乘客各搭乘次數區間月人數變化"
# )
# #
# # 花蓮縣市區客運 TPASS:
# generate_bus_monthly_passenger_changes_plot(
#     2024, 7, 2025, 6,
#     data = bus_hualien_TPASS,
#     png_title = "113年7月至114年6月花蓮縣市區客運TPASS乘客各搭乘次數區間月人數變化折線圖",
#     xlsx_title = "113年7月至114年6月花蓮縣市區客運TPASS乘客各搭乘次數區間月人數變化"
# )
# #
# # 花蓮縣公路客運:
# generate_bus_monthly_passenger_changes_plot(
#     2024, 7, 2025, 6,
#     data = Hualien_THB,
#     png_title = "113年7月至114年6月花蓮縣公路客運乘客各搭乘次數區間月人數變化折線圖",
#     xlsx_title = "113年7月至114年6月花蓮縣公路客運乘客各搭乘次數區間月人數變化"
# )
# #
# # 花蓮縣公路客運 TPASS:
# generate_bus_monthly_passenger_changes_plot(
#     2024, 7, 2025, 6,
#     data = Hualien_THB_TPASS,
#     png_title = "113年7月至114年6月花蓮縣公路客運TPASS乘客各搭乘次數區間月人數變化折線圖",
#     xlsx_title = "113年7月至114年6月花蓮縣公路客運TPASS乘客各搭乘次數區間月人數變化"
# )
#
# # ----------------------------------------------
#
# # 臺東縣市區客運:
# generate_bus_monthly_passenger_changes_plot(
#     2024, 7, 2025, 6,
#     data = bus_taitung,
#     png_title = "113年7月至114年6月臺東縣市區客運乘客各搭乘次數區間月人數變化折線圖",
#     xlsx_title = "113年7月至114年6月臺東縣市區客運乘客各搭乘次數區間月人數變化"
# )
# #
# # 臺東縣市區客運 TPASS:
# generate_bus_monthly_passenger_changes_plot(
#     2024, 7, 2025, 6,
#     data = bus_taitung_TPASS,
#     png_title = "113年7月至114年6月臺東縣市區客運TPASS乘客各搭乘次數區間月人數變化折線圖",
#     xlsx_title = "113年7月至114年6月臺東縣市區客運TPASS乘客各搭乘次數區間月人數變化"
# )
# #
# # 臺東縣公路客運:
# generate_bus_monthly_passenger_changes_plot(
#     2024, 7, 2025, 6,
#     data = Taitung_THB,
#     png_title = "113年7月至114年6月臺東縣公路客運乘客各搭乘次數區間月人數變化折線圖",
#     xlsx_title = "113年7月至114年6月臺東縣公路客運乘客各搭乘次數區間月人數變化"
# )
# #
# # 臺東縣公路客運 TPASS:
# generate_bus_monthly_passenger_changes_plot(
#     2024, 7, 2025, 6,
#     data = Taitung_THB_TPASS,
#     png_title = "113年7月至114年6月臺東縣公路客運TPASS乘客各搭乘次數區間月人數變化折線圖",
#     xlsx_title = "113年7月至114年6月臺東縣公路客運TPASS乘客各搭乘次數區間月人數變化"
# )

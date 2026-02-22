#### Load Packages #####

library(shiny)
library(shinyjs)
library(quantmod)
library(dplyr)
library(gt)
library(plotly)
library(rvest)
library(xml2)
library(stringr)
library(lubridate)
library(shinychat)
library(ellmer)
library(shinyLP)
library(riingo)
library(httr)
library(sentimentr)
library(gtExtras)
library(ggsci)
library(svglite)
library(zoo)
library(base64enc)
library(randomForest)

# Optional: loading spinners (install with install.packages("shinycssloaders"))
if (requireNamespace("shinycssloaders", quietly = TRUE)) {
  library(shinycssloaders)
} else {
  # Fallback: withSpinner is a no-op pass-through
  withSpinner <- function(ui, ...) ui
}

# ----------------------------------------------------
############ Constants & Icons ######################
# ----------------------------------------------------

ICON_MAP <- list(
  volume  = tags$i(class = "fa fa-chart-bar",   style = "color:#2962ff;"),
  high    = tags$i(class = "fa fa-arrow-up",    style = "color:#26a69a;"),
  low     = tags$i(class = "fa fa-arrow-down",  style = "color:#ef5350;"),
  avg     = tags$i(class = "fa fa-calculator",  style = "color:#787b86;"),
  brain   = tags$i(class = "fa fa-brain",       style = "color:#9c27b0;"),
  tree    = tags$i(class = "fa fa-tree",        style = "color:#26a69a;"),
  context = tags$i(class = "fa fa-database",    style = "color:#ff9800;")
)

LOGO_SRC_MAP <- c(
  "cnbc.com"        = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/CNBC_logo.svg/2560px-CNBC_logo.svg.png",
  "bloomberg.com"   = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTHIywl5QW0qg7BDqKUzx6u10njBQ5bxfDpww&s",
  "reuters.com"     = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8d/Reuters_Logo.svg/2560px-Reuters_Logo.svg.png",
  "seekingalpha.com"= "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR5nu0mRRhfXr31SyaQY7BfbZn3hCA9iL75wQ&s",
  "marketwatch.com" = "https://1000logos.net/wp-content/uploads/2024/09/MarketWatch-Logo.jpg",
  "wsj.com"         = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQhUPG0NJYW4RfZuwUf3L5_8t9p6NHNMwh6yw&s"
)

# ----------------------------------------------------
############ Helper Functions ########################
# ----------------------------------------------------

get_sp500_tickers <- function() {
  url  <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  page <- tryCatch(read_html(url), error = function(e) NULL)

  if (is.null(page)) {
    df <- data.frame(
      ticker = c("AAPL","MSFT","GOOGL","AMZN","NVDA","META","TSLA","JPM","V","JNJ"),
      name   = c("Apple Inc.","Microsoft","Alphabet Inc.","Amazon","NVIDIA",
                 "Meta Platforms","Tesla","JPMorgan Chase","Visa","Johnson & Johnson"),
      sector = NA_character_, stringsAsFactors = FALSE
    )
  } else {
    tab_node <- html_nodes(page, "table")[[1]]
    tab      <- html_table(tab_node, fill = TRUE)

    sym_col  <- c("Symbol","Ticker","Tickers")
    name_col <- c("Security","Company","Name")
    sect_col <- c("GICS Sector","Sector","GICS sector")

    sym_name  <- sym_col [sym_col  %in% names(tab)][1]
    comp_name <- name_col[name_col %in% names(tab)][1]
    sect_name <- sect_col[sect_col %in% names(tab)][1]

    if (is.na(sym_name) || is.na(comp_name)) {
      df <- data.frame(
        ticker = c("AAPL","MSFT","GOOGL","AMZN","NVDA","META","TSLA","JPM","V","JNJ"),
        name   = c("Apple Inc.","Microsoft","Alphabet Inc.","Amazon","NVIDIA",
                   "Meta Platforms","Tesla","JPMorgan Chase","Visa","Johnson & Johnson"),
        sector = NA_character_, stringsAsFactors = FALSE
      )
    } else {
      tab       <- tab[!is.na(tab[[sym_name]]) & nzchar(tab[[sym_name]]), , drop = FALSE]
      sect_vec  <- if (!is.na(sect_name)) tab[[sect_name]] else NA_character_
      df <- data.frame(
        ticker = gsub("\\.", "-", tab[[sym_name]]),
        name   = tab[[comp_name]],
        sector = sect_vec,
        stringsAsFactors = FALSE
      )
    }
  }

  MANUAL_DOMAIN_MAP <- c(
    "BRK.B"="berkshirehathaway.com","BRK.A"="berkshirehathaway.com",
    "GOOGL"="google.com","GOOG"="google.com","META"="meta.com",
    "PM"="pmi.com","PG"="pg.com","KO"="coca-cola-company.com","PEP"="pepsico.com",
    "XOM"="corporate.exxonmobil.com","CVX"="chevron.com","HD"="homedepot.com",
    "MA"="mastercard.com","V"="visa.com","UNH"="unitedhealthgroup.com","JNJ"="jnj.com",
    "WMT"="walmart.com","COST"="costco.com","INTC"="intel.com","NVDA"="nvidia.com",
    "AMD"="amd.com","NFLX"="netflix.com","TSLA"="tesla.com","MSFT"="microsoft.com",
    "AAPL"="apple.com","AMZN"="aboutamazon.com"
  )

  google_favicon <- function(domain, size = 64)
    paste0("https://www.google.com/s2/favicons?sz=", size, "&domain=", domain)

  domain <- ifelse(df$ticker %in% names(MANUAL_DOMAIN_MAP),
                   MANUAL_DOMAIN_MAP[df$ticker],
                   paste0(tolower(df$ticker), ".com"))
  logo         <- google_favicon(domain)
  df$domain    <- domain
  df$logo      <- logo
  df$display   <- paste0(df$name, " (", df$ticker, ")")
  df
}

pf_get_prices_for_sparkline <- function(tickers, days = 30) {
  end_date   <- Sys.Date()
  start_date <- end_date - days
  lst <- lapply(tickers, function(tk) {
    x <- tryCatch(
      quantmod::getSymbols(tk, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE),
      error = function(e) NULL
    )
    if (is.null(x) || NROW(x) < 2) return(NULL)
    zoo::coredata(Cl(x))[, 1]
  })
  names(lst) <- tickers
  lst
}

get_date_range <- function(interval) {
  end_date   <- Sys.Date()
  start_date <- switch(interval,
    "1day"    = end_date - 1,
    "7days"   = end_date - 7,
    "1month"  = end_date - 30,
    "3months" = end_date - 90,
    "6months" = end_date - 180,
    "1year"   = end_date - 365,
    "2years"  = end_date - 730,
    "5years"  = end_date - 1825
  )
  list(start = start_date, end = end_date)
}

spark_area_svg <- function(values, width = 160, height = 60,
                           up_col = "#26a69a", down_col = "#ef5350",
                           highlight_col = "#e91e63") {
  if (is.null(values) || length(values) < 2 || all(is.na(values)))
    return("data:image/svg+xml;utf8, ")

  vals     <- as.numeric(values)
  trend_up <- tail(vals, 1) >= vals[1]
  line_col <- if (trend_up) up_col else down_col
  fill_col <- grDevices::adjustcolor(line_col, alpha.f = 0.20)
  idx      <- if (trend_up) which.min(vals) else which.max(vals)
  df       <- data.frame(x = seq_along(vals), y = vals)

  g <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_area(fill = fill_col) +
    ggplot2::geom_line(linewidth = 1, color = line_col) +
    ggplot2::geom_point(data = df[idx, , drop = FALSE], ggplot2::aes(x, y),
                        size = 2.5, color = highlight_col) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  svg_txt <- svglite::stringSVG(
    width = width / 96, height = height / 96, bg = "transparent",
    { print(g) }
  )
  paste0("data:image/svg+xml;utf8,", URLencode(svg_txt, reserved = TRUE))
}

get_stock_data <- function(ticker, interval) {
  dr <- get_date_range(interval)
  tryCatch(
    getSymbols(ticker, src = "yahoo", from = dr$start, to = dr$end, auto.assign = FALSE),
    error = function(e) NULL
  )
}

favicon_url <- function(domain) paste0("https://www.google.com/s2/favicons?sz=64&domain=", domain)

domain_from_url_or_source <- function(url, source) {
  if (!is.null(source) && nzchar(source) && grepl("\\.", source)) return(tolower(source))
  dom <- tryCatch({
    dom1 <- sub("^https?://(www\\.)?", "", url)
    sub("/.*$", "", dom1)
  }, error = function(e) NA_character_)
  tolower(ifelse(is.na(dom), "news.google.com", dom))
}

make_riingo_gt <- function(df) {
  if (!NROW(df)) {
    return(
      gt::gt(data.frame(Message = "No recent headlines found.")) |>
        gt::tab_options(table.width = gt::pct(100))
    )
  }
  gt::gt(df) |>
    gt::cols_label(when = "Published", title_md = "Title", source = "source", sentiment = "Sentiment") |>
    gt::fmt_datetime(columns = when, date_style = 6, time_style = 4) |>
    gt::fmt_markdown(columns = title_md) |>
    gt::text_transform(
      locations = gt::cells_body(columns = source),
      fn = function(x) {
        lapply(x, function(u) {
          if (is.na(u) || !nzchar(u)) gt::html("") else
            gt::html(sprintf("<img src='%s' style='height:18px;'>", u))
        })
      }
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "rgba(38,166,154,0.18)"),
                   gt::cell_text(color = "#26a69a", weight = "bold")),
      locations = gt::cells_body(columns = sentiment, rows = sentiment == "Positive")
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "rgba(239,83,80,0.18)"),
                   gt::cell_text(color = "#ef5350", weight = "bold")),
      locations = gt::cells_body(columns = sentiment, rows = sentiment == "Negative")
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "rgba(128,128,128,0.20)"),
                   gt::cell_text(color = "#9aa3af", weight = "bold")),
      locations = gt::cells_body(columns = sentiment, rows = sentiment == "Neutral")
    ) |>
    gt::tab_options(
      table.width = gt::pct(100), table.font.size = gt::px(12),
      table.font.names = "JetBrains Mono", table.background.color = "#1e2431",
      column_labels.background.color = "#252b3d", column_labels.font.weight = "bold",
      column_labels.border.bottom.color = "#2962ff", column_labels.border.bottom.width = gt::px(3),
      data_row.padding = gt::px(8)
    ) |>
    gt::tab_style(style = gt::cell_text(color = "#ffffff", weight = "bold"),
                  locations = gt::cells_column_labels(columns = gt::everything())) |>
    gt::tab_style(style = gt::cell_text(color = "#e0e6ed"),
                  locations = gt::cells_body(columns = c(when, title_md)))
}

# Market cap helpers --------------------------------------------------------

market_cap_fallback_df <- function() {
  df <- data.frame(
    Ticker    = c("AAPL","MSFT","GOOGL","AMZN","NVDA"),
    Name      = c("Apple","Microsoft","Alphabet","Amazon","NVIDIA"),
    MarketCap = c(3000,2800,2200,1900,1500),
    Price     = c(200,400,180,150,1000),
    Change    = c(1.5,0.8,2.1,-0.5,3.2),
    Revenue   = c(383,211,305,575,76),
    stringsAsFactors = FALSE
  )
  logo_map  <- setNames(get_sp500_tickers()$logo, get_sp500_tickers()$ticker)
  df$Logo   <- logo_map[df$Ticker]
  df        <- df |> mutate(Name_Logo = paste0(Name, "||", Logo))
  df
}

get_market_cap_data <- function() {
  url <- "https://stockanalysis.com/list/sp-500-stocks/"
  tryCatch({
    page  <- xml2::read_html(url)
    tab   <- page |>
      rvest::html_node(xpath = "/html/body/div[1]/div[1]/div[2]/main/div[2]/div/div/div[5]/table") |>
      rvest::html_table()
    tab10 <- head(tab, 10)
    tab10$`Stock Price` <- as.numeric(tab10$`Stock Price`)
    tab10$`% Change`    <- gsub("%","",tab10$`% Change`) |> as.numeric()
    tab10$Revenue       <- gsub("B","",tab10$Revenue)   |> as.numeric()
    tab10$`Market Cap`  <- gsub("T","",tab10$`Market Cap`) |> gsub(",","",x=_) |> as.numeric()
    df <- tab10[, -1]
    colnames(df) <- c("Ticker","Name","MarketCap","Price","Change","Revenue")
    df$Ticker[df$Ticker == "BRK.B"] <- "BRK-B"
    logo_map  <- setNames(get_sp500_tickers()$logo, get_sp500_tickers()$ticker)
    df$Logo   <- logo_map[df$Ticker]
    df        <- df |> mutate(Name_Logo = paste0(Name, "||", Logo))
    df
  }, error = function(e) {
    message("Market cap scraping failed: ", e$message)
    market_cap_fallback_df()
  })
}

make_key_stats_gt_alternative <- function(data, spark_data) {
  data$Price_Sparkline_Data <- lapply(data$Ticker, function(t) spark_data[[t]])
  df_display <- data |>
    dplyr::select(c("Name_Logo","MarketCap","Price_Sparkline_Data","Revenue"))
  colnames(df_display)[3] <- "30D_Trend"
  series_list <- df_display$`30D_Trend`

  tbl <- gt::gt(df_display) |>
    gt::cols_label(Name_Logo = "Name", MarketCap = "Market Cap (B)",
                   `30D_Trend` = "30D Trend", Revenue = "Revenue (B)") |>
    gt::text_transform(
      locations = gt::cells_body(columns = Name_Logo),
      fn = function(x) {
        lapply(x, function(val) {
          parts <- stringr::str_split(val, "\\|\\|")[[1]]
          name  <- parts[1]; logo <- parts[2]
          gt::html(paste0(
            '<div style="display:flex;align-items:center;padding:5px 0;">',
            '<img src="', logo, '" style="width:20px;height:20px;margin-right:8px;border-radius:4px;object-fit:contain;">',
            '<span>', name, '</span></div>'
          ))
        })
      }
    ) |>
    gtExtras::gt_plt_bar(MarketCap, labels = TRUE, color = "#2962ff",
                         scale_type = "number", width = 70)

  tbl <- tbl |>
    gt::text_transform(
      locations = gt::cells_body(columns = `30D_Trend`),
      fn = function(x) {
        lapply(seq_along(x), function(i) {
          vals <- series_list[[i]]
          if (is.null(vals) || length(vals) < 2)
            return(gt::html('<span style="color:#787b86;">n/a</span>'))
          uri <- spark_area_svg(vals)
          gt::html(paste0("<img src='", uri, "' style='width:140px;height:48px;border-radius:6px;'>"))
        })
      }
    ) |>
    gtExtras::gt_color_box(
      columns = Revenue,
      domain  = c(min(data$Revenue, na.rm = TRUE) - 5, max(data$Revenue, na.rm = TRUE) + 5),
      palette = "ggsci::blue_material", accuracy = 1
    ) |>
    gt::tab_options(
      table.width = gt::pct(100), table.font.size = gt::px(12),
      table.font.names = "JetBrains Mono", table.background.color = "#131722",
      column_labels.background.color = "#1e2431", column_labels.font.weight = "bold",
      column_labels.border.bottom.color = "#2962ff", column_labels.border.bottom.width = gt::px(3),
      data_row.padding = gt::px(8), table.border.top.style = "none"
    ) |>
    gt::tab_style(locations = gt::cells_column_labels(columns = gt::everything()),
                  style = gt::cell_text(color = "#ffffff", weight = "bold")) |>
    gt::tab_style(locations = gt::cells_body(columns = `30D_Trend`),
                  style = gt::cell_text(color = "#e0e6ed")) |>
    gt::tab_header(title = gt::md("**Top 10 Companies by Market Cap**"),
                   subtitle = gt::md("Source: stockanalysis.com")) |>
    gt::tab_style(locations = gt::cells_title(groups = "title"),
                  style = gt::cell_text(font = "JetBrains Mono", size = "medium",
                                        weight = "bold", color = "#fff")) |>
    gt::tab_style(locations = gt::cells_title(groups = "subtitle"),
                  style = gt::cell_text(size = "small", color = "#787b86"))
  tbl
}

# Fundamental table helpers -------------------------------------------------

get_fund_data <- function() {
  sp500_info <- get_sp500_tickers()
  logo_map   <- setNames(sp500_info$logo, sp500_info$ticker)

  fallback_df <- data.frame(
    Symbol_Cleaned = c("AAPL","MSFT","GOOGL","NVDA","JPM"),
    Name           = c("Apple Inc.","Microsoft Corp.","Alphabet Inc.","NVIDIA Corp.","JPMorgan Chase & Co."),
    P_E            = c(30.2,35.8,27.1,74.3,11.5),
    EPS_dilTTM     = c(6.40,10.30,5.50,12.50,15.10),
    Div_Yield_Pct  = c(0.005,0.007,0,0.001,0.025),
    Analyst_Rating = c("Buy","Strong buy","Buy","Strong buy","Neutral"),
    stringsAsFactors = FALSE
  ) |>
    mutate(Logo = logo_map[Symbol_Cleaned], Name_Logo = paste0(Name, "||", Logo)) |>
    select(Name_Logo, P_E, EPS_dilTTM, Div_Yield_Pct, Analyst_Rating)

  tryCatch({
    page       <- read_html("https://www.tradingview.com/symbols/SPX/components/")
    table_node <- page |> rvest::html_nodes("table")
    if (length(table_node) == 0) stop("No table found.")
    fund_table <- table_node[[1]] |> html_table(fill = TRUE, header = TRUE)
    data       <- fund_table[, c(1,7,8,10,12)]
    colnames(data) <- c("Symbol","P_E","EPS_dilTTM","Div_Yield_Pct","Analyst_Rating")
    ticker_list <- names(logo_map)
    parsed_data <- data |>
      rowwise() |>
      mutate(
        Symbol_Cleaned = {
          best <- ""
          for (t in ticker_list) if (startsWith(Symbol, t) && nchar(t) > nchar(best)) best <- t
          best
        },
        Name = if (nchar(Symbol_Cleaned) > 0)
          trimws(sub(Symbol_Cleaned, "", Symbol, fixed = TRUE)) else Symbol
      ) |>
      ungroup()
    parsed_data |>
      mutate(
        P_E           = as.numeric(gsub("[^0-9.-]","", P_E)),
        EPS_dilTTM    = as.numeric(gsub("[^0-9.-]","", EPS_dilTTM)),
        Div_Yield_Pct = as.numeric(gsub("[^0-9.-]","", Div_Yield_Pct)) / 100,
        Logo          = logo_map[Symbol_Cleaned],
        Name_Logo     = paste0(Name, "||", Logo)
      ) |>
      select(Name_Logo, P_E, EPS_dilTTM, Div_Yield_Pct, Analyst_Rating)
  }, error = function(e) {
    message("Fundamental scraping failed: ", e$message)
    fallback_df
  })
}

render_name_logo <- function(x) {
  lapply(x, function(val) {
    parts <- stringr::str_split(val, "\\|\\|")[[1]]
    gt::html(paste0(
      '<div style="display:flex;align-items:center;padding:5px 0;">',
      '<img src="', parts[2], '" style="width:20px;height:20px;margin-right:8px;border-radius:4px;object-fit:contain;">',
      '<span>', parts[1], '</span></div>'
    ))
  })
}

make_fund_gt <- function(data) {
  rating_colors <- c(
    "Strong buy" = "#034C02","Buy" = "#037302","Neutral" = "darkorange",
    "Sell" = "#ef5350","Strong sell" = "darkred"
  )
  df_styled <- data |>
    mutate(`Analyst Rating` = factor(Analyst_Rating, levels = names(rating_colors))) |>
    select(-Analyst_Rating)

  gt(df_styled) |>
    cols_label(Name_Logo = "Company", P_E = "P/E Ratio", EPS_dilTTM = "EPS (TTM)",
               Div_Yield_Pct = "Div. Yield", `Analyst Rating` = "Rating") |>
    text_transform(locations = cells_body(columns = Name_Logo), fn = render_name_logo) |>
    fmt_currency(columns = EPS_dilTTM, currency = "USD") |>
    fmt_percent(columns = Div_Yield_Pct, decimals = 2) |>
    gt_plt_bar(P_E, color = "#2962ff", background = "#131722", labels = TRUE,
               width = 40, scale_type = "number") |>
    gt_color_rows(EPS_dilTTM, palette = c("steelblue","#2962ff")) |>
    gt_color_rows(Div_Yield_Pct, palette = c("steelblue","#2962ff")) |>
    tab_style(style = list(cell_fill(color = rating_colors["Strong buy"]),
                           cell_text(color = "#131722", weight = "bold")),
              locations = cells_body(`Analyst Rating`, rows = `Analyst Rating` == "Strong buy")) |>
    tab_style(style = list(cell_fill(color = rating_colors["Buy"]),
                           cell_text(color = "#131722", weight = "bold")),
              locations = cells_body(`Analyst Rating`, rows = `Analyst Rating` == "Buy")) |>
    tab_style(style = list(cell_fill(color = rating_colors["Neutral"]),
                           cell_text(color = "#131722", weight = "bold")),
              locations = cells_body(`Analyst Rating`, rows = `Analyst Rating` == "Neutral")) |>
    opt_all_caps() |>
    tab_options(
      table.width = pct(100), table.font.size = px(12),
      table.font.names = "JetBrains Mono", table.background.color = "#131722",
      column_labels.background.color = "#1e2431", column_labels.border.bottom.color = "#2962ff",
      column_labels.border.bottom.width = px(3), data_row.padding = px(8)
    ) |>
    tab_style(locations = cells_column_labels(everything()),
              style = cell_text(color = "#ffffff", weight = "bold")) |>
    tab_style(locations = cells_body(columns = -`Analyst Rating`),
              style = cell_text(color = "#e0e6ed")) |>
    tab_header(title = "", subtitle = md("Source: tradingview.com"))
}

weight_pill_html <- function(v_prop) {
  v     <- suppressWarnings(as.numeric(v_prop))
  if (!is.finite(v) || is.na(v)) v <- 0
  pct   <- max(0, min(1, v))
  label <- sprintf("%.1f%%", pct * 100)
  fill_w <- round(100 * pct)
  htmltools::HTML(sprintf(
    "<div style='position:relative;width:120px;height:26px;background:#2a2e39;border-radius:13px;overflow:hidden;display:flex;align-items:center;justify-content:center;'>
 <div style='position:absolute;left:0;top:0;bottom:0;width:%dpx;background:#1f7a74;border-radius:13px;opacity:.85;'></div>
 <span style='position:relative;color:#e0e6ed;font-weight:700;'>%s</span>
 </div>", fill_w, label
  ))
}

create_table <- function(data, ticker) {
  req(data)
  Close_Prices <- Cl(data)
  rsi          <- round(as.numeric(RSI(Close_Prices, n = 14)), 2)
  macd_values  <- MACD(Close_Prices, nFast = 12, nSlow = 26, nSig = 9)
  macd         <- round(as.numeric(macd_values[,"macd"]), 2)
  signal       <- round(as.numeric(macd_values[,"signal"]), 2)

  df_full <- data.frame(
    Date      = index(data),
    Open      = round(as.numeric(Op(data)), 2),
    High      = round(as.numeric(Hi(data)), 2),
    Low       = round(as.numeric(Lo(data)), 2),
    Close     = round(as.numeric(Cl(data)), 2),
    Volume    = round(as.numeric(Vo(data)) / 1e6, 1),
    Change    = c(NA, round(diff(as.numeric(Cl(data))), 2)),
    ChangePct = c(NA, round((diff(as.numeric(Cl(data))) / as.numeric(Cl(data))[-NROW(data)]) * 100, 2)),
    RSI       = rsi, MACD = macd, Signal = signal
  )
  last_5 <- tail(df_full, 5)

  arrow_transform <- function(col) {
    text_transform(
      locations = cells_body(columns = !!col),
      fn = function(x) {
        lapply(as.numeric(x), function(val) {
          if (is.na(val) || is.infinite(val))
            return(gt::html('<span style="color:#787b86;">-</span>'))
          col_css <- if (val > 0) "#26a69a" else if (val < 0) "#ef5350" else "#e0e6ed"
          icon    <- if (val > 0) "\u25b2" else if (val < 0) "\u25bc" else ""
          gt::html(paste0('<span style="color:', col_css, '; font-weight:700; margin-right:4px;">', icon,
                          '</span><span style="color:#e0e6ed;">', abs(val), '</span>'))
        })
      }
    )
  }

  gt(last_5) |>
    cols_label(Date="Date", Open="Open", High="High", Low="Low", Close="Close",
               Volume="Volume", Change="Change", ChangePct="Change %",
               RSI="RSI(14)", MACD="MACD", Signal="Signal") |>
    arrow_transform(Change) |>
    arrow_transform(ChangePct) |>
    fmt_currency(columns = c(Open,High,Low,Close), currency = "USD") |>
    fmt_number(columns = Volume, decimals = 1, pattern = "{x}M") |>
    tab_style(style = list(cell_fill(color = "rgba(38,166,154,0.1)"),
                           cell_text(color = "#26a69a", weight = "bold")),
              locations = cells_body(columns = RSI, rows = RSI >= 70)) |>
    tab_style(style = list(cell_fill(color = "rgba(239,83,80,0.1)"),
                           cell_text(color = "#ef5350", weight = "bold")),
              locations = cells_body(columns = RSI, rows = RSI <= 30)) |>
    tab_style(style = cell_text(color = "#2962ff", weight = "bold"),
              locations = cells_body(columns = MACD)) |>
    tab_style(style = cell_text(color = "#ff9800", weight = "bold"),
              locations = cells_body(columns = Signal)) |>
    tab_options(
      table.width = pct(100), table.font.size = px(12),
      table.font.names = "JetBrains Mono", table.background.color = "#1e2431",
      column_labels.background.color = "#252b3d", column_labels.font.weight = "bold",
      column_labels.border.bottom.color = "#2962ff", column_labels.border.bottom.width = px(3),
      data_row.padding = px(8)
    ) |>
    tab_style(style = cell_text(color = "#ffffff", weight = "bold"),
              locations = cells_column_labels(columns = everything())) |>
    tab_style(style = cell_text(color = "#e0e6ed"),
              locations = cells_body(columns = c(Date,Open,High,Low,Close,Volume,RSI,MACD,Signal)))
}

# ----------------------------------------------------
############ ML Prediction Functions #################
# ----------------------------------------------------

# LSTM 7-day price prediction -----------------------------------------------
run_lstm_prediction <- function(prices, n_ahead = 7, lag = 60) {
  if (!is.numeric(prices) || length(prices) < (lag + n_ahead + 10))
    return(list(predictions = NULL, method = "insufficient_data", error = "Not enough data"))

  mn <- min(prices, na.rm = TRUE)
  mx <- max(prices, na.rm = TRUE)
  if (mx == mn) return(list(predictions = rep(tail(prices, 1), n_ahead), method = "constant"))

  norm <- (prices - mn) / (mx - mn)

  # Check for keras3 availability
  has_keras <- requireNamespace("keras3", quietly = TRUE)

  if (has_keras) {
    tryCatch({
      library(keras3)

      # Build sliding-window sequences
      n      <- length(norm)
      n_seq  <- n - lag
      X_arr  <- array(0, dim = c(n_seq, lag, 1))
      y_vec  <- numeric(n_seq)
      for (i in seq_len(n_seq)) {
        X_arr[i, , 1] <- norm[i:(i + lag - 1)]
        y_vec[i]      <- norm[i + lag]
      }

      model <- keras_model_sequential(name = "lstm_price") |>
        layer_lstm(units = 64, return_sequences = TRUE, input_shape = c(lag, 1)) |>
        layer_dropout(rate = 0.2) |>
        layer_lstm(units = 32, return_sequences = FALSE) |>
        layer_dropout(rate = 0.2) |>
        layer_dense(units = 16, activation = "relu") |>
        layer_dense(units = 1)

      model |> compile(optimizer = optimizer_adam(learning_rate = 0.001), loss = "mse")

      model |> fit(
        X_arr, array(y_vec, dim = c(n_seq, 1)),
        epochs = 25, batch_size = 32, validation_split = 0.1,
        verbose = 0,
        callbacks = list(callback_early_stopping(patience = 5, restore_best_weights = TRUE))
      )

      last_seq    <- tail(norm, lag)
      preds_norm  <- numeric(n_ahead)
      for (i in seq_len(n_ahead)) {
        inp         <- array(last_seq, dim = c(1, lag, 1))
        p           <- as.numeric(model(inp))
        p           <- max(0, min(1, p))
        preds_norm[i] <- p
        last_seq    <- c(last_seq[-1], p)
      }

      preds      <- preds_norm * (mx - mn) + mn
      recent_vol <- sd(diff(tail(prices, 30)), na.rm = TRUE)
      horizon_sd <- recent_vol * sqrt(seq_len(n_ahead))

      return(list(
        predictions = preds,
        lower       = preds - 1.96 * horizon_sd,
        upper       = preds + 1.96 * horizon_sd,
        method      = "LSTM (keras3)"
      ))
    }, error = function(e) {
      message("LSTM (keras3) failed: ", e$message, " — falling back to ETS.")
    })
  }

  # Fallback: ETS / exponential smoothing via forecast
  if (requireNamespace("forecast", quietly = TRUE)) {
    tryCatch({
      fit <- forecast::ets(prices)
      fc  <- forecast::forecast(fit, h = n_ahead)
      return(list(
        predictions = as.numeric(fc$mean),
        lower       = as.numeric(fc$lower[, 2]),
        upper       = as.numeric(fc$upper[, 2]),
        method      = "ETS (forecast pkg — install keras3 for LSTM)"
      ))
    }, error = function(e) NULL)
  }

  # Final fallback: naive drift
  last_val <- tail(prices, 1)
  drift    <- mean(diff(tail(prices, 30)), na.rm = TRUE)
  preds    <- last_val + drift * seq_len(n_ahead)
  list(predictions = preds, lower = preds * 0.97, upper = preds * 1.03, method = "Naive Drift")
}

# Random Forest 7-day return prediction ------------------------------------
run_rf_prediction <- function(data_xts, n_ahead = 7) {
  if (is.null(data_xts) || NROW(data_xts) < 40)
    return(list(predictions = NULL, error = "Not enough data for Random Forest"))

  close <- as.numeric(Cl(data_xts))
  high  <- as.numeric(Hi(data_xts))
  low   <- as.numeric(Lo(data_xts))
  vol   <- as.numeric(Vo(data_xts))
  n     <- length(close)

  log_ret <- c(NA, diff(log(close)))

  lag_vec <- function(x, k) c(rep(NA, k), head(x, length(x) - k))
  roll_ma <- function(x, k) {
    r <- rep(NA, length(x))
    for (i in k:length(x)) r[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
    r
  }

  rsi14      <- tryCatch(as.numeric(RSI(Cl(data_xts), n = 14)), error = function(e) rep(NA, n))
  macd_obj   <- tryCatch(MACD(Cl(data_xts), nFast = 12, nSlow = 26, nSig = 9), error = function(e) NULL)
  macd_line  <- if (!is.null(macd_obj)) as.numeric(macd_obj[,"macd"])   else rep(NA, n)
  macd_sig   <- if (!is.null(macd_obj)) as.numeric(macd_obj[,"signal"]) else rep(NA, n)

  vol20ma    <- roll_ma(vol, 20)
  vol_ratio  <- ifelse(vol20ma > 0, vol / vol20ma, 1)
  hl_range   <- (high - low) / close

  df <- data.frame(
    ret_fwd  = c(log_ret[-1], NA),
    ret_lag1 = lag_vec(log_ret, 1),
    ret_lag2 = lag_vec(log_ret, 2),
    ret_lag5 = lag_vec(log_ret, 5),
    ma5_ret  = roll_ma(log_ret, 5),
    ma20_ret = roll_ma(log_ret, 20),
    rsi14    = rsi14,
    macd     = macd_line,
    macd_sig = macd_sig,
    vol_ratio= vol_ratio,
    hl_range = hl_range
  )

  df_clean <- na.omit(df)
  if (nrow(df_clean) < 30)
    return(list(predictions = NULL, error = "Not enough clean rows"))

  train <- df_clean[-nrow(df_clean), ]

  rf_model <- tryCatch(
    randomForest::randomForest(ret_fwd ~ ., data = train, ntree = 500, importance = TRUE),
    error = function(e) NULL
  )
  if (is.null(rf_model))
    return(list(predictions = NULL, error = "randomForest training failed"))

  current <- df_clean[nrow(df_clean), -1, drop = FALSE]
  preds   <- numeric(n_ahead)
  for (i in seq_len(n_ahead)) {
    p           <- tryCatch(as.numeric(predict(rf_model, newdata = current)), error = function(e) 0)
    preds[i]    <- p
    current$ret_lag2 <- current$ret_lag1
    current$ret_lag1 <- p
    current$ma5_ret  <- mean(c(preds[max(1, i-4):i]), na.rm = TRUE)
  }

  # Importance
  imp <- tryCatch({
    imp_mat <- randomForest::importance(rf_model)
    data.frame(
      Feature    = rownames(imp_mat),
      Importance = round(imp_mat[, "%IncMSE"], 3)
    ) |> arrange(desc(Importance)) |> head(6)
  }, error = function(e) NULL)

  list(predictions = preds, importance = imp, method = "Random Forest (randomForest pkg)")
}

# ----------------------------------------------------
############ LLM Context Builder ####################
# ----------------------------------------------------

make_stock_context_string <- function(ticker, data_xts, name = ticker) {
  if (is.null(data_xts) || NROW(data_xts) < 2)
    return(paste0("Currently viewing: ", name, " (", ticker, "). No recent market data available."))

  close      <- Cl(data_xts)
  last_price <- as.numeric(tail(close, 1))
  prev_price <- as.numeric(tail(close, 2)[1])
  change     <- last_price - prev_price
  change_pct <- (change / prev_price) * 100
  high_52w   <- as.numeric(max(Hi(data_xts), na.rm = TRUE))
  low_52w    <- as.numeric(min(Lo(data_xts), na.rm = TRUE))
  last_vol   <- as.numeric(tail(Vo(data_xts), 1))
  avg_vol    <- mean(as.numeric(Vo(data_xts)), na.rm = TRUE)

  rsi14      <- tryCatch(as.numeric(tail(RSI(close, n = 14), 1)), error = function(e) NA)
  macd_obj   <- tryCatch(MACD(close), error = function(e) NULL)
  last_macd  <- if (!is.null(macd_obj)) as.numeric(tail(macd_obj[,"macd"], 1))   else NA
  last_sig   <- if (!is.null(macd_obj)) as.numeric(tail(macd_obj[,"signal"], 1)) else NA

  rsi_label  <- if (!is.na(rsi14)) {
    if (rsi14 >= 70) "OVERBOUGHT" else if (rsi14 <= 30) "OVERSOLD" else "NEUTRAL"
  } else "N/A"

  macd_label <- if (!is.na(last_macd) && !is.na(last_sig)) {
    if (last_macd > last_sig) "BULLISH CROSSOVER" else "BEARISH CROSSOVER"
  } else "N/A"

  last5 <- tail(data.frame(
    Date  = format(index(data_xts)),
    Close = round(as.numeric(Cl(data_xts)), 2)
  ), 5)
  price_lines <- paste(apply(last5, 1, function(r) paste0("  ", r[1], ": $", r[2])), collapse = "\n")

  sprintf(
"## Live Dashboard Context — %s (%s)

### Price Action
- **Last Price:** $%.2f
- **Change:** %+.2f (%+.2f%%)
- **52-Week High:** $%.2f | **52-Week Low:** $%.2f
- **Volume (last session):** %.2fM shares
- **Avg Daily Volume:** %.2fM shares

### Technical Indicators
| Indicator | Value | Signal |
|-----------|-------|--------|
| RSI(14)   | %.1f  | %s     |
| MACD      | %.4f  | %s     |
| Signal    | %.4f  |        |

### Recent Closing Prices (Last 5 Sessions)
%s

---
*Data sourced from Yahoo Finance via quantmod. Respond to user queries about this stock using the above data.*",
    name, ticker,
    last_price,
    change, change_pct,
    high_52w, low_52w,
    last_vol / 1e6, avg_vol / 1e6,
    if (!is.na(rsi14)) rsi14 else 0, rsi_label,
    if (!is.na(last_macd)) last_macd else 0, macd_label,
    if (!is.na(last_sig))  last_sig  else 0,
    price_lines
  )
}

make_chat_client <- function(api_key, system_prompt) {
  tryCatch(
    ellmer::chat_google_gemini(
      model         = "gemini-2.5-flash",
      api_key       = api_key,
      system_prompt = system_prompt
    ),
    error = function(e) e
  )
}

# ----------------------------------------------------
########### CSS Styles ##############################
# ----------------------------------------------------

APP_CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500;600;700&display=swap');

/* ── Base ── */
body {
  background: linear-gradient(135deg, #0a0e27 0%, #131722 100%);
  font-family: 'Inter', sans-serif;
  color: #e0e6ed;
  padding-bottom: 60px;
}

/* ── Navbar ── */
.navbar-default {
  background: linear-gradient(180deg, #0d1117 0%, #131722 100%) !important;
  border-bottom: 2px solid #2962ff !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.5);
  padding: 0 20px;
  min-height: 56px;
}
.navbar-brand {
  color: #2962ff !important;
  font-family: 'JetBrains Mono', monospace !important;
  font-weight: 700 !important;
  font-size: 20px !important;
  letter-spacing: 1px;
  padding: 12px 15px !important;
}
.navbar-nav > li > a {
  color: #9aa3af !important;
  font-weight: 500 !important;
  font-size: 13px;
  letter-spacing: 0.5px;
  padding: 18px 16px !important;
  transition: color 0.2s, background 0.2s;
}
.navbar-nav > .active > a,
.navbar-nav > .active > a:hover,
.navbar-nav > .active > a:focus {
  color: #ffffff !important;
  background: rgba(41,98,255,0.18) !important;
}
.navbar-nav > li > a:hover {
  color: #ffffff !important;
  background: rgba(255,255,255,0.06) !important;
}

/* ── Header banner ── */
.header-image-container {
  width: 100%;
  max-height: 180px;
  overflow: hidden;
}
.header-image-container img {
  width: 100%;
  object-fit: cover;
}

/* ── Cards ── */
.card-dark {
  background: #1e2431;
  border-radius: 12px;
  padding: 20px;
  border: 1px solid #2a2e39;
  margin-bottom: 20px;
  transition: box-shadow 0.25s ease, transform 0.25s ease;
}
.card-dark:hover {
  box-shadow: 0 8px 32px rgba(41,98,255,0.15);
  transform: translateY(-1px);
}
.card-darker {
  background: #131722;
  border-radius: 12px;
  padding: 20px;
  border: 1px solid #2a2e39;
  margin-bottom: 20px;
}
.stock-header {
  background: linear-gradient(135deg, #1e2431 0%, #252b3d 100%);
  border-radius: 10px;
  padding: 20px;
  margin-bottom: 20px;
  border: 1px solid #2a2e39;
  border-left: 4px solid #2962ff;
}
.stat-card {
  background: #1e2431;
  border-radius: 8px;
  padding: 15px;
  border: 1px solid #2a2e39;
  margin-bottom: 10px;
  transition: border-color 0.2s;
}
.stat-card:hover { border-color: #2962ff; }
.stat-label {
  font-size: 11px;
  color: #787b86;
  text-transform: uppercase;
  letter-spacing: 0.8px;
  font-weight: 600;
  margin-bottom: 6px;
  display: flex;
  align-items: center;
  gap: 6px;
}
.stat-value { font-size: 20px; font-weight: 700; color: #fff; font-family: 'JetBrains Mono', monospace; }
.stat-high  { color: #26a69a; }
.stat-low   { color: #ef5350; }

/* ── Prediction cards ── */
.pred-card {
  background: #1e2431;
  border-radius: 12px;
  padding: 20px;
  border: 1px solid #2a2e39;
  margin-bottom: 20px;
}
.pred-title {
  font-family: 'JetBrains Mono', monospace;
  font-size: 15px;
  font-weight: 700;
  color: #fff;
  margin-bottom: 12px;
  display: flex;
  align-items: center;
  gap: 10px;
}
.model-badge {
  display: inline-flex;
  align-items: center;
  padding: 3px 10px;
  border-radius: 20px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.5px;
  font-family: 'JetBrains Mono', monospace;
}
.badge-lstm { background: rgba(41,98,255,0.2); color: #5c8aff; border: 1px solid rgba(41,98,255,0.35); }
.badge-rf   { background: rgba(38,166,154,0.2); color: #26a69a; border: 1px solid rgba(38,166,154,0.35); }

/* ── Typography ── */
.stock-name   { font-size: clamp(18px,2.2vw,24px); font-weight: 700; color: #fff; font-family: 'JetBrains Mono', monospace; }
.stock-ticker { font-size: 13px; color: #9aa3af; font-weight: 600; font-family: 'JetBrains Mono', monospace; letter-spacing: 1px; }
.price-big    { font-size: clamp(24px,3.2vw,36px); font-weight: 700; color: #fff; font-family: 'JetBrains Mono', monospace; }
.price-change { font-size: 16px; font-weight: 600; padding: 5px 12px; border-radius: 6px; display: inline-block; font-family: 'JetBrains Mono', monospace; }
.price-up     { color: #26a69a; background: rgba(38,166,154,0.12); }
.price-down   { color: #ef5350; background: rgba(239,83,80,0.12); }
h3, h4        { color: #fff; font-weight: 700; font-family: 'JetBrains Mono', monospace; }
.section-header {
  font-family: 'JetBrains Mono', monospace;
  font-size: 16px;
  font-weight: 700;
  color: #fff;
  padding: 12px 18px;
  background: linear-gradient(135deg, #1e2431 0%, #252b3d 100%);
  border-radius: 8px;
  border-left: 4px solid #2962ff;
  margin-bottom: 15px;
}

/* ── Buttons ── */
.btn-primary-custom {
  background: linear-gradient(135deg, #2962ff 0%, #1e88e5 100%);
  border: none;
  border-radius: 8px;
  padding: 10px 20px;
  font-weight: 600;
  font-size: 13px;
  color: #fff;
  cursor: pointer;
  transition: all 0.25s;
  letter-spacing: 0.5px;
}
.btn-primary-custom:hover {
  background: linear-gradient(135deg, #1e88e5 0%, #1565c0 100%);
  box-shadow: 0 4px 16px rgba(41,98,255,0.45);
  transform: translateY(-1px);
}
.btn-primary { background: linear-gradient(135deg, #2962ff 0%, #1e88e5 100%); border: none; border-radius: 8px; padding: 12px 24px; font-weight: 600; transition: all .3s; }
.btn-primary:hover { background: linear-gradient(135deg, #1e88e5 0%, #1565c0 100%); transform: translateY(-2px); box-shadow: 0 4px 12px rgba(41,98,255,.4); }
.btn-context {
  background: linear-gradient(135deg, #ff6f00 0%, #ff8f00 100%);
  border: none; border-radius: 8px; padding: 10px 16px;
  font-weight: 600; font-size: 12px; color: #fff; cursor: pointer;
  transition: all 0.25s; letter-spacing: 0.5px; width: 100%;
}
.btn-context:hover { background: linear-gradient(135deg, #e65100 0%, #ff6f00 100%); box-shadow: 0 4px 16px rgba(255,111,0,0.4); transform: translateY(-1px); }

/* ── Form controls ── */
.selectize-input, .form-control, select {
  background: #1e2431 !important;
  border: 1px solid #2a2e39 !important;
  color: #e0e6ed !important;
  font-weight: 600;
}
.selectize-input .item, .selectize-input .item * { color: #fff !important; background-color: #252b3d !important; }
.selectize-input input { color: #e0e6ed !important; }
.selectize-dropdown { background: #1e2431 !important; border: 1px solid #2a2e39 !important; }
.selectize-dropdown .option { color: #e0e6ed !important; border-bottom: 1px solid #2a2e39; }
.selectize-dropdown .option:hover { background: #252b3d !important; }
.well { background: #131722; border: 1px solid #2a2e39; border-radius: 12px; box-shadow: none; }
.api-input { background: #1e2431; border: 1px solid #2a2e39; border-radius: 8px; padding: 12px; margin-bottom: 12px; }
label { color: #9aa3af !important; font-size: 12px; font-weight: 600; letter-spacing: 0.4px; }
.checkbox label { color: #e0e6ed !important; font-weight: 500; font-size: 14px; }
hr { border-color: #2a2e39; opacity: 0.4; }

/* ── Tabs (inner) ── */
.nav-tabs { border-bottom: 2px solid #2a2e39; }
.nav-tabs > li > a { color: #787b86 !important; border: none !important; border-bottom: 2px solid transparent; background: transparent !important; font-weight: 600; font-size: 13px; padding: 10px 16px; transition: all 0.2s; }
.nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover { color: #fff !important; border-bottom: 2px solid #2962ff !important; background: transparent !important; }
.nav-tabs > li > a:hover { color: #c0c8d4 !important; border-bottom: 2px solid #2a2e39 !important; }
.tab-content { padding-top: 15px; }

/* ── Footer ── */
.app-footer {
  position: fixed; left: 0; right: 0; bottom: 0;
  height: 52px; z-index: 1000;
  display: flex; align-items: center; justify-content: center; gap: 18px;
  background: linear-gradient(180deg, #252b3d 0%, #1e2431 100%);
  border-top: 1px solid #2a2e39;
  box-shadow: 0 -6px 20px rgba(0,0,0,0.4);
  font-family: 'JetBrains Mono', monospace;
}
.app-footer, .app-footer *:not(i) { color: #9aa3af !important; font-family: 'JetBrains Mono', monospace !important; }
.app-footer a { text-decoration: none; font-weight: 600; }
.app-footer a:hover { color: #fff !important; text-decoration: underline; }

/* ── Misc ── */
::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: #0d1117; }
::-webkit-scrollbar-thumb { background: #2a2e39; border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: #363a45; }
.context-badge {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 4px 10px; border-radius: 20px;
  background: rgba(255,152,0,0.15); border: 1px solid rgba(255,152,0,0.3);
  color: #ff9800; font-size: 11px; font-weight: 700; font-family: 'JetBrains Mono', monospace;
}
.shiny-spinner-output-container .load-container { background: transparent !important; }
"

# ----------------------------------------------------
########### UI #######################################
# ----------------------------------------------------

ui <- navbarPage(
  title = "\u26a1 FinDash Pro",
  id    = "main_nav",
  collapsible = TRUE,
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"),
      tags$style(HTML(APP_CSS))
    ),
    # Cover image below navbar
    tags$div(
      class = "header-image-container",
      tags$img(src = "https://ozancanozdemir.github.io/shiny_cover.png")
    )
  ),
  footer = tags$div(
    class = "app-footer",
    tags$span(HTML("&copy;"), "2025 Ozancan Ozdemir"),
    tags$span("|"),
    tags$a(href = "https://github.com/ozancanozdemir/2025-Posit-Table-Contest", target = "_blank",
           tags$i(class = "fab fa-github"), " Source"),
    tags$span("|"),
    tags$a(href = "https://github.com/ozancanozdemir/2025-Posit-Table-Contest/issues", target = "_blank",
           tags$i(class = "fa fa-bug"), " Report Issue")
  ),

  # ── Tab 1: Market Overview ────────────────────────────────────────────────
  tabPanel(
    title = tags$span(tags$i(class = "fa fa-chart-pie", style = "margin-right:5px;"), "Market"),
    value = "tab_market",
    div(style = "padding: 20px;",
        fluidRow(
          column(6,
            div(class = "card-darker",
                h4(tags$i(class="fa fa-building-columns", style="color:#2962ff;margin-right:8px;"),
                   "S&P 500 Market Overview"),
                fluidRow(
                  column(6, selectInput("market_cap_sort_col","Sort By",
                                        choices = c("MarketCap","Price","Revenue"),
                                        selected = "MarketCap")),
                  column(6, selectInput("market_cap_sort_dir","Direction",
                                        choices = c("Descending"="desc","Ascending"="asc"),
                                        selected = "desc"))
                ),
                div(style = "max-height:640px;overflow-y:auto;",
                    withSpinner(gt_output("key_stats_table"), color = "#2962ff"))
            )
          ),
          column(6,
            div(class = "card-dark",
                h4(tags$i(class="fa fa-table-cells", style="color:#26a69a;margin-right:8px;"),
                   "S&P 500 Fundamental Metrics"),
                fluidRow(
                  column(6, selectInput("fund_sort_col","Sort By",
                                        choices = c("P/E Ratio"="P_E","EPS (TTM)"="EPS_dilTTM",
                                                    "Div. Yield"="Div_Yield_Pct","Rating"="Analyst_Rating"),
                                        selected = "P_E")),
                  column(6, selectInput("fund_sort_dir","Direction",
                                        choices = c("Ascending"="asc","Descending"="desc"),
                                        selected = "asc"))
                ),
                selectInput("fund_table_rows","Rows",
                            choices = c("10","20","50","All"), selected = "10"),
                div(style = "max-height:420px;overflow-y:auto;",
                    withSpinner(gt_output("fundamental_table"), color = "#26a69a"))
            )
          )
        )
    )
  ),

  # ── Tab 2: Stock Analysis ─────────────────────────────────────────────────
  tabPanel(
    title = tags$span(tags$i(class = "fa fa-chart-line", style = "margin-right:5px;"), "Stock Analysis"),
    value = "tab_stock",
    div(style = "padding: 20px;",

      # Controls row
      div(class = "card-dark",
          fluidRow(
            column(3, selectInput("interval","TIME PERIOD",
                                  choices = c("1D"="1day","1W"="7days","1M"="1month",
                                              "3M"="3months","6M"="6months","1Y"="1year",
                                              "2Y"="2years","5Y"="5years"),
                                  selected = "3months")),
            column(3, uiOutput("ticker1_select_ui")),
            column(3, conditionalPanel("input.compare_mode == true", uiOutput("ticker2_select_ui"))),
            column(3,
                   checkboxInput("compare_mode","Compare Mode", FALSE),
                   actionButton("refresh","REFRESH DATA", class = "btn-primary",
                                style = "width:100%;margin-top:6px;font-size:12px;"))
          )
      ),

      # Stock header
      uiOutput("top_header_ui"),

      # Stats bar
      fluidRow(
        column(3, div(class="stat-card",
                      div(class="stat-label", ICON_MAP$volume, "VOLUME"),
                      div(class="stat-value", textOutput("volume_stat", inline=TRUE)))),
        column(3, div(class="stat-card",
                      div(class="stat-label", ICON_MAP$high, "52W HIGH"),
                      div(class="stat-value stat-high", textOutput("high_52w_stat", inline=TRUE)))),
        column(3, div(class="stat-card",
                      div(class="stat-label", ICON_MAP$low, "52W LOW"),
                      div(class="stat-value stat-low", textOutput("low_52w_stat", inline=TRUE)))),
        column(3, div(class="stat-card",
                      div(class="stat-label", ICON_MAP$avg, "AVG VOLUME"),
                      div(class="stat-value", textOutput("avg_volume_stat", inline=TRUE))))
      ),

      br(),

      # Inner tab: Chart vs Predictions
      tabsetPanel(id = "stock_inner_tabs",

        # ── Chart & Data ──────────────────────────────────
        tabPanel(
          title = tags$span(tags$i(class="fa fa-chart-candlestick",
                                   style="margin-right:5px;"), "Chart & Data"),
          br(),
          conditionalPanel(
            "input.compare_mode == false",
            div(class = "section-header",
                tags$i(class="fa fa-table", style="color:#2962ff;margin-right:8px;"),
                "RECENT TRADING DATA — ", tags$span(style="color:#2962ff;", textOutput("table_ticker1", inline=TRUE))),
            withSpinner(gt_output("price_table1"), color = "#2962ff")
          ),
          conditionalPanel(
            "input.compare_mode == true",
            h4(tags$i(class="fa fa-scale-balanced", style="color:#ff9800;margin-right:8px;"),
               "COMPARISON DATA"),
            fluidRow(
              column(6,
                     div(class="section-header", style="border-left-color:#2962ff;",
                         tags$i(class="fa fa-chart-line"), " ", textOutput("table_ticker1_compare", inline=TRUE)),
                     gt_output("price_table1_compare")),
              column(6,
                     div(class="section-header", style="border-left-color:#26a69a;",
                         tags$i(class="fa fa-chart-line"), " ", textOutput("table_ticker2_compare", inline=TRUE)),
                     gt_output("price_table2_compare"))
            )
          ),
          br(),
          withSpinner(plotlyOutput("price_plot", height = "480px"), color = "#2962ff")
        ),

        # ── Predictions ───────────────────────────────────
        tabPanel(
          title = tags$span(tags$i(class="fa fa-brain", style="margin-right:5px;color:#9c27b0;"),
                            "ML Predictions"),
          br(),
          div(class = "card-dark",
              fluidRow(
                column(8,
                       p(style = "color:#9aa3af;font-size:13px;margin-bottom:0;",
                         "Run ML models on the selected stock to forecast future prices (LSTM) and returns (Random Forest).",
                         " Training runs live on historical data — may take up to ~30 seconds for LSTM."),
                       p(style = "color:#787b86;font-size:11px;margin-bottom:0;margin-top:4px;",
                         tags$i(class="fa fa-circle-info"),
                         " LSTM requires the ", tags$code("keras3"), " R package + TensorFlow.",
                         " If unavailable, ETS forecasting is used as fallback.")
                ),
                column(4,
                       actionButton("run_predictions","RUN PREDICTIONS",
                                    class = "btn-primary",
                                    style = "width:100%;font-weight:700;letter-spacing:1px;"),
                       uiOutput("pred_method_badges")
                )
              )
          ),

          fluidRow(
            # LSTM Panel
            column(6,
                   div(class = "pred-card",
                       div(class = "pred-title",
                           tags$i(class="fa fa-brain", style="color:#9c27b0;"),
                           "LSTM — 7-Day Price Forecast",
                           tags$span(class="model-badge badge-lstm", "DEEP LEARNING")),
                       uiOutput("lstm_metrics_ui"),
                       withSpinner(plotlyOutput("lstm_plot", height = "340px"), color = "#2962ff"),
                       br(),
                       withSpinner(gt_output("lstm_table"), color = "#2962ff")
                   )
            ),
            # RF Panel
            column(6,
                   div(class = "pred-card",
                       div(class = "pred-title",
                           tags$i(class="fa fa-tree", style="color:#26a69a;"),
                           "Random Forest — 7-Day Return Forecast",
                           tags$span(class="model-badge badge-rf", "ENSEMBLE")),
                       uiOutput("rf_metrics_ui"),
                       withSpinner(plotlyOutput("rf_plot", height = "340px"), color = "#26a69a"),
                       br(),
                       withSpinner(gt_output("rf_table"), color = "#26a69a")
                   )
            )
          )
        )
      )
    )
  ),

  # ── Tab 3: Portfolio ──────────────────────────────────────────────────────
  tabPanel(
    title = tags$span(tags$i(class="fa fa-briefcase", style="margin-right:5px;"), "Portfolio"),
    value = "tab_portfolio",
    div(style = "padding: 20px;",
        div(class = "card-dark",
            h4(tags$i(class="fa fa-wallet", style="color:#ff9800;margin-right:8px;"),
               "Portfolio Calculator"),
            fluidRow(
              column(6,
                     uiOutput("pf_tickers_ui"),
                     uiOutput("pf_weights_ui")
              ),
              column(3,
                     selectInput("pf_interval","PERIOD",
                                 choices = c("3M"="3months","6M"="6months","1Y"="1year","2Y"="2years"),
                                 selected = "1year"),
                     numericInput("pf_capital","Initial Capital (USD)", value = 10000, min = 100, step = 100),
                     checkboxInput("pf_normalize","Auto-normalise weights", TRUE),
                     actionButton("pf_run","CALCULATE", class = "btn-primary", style = "width:100%;")
              ),
              column(3,
                     div(class="stat-card",
                         div(class="stat-label", ICON_MAP$avg, "CAGR"),
                         div(class="stat-value", textOutput("pf_cagr", inline=TRUE))),
                     div(class="stat-card",
                         div(class="stat-label", ICON_MAP$volume, "Volatility (ann.)"),
                         div(class="stat-value", textOutput("pf_vol", inline=TRUE))),
                     div(class="stat-card",
                         div(class="stat-label", ICON_MAP$high, "Sharpe (rf=0)"),
                         div(class="stat-value stat-high", textOutput("pf_sharpe", inline=TRUE)))
              )
            ),
            br(),
            fluidRow(
              column(6, gt_output("pf_alloc_table")),
              column(6, withSpinner(plotlyOutput("pf_equity_plot", height = "360px"), color = "#2962ff"))
            )
        )
    )
  ),

  # ── Tab 4: News & AI ──────────────────────────────────────────────────────
  tabPanel(
    title = tags$span(tags$i(class="fa fa-newspaper", style="margin-right:5px;"), "News & AI"),
    value = "tab_news",
    div(style = "padding: 20px;",
        fluidRow(
          # News column
          column(6,
                 div(class = "card-darker",
                     h4(tags$i(class="fa fa-rss", style="color:#ff9800;margin-right:8px;"),
                        "MARKET NEWS"),
                     uiOutput("riingo_selected_info"),
                     br(),
                     fluidRow(
                       column(8, selectInput("riingo_source","SOURCE DOMAIN",
                                             choices = c("bloomberg.com","reuters.com","wsj.com",
                                                         "seekingalpha.com","marketwatch.com","cnbc.com"),
                                             selected = "bloomberg.com")),
                       column(4, numericInput("riingo_limit","SHOW LAST",
                                              value = 10, min = 5, max = 50, step = 1))
                     ),
                     withSpinner(gt_output("riingo_news_gt"), color = "#ff9800")
                 )
          ),
          # AI Chat column
          column(6,
                 div(class = "card-dark",
                     h4(tags$i(class="fa fa-robot", style="color:#2962ff;margin-right:8px;"),
                        "AI TRADING ASSISTANT"),

                     # API key + context section
                     div(class = "api-input",
                         fluidRow(
                           column(9,
                                  passwordInput("gemini_api_key","GEMINI API KEY",
                                                placeholder = "Enter your Gemini API key…",
                                                width = "100%"),
                                  tags$small(style = "color:#787b86;",
                                             "Get a free key at ",
                                             tags$a(href="https://aistudio.google.com/app/apikey",
                                                    target="_blank", style="color:#2962ff;",
                                                    "Google AI Studio"))
                           ),
                           column(3,
                                  br(),
                                  actionButton("set_api_key","CONNECT",
                                               class="btn-primary",
                                               style="width:100%;margin-top:4px;font-size:12px;")
                           )
                         )
                     ),

                     # Context status + refresh button
                     div(class = "api-input", style="padding:10px;",
                         fluidRow(
                           column(8, uiOutput("context_status_ui")),
                           column(4,
                                  actionButton("refresh_context","UPDATE CONTEXT",
                                               class="btn-context",
                                               title="Reload current stock data into LLM context"))
                         )
                     ),

                     div(style = "height:420px;", chat_mod_ui("stock_chat"))
                 )
          )
        )
    )
  )
)

# ----------------------------------------------------
########### Server ###################################
# ----------------------------------------------------

server <- function(input, output, session) {

  TIINGO_TOKEN <- "8c7094ec74e7fc1ceca99a468fc4770df03dd0ec"
  riingo_set_token(TIINGO_TOKEN)

  sp500_tickers <- get_sp500_tickers()

  # ── Ticker selects ----------------------------------------------------------
  make_ticker_selectize <- function(id, label, default) {
    labels_with_logo <- paste0(sp500_tickers$display, "||", sp500_tickers$logo)
    choices_named    <- setNames(sp500_tickers$ticker, labels_with_logo)
    render_js <- I("{
option: function(item, escape) {
  var parts = String(item.label).split('||');
  var img = parts[1] ? '<img src=\"'+escape(parts[1])+'\" width=\"18\" height=\"18\" style=\"margin-right:7px;vertical-align:middle;border-radius:3px;\">' : '';
  return '<div>' + img + escape(parts[0]) + '</div>';
},
item: function(item, escape) {
  var parts = String(item.label).split('||');
  var img = parts[1] ? '<img src=\"'+escape(parts[1])+'\" width=\"16\" height=\"16\" style=\"margin-right:6px;vertical-align:middle;border-radius:3px;\">' : '';
  return '<div>' + img + escape(parts[0]) + '</div>';
}}")
    selectizeInput(id, label, choices = choices_named, selected = default,
                   width = "100%", options = list(render = render_js))
  }

  output$ticker1_select_ui <- renderUI({ make_ticker_selectize("ticker1","PRIMARY STOCK","AAPL") })
  output$ticker2_select_ui <- renderUI({ make_ticker_selectize("ticker2","COMPARE WITH","MSFT") })

  # ── Stock data reactives ────────────────────────────────────────────────────
  stock_data1 <- reactive({
    req(input$ticker1)
    get_stock_data(input$ticker1, input$interval)
  }) |> bindEvent(input$ticker1, input$interval, input$refresh)

  stock_data2 <- reactive({
    req(input$ticker2, input$compare_mode)
    get_stock_data(input$ticker2, input$interval)
  }) |> bindEvent(input$ticker2, input$interval, input$refresh, input$compare_mode)

  # ── Stock header ─────────────────────────────────────────────────────────────
  make_stock_header <- function(ticker) {
    tinfo <- sp500_tickers[sp500_tickers$ticker == ticker, ]
    logo  <- if (nrow(tinfo) > 0) tinfo$logo[1] else paste0("https://www.google.com/s2/favicons?sz=64&domain=", tolower(ticker), ".com")
    name  <- if (nrow(tinfo) > 0) tinfo$name[1] else ticker
    div(style = "display:flex;align-items:center;gap:14px;",
        tags$img(src = logo, style = "width:44px;height:44px;border-radius:8px;object-fit:contain;"),
        div(div(class="stock-name", name), div(class="stock-ticker", ticker))
    )
  }

  make_price_bar <- function(data_xts) {
    req(data_xts)
    last_price  <- round(as.numeric(tail(Cl(data_xts), 1)), 2)
    prev_price  <- round(as.numeric(tail(Cl(data_xts), 2)[1]), 2)
    change      <- last_price - prev_price
    change_pct  <- (change / prev_price) * 100
    chg_class   <- if (change >= 0) "price-up" else "price-down"
    chg_sym     <- if (change >= 0) "\u25b2" else "\u25bc"
    div(style = "text-align:right;",
        div(class="price-big", paste0("$", format(last_price, big.mark=","))),
        div(class=paste("price-change", chg_class),
            paste0(chg_sym, " $", abs(round(change,2)), " (",
                   sprintf("%.2f", abs(change_pct)), "%)"))
    )
  }

  output$top_header_ui <- renderUI({
    if (isTRUE(input$compare_mode)) {
      div(class="stock-header",
          fluidRow(
            column(6, make_stock_header(input$ticker1)),
            column(6, make_stock_header(input$ticker2))
          ),
          br(),
          fluidRow(
            column(6, make_price_bar(stock_data1())),
            column(6, make_price_bar(stock_data2()))
          )
      )
    } else {
      div(class="stock-header",
          fluidRow(
            column(6, make_stock_header(input$ticker1)),
            column(6, make_price_bar(stock_data1()))
          )
      )
    }
  })

  # ── Stats ───────────────────────────────────────────────────────────────────
  output$volume_stat <- renderText({
    d <- stock_data1(); req(d)
    paste0(format(round(as.numeric(tail(Vo(d),1))/1e6, 1), big.mark=","), "M")
  })
  output$high_52w_stat <- renderText({
    d <- stock_data1(); req(d)
    paste0("$", format(round(max(Hi(d), na.rm=TRUE), 2), big.mark=","))
  })
  output$low_52w_stat <- renderText({
    d <- stock_data1(); req(d)
    paste0("$", format(round(min(Lo(d), na.rm=TRUE), 2), big.mark=","))
  })
  output$avg_volume_stat <- renderText({
    d <- stock_data1(); req(d)
    paste0(format(round(mean(as.numeric(Vo(d)), na.rm=TRUE)/1e6, 1), big.mark=","), "M")
  })

  output$table_ticker1         <- renderText({ req(input$ticker1); input$ticker1 })
  output$table_ticker1_compare <- renderText({ req(input$ticker1); input$ticker1 })
  output$table_ticker2_compare <- renderText({ req(input$ticker2); input$ticker2 })

  # ── Market cap data ─────────────────────────────────────────────────────────
  market_cap_data_raw <- reactive({ get_market_cap_data() })

  last_month_prices <- reactive({
    req(market_cap_data_raw())
    pf_get_prices_for_sparkline(market_cap_data_raw()$Ticker, days = 30)
  })

  market_cap_data_sorted <- reactive({
    df <- market_cap_data_raw()
    req(df, input$market_cap_sort_col, input$market_cap_sort_dir)
    if (input$market_cap_sort_dir == "asc")
      df |> arrange(!!sym(input$market_cap_sort_col))
    else
      df |> arrange(desc(!!sym(input$market_cap_sort_col)))
  })

  output$key_stats_table <- render_gt({
    make_key_stats_gt_alternative(market_cap_data_sorted(), last_month_prices())
  })

  # ── Price tables ─────────────────────────────────────────────────────────────
  output$price_table1         <- render_gt({ create_table(stock_data1(), input$ticker1) })
  output$price_table1_compare <- render_gt({ create_table(stock_data1(), input$ticker1) })
  output$price_table2_compare <- render_gt({ create_table(stock_data2(), input$ticker2) })

  # ── Main candlestick / comparison plot ──────────────────────────────────────
  output$price_plot <- renderPlotly({
    data1 <- stock_data1(); req(data1)

    if (!input$compare_mode) {
      df <- data.frame(
        Date  = index(data1), Open  = as.numeric(Op(data1)),
        High  = as.numeric(Hi(data1)), Low   = as.numeric(Lo(data1)),
        Close = as.numeric(Cl(data1))
      )
      plot_ly(df, type="candlestick",
              x=~Date, open=~Open, close=~Close, high=~High, low=~Low,
              increasing=list(fillcolor="#26a69a", line=list(color="#26a69a")),
              decreasing=list(fillcolor="#ef5350", line=list(color="#ef5350"))) |>
        layout(
          title=list(text=paste("Price Chart —", input$ticker1), font=list(color="#787b86")),
          paper_bgcolor="#131722", plot_bgcolor="#131722",
          xaxis=list(title="", gridcolor="#2a2e39", color="#787b86"),
          yaxis=list(title="Price (USD)", gridcolor="#2a2e39", color="#787b86"),
          font=list(color="#e0e6ed"), hovermode="x unified",
          legend=list(x=0.01,y=0.99,bgcolor="rgba(30,36,49,.8)",
                      bordercolor="#2a2e39",font=list(color="#e0e6ed")),
          margin=list(l=60,r=40,t=40,b=40)
        )
    } else {
      data2 <- stock_data2(); req(data2)
      df1   <- data.frame(Date=index(data1), Close=as.numeric(Cl(data1)))
      df2   <- data.frame(Date=index(data2), Close=as.numeric(Cl(data2)))
      df1$N <- (df1$Close / df1$Close[1] - 1) * 100
      df2$N <- (df2$Close / df2$Close[1] - 1) * 100

      plot_ly() |>
        add_lines(data=df1, x=~Date, y=~N, name=input$ticker1,
                  line=list(color="#2962ff",width=2.5)) |>
        add_lines(data=df2, x=~Date, y=~N, name=input$ticker2,
                  line=list(color="#26a69a",width=2.5)) |>
        layout(
          title=list(text=paste("Normalised Return Comparison:",input$ticker1,"vs",input$ticker2),
                     font=list(color="#787b86")),
          paper_bgcolor="#131722", plot_bgcolor="#131722",
          xaxis=list(title="",gridcolor="#2a2e39",color="#787b86"),
          yaxis=list(title="Return (%)",gridcolor="#2a2e39",color="#787b86"),
          font=list(color="#e0e6ed"), hovermode="x unified",
          legend=list(x=0.01,y=0.99,bgcolor="rgba(30,36,49,.8)",
                      bordercolor="#2a2e39",font=list(color="#e0e6ed")),
          margin=list(l=60,r=40,t=40,b=40)
        )
    }
  })

  # ── Fundamental table ────────────────────────────────────────────────────────
  fundamental_data_all <- reactive({ get_fund_data() })

  fundamental_data_sorted <- reactive({
    data <- fundamental_data_all()
    req(data, input$fund_sort_col, input$fund_sort_dir)
    if (input$fund_sort_col == "Analyst_Rating") {
      lvls           <- c("Strong buy","Buy","Neutral","Sell","Strong sell")
      data$rat_order <- match(data$Analyst_Rating, lvls)
      data <- if (input$fund_sort_dir == "desc")
        data |> arrange(rat_order) else data |> arrange(desc(rat_order))
      data <- data |> select(-rat_order)
    } else {
      data <- if (input$fund_sort_dir == "asc")
        data |> arrange(!!sym(input$fund_sort_col)) else
        data |> arrange(desc(!!sym(input$fund_sort_col)))
    }
    rows <- input$fund_table_rows
    if (rows != "All") data <- head(data, as.numeric(rows))
    data
  })

  output$fundamental_table <- render_gt({ req(fundamental_data_sorted()); make_fund_gt(fundamental_data_sorted()) })

  # ── ML Predictions ────────────────────────────────────────────────────────────
  pred_result <- reactiveVal(NULL)

  observeEvent(input$run_predictions, {
    req(input$ticker1)

    withProgress(message = "Running ML models…", value = 0, {

      setProgress(0.05, detail = "Fetching 2-year price history…")
      # Always use 2 years of data for training regardless of chosen interval
      hist_data <- tryCatch(
        getSymbols(input$ticker1, src="yahoo",
                   from = Sys.Date()-730, to = Sys.Date(),
                   auto.assign=FALSE),
        error = function(e) NULL
      )
      if (is.null(hist_data)) {
        showNotification("Could not fetch training data.", type="error")
        return()
      }

      prices <- as.numeric(Cl(hist_data))
      dates  <- index(hist_data)

      setProgress(0.15, detail = "Training LSTM (this may take ~20s)…")
      lstm_res <- run_lstm_prediction(prices, n_ahead = 7)

      setProgress(0.70, detail = "Training Random Forest…")
      rf_res   <- run_rf_prediction(hist_data, n_ahead = 7)

      setProgress(0.95, detail = "Rendering outputs…")
      pred_result(list(
        prices   = prices,
        dates    = dates,
        lstm     = lstm_res,
        rf       = rf_res,
        ticker   = input$ticker1
      ))
    })
  })

  # ── Method badges ────────────────────────────────────────────────────────────
  output$pred_method_badges <- renderUI({
    res <- pred_result(); req(res)
    tagList(
      br(),
      tags$span(class="model-badge badge-lstm",
                tags$i(class="fa fa-brain", style="margin-right:4px;"),
                res$lstm$method %||% "—"),
      tags$span(style="margin-left:6px;", class="model-badge badge-rf",
                tags$i(class="fa fa-tree", style="margin-right:4px;"),
                res$rf$method %||% "—")
    )
  })

  # operator for NULL fallback
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ── LSTM Metrics UI ───────────────────────────────────────────────────────────
  output$lstm_metrics_ui <- renderUI({
    res <- pred_result(); req(res, res$lstm, res$lstm$predictions)
    preds  <- res$lstm$predictions
    last_p <- tail(res$prices, 1)
    pct_ch <- (tail(preds, 1) - last_p) / last_p * 100
    tagList(
      fluidRow(
        column(6, div(class="stat-card",
                      div(class="stat-label", tags$i(class="fa fa-crosshairs"), " 7D TARGET"),
                      div(class=paste("stat-value", if(pct_ch>=0)"stat-high" else "stat-low"),
                          paste0("$", round(tail(preds,1), 2))))),
        column(6, div(class="stat-card",
                      div(class="stat-label", tags$i(class="fa fa-percent"), " EXPECTED CHANGE"),
                      div(class=paste("stat-value", if(pct_ch>=0)"stat-high" else "stat-low"),
                          sprintf("%+.2f%%", pct_ch))))
      )
    )
  })

  # ── RF Metrics UI ─────────────────────────────────────────────────────────────
  output$rf_metrics_ui <- renderUI({
    res <- pred_result(); req(res, res$rf, res$rf$predictions)
    preds      <- res$rf$predictions
    cum_ret    <- prod(1 + preds) - 1
    avg_daily  <- mean(preds) * 100
    tagList(
      fluidRow(
        column(6, div(class="stat-card",
                      div(class="stat-label", tags$i(class="fa fa-arrow-trend-up"), " 7D CUM. RETURN"),
                      div(class=paste("stat-value", if(cum_ret>=0)"stat-high" else "stat-low"),
                          sprintf("%+.2f%%", cum_ret * 100)))),
        column(6, div(class="stat-card",
                      div(class="stat-label", tags$i(class="fa fa-calendar-day"), " AVG DAILY RET"),
                      div(class=paste("stat-value", if(avg_daily>=0)"stat-high" else "stat-low"),
                          sprintf("%+.3f%%", avg_daily))))
      )
    )
  })

  # ── LSTM Plot ─────────────────────────────────────────────────────────────────
  output$lstm_plot <- renderPlotly({
    res <- pred_result(); req(res, res$lstm, res$lstm$predictions)

    # Historical: last 60 days
    hist_n  <- min(60, length(res$prices))
    hist_px <- tail(res$prices, hist_n)
    hist_dt <- tail(res$dates,  hist_n)

    # Forecast dates
    last_dt   <- max(res$dates)
    pred_dts  <- seq(last_dt + 1, by = "day", length.out = 14)
    # Remove weekends
    pred_dts  <- pred_dts[!weekdays(pred_dts) %in% c("Saturday","Sunday")][1:7]

    preds <- res$lstm$predictions
    lower <- res$lstm$lower
    upper <- res$lstm$upper

    p <- plot_ly() |>
      add_lines(x = hist_dt, y = hist_px,
                name = "Historical", line = list(color="#9aa3af", width=1.8)) |>
      add_ribbons(x = pred_dts, ymin = lower, ymax = upper,
                  name = "95% CI", fillcolor = "rgba(41,98,255,0.15)",
                  line = list(color="rgba(0,0,0,0)")) |>
      add_lines(x = pred_dts, y = preds,
                name = "LSTM Forecast",
                line = list(color="#2962ff", width=2.5, dash="dot")) |>
      add_markers(x = pred_dts, y = preds,
                  marker = list(color="#2962ff", size=7, line=list(color="#fff",width=1.5)),
                  name = "Forecast Points", showlegend = FALSE) |>
      layout(
        paper_bgcolor="#1e2431", plot_bgcolor="#1e2431",
        xaxis = list(title="", gridcolor="#2a2e39", color="#787b86"),
        yaxis = list(title="Price (USD)", gridcolor="#2a2e39", color="#787b86"),
        font  = list(color="#e0e6ed", size=11),
        legend= list(x=0,y=1,bgcolor="rgba(0,0,0,0)",font=list(color="#e0e6ed",size=10)),
        hovermode="x unified", margin=list(l=50,r=10,t=10,b=40)
      )
    p
  })

  # ── LSTM Table ────────────────────────────────────────────────────────────────
  output$lstm_table <- render_gt({
    res <- pred_result(); req(res, res$lstm, res$lstm$predictions)

    last_dt  <- max(res$dates)
    pred_dts <- seq(last_dt + 1, by="day", length.out=14)
    pred_dts <- pred_dts[!weekdays(pred_dts) %in% c("Saturday","Sunday")][1:7]

    preds <- res$lstm$predictions
    lower <- res$lstm$lower
    upper <- res$lstm$upper
    last_p <- tail(res$prices, 1)

    df <- data.frame(
      Date     = pred_dts,
      Forecast = round(preds, 2),
      Low_CI   = round(lower, 2),
      High_CI  = round(upper, 2),
      Change   = round(preds - last_p, 2),
      ChgPct   = round((preds - last_p) / last_p * 100, 2)
    )

    gt(df) |>
      cols_label(Date="Date", Forecast="Forecast ($)", Low_CI="Lower CI",
                 High_CI="Upper CI", Change="vs Last", ChgPct="Change %") |>
      fmt_currency(columns=c(Forecast,Low_CI,High_CI,Change), currency="USD") |>
      fmt_number(columns=ChgPct, decimals=2, pattern="{x}%") |>
      tab_style(style=cell_text(color="#26a69a",weight="bold"),
                locations=cells_body(columns=c(Forecast,Change,ChgPct), rows=Change>=0)) |>
      tab_style(style=cell_text(color="#ef5350",weight="bold"),
                locations=cells_body(columns=c(Forecast,Change,ChgPct), rows=Change<0)) |>
      tab_options(
        table.width=pct(100), table.font.size=px(11), table.font.names="JetBrains Mono",
        table.background.color="#1e2431", column_labels.background.color="#252b3d",
        column_labels.font.weight="bold", column_labels.border.bottom.color="#2962ff",
        column_labels.border.bottom.width=px(2), data_row.padding=px(7)
      ) |>
      tab_style(style=cell_text(color="#fff",weight="bold"),
                locations=cells_column_labels(everything())) |>
      tab_style(style=cell_text(color="#9aa3af"),
                locations=cells_body(columns=c(Date,Low_CI,High_CI)))
  })

  # ── RF Plot ────────────────────────────────────────────────────────────────────
  output$rf_plot <- renderPlotly({
    res <- pred_result(); req(res, res$rf, res$rf$predictions)

    # Historical returns: last 30 days
    hist_n  <- min(30, length(res$prices))
    hist_ret <- diff(log(tail(res$prices, hist_n + 1))) * 100
    hist_dt  <- tail(res$dates, hist_n)

    last_dt  <- max(res$dates)
    pred_dts <- seq(last_dt + 1, by="day", length.out=14)
    pred_dts <- pred_dts[!weekdays(pred_dts) %in% c("Saturday","Sunday")][1:7]

    preds_pct <- res$rf$predictions * 100
    bar_colors <- ifelse(preds_pct >= 0, "#26a69a", "#ef5350")
    hist_colors<- ifelse(hist_ret >= 0, "rgba(38,166,154,0.45)", "rgba(239,83,80,0.45)")

    plot_ly() |>
      add_bars(x=hist_dt, y=hist_ret, name="Historical Returns",
               marker=list(color=hist_colors)) |>
      add_bars(x=pred_dts, y=preds_pct, name="RF Forecast",
               marker=list(color=bar_colors,
                           line=list(color=ifelse(preds_pct>=0,"#26a69a","#ef5350"),width=1.5))) |>
      add_lines(x=c(min(hist_dt), max(pred_dts)), y=c(0,0),
                line=list(color="#787b86",width=1,dash="dot"),
                showlegend=FALSE) |>
      layout(
        barmode = "overlay",
        paper_bgcolor="#1e2431", plot_bgcolor="#1e2431",
        xaxis=list(title="", gridcolor="#2a2e39", color="#787b86"),
        yaxis=list(title="Daily Log Return (%)", gridcolor="#2a2e39", color="#787b86"),
        font=list(color="#e0e6ed", size=11),
        legend=list(x=0,y=1,bgcolor="rgba(0,0,0,0)",font=list(color="#e0e6ed",size=10)),
        hovermode="x unified", margin=list(l=50,r=10,t=10,b=40)
      )
  })

  # ── RF Table ─────────────────────────────────────────────────────────────────
  output$rf_table <- render_gt({
    res <- pred_result(); req(res, res$rf, res$rf$predictions)

    last_dt  <- max(res$dates)
    pred_dts <- seq(last_dt + 1, by="day", length.out=14)
    pred_dts <- pred_dts[!weekdays(pred_dts) %in% c("Saturday","Sunday")][1:7]

    preds_log <- res$rf$predictions
    last_px   <- tail(res$prices, 1)
    cum_ret   <- cumprod(1 + preds_log)

    df <- data.frame(
      Date     = pred_dts,
      Log_Ret  = round(preds_log * 100, 4),
      Cum_Ret  = round((cum_ret - 1) * 100, 3),
      Est_Price= round(last_px * cum_ret, 2)
    )

    imp_note <- if (!is.null(res$rf$importance)) {
      top_feat <- res$rf$importance$Feature[1]
      paste0("Top feature: ", top_feat)
    } else ""

    gt(df) |>
      cols_label(Date="Date", Log_Ret="Daily Return (%)",
                 Cum_Ret="Cum. Return (%)", Est_Price="Est. Price ($)") |>
      fmt_currency(columns=Est_Price, currency="USD") |>
      fmt_number(columns=c(Log_Ret,Cum_Ret), decimals=3, pattern="{x}%") |>
      tab_style(style=cell_text(color="#26a69a",weight="bold"),
                locations=cells_body(columns=c(Log_Ret,Cum_Ret,Est_Price), rows=Log_Ret>=0)) |>
      tab_style(style=cell_text(color="#ef5350",weight="bold"),
                locations=cells_body(columns=c(Log_Ret,Cum_Ret,Est_Price), rows=Log_Ret<0)) |>
      tab_source_note(source_note=md(paste0("*Random Forest (500 trees) | ", imp_note, "*"))) |>
      tab_options(
        table.width=pct(100), table.font.size=px(11), table.font.names="JetBrains Mono",
        table.background.color="#1e2431", column_labels.background.color="#252b3d",
        column_labels.font.weight="bold", column_labels.border.bottom.color="#26a69a",
        column_labels.border.bottom.width=px(2), data_row.padding=px(7)
      ) |>
      tab_style(style=cell_text(color="#fff",weight="bold"),
                locations=cells_column_labels(everything())) |>
      tab_style(style=cell_text(color="#9aa3af"),
                locations=cells_body(columns=Date))
  })

  # ── Context-Aware AI Chat ────────────────────────────────────────────────────
  stored_api_key <- reactiveVal("")
  chat_client    <- reactiveVal(NULL)
  context_ticker <- reactiveVal("")

  build_and_set_client <- function(api_key, ticker, data_xts) {
    name <- {
      idx <- match(ticker, sp500_tickers$ticker)
      if (!is.na(idx)) sp500_tickers$name[idx] else ticker
    }
    ctx  <- make_stock_context_string(ticker, data_xts, name)
    sys_prompt <- paste0(
      "You are an expert financial analyst and AI trading assistant integrated into an institutional-grade stock market dashboard.\n\n",
      ctx,
      "\n\nRespond concisely and professionally. Use the dashboard data above when analysing this stock. ",
      "Speak in the same language the user writes in. ",
      "When you reference numbers, be precise. Never fabricate data not provided above."
    )
    client <- make_chat_client(api_key, sys_prompt)
    if (inherits(client, "error")) {
      showNotification(paste("Gemini connection failed:", client$message), type="error", duration=8)
      return(invisible(NULL))
    }
    chat_client(client)
    context_ticker(ticker)
    invisible(client)
  }

  observeEvent(input$set_api_key, {
    req(input$gemini_api_key)
    api_key <- trimws(input$gemini_api_key)
    if (!nzchar(api_key)) { showNotification("Please enter a valid API key.", type="error"); return() }
    stored_api_key(api_key)
    data <- isolate(stock_data1())
    build_and_set_client(api_key, isolate(input$ticker1), data)
    showNotification("Connected to Gemini. Context loaded with current stock data.", type="message", duration=4)
  })

  observeEvent(input$refresh_context, {
    api_key <- stored_api_key()
    if (!nzchar(api_key)) {
      showNotification("Set your Gemini API key first.", type="warning"); return()
    }
    ticker <- input$ticker1
    data   <- stock_data1()
    build_and_set_client(api_key, ticker, data)
    showNotification(paste("Context updated for", ticker), type="message", duration=3)
  })

  observe({
    if (!is.null(chat_client())) {
      chat_mod_server("stock_chat", chat_client())
    }
  }) |> bindEvent(chat_client())

  output$context_status_ui <- renderUI({
    tk <- context_ticker()
    if (nzchar(tk)) {
      name <- {
        idx <- match(tk, sp500_tickers$ticker)
        if (!is.na(idx)) sp500_tickers$name[idx] else tk
      }
      tagList(
        tags$span(class="context-badge",
                  tags$i(class="fa fa-circle-check"),
                  "Context: ", name, " (", tk, ")"),
        tags$br(), tags$br(),
        tags$small(style="color:#787b86;font-size:10px;",
                   tags$i(class="fa fa-circle-info"), " Gemini has real-time dashboard data.")
      )
    } else {
      tags$span(style="color:#787b86;font-size:12px;",
                tags$i(class="fa fa-circle-xmark", style="color:#ef5350;"),
                " No context loaded. Connect API key to enable.")
    }
  })

  # ── Riingo News ───────────────────────────────────────────────────────────────
  output$riingo_selected_info <- renderUI({
    req(input$ticker1)
    idx <- match(input$ticker1, sp500_tickers$ticker)
    nm  <- if (!is.na(idx)) sp500_tickers$name[idx] else input$ticker1
    lg  <- if (!is.na(idx)) sp500_tickers$logo[idx] else
      paste0("https://www.google.com/s2/favicons?sz=64&domain=", tolower(input$ticker1), ".com")
    htmltools::HTML(sprintf(
      '<div style="display:flex;align-items:center;gap:8px;margin:6px 0;">
<img src="%s" style="width:18px;height:18px;border-radius:3px;object-fit:contain;">
<span style="color:#9aa3af;">Showing news for <strong style="color:#e0e6ed">%s</strong> (<code>%s</code>)</span>
</div>', lg, nm, input$ticker1
    ))
  })

  riingo_news_data <- reactive({
    req(input$ticker1)
    src      <- if (!is.null(input$riingo_source) && nzchar(input$riingo_source)) input$riingo_source else "bloomberg.com"
    lim      <- if (!is.null(input$riingo_limit)) input$riingo_limit else 10
    logo_url <- LOGO_SRC_MAP[[src]] %||% ""

    raw <- tryCatch(riingo_news(ticker=input$ticker1, source=src), error=function(e) data.frame())
    if (!NROW(raw)) return(data.frame(when=as.POSIXct(character()), title_md=character(),
                                      source=character(), sentiment=character()))
    df <- raw |> arrange(desc(publishedDate)) |> head(lim)
    score <- tryCatch(sentimentr::sentiment_by(df$title)$ave_sentiment,
                      error=function(e) rep(0, nrow(df)))
    label <- ifelse(score > 0.2,"Positive", ifelse(score < -0.2,"Negative","Neutral"))
    df |>
      mutate(when=lubridate::with_tz(as.POSIXct(publishedDate, tz="UTC"), tzone=Sys.timezone()),
             title_md=sprintf("[%s](%s)", title, url),
             source=logo_url, sentiment=label) |>
      dplyr::select(when, title_md, source, sentiment)
  }) |> bindEvent(input$ticker1, input$riingo_source, input$riingo_limit, input$refresh)

  output$riingo_news_gt <- render_gt({ make_riingo_gt(riingo_news_data()) })

  # ── Portfolio ─────────────────────────────────────────────────────────────────
  output$pf_tickers_ui <- renderUI({
    labels_with_logo <- paste0(sp500_tickers$display, "||", sp500_tickers$logo)
    choices_named    <- setNames(sp500_tickers$ticker, labels_with_logo)
    selectizeInput("pf_tickers","TICKERS", choices=choices_named, multiple=TRUE, width="100%",
      options=list(maxItems=8, plugins=list("remove_button"),
        render=I('{
  option: function(item, escape) {
    var p=String(item.label).split("||"), img=p[1]?"<img src=\\""+escape(p[1])+"\\" width=\\"18\\" height=\\"18\\" style=\\"margin-right:6px;vertical-align:middle;border-radius:3px;\\">":"";
    return "<div>"+img+escape(p[0])+"</div>";
  },
  item: function(item, escape) {
    var p=String(item.label).split("||"), img=p[1]?"<img src=\\""+escape(p[1])+"\\" width=\\"16\\" height=\\"16\\" style=\\"margin-right:6px;vertical-align:middle;border-radius:3px;\\">":"";
    return "<div>"+img+escape(p[0])+"</div>";
  }
}')))
  })

  output$pf_weights_ui <- renderUI({
    req(input$pf_tickers); n <- length(input$pf_tickers); if (n==0) return(NULL)
    eq <- round(100/n, 2)
    tagList(tags$label(style="color:#787b86;font-weight:600;","WEIGHTS (%)"),
            fluidRow(lapply(seq_len(n), function(i) {
              column(4, numericInput(paste0("pf_w_",i), input$pf_tickers[i], value=eq, min=0, max=100, step=1))
            })))
  })

  pf_get_prices <- function(tickers, interval) {
    if (length(tickers)==0) return(NULL)
    lst  <- lapply(tickers, function(tk){ x <- get_stock_data(tk, interval); if(is.null(x)) return(NULL); Cl(x) })
    keep <- !sapply(lst, is.null); lst <- lst[keep]; tickers <- tickers[keep]
    if (length(lst)==0) return(NULL)
    px <- do.call(merge, lst); colnames(px) <- tickers
    zoo::na.locf(zoo::na.locf(px, na.rm=FALSE), fromLast=TRUE)
  }

  pf_weights <- reactive({
    req(input$pf_tickers)
    ws <- sapply(seq_along(input$pf_tickers), function(i) input[[paste0("pf_w_",i)]])
    ws <- as.numeric(ws); ws[is.na(ws)] <- 0
    if (isTRUE(input$pf_normalize) && sum(ws) != 100)
      ws <- if (sum(ws)==0) rep(100/length(ws), length(ws)) else ws/sum(ws)*100
    ws/100
  })

  pf_prices   <- eventReactive(input$pf_run, { pf_get_prices(input$pf_tickers, input$pf_interval) })
  pf_returns  <- reactive({ px <- pf_prices(); req(px); na.omit(px/lag(px)-1) })
  pf_port_ret <- reactive({
    R <- pf_returns(); req(NCOL(R)>0); w <- pf_weights(); req(length(w)==NCOL(R))
    xts::xts(as.numeric(R %*% matrix(w, ncol=1)), order.by=zoo::index(R))
  })
  pf_equity <- reactive({
    R <- pf_port_ret(); req(R, input$pf_capital)
    xts::xts(as.numeric(input$pf_capital) * as.numeric(cumprod(1+R)), order.by=zoo::index(R))
  })

  output$pf_cagr   <- renderText({ eq <- pf_equity(); req(eq); n <- NROW(eq); if(n<2) return("-")
    cagr <- (as.numeric(tail(eq,1))/as.numeric(head(eq,1)))^(252/n)-1
    paste0(sprintf("%.2f",cagr*100),"%") })
  output$pf_vol    <- renderText({ R <- pf_port_ret(); req(R)
    paste0(sprintf("%.2f", sd(as.numeric(R),na.rm=TRUE)*sqrt(252)*100), "%") })
  output$pf_sharpe <- renderText({ R <- pf_port_ret(); req(R)
    mu <- mean(as.numeric(R),na.rm=TRUE)*252; sig <- sd(as.numeric(R),na.rm=TRUE)*sqrt(252)
    if(sig==0) return("-"); sprintf("%.2f",mu/sig) })

  output$pf_alloc_table <- render_gt({
    px <- pf_prices(); req(px)
    tick    <- as.character(colnames(px))
    w_named <- setNames(as.numeric(pf_weights()), input$pf_tickers)
    w       <- as.numeric(w_named[tick]); w[is.na(w)] <- 0
    last_px <- as.numeric(tail(px, 1))
    info    <- sp500_tickers[match(tick, sp500_tickers$ticker), c("name","logo","sector")]
    name_safe   <- ifelse(is.na(info$name)  |info$name=="",   tick, info$name)
    logo_safe   <- ifelse(is.na(info$logo)  |info$logo=="",
                          paste0("https://www.google.com/s2/favicons?sz=64&domain=",tolower(tick),".com"), info$logo)
    sector_safe <- ifelse(is.na(info$sector)|info$sector=="","—", info$sector)

    df <- dplyr::tibble(Name_Logo=paste0(name_safe,"||",logo_safe), Ticker=tick,
                        Sector=sector_safe, Weight=w, `Last Price`=last_px) |>
      dplyr::arrange(dplyr::desc(Weight))
    w_vals <- df$Weight

    gt::gt(df) |>
      gt::text_transform(locations=gt::cells_body(columns=Name_Logo), fn=function(x){
        lapply(x, function(val){
          p <- strsplit(val,"\\|\\|")[[1]]
          gt::html(paste0('<div style="display:flex;align-items:center;padding:4px 0;">',
                          '<img src="',p[2],'" style="width:18px;height:18px;margin-right:8px;border-radius:3px;object-fit:contain;">',
                          '<span>',p[1],'</span></div>'))
        })
      }) |>
      gt::cols_label(Name_Logo="Name", Ticker="Ticker", Sector="Sector",
                     Weight="Weight", `Last Price`="Last Price (USD)") |>
      gt::text_transform(locations=gt::cells_body(columns=Weight), fn=function(x){
        lapply(seq_along(x), function(i) weight_pill_html(w_vals[i]))
      }) |>
      gt::fmt_currency(columns=`Last Price`, currency="USD") |>
      gt::tab_options(
        table.width=gt::pct(100), table.font.size=gt::px(12), table.font.names="JetBrains Mono",
        table.background.color="#1e2431", column_labels.background.color="#252b3d",
        column_labels.font.weight="bold", column_labels.border.bottom.color="#2962ff",
        column_labels.border.bottom.width=px(3), data_row.padding=gt::px(8)
      ) |>
      gt::tab_style(style=gt::cell_text(color="#fff",weight="bold"),
                    locations=gt::cells_column_labels(columns=gt::everything())) |>
      gt::tab_style(style=gt::cell_text(color="#e0e6ed"),
                    locations=gt::cells_body(columns=gt::everything()))
  })

  output$pf_equity_plot <- renderPlotly({
    eq <- pf_equity(); req(eq)
    df_port <- data.frame(Date=zoo::index(eq), Value=as.numeric(eq))
    start_d <- min(df_port$Date,na.rm=TRUE); end_d <- max(df_port$Date,na.rm=TRUE)
    idx_xts <- tryCatch(quantmod::getSymbols("^GSPC",src="yahoo",from=start_d,to=end_d,auto.assign=FALSE),
                        error=function(e) NULL)
    df_spx <- if (!is.null(idx_xts) && length(Cl(idx_xts))>1) {
      idx_close <- as.numeric(Cl(idx_xts))
      data.frame(Date=zoo::index(idx_xts), Value=(idx_close/idx_close[1])*as.numeric(input$pf_capital))
    } else NULL

    plt <- plotly::plot_ly(df_port, type="scatter", mode="lines",
                           x=~Date, y=~Value, name="Portfolio",
                           line=list(color="#2962ff",width=2.5))
    if (!is.null(df_spx))
      plt <- plt |> plotly::add_lines(data=df_spx, x=~Date, y=~Value,
                                      name="S&P 500", line=list(color="#787b86",width=1.5,dash="dot"))
    plt |> plotly::layout(
      title=list(text="Portfolio Value vs S&P 500", font=list(color="#787b86")),
      paper_bgcolor="#131722", plot_bgcolor="#131722",
      xaxis=list(title="",gridcolor="#2a2e39",color="#787b86"),
      yaxis=list(title="USD",gridcolor="#2a2e39",color="#787b86"),
      font=list(color="#e0e6ed"), hovermode="x unified",
      legend=list(x=0.01,y=0.99,bgcolor="rgba(30,36,49,.8)",
                  bordercolor="#2a2e39",font=list(color="#e0e6ed")),
      margin=list(l=60,r=40,t=40,b=40)
    )
  })
}

shinyApp(ui = ui, server = server)

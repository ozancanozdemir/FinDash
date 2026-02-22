# FinDash Pro

> **An integrated R Shiny dashboard for real-time financial analysis, machine learning forecasting, and LLM-augmented decision support.**

---

## Overview

**FinDash Pro** is a production-grade interactive financial dashboard built entirely in R with the Shiny ecosystem. It combines live market data aggregation, technical analysis, ensemble machine learning forecasting, and a context-aware large language model interface — all within a single, reproducible open-source application.

Submitted to the **useR! 2026**.

---

## Screenshots

| Market Overview | Stock Analysis |
|:---:|:---:|
| S&P 500 rankings with 30-day sparklines | Candlestick chart with RSI & MACD |

| ML Predictions | News & AI Chat |
|:---:|:---:|
| LSTM price forecast + RF return forecast | Sentiment-scored headlines + Gemini AI |

---

## Features

### 📊 Market Overview
- Top 10 S&P 500 companies ranked by market cap, price, or revenue
- 30-day trend sparklines (SVG, rendered via `ggplot2` + `svglite`)
- S&P 500 fundamental metrics: P/E ratio, EPS (TTM), dividend yield, analyst ratings
- Sortable and filterable tables built with `gt` and `gtExtras`

### 📈 Stock Analysis
- Live OHLCV data via `quantmod` / Yahoo Finance
- Candlestick chart with `plotly`
- Technical indicators: RSI(14), MACD(12,26,9), Signal line
- 52-week high/low, volume statistics
- **Compare mode**: normalised return comparison between two tickers
- Last 5 sessions trading data table with colour-coded signals

### 🤖 ML Predictions
| Model | Target | Horizon | Method |
|-------|--------|---------|--------|
| **LSTM** | Price | 7 days | `keras3` (falls back to ETS if unavailable) |
| **Random Forest** | Daily log return | 7 days | `randomForest` (500 trees) |

- LSTM trained on 2 years of closing prices with a 60-day sliding window
- 95% confidence intervals on LSTM forecasts (historical volatility scaling)
- RF features: lagged returns, rolling means, RSI, MACD, volume ratio, HL range
- Iterative multi-step prediction for both models
- Feature importance ranking from the Random Forest
- Interactive `plotly` charts and `gt` forecast tables for both models

### 💼 Portfolio Calculator
- Multi-stock portfolio with up to 8 tickers
- Custom weight allocation with auto-normalisation
- Metrics: CAGR, annualised volatility, Sharpe ratio (rf = 0)
- Equity curve benchmarked against the S&P 500 (^GSPC)
- Allocation table with sector labels and company logos

### 📰 News & AI Chat
- Real-time financial headlines via the **Tiingo API** (`riingo`)
- Source filter: Bloomberg, Reuters, WSJ, CNBC, Seeking Alpha, MarketWatch
- Lexicon-based sentiment scoring (`sentimentr`) — Positive / Neutral / Negative
- **Context-aware Gemini LLM** (Google Gemini 2.5 Flash via `ellmer` + `shinychat`):
  - System prompt is dynamically populated with live price, RSI, MACD, and recent closing prices
  - "Update Context" button reloads fresh market data into the LLM at any time
  - Context badge displays which stock the AI is currently analysing

---

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/ozancanozdemir/useR_2026.git
cd useR_2026
```

### 2. Install required R packages

```r
install.packages(c(
  "shiny", "shinyjs", "quantmod", "dplyr", "gt", "plotly",
  "rvest", "xml2", "stringr", "lubridate", "riingo", "httr",
  "sentimentr", "gtExtras", "ggsci", "svglite", "zoo",
  "base64enc", "randomForest", "shinycssloaders"
))

# From CRAN or GitHub (Posit ecosystem)
install.packages(c("shinychat", "ellmer", "shinyLP"))
```

### 3. Optional: LSTM support

LSTM predictions require `keras3` with a working TensorFlow backend:

```r
install.packages("keras3")
keras3::install_keras()   # installs TensorFlow via reticulate
```

> If `keras3` is not available, the app automatically falls back to ETS (exponential smoothing) from the `forecast` package, or naive drift — no error is thrown.

### 4. Run the app

```r
shiny::runApp("app.R")
```

---

## API Keys

| Service | Purpose | Where to get it |
|---------|---------|----------------|
| **Google Gemini** | AI chat assistant | [Google AI Studio](https://aistudio.google.com/app/apikey) |
| **Tiingo** | Financial news headlines | [tiingo.com](https://www.tiingo.com) |

- The Tiingo token is pre-set in the app for demo purposes.
- Enter your Gemini API key directly in the **News & AI** tab and click **CONNECT**.

---

## Project Structure

```
useR_2026/
├── app.R          # Main Shiny application (UI + Server)
├── abstract.md    # Conference abstract (useR! 2026)
└── README.md      # This file
```

---

## Dependencies

| Package | Role |
|---------|------|
| `shiny`, `shinyjs` | Application framework |
| `quantmod` | Yahoo Finance data (OHLCV) |
| `riingo` | Tiingo API (news) |
| `gt`, `gtExtras` | Table rendering |
| `plotly` | Interactive charts |
| `randomForest` | Return forecasting (ML) |
| `keras3` *(optional)* | LSTM price forecasting (Deep Learning) |
| `ellmer`, `shinychat` | LLM integration (Gemini) |
| `sentimentr` | News sentiment scoring |
| `rvest`, `xml2` | Web scraping (market cap, fundamentals) |
| `svglite`, `ggplot2` | Sparkline generation |
| `shinycssloaders` *(optional)* | Loading spinners |

---

## Technical Notes

- **Data sources**: Yahoo Finance (prices), Tiingo (news), stockanalysis.com (market cap), tradingview.com (fundamentals), Wikipedia (S&P 500 ticker list)
- **Fallback handling**: all web scraping and model training is wrapped in `tryCatch`; the app degrades gracefully on network or package failures
- **LSTM training time**: approximately 15–30 seconds for 2 years of daily data with 25 epochs and early stopping (patience = 5)
- **Theming**: fully custom dark theme via inline CSS; fonts loaded from Google Fonts (Inter + JetBrains Mono)

---

## Citation

If you use this dashboard in your research or teaching, please cite:

```
Ozdemir, O. (2026). FinDash Pro: An Integrated R Shiny Dashboard for Real-Time
Financial Analysis, Machine Learning Forecasting, and LLM-Augmented Decision Support.
useR! 2026 Conference.
```

---

## License

MIT License © 2025 Ozancan Ozdemir

---

## Contact

**Ozancan Ozdemir**
[GitHub](https://github.com/ozancanozdemir) · [Report an Issue](https://github.com/ozancanozdemir/useR_2026/issues)

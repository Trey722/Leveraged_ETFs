import yfinance as yf
import pandas as pd


def fetch_stock_data(ticker):
    stock = yf.Ticker(ticker)
    data = stock.history(period="max", interval="1mo")
    data.to_csv(f"{ticker}.csv")
    return data
  
  
UPRO = fetch_stock_data("UPRO")
TQQQ = fetch_stock_data("TQQQ")
QLD = fetch_stock_data("QLD")
SSO = fetch_stock_data("SSO")


SPY = fetch_stock_data("SPY")
QQQ = fetch_stock_data("QQQ")


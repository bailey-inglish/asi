# Deployment code (uses hidden tokens)
setwd("C:/Users/baile/Desktop/asi/sdr/live")
codes <- read_csv(".shiny-token.csv")
rsconnect::setAccountInfo(
  name = "bailey-inglish",
  token = codes$token,
  secret = codes$secret
)
rsconnect::deployApp(
  appDir = "C:/Users/baile/Desktop/asi/sdr/live",
  appPrimaryDoc = "sdr_explorer.r",
  appName = "sdr-explorer"
)

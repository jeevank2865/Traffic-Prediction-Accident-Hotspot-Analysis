#!/usr/bin/env Rscript
# ==============================================================================
# UrbanPulse: Deploy to shinyapps.io
# ==============================================================================

cat("\n==============================================================================\n")
cat("  UrbanPulse: shinyapps.io Deployment Helper\n")
cat("==============================================================================\n\n")

# 1. Check for rsconnect package
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat(">>> The 'rsconnect' package is required to deploy to shinyapps.io.\n")
  cat(">>> Installing rsconnect...\n")
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

library(rsconnect)

# 2. Check if shinyapps.io account credentials are provided in environment
acc_name   <- Sys.getenv("SHINYAPPS_ACCOUNT")
acc_token  <- Sys.getenv("SHINYAPPS_TOKEN")
acc_secret <- Sys.getenv("SHINYAPPS_SECRET")

if (nchar(acc_name) > 0 && nchar(acc_token) > 0 && nchar(acc_secret) > 0) {
  cat(">>> Configuring shinyapps.io account from environment variables...\n")
  rsconnect::setAccountInfo(name = acc_name, token = acc_token, secret = acc_secret)
}

# 3. Check configured accounts
accounts <- rsconnect::accounts()
if (nrow(accounts) == 0) {
  cat(">>> No shinyapps.io account is currently configured on this machine.\n\n")
  cat("To link your free shinyapps.io account:\n")
  cat("  1. Sign in to https://www.shinyapps.io/\n")
  cat("  2. In the top-right menu, go to: Account -> Tokens\n")
  cat("  3. Click 'Show' next to your active token, then click 'Copy Secret'\n")
  cat("  4. In R or RStudio, run the copied command, which looks like:\n")
  cat("     rsconnect::setAccountInfo(\n")
  cat("       name   = '<YOUR_ACCOUNT_NAME>',\n")
  cat("       token  = '<YOUR_TOKEN>',\n")
  cat("       secret = '<YOUR_SECRET>'\n")
  cat("     )\n")
  cat("  5. Then run this script again or execute:\n")
  cat("     rsconnect::deployApp(appName = 'urbanpulse', launch.browser = TRUE)\n\n")
  quit(status = 1)
}

# 4. Deploy Application
cat(sprintf(">>> Deploying 'UrbanPulse' to shinyapps.io account '%s'...\n", accounts$name[1]))
cat(">>> Packaging bundle (excluding raw files via .rscignore)...\n\n")

rsconnect::deployApp(
  appDir         = getwd(),
  appName        = "urbanpulse",
  appTitle       = "UrbanPulse: Traffic Prediction & Hotspots",
  launch.browser = TRUE,
  forceUpdate    = TRUE
)

cat("\n==============================================================================\n")
cat(">>> Deployment complete! App is live on shinyapps.io.\n")
cat("==============================================================================\n")

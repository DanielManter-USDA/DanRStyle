# Pipe-at-start transformer
# Rewrites:
#   df %>% filter(x > 0)
# Into:
#   df
#     %>% filter(x > 0)

pipe_rewriter <- function(pd_flat) {
  # Identify pipe tokens (%>% or |>)
  pipe_tokens <- which(pd_flat$token %in% c("SPECIAL-PIPE", "PIPE"))
  
  if (length(pipe_tokens) == 0) {
    return(pd_flat)
  }
  
  for (i in pipe_tokens) {
    # If pipe is already at start of line, skip
    if (pd_flat$lag_newlines[i] > 0) next
    
    # Force pipe to start a new line
    pd_flat$lag_newlines[i] <- 1
    
    # Indent continuation line by 2 spaces (tidyverse default)
    pd_flat$indent[i] <- pd_flat$indent[i] + 2
  }
  
  pd_flat
}
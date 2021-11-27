# `%>%` <- magrittr::`%>%`
# 
# env_vars <- names(Sys.getenv())
# pwds <- Sys.getenv(env_vars[grep("PW_", env_vars)]) 
# 
# if(length(pwds) == length(Sys.getenv())){
#   stop("No env var with prefix 'PW_' detected.", call. = FALSE)  
# }
# 
# if(length(pwds) == 1){
#   df <- data.frame(
#     site_id = names(which(Sys.getenv() == pwds)),
#     password = pwds
#   )
# }
# 
# if(length(pwds) > 1){
#   df <- pwds %>% 
#     as.data.frame() %>% 
#     cbind(rownames(.), .) %>% 
#     setNames(c("site_id", "password"))
# }
# 
# df <- df  %>% 
#   dplyr::mutate(
#     site_dir = tolower(stringr::str_remove_all(site_id, "PW_")),
#     site_dir = stringr::str_replace_all(site_dir, "_", "-"),
#     site_dir = here::here(site_dir, "index.html"),
#     cmd = paste("staticrypt", site_dir, password, "-o", site_dir)
#   )
# 
# purrr::walk(df$cmd, ~system(.x))


cmd <- paste("staticrypt", here::here("content/ewd/index.html"), 
      "password", "-o", here::here("content/ewd/index.html"), 
      "-f", here::here("src/etc/password_template.html"))

system(cmd)

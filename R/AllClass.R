setClass("Login",
         slots = c(
           domain = "character",
           token = "character",
           access = "character"
         ))

setClass("Workspace",
         contains = "Login",
         slots = c(
           login = "Login",
           label = "character",
           id = "numeric"
         ))

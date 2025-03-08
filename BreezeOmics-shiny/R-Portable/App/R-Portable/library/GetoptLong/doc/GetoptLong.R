## ---- echo = FALSE, message = FALSE-------------------------------------------
library(knitr)
knitr::opts_chunk$set(
    error = FALSE,
    tidy  = FALSE,
    message = FALSE,
    comment = NA,
    fig.align = "center")
library(GetoptLong)

## ---- echo = FALSE, out.width = "600px"---------------------------------------
include_graphics("workflow.png")

## ----simple, eval = FALSE-----------------------------------------------------
#  library(GetoptLong)
#  
#  cutoff = 0.05
#  GetoptLong(
#      "number=i", "Number of items.",
#      "cutoff=f", "Cutoff for filtering results.",
#      "verbose",  "Print message."
#  )

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ eval(parse(text = chunks[["simple"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
GetoptLong.options("help_style" = "two-column")
local({ eval(parse(text = chunks[["simple"]])) })
GetoptLong.options("__argv_str__" = NULL)
GetoptLong.options("help_style" = "one-column")

## ----template, eval = FALSE---------------------------------------------------
#  spec = "
#  This is an example of using template to specify options.
#  
#  Usage: Rscript foo.R [options]
#  
#  Options:
#    <number=i> Number of items.
#    <cutoff=f> Cutoff for filtering results.
#    <verbose> Print messages.
#  
#  Contact: name@address
#  "
#  
#  GetoptLong(spec, template_control = list(opt_width = 21))

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ eval(parse(text = chunks[["template"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ----print_var, eval = FALSE--------------------------------------------------
#  library(GetoptLong)
#  
#  cutoff = 0.05
#  GetoptLong(
#      "number=i{1,}", "Number of items.",
#      "cutoff=f", "Cutoff for filtering results.",
#      "param=s%", "Parameters specified by name=value pairs.",
#      "verbose",  "Print message."
#  )
#  
#  print(number)
#  print(cutoff)
#  print(param)
#  print(verbose)

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--number 1 2 --param var1=a var2=b --verbose")
local({ eval(parse(text = chunks[["print_var"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ---- eval = FALSE------------------------------------------------------------
#  verbose = TRUE
#  GetoptLong(
#      "verbose!",  "Print message."
#  )

## ----with_env, eval = FALSE---------------------------------------------------
#  opt = new.env()
#  
#  opt$cutoff = 0.05
#  GetoptLong(
#      "number=i@", "Number of items.",
#      "cutoff=f", "Cutoff for filtering results.",
#      "param=s%", "Parameters specified by name=value pairs.",
#      "verbose",  "Print message.",
#  
#      envir = opt
#  )
#  print(as.list(opt))

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--number 1 2 --param var1=a var2=b --verbose")
local({ eval(parse(text = chunks[["with_env"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ----complex_example, eval = FALSE--------------------------------------------
#  GetoptLong(
#      "count=i",  paste("This is a count. This is a count. This is a count.",
#                        "This is a count.  This is a count. This is a count."),
#      "number=f", paste("This is a number. This is a number. This is a number.",
#                        "This is a number. This is a number. This is a number."),
#      "array=f@", paste("This is an array. This is an array. This is an array.",
#                        "This is an array. This is an array. This is an array."),
#      "hash=s%",  paste("This is a hash. This is a hash. This is a hash.",
#                        "This is a hash. This is a hash. This is a hash."),
#      "verbose!", "Whether show messages",
#      "flag",     "a non-sense option"
#  )

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ eval(parse(text = chunks[["complex_example"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ count = 1
    number = 0.1
    array = c(1, 2)
    hash = list("foo" = "a", "bar" = "b")
    verbose = TRUE
    eval(parse(text = chunks[["complex_example"]])) 
})
GetoptLong.options("__argv_str__" = NULL)

## ---- eval = FALSE------------------------------------------------------------
#  GetoptLong.options(help_style = "two-column")
#  # specifying the defaults
#  GetoptLong{
#      ...
#  }

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
GetoptLong.options("help_style" = "two-column")
local({ count = 1
    number = 0.1
    array = c(1, 2)
    hash = list("foo" = "a", "bar" = "b")
    verbose = TRUE
    eval(parse(text = chunks[["complex_example"]])) 
})
GetoptLong.options("__argv_str__" = NULL)
GetoptLong.options("help_style" = "one-column")

## ----sub_opt, eval = FALSE----------------------------------------------------
#  foo = list(a = 1, b = 2)
#  GetoptLong(
#      "foo=i%", paste("This is foo. This is foo. This is foo. This is foo.",
#                      "This is foo. This is foo. This is foo. This is foo."),
#      "foo$name1", paste("name1 in foo. name1 in foo. name1 in foo. name1 in foo.",
#                         "name1 in foo. name1 in foo. name1 in foo. name1 in foo."),
#      "foo$name2", paste("name2 in foo. name2 in foo. name2 in foo. name2 in foo.",
#                         "name2 in foo. name2 in foo. name2 in foo. name2 in foo."),
#      "bar=s%", paste("This is bar. This is bar. This is bar. This is bar.",
#                      "This is bar. This is bar. This is bar. This is bar."),
#      "bar$name3", paste("name3 in bar. name3 in bar. name3 in bar. name3 in bar.",
#                         "name3 in bar. name3 in bar. name3 in bar. name3 in bar."),
#      "bar$name4", paste("name4 in bar. name4 in bar. name4 in bar. name4 in bar.",
#                         "name4 in bar. name4 in bar. name4 in bar. name4 in bar.")
#  )

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ eval(parse(text = chunks[["sub_opt"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ---- eval = FALSE------------------------------------------------------------
#  GetoptLong.options(help_style = "two-column")
#  GetoptLong{
#      ...
#  }

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
GetoptLong.options(help_style = "two-column")
local({ eval(parse(text = chunks[["sub_opt"]])) })
GetoptLong.options("__argv_str__" = NULL)
GetoptLong.options(help_style = "one-column")

## ----head_and_foot, eval = FALSE----------------------------------------------
#  GetoptLong(
#      help_head = "This is a demonstration of adding usage head and foot.",
#  
#      "number=i", "Number of items.",
#      "cutoff=f", "Cutoff for filtering results.",
#      "verbose",  "Print message.",
#  
#      help_foot = "Please contact name@address."
#  )

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ eval(parse(text = chunks[["head_and_foot"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ----grouped_options, eval = FALSE--------------------------------------------
#  count = 1
#  array = c(0.1, 0.2)
#  GetoptLong(
#      "--------", "Binary options:",
#      "verbose!", "Whether show messages",
#      "flag",     "a non-sense option",
#  
#      "-------", "Single-value options:",
#      "count=i",  paste("This is a count. This is a count. This is a count.",
#                        "This is a count.  This is a count. This is a count."),
#      "number=f", paste("This is a number. This is a number. This is a number.",
#                        "This is a number. This is a number. This is a number."),
#  
#      "--------", paste("Multiple-vlaue options: long text long text long text",
#                        " long text long text long text long text long text"),
#      "array=f@", paste("This is an array. This is an array. This is an array.",
#                        "This is an array. This is an array. This is an array."),
#      "hash=s%",  paste("This is a hash. This is a hash. This is a hash.",
#                        "This is a hash. This is a hash. This is a hash."),
#  
#      "-------", "Other options:"
#  )

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ eval(parse(text = chunks[["grouped_options"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ---- eval = FALSE------------------------------------------------------------
#  GetoptLong.options(help_style = "two-column")
#  GetoptLong{
#      ...
#  }

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
GetoptLong.options(help_style = "two-column")
local({ eval(parse(text = chunks[["grouped_options"]])) })
GetoptLong.options("__argv_str__" = NULL)
GetoptLong.options(help_style = "one-column")

## ----complex_template, eval = FALSE-------------------------------------------
#  GetoptLong("
#  This is a demonstration of using template as the option specification.
#  
#  Usage: Rscript foo.R [options]
#  
#  Binary options:
#    <verbose!> Whether show messages
#    <flag> A non-sense option
#  
#  Single-value options:
#    <count=i> This is a count. This is a count.
#    <number=f> This is a number. This is a number.
#  
#  Multiple-vlaue options:
#    <array=f@> This is an array. This is an array.
#    <hash=s%> This is a hash. This is a hash.
#  
#  Questions, please contact your.name@email
#  ", template_control = list(opt_width = c(verbose = 23, flag = 23,
#                                           count = 22, number = 22,
#                                           array = 30, hash = 30)
#  ))

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ eval(parse(text = chunks[["complex_template"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ----template_multi_line, eval = FALSE----------------------------------------
#  GetoptLong("
#  Usage: Rscript foo.R [options]
#  
#    <count=i{2,}> This is a count. This is a count. This is a count.
#    <#count> This is a count. This is a count.
#    <number=f> This is a number. This is a number. This is a number.
#    <#number> This is a number. This is a number.
#  ", template_control = list(opt_width = c(count = 28, number = 28))
#  )

## ---- echo = FALSE------------------------------------------------------------
chunks <- knitr:::knit_code$get()
GetoptLong.options("__argv_str__" = "--help")
local({ eval(parse(text = chunks[["template_multi_line"]])) })
GetoptLong.options("__argv_str__" = NULL)

## ----version, eval = FALSE----------------------------------------------------
#  VERSION = "0.0.1"
#  GetoptLong(
#      "tag=i", "...",
#  )

## ---- eval = FALSE------------------------------------------------------------
#  GetoptLong.options("config" = "bundling")
#  GetoptLong.options("config" = c("no_ignore_case", "bundling"))

## ---- eval = FALSE------------------------------------------------------------
#  source_script("foo.R", argv_str = "--cutoff 0.01 --input file=foo.txt --verbose")

## ---- eval = TRUE-------------------------------------------------------------
sessionInfo()

## ---- echo = FALSE, results = "asis"------------------------------------------
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
  os <- sysinf['sysname']
  if (os == 'Darwin')
    os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

is.solaris = function() {
  os = get_os()
  !(os %in% c("windows", "unix", "linux", "osx"))
}

if(is.solaris()) {
  cat("**GetoptLong** is not supported on Solaris platform.\n")
} else {
  if(!is.solaris()) {
      invisible(knit("GetoptLong.Rmd2", "GetoptLong.md2"))
  }

  if(Sys.info()["user"] == "jokergoo") {
      invisible(file.copy("GetoptLong.md2", "/Users/jokergoo/project/GetoptLong/vignettes/GetoptLong.md2", overwrite = TRUE))
  }

  ln = readLines("GetoptLong.md2")
  cat(paste(ln, collapse = "\n"))
}


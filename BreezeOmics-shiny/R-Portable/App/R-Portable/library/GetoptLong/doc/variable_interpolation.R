## ---- echo = FALSE, message = FALSE---------------------------------------------------------------
library(markdown)
options(markdown.HTML.options = c(options('markdown.HTML.options')[[1]], "toc"))

library(knitr)
knitr::opts_chunk$set(
    error = FALSE,
    tidy  = FALSE,
    message = FALSE,
    fig.align = "center")
options(markdown.HTML.stylesheet = "custom.css")

options(width = 100)

## -------------------------------------------------------------------------------------------------
region = c(1, 2)
value = 4
name = "name"
str = paste("region = (", region[1], ", ", region[2], "), value = ", value, 
            ", name = '", name, "'", sep = "")
cat(str)

## -------------------------------------------------------------------------------------------------
library(GetoptLong)

str = qq("region = (@{region[1]}, @{region[2]}), value = @{value}, name = '@{name}'")
cat(str)

## -------------------------------------------------------------------------------------------------
qqcat("region = (@{region[1]}, @{region[2]}), value = @{value}, name = '@{name}'")

## -------------------------------------------------------------------------------------------------
qq.options("cat_prefix" = "[INFO] ")
qqcat("This is a message")

qq.options("cat_prefix" = function() format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "))
qqcat("This is a message")
Sys.sleep(2)
qqcat("This is a message after 2 seconds")

qq.options("cat_prefix" = "")
qqcat("This is a message")

## -------------------------------------------------------------------------------------------------
qq.options("cat_prefix" = "[INFO] ", "cat_verbose" = FALSE)
qqcat("This is a message")

## -------------------------------------------------------------------------------------------------
qq.options(RESET = TRUE)
qq.options("cat_prefix" = "[DEBUG] ")
qqcat("This is a message", cat_prefix = "[INFO] ")
qqcat("This is a message")

## -------------------------------------------------------------------------------------------------
qq.options("cat_prefix" = "[DEBUG] ")
qq.options(LOCAL = TRUE)
qq.options("cat_prefix" = "[INFO] ")
qqcat("This is the first message")
qqcat("This is the second message")
qq.options(LOCAL = FALSE)
qqcat("This is the third message")

## ---- eval = TRUE, results = 'hide', echo = TRUE--------------------------------------------------
qq.options(RESET = TRUE)

## -------------------------------------------------------------------------------------------------
n = 1
qqcat("There @{ifelse(n == 1, 'is', 'are')} @{n} dog@{ifelse(n == 1, '', 's')}.\n")
n = 2
qqcat("There @{ifelse(n == 1, 'is', 'are')} @{n} dog@{ifelse(n == 1, '', 's')}.\n")

## -------------------------------------------------------------------------------------------------
qq.options("cat_strwrap" = TRUE)
qqcat("one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve, thirteen, fourteen, fifteen, sixteen, seventeen, eighteen, nineteen, twenty.")

## -------------------------------------------------------------------------------------------------
a = 1; b = 2; c = 3
txt = qq("command -a @{a}",
         "        -b @{b}",
         "        -c @{c}\n", sep = " \\\n")
cat(txt)

## ---- eval = FALSE--------------------------------------------------------------------------------
#  qq = GetoptLong::qq

## -------------------------------------------------------------------------------------------------
x = 1
qqcat("x = #{x}", code.pattern = "#\\{CODE\\}")

## ---- eval = FALSE--------------------------------------------------------------------------------
#  qq.options("code.pattern" = "#\\{CODE\\}")

## ---- eval = FALSE--------------------------------------------------------------------------------
#  code.pattern = "@\\{CODE\\}"    # default style
#  code.pattern = "@\\[CODE\\]"
#  code.pattern = "@\\(CODE\\)"
#  code.pattern = "%\\{CODE\\}"
#  code.pattern = "%\\[CODE\\]"
#  code.pattern = "%\\(CODE\\)"
#  code.pattern = "\\$\\{CODE\\}"
#  code.pattern = "\\$\\[CODE\\]"
#  code.pattern = "\\$\\(CODE\\)"
#  code.pattern = "#\\{CODE\\}"
#  code.pattern = "#\\[CODE\\]"
#  code.pattern = "#\\(CODE\\)"
#  code.pattern = "\\[%CODE%\\]"  # Template Toolkit (Perl module) style :)

## ---- eval = FALSE--------------------------------------------------------------------------------
#  code.pattern = "`CODE`"

## -------------------------------------------------------------------------------------------------
x = 1
y = 2
qqcat("x = @{x}, y = @{y}", envir = list(x = "a", y = "b"))

## -------------------------------------------------------------------------------------------------
x = 1:6
qqcat("@{x} is an @{ifelse(x %% 2, 'odd', 'even')} number.\n")


y = c("a", "b")
z = c("A", "B", "C", "D", "E")
qqcat("@{x}, @{y}, @{z}\n")

## -------------------------------------------------------------------------------------------------
name = letters[1:4]
value = 1:4
qqcat("<tr><td>@{name}</td><td>@{value}</td><tr>\n")

## -------------------------------------------------------------------------------------------------
str = qq("@{x}, @{y}, @{z}", collapse = FALSE)
length(str)
str

## -------------------------------------------------------------------------------------------------
sessionInfo()


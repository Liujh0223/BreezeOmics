### R code from vignette source 'tkWidgets.Rnw'

###################################################
### code chunk number 1: tkWidgets.Rnw:55-87
###################################################
library(tkWidgets)
PWEnv <- new.env(hash = TRUE, parent = parent.frame(1))
argsFun <- function(){
    argsWidget(list(Entry1 = "aaaaa", Ehtry2 = "bbbb"))
}
button1 <- button(wName = "button1", wValue = "argsWidget",
                     wFuns = list(command = argsFun), wEnv = PWEnv)
button2 <- button(wName = "button2", wValue = "DPExplorer",
                     wFuns = list(command = DPExplorer), wEnv = PWEnv)
button3 <- button(wName = "button3", wValue = "fileBrowser",
                     wFuns = list(command = fileBrowser), wEnv = PWEnv)
button4 <- button(wName = "button4", wValue = "importWizard",
                     wFuns = list(command = importWizard), wEnv = PWEnv)
button5 <- button(wName = "button5", wValue = "objectBrowser",
                     wFuns = list(command = objectBrowser), wEnv = PWEnv)
button6 <- button(wName = "button6", wValue = "vExplorer",
                     wFuns = list(command = vExplorer), wEnv = PWEnv)
button7 <- button(wName = "button7", wValue = "pExplorer",
                     wFuns = list(command = pExplorer), wEnv = PWEnv)
fun <- function(){eExplorer("tkWidgets")}
button8 <- button(wName = "button8", wValue = "eExplorer",
                     wFuns = list(command = fun), wEnv = PWEnv)

pWidgets <- list(A = list(button1 = button1, button2 = button2), C =
list(button3 = button3, button4 = button4), D = list(button5 = button5,
button6 = button6), E = list(button7 = button7, button8 = button8))

if(interactive()){
viewWidget <- widget(wTitle = "tkWidgets Examples", pWidgets, funs = list(),
                 preFun = function() print("Hello"),
                 postFun = function() print("Bye"), env = PWEnv)
}


###################################################
### code chunk number 2: tkWidgets.Rnw:244-245
###################################################
toLatex(sessionInfo())



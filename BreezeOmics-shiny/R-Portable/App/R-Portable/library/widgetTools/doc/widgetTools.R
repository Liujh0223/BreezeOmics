### R code from vignette source 'widgetTools.Rnw'

###################################################
### code chunk number 1: widgetTools.Rnw:111-113
###################################################
library(widgetTools)
PWEnv <- new.env(hash = TRUE, parent = parent.frame(1))


###################################################
### code chunk number 2: widgetTools.Rnw:170-192
###################################################
label1 <- label(wName = "label1", wValue = "File Name: ", wEnv = PWEnv)
entry1 <- entryBox(wName = "entry1", wValue = "Feed me using browse",
                   wEnv = PWEnv)
browse2Entry1 <- function(){
    tempValue <- tclvalue(tkgetOpenFile())
    temp <- get(wName(entry1), env = PWEnv)
    wValue(temp) <- paste(tempValue, sep = "", collapse = ";")
    assign(wName(entry1), temp, env = PWEnv)
}
button1 <- button(wName = "button1", wValue = "Browse",
                     wFuns = list(command = browse2Entry1), wEnv = PWEnv)
list1 <- listBox(wName = "list1", wValue = c(Option1 = TRUE, Option2 = FALSE,
                                 Option3 = FALSE), wEnv = PWEnv)
text1 <- textBox(wName = "text1", wValue = "Feed me something",
                 wEnv = PWEnv)
label2 <- label(wName = "label2", wValue = "Select one:  ", wEnv = PWEnv)
radios1 <- radioButton(wName = "radios1", wValue = c(radio1 = TRUE,
                       radio2 = FALSE, radio3 = FALSE), wEnv = PWEnv)
label3 <- label(wName = "label3", wValue = "Select one to many: ",
                wEnv = PWEnv)
checks1 <- checkButton(wName = "checks1", wValue = c(check1 = TRUE,
                       check22 = FALSE, check3 = FALSE), wEnv = PWEnv)


###################################################
### code chunk number 3: widgetTools.Rnw:232-236
###################################################
wName(label1)
wName(label1) <- "YYY"
wName(label1)
wName(label1) <- "label1"


###################################################
### code chunk number 4: widgetTools.Rnw:251-256
###################################################
pWidgets <- list(topRow = list(label1 = label1, entry1 = entry1,
                 button1 = button1), textRow = list(list1 = list1,
                 text1 = text1), radGroup = list(label2 = label2,
                 radios1 = radios1), chkGroup = list(label3 = label3,
                                     checks1 = checks1))


###################################################
### code chunk number 5: widgetTools.Rnw:303-308
###################################################
if(interactive()){
aWidget <- widget(wTitle = "A test widget", pWidgets, funs = list(),
                 preFun = function() print("Hello"),
                 postFun = function() print("Bye"), env = PWEnv)
}


###################################################
### code chunk number 6: widgetTools.Rnw:328-335
###################################################
if(interactive()){
wValue(pWidgets(aWidget)[["topRow"]][["entry1"]])
wValue(pWidgets(aWidget)[["textRow"]][["list1"]])
wValue(pWidgets(aWidget)[["textRow"]][["text1"]])
wValue(pWidgets(aWidget)[["radGroup"]][["radios1"]])
wValue(pWidgets(aWidget)[["chkGroup"]][["checks1"]])
}



print.raov <-
#function (x, digits = max(5, .Options$digits - 2), ...) 
function (x,...) 
{
#    cat("Call:\n")
#    print(x$call)
	cat("\nRobust ANOVA Table\n")
	Table<-x$tab
#    print(format(Table,digit=digits), ...)
    print(Table,...)
}

print.raov <-
function (x, digits = max(5, .Options$digits - 2), ...) 
#function (x,...) 
{
#    cat("Call:\n")
#    print(x$call)
	cat("\nRobust ANOVA Table\n")
	Table<-round(x$tab,digits)
#    print(format(Table,digit=digits), ...)
    print(Table,...)
}

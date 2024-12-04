
Problemet verkar vara att om vi kallar 

measureFiveValues timingFun nSamples funs

där funs är en lista med kopior av en och samma funktion, tar den första körningen förväntad tid, dvs. typ 8 sekunder, medan övriga tar ca en tusendels sekund.

Problemet finns även med GenFuns, första körningen tar lång tid medan efterföljande blir snabba.
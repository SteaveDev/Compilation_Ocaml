Lambda 65
DclArg i
SetVar bar
Continue 17 False
GetVar x
CstRe 2.
AddiRe
GetVar bar
StCall
GetVar i
CstRe 3.
MultRe
SetArg
Call
Print
Drop
Jump 3
SetVar err
GetVar err
Print
Continue 36 False
GetVar x
CstRe 2.
AddiRe
CstStrsalut
SetVar y
GetVar bar
StCall
GetVar i
CstRe 3.
MultRe
SetArg
Call
Print
Drop
Jump 3
SetVar err
GetVar err
Print
DclVar i
CstRe 10.
SetVar i
GetVar i
CstRe 0.
GreStR
ConJmp 19
Continue 57 False
GetVar bar
StCall
GetVar i
CstRe 3.
MultRe
SetArg
Call
Print
Drop
Jump 3
SetVar err
GetVar err
Print
GetVar i
CstRe 1.
SubsRe
SetVar i
Jump -23
Jump 12
GetVar i
CstRe 0.
Equal
ConJmp 3
CstRe 42.
Throw
Jump 3
GetVar x
CstRe 2.
AddiRe
GetVar i
Return 
Halt

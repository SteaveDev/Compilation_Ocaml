
type instruction_a =
   | AddiRe
   | SubsRe 
   | MultRe
   | DiviRe 
   | NegaRe 
   | Modulo 
   | Not   
   | BoToRe 
   | ReToBo 
   | Equal
   | NotEq 
   | LowEqR  
   | GreEqR 
   | LowStR 
   | GreStR
   | Jump   of int 
   | ConJmp of int 
   | Case   of int
   | CstRe  of float 
   | CstBo  of bool
   | CstUn
   | Copy  
   | Swap 
   | Drop
   | Noop
   | TypeOf
   | Halt 
   | Error  of string
   | Assert of string
   | Print  
   | SetVar of string
   | GetVar of string
   | SetArg  
   | StCall    
   | Call  
   | Return  
   | DclVar of string
   | Lambda of int 
   | DclArg of string
   | Throw
   | Continue of int * bool
   | Concat
   | CstStr of string
   | LowEqS  
   | GreEqS 
   | LowStS 
   | GreStS
   | StToRe
   | ReToSt
   | IsEmpt
   | Point  of string
   | Null
   | NewObj
   | SetObj of string
   | Protot
   | GetPrt            
   | TlCall
   | RetOpt
   | RetZer
   | RetZOp
   | SetVD of string
   | SetVS of int
   | GetVS of int
   | Catch of int
   | Finally of int
;;




let rec print_ast form = function
   | AddiRe          -> Format.fprintf form "AddiRe\n"
   | SubsRe          -> Format.fprintf form "SubsRe\n"
   | MultRe          -> Format.fprintf form "MultRe\n"
   | DiviRe          -> Format.fprintf form "DiviRe\n"
   | NegaRe          -> Format.fprintf form "NegaRe\n"
   | Modulo          -> Format.fprintf form "Modulo\n"
   | Not             -> Format.fprintf form "Not\n"
   | BoToRe          -> Format.fprintf form "BoToRe\n"
   | ReToBo          -> Format.fprintf form "ReToBo\n"
   | Equal           -> Format.fprintf form "Equal\n"
   | NotEq           -> Format.fprintf form "NotEq\n"
   | LowEqR          -> Format.fprintf form "GreEqR\n"
   | GreEqR          -> Format.fprintf form "GreEqR\n"
   | LowStR          -> Format.fprintf form "LowStR\n"
   | GreStR          -> Format.fprintf form "GreStR\n"
   | Jump(n)         -> Format.fprintf form "Jump %i\n" n
   | ConJmp(n)       -> Format.fprintf form "ConJmp %i\n" n
   | Case(n)         -> Format.fprintf form "Case %i\n" n
   | CstRe(f)        -> Format.fprintf form "CstRe %g\n" f
   | CstBo(b)        -> Format.fprintf form "CstBo %s\n" (if b then "True" else "False")
   | CstUn           -> Format.fprintf form "CstUn\n"
   | Copy            -> Format.fprintf form "Copy\n"
   | Swap            -> Format.fprintf form "Swap\n"
   | Drop            -> Format.fprintf form "Drop\n"
   | Noop            -> Format.fprintf form "Noop\n"
   | TypeOf          -> Format.fprintf form "TypeOf\n"
   | Halt            -> Format.fprintf form "Halt\n"
   | Error(s)        -> Format.fprintf form "Error %s\n" s
   | Assert(s)       -> Format.fprintf form "Assert %s\n" s
   | Print           -> Format.fprintf form "Print\n"
   | SetVar(var)     -> Format.fprintf form "SetVar %s\n" var
   | GetVar(var)     -> Format.fprintf form "GetVar %s\n" var
   | SetArg          -> Format.fprintf form "SetArg\n"
   | StCall            -> Format.fprintf form "StCall\n"
   | Call            -> Format.fprintf form "Call\n"
   | Return          -> Format.fprintf form "Return\n"
   | DclVar(var)     -> Format.fprintf form "DclVar %s\n" var
   | Lambda(n)       -> Format.fprintf form "Lambda %i\n" n
   | DclArg(var)     -> Format.fprintf form "DclArg %s\n" var
   | Throw           -> Format.fprintf form "Throw\n"
   | Continue (i,b)  -> Format.fprintf form "Continue %i %s\n" i (if b then "True" else "False")
   | Concat          -> Format.fprintf form "Concat\n"
   | CstStr(s)       -> Format.fprintf form "CstStr %s\n" s
   | LowEqS          -> Format.fprintf form "LowEqS\n"
   | GreEqS          -> Format.fprintf form "GreEqS \n"
   | LowStS          -> Format.fprintf form "LowStS\n"
   | GreStS          -> Format.fprintf form "GreStS\n "
   | StToRe          -> Format.fprintf form "StToRe\n"
   | ReToSt          -> Format.fprintf form "ReToSt\n"
   | IsEmpt          -> Format.fprintf form "IsEmpt\n"
   | Point(var)      -> Format.fprintf form "Point %s\n" var
   | Null            -> Format.fprintf form "Null\n"
   | NewObj          -> Format.fprintf form "NewObj\n"
   | SetObj(var)     -> Format.fprintf form "SetObj %s\n" var
   | Protot          -> Format.fprintf form "Protot\n"
   | GetPrt         -> Format.fprintf form "GetProt\n"
   | TlCall          -> Format.fprintf form "TlCall\n"
   | RetOpt          -> Format.fprintf form "RetOpt\n"
   | RetZer          -> Format.fprintf form "RetZer\n"
   | RetZOp          -> Format.fprintf form "RetZOp\n"
   | SetVD(var)      -> Format.fprintf form "SetVar2 %s\n" var
   | SetVS(i)        -> Format.fprintf form "SetVarStack %i\n" i
   | GetVS(i)        -> Format.fprintf form "GetVarStack %i\n" i
   | Catch(i)        -> Format.fprintf form "Catch %i\n" i
   | Finally(i)      -> Format.fprintf form "Finally %i\n" i
;;

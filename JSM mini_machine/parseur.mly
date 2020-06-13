
%{
open AST
%}

%token <int> NOMBRE
%token <float> DOUBLE
%token <bool> BOOL
%token <string> VAR STRING
%token  AddiRe SubsRe MultRe DiviRe NegaRe Modulo Not BoToRe ReToBo Catch Finally
        Equal  NotEq LowEqR GreEqR LowStR GreStR IsTrue Jump  ConJmp Case
	CstRe CstBo Copy Swap Drop Noop TypeOf Halt Error Assert Print SetVar SetVD
	GetVar SetArg StCall Call Return DclVar Lambda DclArg Throw Continue Concat
	CstStr LowEqS GreEqS LowStS GreStS StToRe ReToSt IsEmpt SetVS GetVS 
	Point Null NewObj SetObj NotEqO CstUn Protot GetPrt TlCall RetOpt RetZer RetZOp
        EOL EOF


%type <AST.instruction_a list> main 
%type <AST.instruction_a > intruction

%start main



%%

main: 
   EOF      {[] }
 | EOL main      {$2 }
 | intruction EOF      {[$1] }
 | intruction EOL main {$1::$3 }
 ;
   
intruction: 
     AddiRe                  { AddiRe }
   | SubsRe                  { SubsRe }
   | MultRe                  { MultRe }
   | DiviRe                  { DiviRe }
   | NegaRe                  { NegaRe }
   | Modulo                  { Modulo }
   | Not                     { Not }
   | BoToRe                  { BoToRe }
   | ReToBo                  { ReToBo }
   | Equal                   { Equal  }
   | NotEq                   { NotEq }
   | LowEqR                  { LowEqR }
   | GreEqR                  { GreEqR }
   | LowStR                  { LowStR }
   | GreStR                  { GreStR }
   | Jump NOMBRE             { Jump($2) }
   | ConJmp NOMBRE           { ConJmp($2) }
   | Case                    { Case(1) }
   | Case NOMBRE             { Case($2) }
   | CstRe NOMBRE            { CstRe(float_of_int($2)) }
   | CstRe DOUBLE            { CstRe($2) }
   | CstBo BOOL              { CstBo($2) }
   | CstUn                   { CstUn }
   | Copy                    { Copy }
   | Swap                    { Swap }
   | Drop                     {Drop }
   | Noop                    { Noop }
   | TypeOf                  { TypeOf }
   | Halt                    { Halt }
   | Error STRING            { Error($2) }  
   | Error                   { Error("") }  
   | Assert STRING           { Assert($2) }
   | Assert                  { Assert("") }
   | Print                   { Print }
   | SetVar VAR              { SetVar($2) }
   | GetVar VAR              { GetVar($2) }
   | SetArg                  { SetArg }
   | StCall                  { StCall }
   | Call                    { Call }
   | Return                  { Return }
   | DclVar VAR              { DclVar($2) }
   | Lambda NOMBRE           { Lambda($2) }
   | DclArg VAR              { DclArg($2) }
   | Throw                   { Throw }
   | Continue NOMBRE BOOL    { Continue($2,$3) }
   | Concat                  { Concat }
   | CstStr STRING           { CstStr($2)}
   | LowEqS                  { LowEqS }
   | GreEqS                  { GreEqS }
   | LowStS                  { LowStS }
   | GreStS                  { GreStS }
   | StToRe                  { StToRe }
   | ReToSt                  { ReToSt }
   | IsEmpt                  { IsEmpt }
   | Point VAR               { Point($2) } 
   | Null                    { Null }
   | NewObj                  { NewObj }
   | SetObj VAR              { SetObj($2) }
   | Protot                  { Protot }
   | GetPrt                  { GetPrt }
   | TlCall                  { TlCall }
   | RetOpt                  { RetOpt }
   | RetZer                  { RetZer }
   | RetZOp                  { RetZOp }
   | SetVD VAR               { SetVD($2) }
   | SetVS NOMBRE            { SetVS($2) }
   | GetVS NOMBRE            { GetVS($2) }
   | Catch NOMBRE            { Catch($2) }
   | Finally NOMBRE          { Finally($2) }
 ;

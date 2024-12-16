# Trial Exam PRDAT

##### Max Brix Koch
##### 11.12.2024

Jeg erklærer hermed at jeg selv har lavet hele denne eksamensbesvarelse uden hjælp fra andre.

### 1 - Regulære udtryk og automater

##### 1. 

d+: En eller flere decimaler
','?: 0 eller 1 komma
d*: 0 eller flere decimaler
RegEx'et vil altså acceptere alle hel- eller kommatal fra 0 eller over. Derudover accepterer den også alle heltal efterfulgt af et komma, men uden decimaler efter kommaet.

Eksempler på accepterede strenge: 
0,0
532563557355,22153252643235242
0,24125465754643543434
25423549025829433029423902,3
4
7,

##### 2.

Tilstandsmaskinen accepterer netop de samme strenge som det regulære udtryk. 
Dette skyldes, at man for at komme videre fra state 1, skal indsætte et decimal, dette er det eneste sted der er påkrævet et tegn, og derfor kan resten af tilstandsmaskinen gås igennem med brug af epsilon og nå frem til et final state, 7.
Ekstra decimaler kan indsættes før komma ved at hoppe mellem state 2 og 3, komma kan indsættes når man hopper fra state 4 til 5.
Decimaler efter komma kan indsættes ved at hoppe mellem state 5 og 6, men kan også undlades ved at hoppe direkte fra state 5 til 7.

Da der er brug af epsilon i tilstandsmaskinen er den ikke-deterministisk.

##### 3. 

(d+(','d+)?)?

##### 4. 

kommatal.fsl:
```
{ // starting
module Kommatal_fslex
open FSharp.Text.Lexing
open System
}

rule Tokenize = parse
  |(['0'-'9']+('.'['0'-'9']+)?)? eof { LexBuffer<char>.LexemeString lexbuf }
  | _ { failwith "Lexer error: illegal symbol" }

{ // ending

[<EntryPoint>]
let main argv =
      printfn "Prøveeksamen, kommatal.\n\nTast et tal:"
      let input = Console.ReadLine()
      let res=Tokenize (LexBuffer<char>.FromString input)

      printfn "Lexer genkender %s" res
      0
}
```

Så køre jeg " fslex --unicode kommatal.fsl "


Outputs:
Tast et tal:
5.123
Lexer genkender 5.123

Tast et tal:
0
Lexer genkender 0

Tast et tal:

Lexer genkender 

### 2. Icon

##### 1. 
```F#
let examEx1 = Every (Write(FromTo(1,6)))
```

Every gør at vi kører Write på alle værdierne i vores FromTo(1,6).

##### 2.
```F#
let examEx2 = Every(Write(Prim("+", Prim("*", FromTo(3, 6), CstI 10), FromTo(3, 4))))
```

##### 3.
typen expr udvides til:
```F#
type expr = 
  | CstI of int
  | CstS of string
  | FromTo of int * int
  | FromToBy of int * int * int
  | Write of expr
  | If of expr * expr * expr
  | Prim of string * expr * expr 
  | And of expr * expr
  | Or  of expr * expr
  | Seq of expr * expr
  | Every of expr 
  | Fail;;
```

Eval udvides med:
```F#
let rec eval (e : expr) (cont : cont) (econt : econt) = 
    match e with
    | ...
    | FromToBy(s, e, i) ->
      let rec loop l = 
          if ((s > e) || (0 > i)) then
              econt ()
          elif l <= e then
              cont (Int l) (fun () -> loop(l + i))
          else
              econt ()
      loop s
    | ...
```

Jeg tester at funktionen virker med:
```F#
let examEx3 = Every (Write(FromToBy(1,6,3)))
```
So udskriver:
```
> open Icon;;
> run examEx3;;
1 4 val it: value = Int 0
```

##### 4. 
```F#
let examEx4 = Every(Every(Write(Prim("+", FromToBy(0, 30, 10), FromTo(33, 34)))))
```

##### 5.
FromToBy genererer det samme tal uendeligt, hvis man sætter i til 0, som f.eks. i FromToBy(1,2,0). 

### 3. Print i micro-ML

##### 1.
I Absyn.fs tilføjer jeg print så expr ser ud som følger:
```F#
type expr = 
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfun of string * string * expr * expr    (* (f, x, fBody, letBody) *)
  | Call of expr * expr
  | Print of expr
```

##### 2.
I FsLex.fsi:
```
let keyword s =
    match s with
    | "else"  -> ELSE 
    | "end"   -> END
    | "false" -> CSTBOOL false
    | "if"    -> IF
    | "in"    -> IN
    | "let"   -> LET
    | "not"   -> NOT
    | "then"  -> THEN
    | "true"  -> CSTBOOL true
    | "print" -> PRINT
    | _       -> NAME s
```

I FunPar.fsy tilføjes token i toppen:
```
%token ELSE END FALSE IF IN LET NOT THEN TRUE PRINT
```
og parsing af Expr:
```
Expr:
    AtExpr                              { $1                     }
  | AppExpr                             { $1                     }
  | IF Expr THEN Expr ELSE Expr         { If($2, $4, $6)         }
  | MINUS Expr                          { Prim("-", CstI 0, $2)  }
  | Expr PLUS  Expr                     { Prim("+",  $1, $3)     }
  | Expr MINUS Expr                     { Prim("-",  $1, $3)     }
  | Expr TIMES Expr                     { Prim("*",  $1, $3)     }
  | Expr DIV   Expr                     { Prim("/",  $1, $3)     } 
  | Expr MOD   Expr                     { Prim("%",  $1, $3)     }
  | Expr EQ    Expr                     { Prim("=",  $1, $3)     }
  | Expr NE    Expr                     { Prim("<>", $1, $3)     }
  | Expr GT    Expr                     { Prim(">",  $1, $3)     }
  | Expr LT    Expr                     { Prim("<",  $1, $3)     }
  | Expr GE    Expr                     { Prim(">=", $1, $3)     }
  | Expr LE    Expr                     { Prim("<=", $1, $3)     }
  | PRINT expr                          { Print($2)              }
;
```

Den abstrake syntaks for eksemplerne ville se ud som følger:
ex1: Print(CstI 1)
ex2: Print(Prim("+", Print(CstI 1), CstI 3))
ex3: Letfun("f", "x", Prim("+", Var "x", CstI 1), Print(Var "f"))

##### 3.
I HigherFun.fs udvides eval til:
```F#
let rec eval (e : expr) (env : value env) : value =
    match e with
    ...
    | Print(e) -> 
      let v = eval e env
      match v with
      | Int i -> printf "%d " i
      | Closure (f, x, e, w) -> printf "Closure (\"%s\", \"%s\", %A, %A) " f x e w
      v;;
```

### 4. Tupler i List-C

##### 1.

###### Absyn.fs: 
Tilføjet PrimN til expr:
```F#
and expr =                                                         
  | ...
  | PrimN of string * expr list      (* N-ary primitive operator    *)
  | ...
```

###### CPar.fsy: 
Tilføjet tokens:
```
%token CHAR ELSE IF INT NULL PRINT PRINTLN RETURN VOID WHILE TUP NTH UPD
```
Og tilføjet parsing:
```
ExprNotAccess:                                                 
    AtExprNotAccess                     { $1                    }
  | ...
  | TUP LPAR Exprs RPAR                 { PrimN("tup", $3)}
  | NTH LPAR Expr COMMA Expr RPAR       { PrimN("nth", [$3; $5])}
  | UPD LPAR Expr COMMA Expr COMMA Expr RPAR
                                        { PrimN("upd", [$3; $5; $7])}
;
```


###### CLex.fsl: 
Opdateret keyword funktionen
```
let keyword s =
    match s with
    | ...
    | "tup"     -> TUP
    | "upd"     -> UPD
    | "nth"     -> NTH         
    | _         -> NAME s
```

###### Machine.fs: 
Tilføjet instruktionerne:
```F#
type instr =
  | Label of label                     (* symbolic label; pseudo-instruc. *)
  | ...
  | TUP of int
  | UPD
  | NTH
```
Og tilføjet numeriske kodeinstruktioner:
```F#
let CODETUP = 32;
let CODEUPD = 33;
let CODENTH = 34;
```
Modificeret makelabenv:
```F#
let makelabenv (addr, labenv) instr = 
    match instr with
    | Label lab      -> (addr, (lab, addr) :: labenv)
    | ...
    | TUP n          -> (addr+2, labenv)
    | UPD            -> (addr+1, labenv)
    | NTH            -> (addr+1, labenv)
```
Modificeret emitints:
```F#
let rec emitints getlab instr ints = 
    match instr with
    | Label lab      -> ints
    | ...
    | TUP n          -> CODETUP    :: n :: ints
    | UPD            -> CODEUPD    :: ints
    | NTH            -> CODENTH    :: ints
```

###### Comp.fs:
Modificeret cExpr:
```F#
and cExpr (e : expr) (varEnv : varEnv) (funEnv : funEnv) : instr list = 
    match e with
    | ...
    | PrimN(f, es) ->
      match f with
      | "tup" -> 
        List.collect (fun e -> cExpr e varEnv funEnv) es 
        @ [TUP (List.length es)]
      | "upd" -> 
        match es with
        | [t; i; e] -> 
            cExpr t varEnv funEnv
            @ cExpr i varEnv funEnv
            @ cExpr e varEnv funEnv
            @ [UPD] // Append UPD instruction
        | _ -> raise (Failure "upd expects exactly three arguments")
      | "nth" -> 
        match es with
        | [t; i] -> 
            cExpr t varEnv funEnv
            @ cExpr i varEnv funEnv
            @ [NTH] // Append NTH instruction
        | _ -> raise (Failure "nth expects exactly two arguments")
      | _ -> raise (Failure "unknown primitive n")
```

###### listmachine.c:
Tilføj TUPTAG:
```C
#define CONSTAG 0
#define TUPTAG 1
```
Tilføjer defines:
```C
#define TUP 32
#define UPD 33
#define NTH 34
```
Opdater printInstruction:
```C
void printInstruction(word p[], word pc) {
  switch (p[pc]) {
  ...
  case TUP:    printf("TUP " WORD_FMT, p[pc + 1]); break;
  case UPD:    printf("UPD"); break;                     
  case NTH:    printf("NTH"); break;   
  default:     printf("<unknown>"); break;
  }
}
```
Opdater execcode:
```C
int execcode(word p[], word s[], word iargs[], int iargc, int /* boolean */ trace) {
	
  word bp = -999;        // Base pointer, for local variable access 
  word sp = -1;          // Stack top pointer
  word pc = 0;           // Program counter: next instruction
  for (;;) {
    if (trace)
      printStackAndPc(s, bp, sp, p, pc);
    switch (p[pc++]) {
    ...
    case TUP: {  // Create a tuple with size given as immediate argument
      word size = p[pc++];
      word* t = allocate(TUPTAG, size, s, sp);
      for (word i = size - 1; i >= 0; i--) {  // Populate tuple
        t[i + 1] = s[sp--];
      }
      s[++sp] = (word)t;  // Push the tuple to the stack
    } break;
    case UPD: {  // Update an element in the tuple
      word value = s[sp--];
      word index = Untag(s[sp--]);
      word* t = (word*)s[sp];
      if (index < 0 || index >= t[0]) {  // Ensure valid index
        printf("Tuple index out of bounds\n"); return -1;
      }
      t[index + 1] = value;  // Update the tuple element
    } break;
    case NTH: {  // Get the nth element from a tuple
      word index = Untag(s[sp--]);
      word* t = (word*)s[sp];
      if (index < 0 || index >= t[0]) {  // Ensure valid index
        printf("Tuple index out of bounds\n"); return -1;
      }
      s[sp] = t[index + 1];  // Replace tuple with nth element
    } break;
    ...
    }
  }
}
```

##### 2.

Jeg lavede en fil der hed "trialex4.lc" der indeholdt eksempel programmet, og lagde den i folderen LCProgs

```wsl
Lectures/Lec10/ListC# fslex --unicode CLex.fsl 
Lectures/Lec10/ListC# fsyacc --module CPar CPar.fsy
Lectures/Lec10/ListC# dotnet build
Lectures/Lec10/ListC# dotnet run LCProgs/trialex4.lc  
Lectures/Lec10/ListC# cd ListVM/ListVM/
Lectures/Lec10/ListC/ListVM/ListVM# gcc -o listmachine listmachine.c
Lectures/Lec10/ListC/ListVM/ListVM# ./listmachine ../../LCProgs/trialex4.out
32 33 34 42 33 34 10 11 12 13 14 42
Used 0 cpu milli-seconds
```
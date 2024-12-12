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
Efterfulgt af " fsharpc -r ../../fsharp/FsLexYacc.Runtime.dll --standalone kommatal.fs "
Og så " mono .\kommatal.exe " for at køre filen.

Outputs:
Tast et tal:
5.123
Lexer genkender 5.123

Tast et tal:
0
Lexer genkender 0

Tast et tal:

Lexer genkender 

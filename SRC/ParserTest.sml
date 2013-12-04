app load ["Obj","Parsing","Lexing","AbSyn","Parser"];

use "Parser.sml";
use "Lexer.sml";

local

  val parse : string -> AbSyn.Prog = Prog Token o Lexing.createLexerString
  fun parseFile f =
  let
    val is = TextIO.openIn f
  in
    (parse (TextIO.inputAll is); true)
    before TextIO.closeIn is
  end handle _ => false

  fun listDir dir =
  let
    val is = FileSys.openDir dir
    fun loop is = case FileSys.readDir is of
                      NONE   => []
                    | SOME f => f :: loop is
  in
    loop is before FileSys.closeDir is
  end

  val tests = List.filter (fn s => s <> ".gitignore") (listDir "../DATA")

  (* test if we can parse with no exceptions *)
  fun runTests () = app (fn s => if not (parseFile ("../DATA/" ^ s))
                                then print (s ^ ": FAILED\n")
                                else print (s ^ ": OK\n")) tests

  (* COMPARE syntax trees without positions *)
  fun expEQ (Literal (v1,_), Literal (v2,_)) = vEQ (v1,v2)
    | expEQ (StrLit (a,_), StrLit (b,_)) = a = b
    | expEQ (ArrLit (a,_), ArrLit (b,_)) = List.all expEQ (ListPair.zipEq (a,b))
    | expEQ (LValue(v1,_),LValue(v2,_)) = lvalEQ (v1,v2)
    | expEQ (Plus  (a1,b1,_), Plus  (a2,b2,_)) = expEQ (a1,a2) andalso expEQ (b1,b2)
    | expEQ (Minus (a1,b1,_), Minus (a2,b2,_)) = expEQ (a1,a2) andalso expEQ (b1,b2)
    | expEQ (Times (a1,b1,_), Times (a2,b2,_)) = expEQ (a1,a2) andalso expEQ (b1,b2)
    | expEQ (Div   (a1,b1,_), Div   (a2,b2,_)) = expEQ (a1,a2) andalso expEQ (b1,b2)
    | expEQ (Equal (a1,b1,_), Equal (a2,b2,_)) = expEQ (a1,a2) andalso expEQ (b1,b2)
    | expEQ (Less  (a1,b1,_), Less  (a2,b2,_)) = expEQ (a1,a2) andalso expEQ (b1,b2)
    | expEQ (And   (a1,b1,_), And   (a2,b2,_)) = expEQ (a1,a2) andalso expEQ (b1,b2)
    | expEQ (Or    (a1,b1,_), Or    (a2,b2,_)) = expEQ (a1,a2) andalso expEQ (b1,b2)
    | expEQ (Not (e1,_), Not (e2,_)) = expEQ (e1,e2)
    | expEQ (FunApp (i1,e1,_), FunApp (i2,e2,_)) = i1 = i2 andalso
                                List.all expEQ (ListPair.zipEq (e1,e2))
    | expEQ _ = false

  and bvalEQ (Num x, Num y) = x = y
    | bvalEQ (Log a, Log b) = a = b
    | bvalEQ (Chr a, Chr b) = a = b
    | bvalEQ _ = false

  and vEQ (BVal a, BVal b) = bvalEQ (a,b)
    | vEQ (Arr (a,la1,la2), Arr (b,lb1,lb2)) = la1 = lb1 andalso
                                              la2 = lb2 andalso
                                              arrEQ (a,b)
    | vEQ _ = false

  and arrEQ (a,b) =
  let
    val l = Array.length a
    fun cmp i = bvalEQ (Array.sub(a,i), Array.sub(b,i))
  in
    l = Array.length b andalso
    List.all cmp (List.tabulate(l, fn x => x))
  end

  and decEQ (Dec (i1,t1,_), Dec (i2,t2,_)) = i1 = i2 andalso
                                            typeEQ (t1,t2)

  and lvalEQ (Var i1, Var i2)              = i1 = i2
    | lvalEQ (Index (i1,e1),Index (i2,e2)) = i1 = i2 andalso
                                List.all expEQ (ListPair.zipEq (e1,e2))
    | lvalEQ _                             = false

  and stmEQ (Return (NONE, _)   , Return (NONE, _))    = true
    | stmEQ (Return (SOME e1, _), Return (SOME e2, _)) = expEQ (e1,e2)
    | stmEQ (ProcCall (i1,e1,_) , ProcCall (i2,e2,_))  = i1 = i2 andalso
                                List.all expEQ (ListPair.zipEq (e1,e2))
    | stmEQ (Assign (lv1,e1,_),Assign (lv2,e2,_)) = lvalEQ (lv1,lv2)
                                            andalso expEQ (e1,e2)
    | stmEQ (IfThEl (e1,s1a,s1b,_), IfThEl (e2,s2a,s2b,_)) =
              expEQ (e1,e2)     andalso
              blockEQ (s1a,s2a) andalso
              blockEQ (s1b,s2b)
    | stmEQ (While (e1,s1,_), While (e2,s2,_)) = expEQ (e1,e2)   andalso
                                                blockEQ (s1,s2)
    | stmEQ _ = false

  and blockEQ (Block(d1,s1), Block(d2,s2)) =
        List.all decEQ (ListPair.zipEq (d1,d2)) andalso
        List.all stmEQ (ListPair.zipEq (s1,s2))

  and typeEQ (Int _ , Int _ ) = true
    | typeEQ (Bool _, Bool _) = true
    | typeEQ (Char _, Char _) = true
    | typeEQ (Array (t1,_), Array (t2,_)) = typeEQ (t1,t2)
    | typeEQ _              = false

  fun funcEQ (Func(t1,i1,decs1,b1,_),Func(t2,i2,decs2,b2,_)) =
        typeEQ (t1,t2)  andalso
        i1 = i2         andalso
        blockEQ (b1,b2) andalso
        List.all decEQ (ListPair.zipEq (decs1,decs2))
    | funcEQ (Proc(   i1,decs1,b1,_),Proc(   i2,decs2,b2,_)) =
        i1 = i2         andalso
        blockEQ (b1,b2) andalso
        List.all decEQ (ListPair.zipEq (decs1,decs2))
    | funcEQ _ = false

  fun progEQ (p1,p2) = List.all funcEQ (ListPair.zipEq (p1,p2))

  (* TESTS of a few different programs by syntax tree (positions don't matter *)

  val shortest_code = "program foo; procedure main() return;"
  val shortest_tree = [Proc("main",[],Block([],[Return (NONE, (0,0))]),(0,0))]

  val hello_code = "program hello; " ^
                   "procedure hello() " ^
                   "    write(\"Hello, World!\\n\");"
  val hello_tree = [Proc("hello"
                        ,[]
                        ,Block([],[ProcCall ("write"
                                            ,[StrLit ("Hello, World!\n"
                                                     ,(0,0))]
                                            ,(0,0))])
                        ,(0,0))]

  val func_code = "program func; " ^
                  "function foo (n : int) : int " ^
                  "   return n + 2;" ^
                  "procedure main () " ^
                  "begin" ^
                  "  write (foo (3));" ^
                  "end;"

  val func_tree = [Func(Int (0,0)
                       ,"foo"
                       ,[Dec("n",Int (0,0), (0,0))]
                       ,Block([]
                             ,[Return (SOME (Plus (LValue (Var "n",(0,0))
                                                  ,Literal (BVal (Num 2),(0,0))
                                                  ,(0,0)))
                                             ,(0,0))])
                       ,(0,0))
                  ,Proc("main"
                       ,[]
                       ,Block([]
                             ,[ProcCall ("write"
                                        ,[FunApp ("foo"
                                                 ,[Literal (BVal (Num 3),(0,0))]
                                                 ,(0,0))]
                                        ,(0,0))])
                      ,(0,0))
                  ]

  val dec_code = "program foo;" ^
                 "procedure main () " ^
                 "var foo : int;" ^
                 "begin" ^
                 "  foo := 2;" ^
                 "  write(foo);" ^
                 "end;"

  val dec_tree = [Proc("main"
                      ,[]
                      ,Block([Dec("foo",Int(0,0),(0,0))]
                            ,[Assign (Var "foo"
                                     ,Literal (BVal (Num 2),(0,0))
                                     ,(0,0))
                             ,ProcCall ("write"
                                       ,[LValue(Var "foo",(0,0))]
                                       ,(0,0))])
                      ,(0,0))]

  fun testTrees () =
  let
    val tests = [("shortest"   ,shortest_code,shortest_tree)
                ,("hello world",hello_code   ,hello_tree   )
                ,("function"   ,func_code    ,func_tree    )
                ,("dec"        ,dec_code     ,dec_tree     )
                ]

  in
    app (fn (n,c,t) => if progEQ (parse c, t) handle _ => false
                       then print (n ^ ": OK\n")
                       else print (n ^ ": FAIL\n")) tests
  end
in
  val _ = (print "Testing for exceptions:\n";
          runTests();
          print "\nTesting hard-coded syntax trees:\n";
          testTrees())
end

(* ************************************************************

 % Waxeye Parser Generator
 % www.waxeye.org
 % Copyright (C) 2008-2012 Orlando Hill
 % Licensed under the MIT license. See 'LICENSE' for details.

 ************************************************************ *)


(* syntax *)
structure Syn
= struct
    datatype mode = NORMAL
                  | PRUNING
                  | VOIDING

    datatype exp = ANY
                 | CHAR of char
                 | CHAR_CLASS of char_class
                 | SEQ of exp list
                 | ALT of exp list
                 | AND of exp
                 | NOT of exp
                 | OPT of exp
                 | STAR of exp
                 | PLUS of exp
                 | VOID of exp
                 | NT of string
    withtype char_class = (char * char) list

    (* A non-terminal consists of an AST mode and a root expression *)
    type nonterm = mode * exp
  end


(* values *)
structure Value
= struct
    (*
     * An abstract syntax tree has one of three forms.
     * AST_EMPTY represents a successful parse from a voided non-terminal.
     * AST_CHAR just holds a character.
     * AST_TREE represents a successful parse from a non-terminal. It holds:
     * - the non-terminal's name
     * - a list of child asts
     *)
    datatype ast = AST_EMPTY
                 | AST_CHAR of char
                 | AST_TREE of string * ast list

    (*
     * Information for error reporting
     * - position of deepest match failiure
     * - list of non-terminals where match errors occured
     * - list of failed character match ranges
     * - name of current non-terminal
     *)
    type error = int * string list * (char * char) list * string

    datatype value = FAIL of error
                   | VAL of int * ast list * error

    datatype result = AST of ast
                    | PARSE_ERROR of parse_error
    (*
     * A parse error contains
     * - position of deepest match failiure
     * - list of non-terminals where match errors occured
     * - list of failed character match ranges
     *)
    withtype parse_error = int * string list * (char * char) list
  end


(* environment *)
structure Env
= struct
    type 'a env = (string * 'a) list

    exception MISSING_NONTERMINAL

    (* 'a env *)
    val empty = []

    (* extend : string * 'a * 'a env -> 'a env *)
    fun extend (x, v, env)
        = (x, v) :: env

    (* lookup : string * 'a env -> 'a *)
    fun lookup (x, [])
        = raise MISSING_NONTERMINAL
      | lookup (x, (y, b) :: env')
        = if x = y
          then b
          else lookup (x, env')
  end


(* signature *)
signature SIG
= sig
    val match : Syn.nonterm Env.env * string * string -> Value.result
  end


(* tests *)
functor mkTest (structure E : SIG)
= struct
    local
      open Syn
      open Value
      open E
    in
      (*  run_tests : ('a * 'a) list -> (int * 'a * 'a) list *)
      fun run_tests tests =
          let
            fun visit ([], _)
                = []
              | visit ((actual, expected) :: ts, n)
                = if actual = expected
                  then visit (ts, n + 1)
                  else ((n, actual, expected)) :: visit (ts, n + 1)
          in visit (tests, 1) end

      (*  test_eval : env * exp * string -> result  *)
      fun test_eval (env, e, s)
          = match (("S", (VOIDING, e)) :: env, "S", s)

      val env1 = [(* simple *)
                  ("A", (NORMAL, CHAR #"a")),
                  ("B", (NORMAL, ALT[CHAR #"b", CHAR #"B"])),

                  (* direct recursion *)
                  ("C", (NORMAL, ALT [(NT "A"), SEQ [CHAR #"(", (NT "C"), CHAR #")"]])),

                  (* mutual recursion *)
                  ("Int", (NORMAL, ALT [CHAR #"0", SEQ [CHAR_CLASS [(#"1", #"9")], STAR (CHAR_CLASS [(#"0", #"9")])]])),
                  ("Unary", (NORMAL, ALT [(NT "Int"), SEQ [CHAR #"(", (NT "Sum"), CHAR #")"]])),
                  ("Prod", (NORMAL, SEQ [(NT "Unary"), STAR (SEQ [CHAR_CLASS [(#"*", #"*"), (#"/", #"/")], (NT "Unary")])])),
                  ("Sum", (NORMAL, SEQ [(NT "Prod"), STAR (SEQ [CHAR_CLASS [(#"+", #"+"), (#"-", #"-")], (NT "Prod")])])),

                  (* voided expressions *)
                  ("V1", (NORMAL, VOID (CHAR #"a"))),
                  ("V2", (NORMAL, VOID (SEQ [CHAR #"a", CHAR #"b"]))),
                  ("V3", (NORMAL, SEQ [CHAR #"a", VOID (CHAR #"b"), CHAR #"c"])),

                  ("WS", (VOIDING, STAR (ALT[CHAR #" ", CHAR #"\t", CHAR #"\n", CHAR #"\r"]))),

                  ("Nums", (PRUNING, OPT (SEQ [(NT "Int"), STAR (SEQ [(NT "WS"), VOID (CHAR #","), (NT "WS"), (NT "Int")])])))
                  ]

      val tests =
          [(match (env1, "V1", "b"), PARSE_ERROR (0, ["V1"], [])),
           (match (env1, "V2", "a"), PARSE_ERROR (1, ["V2"], [])),
           (match (env1, "V3", "ac"), PARSE_ERROR (1, ["V3"], [])),
           (match (env1, "V3", "a"), PARSE_ERROR (1, ["V3"], [])),
           (match (env1, "V3", "ab"), PARSE_ERROR (2, ["V3"], [])),

           (match (env1, "V1", "a"), AST (AST_TREE ("V1", []))),
           (match (env1, "V2", "ab"), AST (AST_TREE ("V2", []))),
           (match (env1, "V3", "abc"), AST (AST_TREE ("V3", [AST_CHAR #"a", AST_CHAR #"c"]))),

           (match (env1, "WS", "a"), PARSE_ERROR (0, ["WS"], [])),
           (match (env1, "WS", " a"), PARSE_ERROR (1, ["WS"], [])),
           (match (env1, "WS", ""), AST AST_EMPTY),
           (match (env1, "WS", " "), AST AST_EMPTY),
           (match (env1, "WS", " \r\n  \t"), AST AST_EMPTY),

           (match (env1, "Nums", "1 2"), PARSE_ERROR (1, [], [])),
           (match (env1, "Nums", "1,"), PARSE_ERROR (1, [], [])),
           (match (env1, "Nums", "1,2,"), PARSE_ERROR (3, [], [])),
           (match (env1, "Nums", "1 , 2 ,"), PARSE_ERROR (5, [], [])),

           (match (env1, "Nums", ""), AST AST_EMPTY),
           (match (env1, "Nums", "1"), AST (AST_TREE ("Int", [AST_CHAR #"1"]))),
           (match (env1, "Nums", "1, 2"), AST (AST_TREE ("Nums", [AST_TREE ("Int", [AST_CHAR #"1"]), AST_TREE ("Int", [AST_CHAR #"2"])]))),
           (match (env1, "Nums", "1, 2,3"), AST (AST_TREE ("Nums", [AST_TREE ("Int", [AST_CHAR #"1"]), AST_TREE ("Int", [AST_CHAR #"2"]), AST_TREE ("Int", [AST_CHAR #"3"])]))),

           (match (env1, "A", ""), PARSE_ERROR (0, ["A"], [])),
           (match (env1, "A", "b"), PARSE_ERROR (0, ["A"], [])),

           (test_eval (env1, SEQ [NT "A", NT "B"], "b"), PARSE_ERROR (0, ["A", "S"], [])),
           (test_eval (env1, SEQ [OPT (NT "A"), NT "B"], "a"), PARSE_ERROR (1, ["B"], [])),

           (match (env1, "C", "()"), PARSE_ERROR (1, ["A", "C"], [])),
           (match (env1, "C", "(a"), PARSE_ERROR (2, ["C"], [])),
           (match (env1, "C", "a)"), PARSE_ERROR (1, [], [])),
           (match (env1, "C", ")("), PARSE_ERROR (0, ["A", "C"], [])),
           (match (env1, "C", ")a("), PARSE_ERROR (0, ["A", "C"], [])),
           (match (env1, "C", "(a))"), PARSE_ERROR (3, [], [])),
           (match (env1, "C", "((a)"), PARSE_ERROR (4, ["C"], [])),
           (match (env1, "C", "()a)"), PARSE_ERROR (1, ["A", "C"], [])),
           (match (env1, "C", "(a()"), PARSE_ERROR (2, ["C"], [])),
           (match (env1, "C", "(a())"), PARSE_ERROR (2, ["C"], [])),
           (match (env1, "C", "(()a)"), PARSE_ERROR (2, ["A", "C"], [])),
           (match (env1, "C", "(())"), PARSE_ERROR (2, ["A", "C"], [])),
           (match (env1, "Int", "00"), PARSE_ERROR (1, [], [])),
           (match (env1, "Int", "01"), PARSE_ERROR (1, [], [])),
           (match (env1, "Int", "010"), PARSE_ERROR (1, [], [])),
           (match (env1, "Int", "099"), PARSE_ERROR (1, [], [])),
           (match (env1, "Unary", "(1"), PARSE_ERROR (2, ["Int","Prod","Sum","Unary"], [])),
           (match (env1, "Unary", "(1123)("), PARSE_ERROR (6, [], [])),
           (match (env1, "Unary", "(11+-23)"), PARSE_ERROR (4, ["Int","Unary"], [])),
           (match (env1, "Unary", "(1123*)"), PARSE_ERROR (6, ["Int","Unary"], [])),
           (match (env1, "Prod", "*"), PARSE_ERROR (0, ["Int","Prod","Unary"], [])),
           (match (env1, "Prod", "787*"), PARSE_ERROR (3, [], [])),
           (match (env1, "Prod", "312*98*"), PARSE_ERROR (6, [], [])),
           (match (env1, "Prod", "()*98"), PARSE_ERROR (1, ["Int","Unary"], [])),
           (match (env1, "Prod", "(*98)*234"), PARSE_ERROR (1, ["Int","Unary"], [])),
           (match (env1, "Prod", "52//23"), PARSE_ERROR (2, [], [])),
           (match (env1, "Sum", "-"), PARSE_ERROR (0, ["Int","Sum","Unary"], [])),
           (match (env1, "Sum", "787-"), PARSE_ERROR (3, [], [])),
           (match (env1, "Sum", "312+98-"), PARSE_ERROR (6, [], [])),
           (match (env1, "Sum", "()-98"), PARSE_ERROR (1, ["Int","Unary"], [])),
           (match (env1, "Sum", "(+98)-234"), PARSE_ERROR (1, ["Int","Unary"], [])),
           (match (env1, "Sum", "52++23"), PARSE_ERROR (2, [], [])),

           (test_eval (env1, SEQ [NT "A", NT "B"], "ab"), AST AST_EMPTY),
           (test_eval (env1, SEQ [NT "A", NT "B"], "aB"), AST AST_EMPTY),
           (test_eval (env1, SEQ [OPT (NT "A"), NT "B"], "b"), AST AST_EMPTY),
           (test_eval (env1, SEQ [OPT (NT "A"), NT "B"], "ab"), AST AST_EMPTY),
           (test_eval (env1, SEQ [OPT (NT "A"), NT "B"], "B"), AST AST_EMPTY),
           (test_eval (env1, SEQ [OPT (NT "A"), NT "B"], "aB"), AST AST_EMPTY),

           (match (env1, "C", "a"), AST (AST_TREE ("C", [AST_TREE ("A", [AST_CHAR #"a"])]))),
           (match (env1, "A", "a"), AST (AST_TREE ("A", [AST_CHAR #"a"]))),
           (match (env1, "C", "(a)"), AST (AST_TREE ("C", [AST_CHAR #"(", AST_TREE ("C", [AST_TREE ("A", [AST_CHAR #"a"])]), AST_CHAR #")"]))),
           (match (env1, "C", "((a))"), AST (AST_TREE ("C", [AST_CHAR #"(", AST_TREE ("C", [AST_CHAR #"(", AST_TREE ("C", [AST_TREE ("A", [AST_CHAR #"a"])]), AST_CHAR #")"]), AST_CHAR #")"]))),

           (match (env1, "Int", "0"), AST (AST_TREE ("Int", [AST_CHAR #"0"]))),
           (match (env1, "Int", "1"), AST (AST_TREE ("Int", [AST_CHAR #"1"]))),
           (match (env1, "Int", "10"), AST (AST_TREE ("Int", [AST_CHAR #"1", AST_CHAR #"0"]))),
           (match (env1, "Int", "7208"), AST (AST_TREE ("Int", [AST_CHAR #"7", AST_CHAR #"2", AST_CHAR #"0", AST_CHAR #"8"]))),

           (match (env1, "Unary", "98"), AST (AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])]))),
           (match (env1, "Unary", "(23)"), AST (AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3"])])])]), AST_CHAR #")"]))),
           (match (env1, "Unary", "(11+2-3)"), AST (AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"1", AST_CHAR #"1"])])]), AST_CHAR #"+", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2"])])]), AST_CHAR #"-", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"3"])])])]), AST_CHAR #")"]))),
           (match (env1, "Unary", "(1*123)"), AST (AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"1"])]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"1", AST_CHAR #"2", AST_CHAR #"3"])])])]), AST_CHAR #")"]))),

           (match (env1, "Prod", "234"), AST (AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3", AST_CHAR #"4"])])]))),
           (match (env1, "Prod", "787*879"), AST (AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"7", AST_CHAR #"8", AST_CHAR #"7"])]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"8", AST_CHAR #"7", AST_CHAR #"9"])])]))),
           (match (env1, "Prod", "312*98/98"), AST (AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"3", AST_CHAR #"1", AST_CHAR #"2"])]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])]), AST_CHAR #"/", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])])]))),
           (match (env1, "Prod", "(123-72)*98"), AST (AST_TREE ("Prod", [AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"1", AST_CHAR #"2", AST_CHAR #"3"])])]), AST_CHAR #"-", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"7", AST_CHAR #"2"])])])]), AST_CHAR #")"]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])])]))),
           (match (env1, "Prod", "(23*98)*234"), AST (AST_TREE ("Prod", [AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3"])]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])])])]), AST_CHAR #")"]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3", AST_CHAR #"4"])])]))),
           (match (env1, "Prod", "52/523/23"), AST (AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"5", AST_CHAR #"2"])]), AST_CHAR #"/", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"5", AST_CHAR #"2", AST_CHAR #"3"])]), AST_CHAR #"/", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3"])])]))),

           (match (env1, "Sum", "(7)"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"7"])])])]), AST_CHAR #")"])])]))),
           (match (env1, "Sum", "234"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3", AST_CHAR #"4"])])])]))),
           (match (env1, "Sum", "787+879"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"7", AST_CHAR #"8", AST_CHAR #"7"])])]), AST_CHAR #"+", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"8", AST_CHAR #"7", AST_CHAR #"9"])])])]))),
           (match (env1, "Sum", "312-98*98"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"3", AST_CHAR #"1", AST_CHAR #"2"])])]), AST_CHAR #"-", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])])])]))),
           (match (env1, "Sum", "(123-72)*98"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"1", AST_CHAR #"2", AST_CHAR #"3"])])]), AST_CHAR #"-", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"7", AST_CHAR #"2"])])])]), AST_CHAR #")"]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])])])]))),
           (match (env1, "Sum", "(23*98)+234"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3"])]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])])])]), AST_CHAR #")"])]), AST_CHAR #"+", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3", AST_CHAR #"4"])])])]))),
           (match (env1, "Sum", "(23+98)*234"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_CHAR #"(", AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3"])])]), AST_CHAR #"+", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"9", AST_CHAR #"8"])])])]), AST_CHAR #")"]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3", AST_CHAR #"4"])])])]))),
           (match (env1, "Sum", "52-523/23"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"5", AST_CHAR #"2"])])]), AST_CHAR #"-", AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"5", AST_CHAR #"2", AST_CHAR #"3"])]), AST_CHAR #"/", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3"])])])]))),
           (match (env1, "Sum", "52*523/23"), AST (AST_TREE ("Sum", [AST_TREE ("Prod", [AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"5", AST_CHAR #"2"])]), AST_CHAR #"*", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"5", AST_CHAR #"2", AST_CHAR #"3"])]), AST_CHAR #"/", AST_TREE ("Unary", [AST_TREE ("Int", [AST_CHAR #"2", AST_CHAR #"3"])])])]))),

           (test_eval (env1, ANY, ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, ANY, "aa"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, ANY, "ba"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, ANY, "a"), AST AST_EMPTY),
           (test_eval (env1, ANY, "b"), AST AST_EMPTY),

           (test_eval (env1, CHAR #"a", ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR #"a", "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR #"a", "ba"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR #"a", "aa"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, CHAR #"a", "ab"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, CHAR #"b", "a"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR #"a", "a"), AST AST_EMPTY),
           (test_eval (env1, CHAR #"b", "b"), AST AST_EMPTY),

           (test_eval (env1, CHAR_CLASS [], ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR_CLASS [(#"a", #"a")], ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR_CLASS [(#"a", #"z")], ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR_CLASS [(#"a", #"z")], "A"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR_CLASS [(#"a", #"c")], "d"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, CHAR_CLASS [(#"a", #"c")], "_"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, PLUS (CHAR_CLASS [(#"a", #"c"), (#"_", #"_")]), ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, PLUS (CHAR_CLASS [(#"a", #"c"), (#"_", #"_")]), "ab$c"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, CHAR_CLASS [(#"a", #"z")], "a"), AST AST_EMPTY),
           (test_eval (env1, CHAR_CLASS [(#"a", #"z")], "q"), AST AST_EMPTY),
           (test_eval (env1, CHAR_CLASS [(#"a", #"z")], "z"), AST AST_EMPTY),
           (test_eval (env1, CHAR_CLASS [(#"a", #"c"), (#"_", #"_")], "_"), AST AST_EMPTY),
           (test_eval (env1, PLUS (CHAR_CLASS [(#"a", #"c"), (#"_", #"_")]), "abc"), AST AST_EMPTY),
           (test_eval (env1, PLUS (CHAR_CLASS [(#"a", #"c"), (#"_", #"_")]), "ab___"), AST AST_EMPTY),
           (test_eval (env1, PLUS (CHAR_CLASS [(#"a", #"c"), (#"_", #"_")]), "_b__"), AST AST_EMPTY),

           (test_eval (env1, SEQ [CHAR #"a"], ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"a"], ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a"], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a"], "aa"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"a"], "a"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", CHAR #"a"], "b"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"b"], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"a"], "ab"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"b", CHAR #"c"], "ab"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"b", CHAR #"c"], "aba"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", CHAR #"a"], "ab"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"a", CHAR #"b", CHAR #"c"], "aabd"), PARSE_ERROR (3, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"c", CHAR #"b", CHAR #"c"], "aabd"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"a", CHAR #"b", CHAR #"c"], "aabcd"), PARSE_ERROR (4, [], [])),
           (test_eval (env1, SEQ [CHAR #"a"], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"a"], "aa"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"b", CHAR #"c"], "abc"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"b", CHAR #"c"], "abc"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"a", CHAR #"b", CHAR #"c"], "aabc"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"b", CHAR #"c", CHAR #"d"], "abcd"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"a", CHAR #"b", CHAR #"c", CHAR #"d", CHAR #"a", CHAR #"b"], "abcdab"), AST AST_EMPTY),

           (test_eval (env1, ALT [CHAR #"a"], ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, ALT [CHAR #"a"], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, ALT [CHAR #"a"], "aa"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, ALT [CHAR #"a"], "ab"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, ALT [SEQ [CHAR #"a", CHAR #"a"], SEQ [CHAR #"b", CHAR #"b"]], "a"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, ALT [SEQ [CHAR #"a", CHAR #"a"], SEQ [CHAR #"b", CHAR #"b"]], "ab"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, ALT [SEQ [CHAR #"a", CHAR #"a"], SEQ [CHAR #"b", CHAR #"b"]], "ba"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, ALT [CHAR #"a", SEQ [CHAR #"b", CHAR #"b"]], "b"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, ALT [SEQ [CHAR #"a"]], "a"), AST AST_EMPTY),
           (test_eval (env1, ALT [CHAR #"a"], "a"), AST AST_EMPTY),
           (test_eval (env1, ALT [SEQ [CHAR #"a", CHAR #"a"], SEQ [CHAR #"b", CHAR #"b"]], "aa"), AST AST_EMPTY),
           (test_eval (env1, ALT [CHAR #"a", SEQ [CHAR #"b", CHAR #"b"]], "a"), AST AST_EMPTY),
           (test_eval (env1, ALT [CHAR #"a", SEQ [CHAR #"b", CHAR #"b"]], "bb"), AST AST_EMPTY),
           (test_eval (env1, ALT [SEQ [CHAR #"a", CHAR #"a"], SEQ [CHAR #"b", CHAR #"b"]], "bb"), AST AST_EMPTY),

           (test_eval (env1, SEQ [AND (CHAR #"a"), CHAR #"b"], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [AND (CHAR #"a"), CHAR #"a"], "aa"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, SEQ [AND (ALT [CHAR #"a", CHAR #"b"]), CHAR #"c"], "c"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, AND (SEQ [CHAR #"a", ALT [CHAR #"b", CHAR #"c"]]), "ad"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, AND (SEQ [CHAR #"a", CHAR #"b"]), "abc"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, AND (SEQ [CHAR #"a", CHAR #"b", NOT (CHAR #"d")]), "abd"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, AND (SEQ [CHAR #"a", CHAR #"b", AND (CHAR #"c"), CHAR #"c"]), "abd"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [AND (ALT [CHAR #"a", CHAR #"A"]), ANY, CHAR #"b", CHAR #"c"], "zbc"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, AND ANY, ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [AND (ALT [CHAR #"a", CHAR #"A"]), ANY, CHAR #"b", CHAR #"c"], "abc"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (ALT [CHAR #"a", CHAR #"A"]), ANY, CHAR #"b", CHAR #"c"], "Abc"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (CHAR #"a"), CHAR #"a"], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (CHAR #"b"), CHAR #"b"], "b"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (ALT [CHAR #"a", CHAR #"b"]), CHAR #"a"], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (ALT [CHAR #"a", CHAR #"b"]), CHAR #"b"], "b"), AST AST_EMPTY),

           (test_eval (env1, SEQ [AND (SEQ [CHAR #"a", CHAR #"b"]), CHAR #"a", CHAR #"b"], "ab"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (SEQ [CHAR #"a", ALT [CHAR #"b", CHAR #"c"]]), CHAR #"a", CHAR #"b"], "ab"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (SEQ [CHAR #"a", ALT [CHAR #"b", CHAR #"c"]]), CHAR #"a", CHAR #"c"], "ac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND ANY, CHAR #"z"], "z"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (AND (CHAR #"a")), CHAR #"a"], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (AND (CHAR #"a")), CHAR #"a"], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [AND (AND (CHAR #"b")), CHAR #"a"], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [AND (AND (CHAR #"a")), CHAR #"b"], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [AND (NOT (CHAR #"a")), CHAR #"a"], "a"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [AND (NOT (CHAR #"b")), CHAR #"a"], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (NOT (CHAR #"a")), CHAR #"b"], "b"), AST AST_EMPTY),

           (test_eval (env1, NOT (CHAR #"a"), "a"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, NOT (CHAR #"b"), "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, NOT (ALT [CHAR #"a", CHAR #"b"]), "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, NOT (ALT [CHAR #"a", CHAR #"b"]), "a"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, NOT (SEQ [CHAR #"a", CHAR #"b"]), "ab"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, NOT (SEQ [CHAR #"a", ALT [CHAR #"b", CHAR #"c"]]), "ab"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, NOT (SEQ [CHAR #"a", ALT [CHAR #"b", CHAR #"c"]]), "ac"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [NOT ANY, CHAR #"a"], "a"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [NOT (SEQ [NOT (CHAR #"a"), CHAR #"b"]), CHAR #"a"], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [NOT (SEQ [NOT (CHAR #"a"), CHAR #"b"]), CHAR #"b"], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [NOT (SEQ [NOT (CHAR #"a"), CHAR #"b"]), CHAR #"c"], "c"), AST AST_EMPTY),
           (test_eval (env1, SEQ [NOT (SEQ [AND (CHAR #"a"), CHAR #"a"]), CHAR #"a"], "a"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [NOT (SEQ [AND (CHAR #"a"), CHAR #"a"]), CHAR #"b"], "b"), AST AST_EMPTY),
           (test_eval (env1, SEQ [NOT (SEQ [AND (CHAR #"a"), CHAR #"a"]), CHAR #"c"], "c"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (SEQ [NOT (CHAR #"a"), CHAR #"b"]), CHAR #"b"], "a"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [AND (SEQ [NOT (CHAR #"a"), CHAR #"b"]), CHAR #"b"], "b"), AST AST_EMPTY),
           (test_eval (env1, SEQ [AND (SEQ [NOT (CHAR #"a"), CHAR #"b"]), CHAR #"b"], "c"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [NOT (CHAR #"a"), CHAR #"b"], "b"), AST AST_EMPTY),
           (test_eval (env1, SEQ [NOT (CHAR #"b"), CHAR #"a", CHAR #"a"], "aa"), AST AST_EMPTY),
           (test_eval (env1, SEQ [NOT (ALT [CHAR #"a", CHAR #"b"]), CHAR #"c"], "c"), AST AST_EMPTY),
           (test_eval (env1, SEQ [NOT (SEQ [CHAR #"a", ALT [CHAR #"b", CHAR #"c"]]), CHAR #"a", CHAR #"d"], "ad"), AST AST_EMPTY),
           (test_eval (env1, NOT ANY, ""), AST AST_EMPTY),

           (test_eval (env1, SEQ [OPT (CHAR #"a")], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [OPT (CHAR #"a"), CHAR #"c"], "bc"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [OPT (CHAR #"a"), CHAR #"c"], "cc"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "c"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "b"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "ac"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "ba"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, SEQ [OPT (CHAR #"a"), CHAR #"c"], "abc"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [OPT (CHAR #"a"), CHAR #"c"], "bac"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "baac"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "baaac"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "baaaac"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, SEQ [OPT (CHAR #"a")], ""), AST AST_EMPTY),
           (test_eval (env1, SEQ [OPT (CHAR #"a")], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [OPT (CHAR #"a"), CHAR #"c"], "c"), AST AST_EMPTY),
           (test_eval (env1, SEQ [OPT (CHAR #"a"), CHAR #"c"], "ac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "bc"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", OPT (CHAR #"a"), CHAR #"c"], "bac"), AST AST_EMPTY),

           (test_eval (env1, SEQ [STAR (CHAR #"a")], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [STAR (CHAR #"a"), CHAR #"c"], "bc"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [STAR (CHAR #"a"), CHAR #"c"], "cc"), PARSE_ERROR (1, [], [])),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "c"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "b"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "ac"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "ba"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, SEQ [STAR (CHAR #"a"), CHAR #"c"], "abc"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [STAR (CHAR #"a"), CHAR #"c"], "bac"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [STAR (CHAR #"a")], ""), AST AST_EMPTY),
           (test_eval (env1, SEQ [STAR (CHAR #"a")], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [STAR (CHAR #"a")], "aa"), AST AST_EMPTY),
           (test_eval (env1, SEQ [STAR (CHAR #"a")], "aaa"), AST AST_EMPTY),
           (test_eval (env1, SEQ [STAR (CHAR #"a"), CHAR #"c"], "c"), AST AST_EMPTY),
           (test_eval (env1, SEQ [STAR (CHAR #"a"), CHAR #"c"], "ac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [STAR (CHAR #"a"), CHAR #"c"], "aac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [STAR (CHAR #"a"), CHAR #"c"], "aaac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "bc"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "bac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "baac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "baaac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", STAR (CHAR #"a"), CHAR #"c"], "baaaac"), AST AST_EMPTY),

           (test_eval (env1, SEQ [PLUS (CHAR #"a")], "b"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [PLUS (CHAR #"a"), CHAR #"c"], "bc"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [PLUS (CHAR #"a"), CHAR #"c"], "cc"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "c"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "b"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "ac"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "ba"), PARSE_ERROR (2, ["S"], [])),
           (test_eval (env1, SEQ [PLUS (CHAR #"a"), CHAR #"c"], "abc"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [PLUS (CHAR #"a"), CHAR #"c"], "bac"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [PLUS (CHAR #"a")], ""), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [PLUS (CHAR #"a"), CHAR #"c"], "c"), PARSE_ERROR (0, ["S"], [])),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "bc"), PARSE_ERROR (1, ["S"], [])),
           (test_eval (env1, SEQ [PLUS (CHAR #"a")], "a"), AST AST_EMPTY),
           (test_eval (env1, SEQ [PLUS (CHAR #"a")], "aa"), AST AST_EMPTY),
           (test_eval (env1, SEQ [PLUS (CHAR #"a")], "aaa"), AST AST_EMPTY),
           (test_eval (env1, SEQ [PLUS (CHAR #"a"), CHAR #"c"], "ac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [PLUS (CHAR #"a"), CHAR #"c"], "aac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [PLUS (CHAR #"a"), CHAR #"c"], "aaac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "bac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "baac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "baaac"), AST AST_EMPTY),
           (test_eval (env1, SEQ [CHAR #"b", PLUS (CHAR #"a"), CHAR #"c"], "baaaac"), AST AST_EMPTY)
           ]

      val results = run_tests tests
    end
  end


(*  uniq : 'a list -> 'a list  *)
val uniq = ListMergeSort.uniqueSort String.compare


(*  update_error : error * int -> error  *)
fun update_error ((err_pos, nts, ccs, nt), pos)
    = if pos > err_pos
      then (pos, [nt], ccs, nt)
      else if pos = err_pos
           then (err_pos, nt :: nts, ccs, nt)
           else (err_pos, nts, ccs, nt)


(* matcher *)
(* direct-style *)
structure Match0 : SIG
= struct
    local
      open Syn
      open Value
    in
      (*  match : env * string * string -> result  *)
      fun match (env, start_name, input)
          = let
              val input_len = String.size input

              val (start_nt, start_expression) = Env.lookup (start_name, env)

              (*  eof : int -> bool  *)
              fun eof pos = pos >= input_len

              (*  ch : int -> char  *)
              fun ch pos = String.sub (input, pos)

              (*  eval : exp * int * ast list * error -> value  *)
              fun eval (ANY, pos, asts, err)
                  = if eof pos
                    then FAIL (update_error (err, pos))
                    else VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)
                | eval (CHAR c, pos, asts, err)
                  = if eof pos orelse c <> ch pos
                    then FAIL (update_error (err, pos))
                    else VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)
                | eval (CHAR_CLASS cc, pos, asts, err)
                  = let fun visit ([])
                            = FAIL (update_error (err, pos))
                          | visit ((c1, c2) :: cs)
                            = if c1 <= ch pos andalso c2 >= ch pos
                              then VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)
                              else visit cs
                    in if eof pos
                       then FAIL (update_error (err, pos))
                       else visit cc
                    end
                (* a sequence is made up of a list of expressions *)
                (* we traverse the list, making sure each expression succeeds *)
                (* the rest of the string return by the expression is used *)
                (* as input to the next expression *)
                | eval (SEQ es, pos, asts, err)
                  = let fun visit ([], pos, asts, err)
                            = VAL (pos, asts, err)
                          | visit (q :: qs, pos, asts, err)
                            = (case eval (q, pos, asts, err)
                                 of (VAL (rest, ts, err))
                                    => visit (qs, rest, ts, err)
                                  | FAIL err
                                    => FAIL err)
                    in visit (es, pos, asts, err)
                    end
                (* an alt is made up of a list of expressions *)
                (* we traverse the list until one succeeds *)
                (* if none succeed, the whole expression fails *)
                | eval (ALT es, pos, asts, err)
                  = let fun visit ([], asts, err)
                            = FAIL err
                          | visit (q :: qs, asts, err)
                            = (case eval (q, pos, asts, err)
                                 of (VAL (rest, ts, err))
                                    => VAL (rest, ts, err)
                                  | FAIL err
                                    => visit (qs, asts, err))
                    in visit (es, asts, err)
                    end
                | eval (AND e, pos, asts, err)
                  = (case eval (e, pos, [], err)
                       of (VAL _)
                          => VAL (pos, asts, err)
                        | FAIL _
                          => FAIL err)
                | eval (NOT e, pos, asts, err)
                  = (case eval (e, pos, [], err)
                       of (VAL _)
                          => FAIL err
                        | FAIL err
                          => VAL (pos, asts, err))
                | eval (OPT e, pos, asts, err)
                  = (case eval (e, pos, asts, err)
                       of (VAL (rest, ts, err))
                          => VAL (rest, ts, err)
                        | FAIL err
                          => VAL (pos, asts, err))
                | eval (STAR e, pos, asts, err)
                  = let fun visit (e, pos, asts, err)
                            = (case eval (e, pos, asts, err)
                                 of (VAL (rest, ts, err))
                                    => visit (e, rest, ts, err)
                                  | FAIL err
                                    => VAL (pos, asts, err))
                    in visit (e, pos, asts, err)
                    end
                | eval (PLUS e, pos, asts, err)
                  = let fun visit (e, pos, asts, err)
                            = (case eval (e, pos, asts, err)
                                 of (VAL (rest, ts, err))
                                    => visit (e, rest, ts, err)
                                  | FAIL err
                                    => VAL (pos, asts, err))
                    in (case eval (e, pos, asts, err)
                          of (VAL (rest, ts, err))
                             => visit (e, rest, ts, err)
                           | FAIL err
                             => FAIL err)
                    end
                | eval (VOID e, pos, asts, err)
                  = (case eval (e, pos, [], err)
                       of (VAL (rest, ts, err))
                          => VAL (rest, asts, err)
                        | FAIL err
                          => FAIL err)
                | eval (NT name, pos, asts, (err_pos, nts, ccs, nt))
                  = let val (mode, e) = Env.lookup (name, env)
                    in (case eval (e, pos, [], (err_pos, nts, ccs, name))
                          of (VAL (rest, ts, (err_pos, nts, ccs, _)))
                             => (case mode
                                   of NORMAL
                                      => VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt))
                                    | PRUNING
                                      => (case List.length ts
                                            of 0
                                               => VAL (rest, asts, (err_pos, nts, ccs, nt))
                                             | 1
                                               => VAL (rest, List.hd ts :: asts, (err_pos, nts, ccs, nt))
                                             | _
                                               => VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt)))
                                    | VOIDING
                                      => VAL (rest, asts, (err_pos, nts, ccs, nt)))
                           | FAIL (err_pos, nts, ccs, _)
                             => FAIL (err_pos, nts, ccs, nt))
                    end
            in (case eval (start_expression, 0, [], (0, [start_name], [], start_name))
                  of (VAL (rest, ts, (err_pos, nts, ccs, _)))
                     => if eof rest
                        then AST (case start_nt
                                    of NORMAL
                                       => AST_TREE (start_name, List.rev ts)
                                     | PRUNING
                                       => (case List.length ts
                                             of 0
                                                => AST_EMPTY
                                              | 1
                                                => List.hd ts
                                              | _
                                                => AST_TREE (start_name, List.rev ts))
                                     | VOIDING
                                       => AST_EMPTY)
                        else if rest = err_pos
                             then PARSE_ERROR (rest, uniq nts, ccs)
                             else PARSE_ERROR (rest, [], [])
                   | FAIL (err_pos, nts, ccs, _)
                     => PARSE_ERROR (err_pos, uniq nts, ccs))
            end

    (* ********** *)
    end
  end;


(* matcher *)
(* continuation-passing-style *)
structure Match1 : SIG
= struct
    local
      open Syn
      open Value
    in
      (*  match : env * string * string -> result  *)
      fun match (env, start_name, input)
          = let
              val input_len = String.size input

              val (start_nt, start_expression) = Env.lookup (start_name, env)

              (*  eof : int -> bool  *)
              fun eof pos = pos >= input_len

              (*  ch : int -> char  *)
              fun ch pos = String.sub (input, pos)

              (*  eval : exp * int * ast list * error * (value -> result) -> result  *)
              fun eval (ANY, pos, asts, err, k)
                  = if eof pos
                    then k (FAIL (update_error (err, pos)))
                    else k (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err))
                | eval (CHAR c, pos, asts, err, k)
                  = if eof pos orelse c <> ch pos
                    then k (FAIL (update_error (err, pos)))
                    else k (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err))
                | eval (CHAR_CLASS cc, pos, asts, err, k)
                  = let fun visit ([])
                            = k (FAIL (update_error (err, pos)))
                          | visit ((c1, c2) :: cs)
                            = if c1 <= ch pos andalso c2 >= ch pos
                              then k (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err))
                              else visit cs
                    in if eof pos
                       then k (FAIL (update_error (err, pos)))
                       else visit cc
                    end
                (* a sequence is made up of a list of expressions *)
                (* we traverse the list, making sure each expression succeeds *)
                (* the rest of the string return by the expression is used *)
                (* as input to the next expression *)
                | eval (SEQ es, pos, asts, err, k)
                  = let fun visit ([], pos, asts, err)
                            = k (VAL (pos, asts, err))
                          | visit (q :: qs, pos, asts, err)
                            = eval (q, pos, asts, err, fn (VAL (rest, ts, err))
                                                          => visit (qs, rest, ts, err)
                                                        | FAIL err
                                                          => k (FAIL err))
                    in visit (es, pos, asts, err)
                    end
                (* an alt is made up of a list of expressions *)
                (* we traverse the list until one succeeds *)
                (* if none succeed, the whole expression fails *)
                | eval (ALT es, pos, asts, err, k)
                  = let fun visit ([], asts, err)
                            = k (FAIL err)
                          | visit (q :: qs, asts, err)
                            = eval (q, pos, asts, err, fn (VAL (rest, ts, err))
                                                          => k (VAL (rest, ts, err))
                                                        | FAIL err
                                                          => visit (qs, asts, err))
                    in visit (es, asts, err)
                    end
                | eval (AND e, pos, asts, err, k)
                  = eval (e, pos, [], err, fn (VAL _)
                                              => k (VAL (pos, asts, err))
                                            | FAIL _
                                              => k (FAIL err))
                | eval (NOT e, pos, asts, err, k)
                  = eval (e, pos, [], err, fn (VAL _)
                                              => k (FAIL err)
                                            | FAIL err
                                              => k (VAL (pos, asts, err)))
                | eval (OPT e, pos, asts, err, k)
                  = eval (e, pos, asts, err, fn (VAL (rest, ts, err))
                                                => k (VAL (rest, ts, err))
                                              | FAIL err
                                                => k (VAL (pos, asts, err)))
                | eval (STAR e, pos, asts, err, k)
                  = let fun visit (e, pos, asts, err)
                            = eval (e, pos, asts, err, fn (VAL (rest, ts, err))
                                                          => visit (e, rest, ts, err)
                                                        | FAIL err
                                                          => k (VAL (pos, asts, err)))
                    in visit (e, pos, asts, err)
                    end
                | eval (PLUS e, pos, asts, err, k)
                  = let fun visit (e, pos, asts, err)
                            = eval (e, pos, asts, err, fn (VAL (rest, ts, err))
                                                          => visit (e, rest, ts, err)
                                                        | FAIL err
                                                          => k (VAL (pos, asts, err)))
                    in eval (e, pos, asts, err, fn (VAL (rest, ts, err))
                                                   => visit (e, rest, ts, err)
                                                 | FAIL err
                                                   => k (FAIL err))
                    end
                | eval (VOID e, pos, asts, err, k)
                  = eval (e, pos, [], err, fn (VAL (rest, ts, err))
                                              => k (VAL (rest, asts, err))
                                            | FAIL err
                                              => k (FAIL err))
                | eval (NT name, pos, asts, (err_pos, nts, ccs, nt), k)
                  = let val (mode, e) = Env.lookup (name, env)
                    in eval (e, pos, [], (err_pos, nts, ccs, name),
                             fn (VAL (rest, ts, (err_pos, nts, ccs, _)))
                                => (case mode
                                      of NORMAL
                                         => k (VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt)))
                                       | PRUNING
                                         => (case List.length ts
                                               of 0
                                                  => k (VAL (rest, asts, (err_pos, nts, ccs, nt)))
                                                | 1
                                                  => k (VAL (rest, List.hd ts :: asts, (err_pos, nts, ccs, nt)))
                                                | _
                                                  => k (VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt))))
                                       | VOIDING
                                         => k (VAL (rest, asts, (err_pos, nts, ccs, nt))))
                              | (FAIL (err_pos, nts, ccs, _))
                                => k (FAIL (err_pos, nts, ccs, nt)))
                    end
            in eval (start_expression, 0, [], (0, [start_name], [], start_name),
                     fn (VAL (rest, ts, (err_pos, nts, ccs, _)))
                        => if eof rest
                           then AST (case start_nt
                                       of NORMAL
                                          => AST_TREE (start_name, List.rev ts)
                                        | PRUNING
                                          => (case List.length ts
                                                of 0
                                                   => AST_EMPTY
                                                 | 1
                                                   => List.hd ts
                                                 | _
                                                   => AST_TREE (start_name, List.rev ts))
                                        | VOIDING
                                          => AST_EMPTY)
                           else if rest = err_pos
                                then PARSE_ERROR (rest, uniq nts, ccs)
                                else PARSE_ERROR (rest, [], [])
                      | FAIL (err_pos, nts, ccs, _)
                        => PARSE_ERROR (err_pos, uniq nts, ccs))
            end

    (* ********** *)
    end
  end;


(* matcher *)
(* defunctionalized continuations *)
(* removed visit functions *)
structure Match2 : SIG
= struct
    local
      open Syn
      open Value
    in
      (*  match : env * string * string -> result  *)
      fun match (env, start_name, input)
          = let
              val input_len = String.size input

              val (start_nt, start_expression) = Env.lookup (start_name, env)

              (*  eof : int -> bool  *)
              fun eof pos = pos >= input_len

              (*  ch : int -> char  *)
              fun ch pos = String.sub (input, pos)

              (* continuations *)
              datatype cont = CONT_SEQ of exp list * cont
                            | CONT_ALT of exp list * int * ast list * cont
                            | CONT_AND of int * ast list * error * cont
                            | CONT_NOT of int * ast list * error * cont
                            | CONT_OPT of int * ast list * cont
                            | CONT_STAR of exp * int * ast list * cont
                            | CONT_PLUS of exp * cont
                            | CONT_VOID of ast list * cont
                            | CONT_NT of mode * string * ast list * string * cont
                            | CONT_INIT

              (* apply : cont * value -> result *)
              fun apply (CONT_SEQ (es, k), VAL (rest, ts, err))
                  = if null es
                    then apply (k, (VAL (rest, ts, err)))
                    else eval (hd es, rest, ts, err, CONT_SEQ (tl es, k))
                | apply (CONT_SEQ (_, k), FAIL err)
                  = apply (k, FAIL err)
                | apply (CONT_ALT (_, _, _, k), VAL (rest, ts, err))
                  = apply (k, (VAL (rest, ts, err)))
                | apply (CONT_ALT (es, pos, asts, k), FAIL err)
                  = if null es
                    then apply (k, FAIL err)
                    else eval (hd es, pos, asts, err, CONT_ALT (tl es, pos, asts, k))
                | apply (CONT_AND (pos, asts, err, k), VAL _)
                  = apply (k, (VAL (pos, asts, err)))
                | apply (CONT_AND (_, _, err, k), FAIL _)
                  = apply (k, FAIL err)
                | apply (CONT_NOT (_, _, err, k), VAL _)
                  = apply (k, FAIL err)
                | apply (CONT_NOT (pos, asts, _, k), FAIL err)
                  = apply (k, (VAL (pos, asts, err)))
                | apply (CONT_OPT (_, _, k), VAL (rest, ts, err))
                  = apply (k, (VAL (rest, ts, err)))
                | apply (CONT_OPT (pos, asts, k), FAIL err)
                  = apply (k, (VAL (pos, asts, err)))
                | apply (CONT_STAR (e, _, _, k), VAL (rest, ts, err))
                  = eval (e, rest, ts, err, CONT_STAR (e, rest, ts, k))
                | apply (CONT_STAR (_, pos, asts, k), FAIL err)
                  = apply (k, (VAL (pos, asts, err)))
                | apply (CONT_PLUS (e, k), VAL (rest, ts, err))
                  (* the second continuation used to evaluate PLUS *)
                  (* is the same as the continuation for STAR *)
                  = eval (e, rest, ts, err, CONT_STAR (e, rest, ts, k))
                | apply (CONT_PLUS (_, k), FAIL err)
                  = apply (k, FAIL err)
                | apply (CONT_VOID (asts, k), VAL (rest, _, err))
                  = apply (k, (VAL (rest, asts, err)))
                | apply (CONT_VOID (_, k), FAIL err)
                  = apply (k, FAIL err)
                | apply (CONT_NT (mode, name, asts, nt, k), VAL (rest, ts, (err_pos, nts, ccs, _)))
                  = (case mode
                       of NORMAL
                          => apply (k, (VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt))))
                        | PRUNING
                          => (case List.length ts
                                of 0
                                   => apply (k, (VAL (rest, asts, (err_pos, nts, ccs, nt))))
                                 | 1
                                   => apply (k, (VAL (rest, List.hd ts :: asts, (err_pos, nts, ccs, nt))))
                                 | _
                                   => apply (k, (VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt)))))
                        | VOIDING
                          => apply (k, (VAL (rest, asts, (err_pos, nts, ccs, nt)))))
                | apply (CONT_NT (_, _, _, nt, k), FAIL (err_pos, nts, ccs, _))
                  = apply (k, FAIL (err_pos, nts, ccs, nt))
                | apply (CONT_INIT, VAL (rest, ts, (err_pos, nts, ccs, _)))
                  = if eof rest
                    then AST (case start_nt
                                of NORMAL
                                   => AST_TREE (start_name, List.rev ts)
                                 | PRUNING
                                   => (case List.length ts
                                         of 0
                                            => AST_EMPTY
                                          | 1
                                            => List.hd ts
                                          | _
                                            => AST_TREE (start_name, List.rev ts))
                                 | VOIDING
                                   => AST_EMPTY)
                    else if rest = err_pos
                         then PARSE_ERROR (rest, uniq nts, ccs)
                         else PARSE_ERROR (rest, [], [])
                | apply (CONT_INIT, FAIL (err_pos, nts, ccs, _))
                  = PARSE_ERROR (err_pos, uniq nts, ccs)

              (*  eval : exp * int * ast list * error * cont -> result  *)
              and eval (ANY, pos, asts, err, k)
                  = if eof pos
                    then apply (k, FAIL (update_error (err, pos)))
                    else apply (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)))
                | eval (CHAR c, pos, asts, err, k)
                  = if eof pos orelse c <> ch pos
                    then apply (k, FAIL (update_error (err, pos)))
                    else apply (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)))
                | eval (CHAR_CLASS cc, pos, asts, err, k)
                  = let fun visit ([])
                            = apply (k, FAIL (update_error (err, pos)))
                          | visit ((c1, c2) :: cs)
                            = if c1 <= ch pos andalso c2 >= ch pos
                              then apply (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)))
                              else visit cs
                    in if eof pos
                       then apply (k, FAIL (update_error (err, pos)))
                       else visit cc
                    end
                (* a sequence is made up of a list of expressions *)
                (* we traverse the list, making sure each expression succeeds *)
                (* the rest of the string return by the expression is used *)
                (* as input to the next expression *)
                | eval (SEQ es, pos, asts, err, k)
                  = if null es
                    then apply (k, (VAL (pos, asts, err)))
                    else eval (hd es, pos, asts, err, CONT_SEQ (tl es, k))
                (* an alt is made up of a list of expressions *)
                (* we traverse the list until one succeeds *)
                (* if none succeed, the whole expression fails *)
                | eval (ALT es, pos, asts, err, k)
                  = if null es
                    then apply (k, FAIL err)
                    else eval (hd es, pos, asts, err, CONT_ALT (tl es, pos, asts, k))
                | eval (AND e, pos, asts, err, k)
                  = eval (e, pos, [], err, CONT_AND (pos, asts, err, k))
                | eval (NOT e, pos, asts, err, k)
                  = eval (e, pos, [], err, CONT_NOT (pos, asts, err, k))
                | eval (OPT e, pos, asts, err, k)
                  = eval (e, pos, asts, err, CONT_OPT (pos, asts, k))
                | eval (STAR e, pos, asts, err, k)
                  = eval (e, pos, asts, err, CONT_STAR (e, pos, asts, k))
                | eval (PLUS e, pos, asts, err, k)
                  = eval (e, pos, asts, err, CONT_PLUS (e, k))
                | eval (VOID e, pos, asts, err, k)
                  = eval (e, pos, [], err, CONT_VOID (asts, k))
                | eval (NT name, pos, asts, (err_pos, nts, ccs, nt), k)
                  = let val (mode, e) = Env.lookup (name, env)
                    in eval (e, pos, [], (err_pos, nts, ccs, name), CONT_NT (mode, name, asts, nt, k))
                    end
            in eval (start_expression, 0, [], (0, [start_name], [], start_name), CONT_INIT)
            end

    (* ********** *)
    end
  end;


(* matcher *)
(* continuation as control stack *)
structure Match3 : SIG
= struct
    local
      open Syn
      open Value
    in
      (*  match : env * string * string -> result  *)
      fun match (env, start_name, input)
          = let
              val input_len = String.size input

              val (start_nt, start_expression) = Env.lookup (start_name, env)

              (*  eof : int -> bool  *)
              fun eof pos = pos >= input_len

              (*  ch : int -> char  *)
              fun ch pos = String.sub (input, pos)

              (* stack frames *)
              datatype cont = CONT_SEQ of exp list
                            | CONT_ALT of exp list * int * ast list
                            | CONT_AND of int * ast list * error
                            | CONT_NOT of int * ast list * error
                            | CONT_OPT of int * ast list
                            | CONT_STAR of exp * int * ast list
                            | CONT_PLUS of exp
                            | CONT_VOID of ast list
                            | CONT_NT of mode * string * ast list * string

              (* apply : cont list * value -> result *)
              fun apply (CONT_SEQ es :: k, VAL (rest, ts, err))
                  = if null es
                    then apply (k, (VAL (rest, ts, err)))
                    else eval (hd es, rest, ts, err, CONT_SEQ (tl es) :: k)
                | apply (CONT_SEQ _ :: k, FAIL err)
                  = apply (k, FAIL err)
                | apply (CONT_ALT _ :: k, VAL (rest, ts, err))
                  = apply (k, (VAL (rest, ts, err)))
                | apply (CONT_ALT (es, pos, asts) :: k, FAIL err)
                  = if null es
                    then apply (k, FAIL err)
                    else eval (hd es, pos, asts, err, CONT_ALT (tl es, pos, asts) :: k)
                | apply (CONT_AND (pos, asts, err) :: k, VAL _)
                  = apply (k, (VAL (pos, asts, err)))
                | apply (CONT_AND (_, _, err) :: k, FAIL _)
                  = apply (k, FAIL err)
                | apply (CONT_NOT (_, _, err) :: k, VAL _)
                  = apply (k, FAIL err)
                | apply (CONT_NOT (pos, asts, _) :: k, FAIL err)
                  = apply (k, (VAL (pos, asts, err)))
                | apply (CONT_OPT _ :: k, VAL (rest, ts, err))
                  = apply (k, (VAL (rest, ts, err)))
                | apply (CONT_OPT (pos, asts) :: k, FAIL err)
                  = apply (k, (VAL (pos, asts, err)))
                | apply (CONT_STAR (e, _, _) :: k, VAL (rest, ts, err))
                  = eval (e, rest, ts, err, CONT_STAR (e, rest, ts) :: k)
                | apply (CONT_STAR (_, pos, asts) :: k, FAIL err)
                  = apply (k, (VAL (pos, asts, err)))
                | apply (CONT_PLUS e :: k, VAL (rest, ts, err))
                  (* the second continuation used to evaluate PLUS *)
                  (* is the same as the continuation for STAR *)
                  = eval (e, rest, ts, err, CONT_STAR (e, rest, ts) :: k)
                | apply (CONT_PLUS _ :: k, FAIL err)
                  = apply (k, FAIL err)
                | apply (CONT_VOID asts :: k, VAL (rest, _, err))
                  = apply (k, (VAL (rest, asts, err)))
                | apply (CONT_VOID _ :: k, FAIL err)
                  = apply (k, FAIL err)
                | apply (CONT_NT (mode, name, asts, nt) :: k, VAL (rest, ts, (err_pos, nts, ccs, _)))
                  = (case mode
                       of NORMAL
                          => apply (k, (VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt))))
                        | PRUNING
                          => (case List.length ts
                                of 0
                                   => apply (k, (VAL (rest, asts, (err_pos, nts, ccs, nt))))
                                 | 1
                                   => apply (k, (VAL (rest, List.hd ts :: asts, (err_pos, nts, ccs, nt))))
                                 | _
                                   => apply (k, (VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt)))))
                        | VOIDING
                          => apply (k, (VAL (rest, asts, (err_pos, nts, ccs, nt)))))
                | apply (CONT_NT (_, _, _, nt) :: k, FAIL (err_pos, nts, ccs, _))
                  = apply (k, FAIL (err_pos, nts, ccs, nt))
                | apply ([], VAL (rest, ts, (err_pos, nts, ccs, _)))
                  = if eof rest
                    then AST (case start_nt
                                of NORMAL
                                   => AST_TREE (start_name, List.rev ts)
                                 | PRUNING
                                   => (case List.length ts
                                         of 0
                                            => AST_EMPTY
                                          | 1
                                            => List.hd ts
                                          | _
                                            => AST_TREE (start_name, List.rev ts))
                                 | VOIDING
                                   => AST_EMPTY)
                    else if rest = err_pos
                         then PARSE_ERROR (rest, uniq nts, ccs)
                         else PARSE_ERROR (rest, [], [])
                | apply ([], FAIL (err_pos, nts, ccs, _))
                  = PARSE_ERROR (err_pos, uniq nts, ccs)

              (*  eval : exp * int * ast list * error * cont list -> result  *)
              and eval (ANY, pos, asts, err, k)
                  = if eof pos
                    then apply (k, FAIL (update_error (err, pos)))
                    else apply (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)))
                | eval (CHAR c, pos, asts, err, k)
                  = if eof pos orelse c <> ch pos
                    then apply (k, FAIL (update_error (err, pos)))
                    else apply (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)))
                | eval (CHAR_CLASS cc, pos, asts, err, k)
                  = let fun visit ([])
                            = apply (k, FAIL (update_error (err, pos)))
                          | visit ((c1, c2) :: cs)
                            = if c1 <= ch pos andalso c2 >= ch pos
                              then apply (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err)))
                              else visit cs
                    in if eof pos
                       then apply (k, FAIL (update_error (err, pos)))
                       else visit cc
                    end
                (* a sequence is made up of a list of expressions *)
                (* we traverse the list, making sure each expression succeeds *)
                (* the rest of the string return by the expression is used *)
                (* as input to the next expression *)
                | eval (SEQ es, pos, asts, err, k)
                  = if null es
                    then apply (k, (VAL (pos, asts, err)))
                    else eval (hd es, pos, asts, err, CONT_SEQ (tl es) :: k)
                (* an alt is made up of a list of expressions *)
                (* we traverse the list until one succeeds *)
                (* if none succeed, the whole expression fails *)
                | eval (ALT es, pos, asts, err, k)
                  = if null es
                    then apply (k, FAIL err)
                    else eval (hd es, pos, asts, err, CONT_ALT (tl es, pos, asts) :: k)
                | eval (AND e, pos, asts, err, k)
                  = eval (e, pos, [], err, CONT_AND (pos, asts, err) :: k)
                | eval (NOT e, pos, asts, err, k)
                  = eval (e, pos, [], err, CONT_NOT (pos, asts, err) :: k)
                | eval (OPT e, pos, asts, err, k)
                  = eval (e, pos, asts, err, CONT_OPT (pos, asts) :: k)
                | eval (STAR e, pos, asts, err, k)
                  = eval (e, pos, asts, err, CONT_STAR (e, pos, asts) :: k)
                | eval (PLUS e, pos, asts, err, k)
                  = eval (e, pos, asts, err, CONT_PLUS e :: k)
                | eval (VOID e, pos, asts, err, k)
                  = eval (e, pos, [], err, CONT_VOID asts :: k)
                | eval (NT name, pos, asts, (err_pos, nts, ccs, nt), k)
                  = let val (mode, e) = Env.lookup (name, env)
                    in eval (e, pos, [], (err_pos, nts, ccs, name), CONT_NT (mode, name, asts, nt) :: k)
                    end
            in eval (start_expression, 0, [], (0, [start_name], [], start_name), [])
            end

    (* ********** *)
    end
  end;


(* matcher *)
(* small-step abstract machine *)
structure Match4 : SIG
= struct
    local
      open Syn
      open Value
    in
      (*  match : env * string * string -> result  *)
      fun match (env, start_name, input)
          = let
              val input_len = String.size input

              val (start_nt, start_expression) = Env.lookup (start_name, env)

              (*  eof : int -> bool  *)
              fun eof pos = pos >= input_len

              (*  ch : int -> char  *)
              fun ch pos = String.sub (input, pos)

              (* stack frames *)
              datatype cont = CONT_SEQ of exp list
                            | CONT_ALT of exp list * int * ast list
                            | CONT_AND of int * ast list * error
                            | CONT_NOT of int * ast list * error
                            | CONT_OPT of int * ast list
                            | CONT_STAR of exp * int * ast list
                            | CONT_PLUS of exp
                            | CONT_VOID of ast list
                            | CONT_NT of mode * string * ast list * string

              (* machine state *)
              datatype configuration = EVAL of exp * int * ast list * error * cont list
                                     | APPLY of cont list * value
              datatype         state = FINAL of result
                                     | INTER of configuration

              (* move : configuration -> state *)
              fun move (EVAL (ANY, pos, asts, err, k))
                  = if eof pos
                    then INTER (APPLY (k, FAIL (update_error (err, pos))))
                    else INTER (APPLY (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err))))
                | move (EVAL (CHAR c, pos, asts, err, k))
                  = if eof pos orelse c <> ch pos
                    then INTER (APPLY (k, FAIL (update_error (err, pos))))
                    else INTER (APPLY (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err))))
                | move (EVAL (CHAR_CLASS cc, pos, asts, err, k))
                  = let fun visit ([])
                            = INTER (APPLY (k, FAIL (update_error (err, pos))))
                          | visit ((c1, c2) :: cs)
                            = if c1 <= ch pos andalso c2 >= ch pos
                              then INTER (APPLY (k, (VAL (pos + 1, AST_CHAR (ch pos) :: asts, err))))
                              else visit cs
                    in if eof pos
                       then INTER (APPLY (k, FAIL (update_error (err, pos))))
                       else visit cc
                    end
                (* a sequence is made up of a list of expressions *)
                (* we traverse the list, making sure each expression succeeds *)
                (* the rest of the string return by the expression is used *)
                (* as input to the next expression *)
                | move (EVAL (SEQ es, pos, asts, err, k))
                  = if null es
                    then INTER (APPLY (k, (VAL (pos, asts, err))))
                    else INTER (EVAL (hd es, pos, asts, err, CONT_SEQ (tl es) :: k))
                (* an alt is made up of a list of expressions *)
                (* we traverse the list until one succeeds *)
                (* if none succeed, the whole expression fails *)
                | move (EVAL (ALT es, pos, asts, err, k))
                  = if null es
                    then INTER (APPLY (k, FAIL err))
                    else INTER (EVAL (hd es, pos, asts, err, CONT_ALT (tl es, pos, asts) :: k))
                | move (EVAL (AND e, pos, asts, err, k))
                  = INTER (EVAL (e, pos, [], err, CONT_AND (pos, asts, err) :: k))
                | move (EVAL (NOT e, pos, asts, err, k))
                  = INTER (EVAL (e, pos, [], err, CONT_NOT (pos, asts, err) :: k))
                | move (EVAL (OPT e, pos, asts, err, k))
                  = INTER (EVAL (e, pos, asts, err, CONT_OPT (pos, asts) :: k))
                | move (EVAL (STAR e, pos, asts, err, k))
                  = INTER (EVAL (e, pos, asts, err, CONT_STAR (e, pos, asts) :: k))
                | move (EVAL (PLUS e, pos, asts, err, k))
                  = INTER (EVAL (e, pos, asts, err, CONT_PLUS e :: k))
                | move (EVAL (VOID e, pos, asts, err, k))
                  = INTER (EVAL (e, pos, [], err, CONT_VOID asts :: k))
                | move (EVAL (NT name, pos, asts, (err_pos, nts, ccs, nt), k))
                  = let val (mode, e) = Env.lookup (name, env)
                    in INTER (EVAL (e, pos, [], (err_pos, nts, ccs, name), CONT_NT (mode, name, asts, nt) :: k))
                    end
                | move (APPLY (CONT_SEQ es :: k, VAL (rest, ts, err)))
                  = if null es
                    then INTER (APPLY (k, (VAL (rest, ts, err))))
                    else INTER (EVAL (hd es, rest, ts, err, CONT_SEQ (tl es) :: k))
                | move (APPLY (CONT_SEQ _ :: k, FAIL err))
                  = INTER (APPLY (k, FAIL err))
                | move (APPLY (CONT_ALT _ :: k, VAL (rest, ts, err)))
                  = INTER (APPLY (k, (VAL (rest, ts, err))))
                | move (APPLY (CONT_ALT (es, pos, asts) :: k, FAIL err))
                  = if null es
                    then INTER (APPLY (k, FAIL err))
                    else INTER (EVAL (hd es, pos, asts, err, CONT_ALT (tl es, pos, asts) :: k))
                | move (APPLY (CONT_AND (pos, asts, err) :: k, VAL _))
                  = INTER (APPLY (k, (VAL (pos, asts, err))))
                | move (APPLY (CONT_AND (_, _, err) :: k, FAIL _))
                  = INTER (APPLY (k, FAIL err))
                | move (APPLY (CONT_NOT (_, _, err) :: k, VAL _))
                  = INTER (APPLY (k, FAIL err))
                | move (APPLY (CONT_NOT (pos, asts, _) :: k, FAIL err))
                  = INTER (APPLY (k, (VAL (pos, asts, err))))
                | move (APPLY (CONT_OPT _ :: k, VAL (rest, ts, err)))
                  = INTER (APPLY (k, (VAL (rest, ts, err))))
                | move (APPLY (CONT_OPT (pos, asts) :: k, FAIL err))
                  = INTER (APPLY (k, (VAL (pos, asts, err))))
                | move (APPLY (CONT_STAR (e, _, _) :: k, VAL (rest, ts, err)))
                  = INTER (EVAL (e, rest, ts, err, CONT_STAR (e, rest, ts) :: k))
                | move (APPLY (CONT_STAR (_, pos, asts) :: k, FAIL err))
                  = INTER (APPLY (k, (VAL (pos, asts, err))))
                | move (APPLY (CONT_PLUS e :: k, VAL (rest, ts, err)))
                  (* the second continuation used to evaluate PLUS *)
                  (* is the same as the continuation for STAR *)
                  = INTER (EVAL (e, rest, ts, err, CONT_STAR (e, rest, ts) :: k))
                | move (APPLY (CONT_PLUS _ :: k, FAIL err))
                  = INTER (APPLY (k, FAIL err))
                | move (APPLY (CONT_VOID asts :: k, VAL (rest, _, err)))
                  = INTER (APPLY (k, (VAL (rest, asts, err))))
                | move (APPLY (CONT_VOID _ :: k, FAIL err))
                  = INTER (APPLY (k, FAIL err))
                | move (APPLY (CONT_NT (mode, name, asts, nt) :: k, VAL (rest, ts, (err_pos, nts, ccs, _))))
                  = (case mode
                       of NORMAL
                          => INTER (APPLY (k, (VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt)))))
                        | PRUNING
                          => (case List.length ts
                                of 0
                                   => INTER (APPLY (k, (VAL (rest, asts, (err_pos, nts, ccs, nt)))))
                                 | 1
                                   => INTER (APPLY (k, (VAL (rest, List.hd ts :: asts, (err_pos, nts, ccs, nt)))))
                                 | _
                                   => INTER (APPLY (k, (VAL (rest, AST_TREE (name, List.rev ts) :: asts, (err_pos, nts, ccs, nt))))))
                        | VOIDING
                          => INTER (APPLY (k, (VAL (rest, asts, (err_pos, nts, ccs, nt))))))
                | move (APPLY (CONT_NT (_, _, _, nt) :: k, FAIL (err_pos, nts, ccs, _)))
                  = INTER (APPLY (k, FAIL (err_pos, nts, ccs, nt)))
                | move (APPLY ([], VAL (rest, ts, (err_pos, nts, ccs, _))))
                  = if eof rest
                    then FINAL (AST (case start_nt
                                       of NORMAL
                                          => AST_TREE (start_name, List.rev ts)
                                        | PRUNING
                                          => (case List.length ts
                                                of 0
                                                   => AST_EMPTY
                                                 | 1
                                                   => List.hd ts
                                                 | _
                                                   => AST_TREE (start_name, List.rev ts))
                                        | VOIDING
                                          => AST_EMPTY))
                    else if rest = err_pos
                         then FINAL (PARSE_ERROR (rest, uniq nts, ccs))
                         else FINAL (PARSE_ERROR (rest, [], []))
                | move (APPLY ([], FAIL (err_pos, nts, ccs, _)))
                  = FINAL (PARSE_ERROR (err_pos, uniq nts, ccs))

              (* drive : state -> halting_state *)
              fun drive (FINAL a) = a
                | drive (INTER g) = drive (move g)

            in drive (move (EVAL (start_expression, 0, [], (0, [start_name], [], start_name), [])))
            end

    (* ********** *)
    end
  end;


(* make tests *)
structure Tests0 = mkTest (structure E = Match0)
structure Tests1 = mkTest (structure E = Match1)
structure Tests2 = mkTest (structure E = Match2)
structure Tests3 = mkTest (structure E = Match3)
structure Tests4 = mkTest (structure E = Match4)


(* run tests *)
val tests_0 = Tests0.results
val tests_1 = Tests1.results
val tests_2 = Tests2.results
val tests_3 = Tests3.results
val tests_4 = Tests4.results

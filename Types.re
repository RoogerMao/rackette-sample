/* examples for rawProgram: 
  1. (+ 3 4)
  2. (* (+ 3 4) 5)
  3. (define (double-one n alon) (cons (* 2 n) alon)) (double-one 1 (list 3 4 5))
      this is double-one from hw #2 
*/
type rawProgram = string;

/* examples for concreteProgramPiece
  1. NumberC(2)
  2. SymbolC("/")
  3. ListC([SymbolC("*"), NumberC(4), NumberC(8)])
*/ 
type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

/* examples for concreteProgram: 
  1. [ListC([SymbolC("define"), SymbolC("a"), NumberC(4)]), 
      ListC([SymbolC("+"), SymbolC("a"), NumberC(4)])]
  2. [ListC([SymbolC("if") SymbolC("true"), NumberC(1), NumberC(4)])]
*/
type concreteProgram = list(concreteProgramPiece);

/* a Rackette name 
  examples: 
  1. Name("banana")
  2. Name("seanKim")
*/ 
type name =
  | Name(string);

/* a Rackette expression 
  examples:
  1. NumE(9)
  2. BoolE(false)
  3. EmptyE
  4. NameE(Name("iMac"))
  5. AndE(BoolE(true), BoolE(false))
  6. OrE(BoolE(true), BoolE(false))
  7. IfE({
    condition: ApplicationE(NameE(Name(">")), NameE(Name("n")), NumE(0)), 
    trueExpr: NameE(Name("n")), 
    falseExpr: ApplicationE(NameE("-"), NumE(0), NameE(Name("n")),
  })
  8. CondE[{ 
    conditionExpr: ApplicationE(NameE(">="), NameE(Name("n")), NumE(0)), 
    resultExpr: NameE(Name("n is greater than 0")),
  }, { 
    conditionExpr: ApplicationE(NameE(Name("<")), NameE(Name("n")), NumE(0)), 
    resultExpr: NameE(Name("n is less than 0")),
  }]
  9. LambdaE({
    nameList: list(NameE(Name("x"))),
    lambdaBody: ApplicationE(NameE(Name("+")), NameE(Name("x")), NumE(3)),
  })
  10. LetE({
    letPairs: list({
      pairName: NameE(Name("x")), 
      pairExpr: NumE(3),  
    }),
    letBody: ApplicationE(NameE(Name("*")), NameE(Name("x")), NumE(2)), 
  })
  11. ApplicationE(NameE(Name("+")), NumE(5), NumE(3))
*/
type expression =
  | NumE(int)
  | BoolE(bool)
  | EmptyE
  | NameE(name)
  | AndE(expression, expression)
  | OrE(expression, expression)
  | IfE(ifData)
  | CondE(list(condData)) 
  | LambdaE(lambdaData)
  | LetE(letData)
  | ApplicationE(list(expression))
  and ifData = {
    boolExpr: expression,
    trueExpr: expression,
    falseExpr: expression,
  }
  and condData = { 
    conditionExpr: expression, 
    resultExpr: expression,
  }
  and lambdaData = {
    nameList: list(name),
    lambdaBody: expression,
  } 
  and letPair = {
    pairName: name, 
    pairExpr: expression,  
  }
  and letData = {
    letPairs: list(letPair),
    letBody: expression, 
  };
  

/* a Rackette definition 
  examples:
  1. (Name("a"), NumE(3))
  2. (Name("b"), Name("foo"))
  3. (Name("c"), LambdaE(
    nameList: [Name("x")], 
    lambdaBody: ApplicationE(NameE("+"), NumE(3), NameE("x"))
    ))
*/
type definition = (name, expression);

/* a piece of Rackette that can be processed:
 * either a definition or an expression
 * (examples for definition and expression provided above)*/
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* a representation of a Rackette program - any number of pieces: 
  examples: 
  1. list(Definition(("x", NumE(3))))
  2. list(Definition((Name("x"), BoolE(true))), AndE(true, NameE(Name("x"))))
  */
type abstractProgram = list(abstractProgramPiece);

/* a Rackette value: the result of evaluating a Rackette expression 
  examples: 
  1. NumV(3)
  2. BoolV(true)
  3. ListV(list(NumV(1),NumV(2),NumV(3))) 
  4. BuiltinV({ 
      printedRep: "+",
      bProc: list(NumV(1), NumV(2)) => NumV(3),
      }) ask if printedRep should be the built-in procedure
  5. ClosureV({
    cNameList: list(Name("myTrue"), Name("myFalse")),
    cExpr: orE(NameE(Name("myTrue")), NameE(Name("myFalse"))), 
    cEnv: [],
  })*/
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)
  and builtinData = { 
    printedRep: string,
    bProc: list(value) => value,
  }
  and closureData = {
    cNameList: list(name),
    cExpr: expression, 
    cEnv: environment,
  }

  /* Environments, bindingLists, and bindings aren't values
     But we use "and" here so closures have access to environments,
     bindings have access to values, etc. */
  and environment = (list(bindingList))
  and bindingList = (list(binding))
  and binding = (name, value);

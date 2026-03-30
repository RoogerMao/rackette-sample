open CS17SetupRackette;
open Read.Reader;
open Types;
open List; 

/* Lab Code */
let emptyEnv: environment = []; 
let emptyBindingList: bindingList = []; 

/* i/o: 
    input: a binding, b, and a bindingList, alob
    output: the bindingList with the binding added
*/
let addBindingToList: (binding, bindingList) => bindingList 
    = (b, alob) => [b, ... alob]; 

/* i/o: 
    input: a binding, b, and an environment, e
    output: the environment with the binding added to the first bindingList 
      in the environment
*/
let addBinding: (binding, environment) => environment = (b, e) => {
    switch(e) {
        | [] => failwith("Empty Environment") 
        | [firstBindingList, ... rest] => 
            [addBindingToList(b, firstBindingList), ... rest]
    }
}

/* i/o: 
    input: a bindingList, alob, and an environment, env
    output: environment with the bindingList added onto the front
*/
let extendEnv: (bindingList, environment) => environment = (alob, env) => 
    [alob, ... env];

/* i/o: 
    input: a binding, b
    output: the value associated with the binding
*/
let valueFromBinding: binding => value = b => {
    switch(b) { 
        | (_, v) => v
    }
};

/* i/o: 
    input: a name, n, and a bindingList, alob
    output: an option with value associated with n in alob, or None if n isn't
      in alob

  recursion diagram #1: 
  oi: (Name("a"), [])
    ri: n/a
    ro: n/a
    ideation space: return none if empty
  oo: None

  recursion diagram #2: 
  oi: (Name("a"), [(Name("b"), NumV(4))])
    ri: (Name("a"), [])
    ro: None
    ideation space: search the rest of the bindingList if the name of the first
      binding doesn't match n
  oo: None
*/
let rec lookupHelper: (name, bindingList) => option(value) = (n, alob) => {
    switch (alob) {
        | [] => None 
        | [(firstName, firstValue), ... rest] => 
          if (firstName == n) { 
            Some(firstValue) 
          } else { 
            lookupHelper(n, rest) 
          }
    }
};

/* i/o: 
    input: a name, n, and an environment, e
    output: an option with value associated with n in env, or None if n isn't
      in alob

  recursion diagram #1: 
  oi: (Name("a"), [])
    ri: n/a
    ro: n/a
    ideation space: return none if empty
  oo: None

  recursion diagram #2: 
  oi: (Name("a"), [[]])
    ri: (Name("a"), [])
    ro: None
    ideation space: use lookupHelper to search each bindingList in the 
      environment
  oo: None
*/
let rec lookup: (name, environment) => option(value) = (name, e) => {
    switch(e) {
        | [] => None
        | [firstBindingList, ... rest] => 
          if (lookupHelper(name, firstBindingList) == None) {
            lookup(name, rest)
          } else {
              lookupHelper(name, firstBindingList)
          }
    }
} 

/* i/o: 
    input: a value, aValue
    output: a string representation of the value that we will display back to 
      the user

  recursion diagram #1: 
  oi: NumV(4)
    ri: n/a
    ro: n/a
    ideation space: return the value of numbers
  oo: "4"

  recursion diagram #2: 
  oi: ListV(NumV(4))
    ri: NumV(4)
    ro: "4"
    ideation space: iterate over the elements in the list of ListV and apply 
      stringOfValue 
  oo: "(cons 4 empty)"
*/
let rec stringOfValue: value => string = aValue => switch(aValue) {
  | NumV(num) => string_of_int(num)
  | BoolV(boolean) => "#" ++ string_of_bool(boolean)
  | ListV(alov) => switch(alov) {
    | [] => "empty"
    | [firstValue, ... tl] => "(cons " ++ stringOfValue(firstValue) ++ " " ++ 
      stringOfValue(ListV(tl)) ++ ")"
  }
  | BuiltinV(builtIn) => builtIn.printedRep
  | ClosureV(closure) => "(lambda (" 
    ++ stringOfClosureArguments(closure.cNameList, 1) ++ ") ...)"
} 
/* i/o: 
    input: a value and a counter
    output: a string representation of the arguments that we'll pass to the 
      string representation of the closure as a larger whole

  recursion diagram #1: 
  oi: ([], 1)
    ri: n/a
    ro: n/a
    ideation: don't return a string representation of arguments if the argument
      list is empty
  oo: ""

  recursion diagram #2: 
  oi: (["a"], 1)
    ri: ([], 2)
    ro: ""
    ideation space: add one to counter every time and pass the rest of the 
      list to the recursive call 
  oo: "(cons 4 empty)"
*/
and stringOfClosureArguments: (list(name), int) => string = 
  (closureArguments, counter) => switch(closureArguments) {
  | [] => ""
  | [_] => "a" ++ string_of_int(counter) 
  | [_, ... tl] => "a" ++ string_of_int(counter) ++ " " 
    ++ stringOfClosureArguments(tl, counter + 1)
};

let initialTle: environment = [[
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a NumV representing the sum of the two NumV inputs
  */
  (Name("+"), 
    BuiltinV({
      printedRep: "+",
      bProc: arguments => switch(arguments) {
        | [NumV(num1), NumV(num2)] => NumV(num1 + num2)
        | [NumV(_), nonNumber] => failwith(
            "+: expects a number, given " ++ stringOfValue(nonNumber))
        | [nonNumber, _] => failwith(
            "+: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a NumV representing the difference between the two NumV inputs
  */
  (Name("-"), 
    BuiltinV({
      printedRep: "-",
      bProc: arguments => switch(arguments) {
        | [NumV(num1), NumV(num2)] => NumV(num1 - num2)
        | [NumV(_), nonNumber] => failwith(
            "-: expects a number, given " ++ stringOfValue(nonNumber))
        | [nonNumber, _] => failwith(
            "-: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a NumV representing the multiplication of the two NumV inputs
  */
  (Name("*"), 
    BuiltinV({
      printedRep: "*",
      bProc: arguments => switch(arguments) {
        | [NumV(num1), NumV(num2)] => NumV(num1 * num2)
        | [NumV(_), nonNumber] => failwith(
            "*: expects a number, given " ++ stringOfValue(nonNumber))
        | [nonNumber, _] => failwith(
            "*: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a NumV representing the quotient of the first and second numbers
  */
  (Name("/"), 
    BuiltinV({
      printedRep: "/",
      bProc: arguments => switch(arguments) {
        | [NumV(_), NumV(0)] => failwith("/: division by zero")
        | [NumV(num1), NumV(num2)] => NumV(num1 / num2)
        | [nonNumber, NumV(_)] => failwith(
            "/: expects a number, given " ++ stringOfValue(nonNumber))
        | [_, nonNumber] => failwith(
            "/: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a NumV representing the remainder of dividing the first number by the
          second number
  */
  (Name("remainder"), 
    BuiltinV({
      printedRep: "remainder",
      bProc: arguments => switch(arguments) {
        | [NumV(_), NumV(0)] => failwith("division by zero")
        | [NumV(num1), NumV(num2)] => NumV(num1 mod num2)
        | [nonNumber, NumV(_)] => failwith(
            "remainder: expects a number, given " ++ stringOfValue(nonNumber))
        | [_, nonNumber] => failwith(
            "remainder: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a BoolV representing if the two inputted NumVs are equal
  */
  (Name("="), 
    BuiltinV({
      printedRep: "=",
      bProc: arguments => switch(arguments) {
        | [NumV(num1), NumV(num2)] => BoolV(num1 == num2)
        | [NumV(_), nonNumber] => failwith(
            "=: expects a number, given " ++ stringOfValue(nonNumber))
        | [nonNumber, _] => failwith(
            "=: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two values
  output: a BoolV representing if the inputted two values are equal
  */
  (Name("equal?"), 
    BuiltinV({
      printedRep: "equal?",
      bProc: arguments => switch(arguments) {
        | [arg1, arg2] => BoolV(arg1 == arg2)
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a BoolV representing if the first number is less than the second 
          number
  */
  (Name("<"), 
    BuiltinV({
      printedRep: "<",
      bProc: arguments => switch(arguments) {
        | [NumV(num1), NumV(num2)] => BoolV(num1 < num2)
        | [NumV(_), nonNumber] => failwith(
            "<: expects a number, given " ++ stringOfValue(nonNumber))
        | [nonNumber, _] => failwith(
            "<: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a BoolV representing if the first number is greater than the second 
          number
  */
  (Name(">"), 
    BuiltinV({
      printedRep: ">",
      bProc: arguments => switch(arguments) {
        | [NumV(num1), NumV(num2)] => BoolV(num1 > num2)
        | [nonNumber, NumV(_)] => failwith(
          ">: expects a number, given " ++ stringOfValue(nonNumber))
        | [_, nonNumber] => failwith(
          ">: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
    /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a BoolV representing if the first number is less than or equal to 
          the second number
  */
    (Name("<="), 
    BuiltinV({
      printedRep: "<=",
      bProc: arguments => switch(arguments) {
        | [NumV(num1), NumV(num2)] => BoolV(num1 <= num2)
        | [NumV(_), nonNumber] => failwith(
            "<=: expects a number, given " ++ stringOfValue(nonNumber))
        | [nonNumber, _] => failwith(
            "<=: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: two NumVs, which represent two integers (failwith if otherwise)
  output: a BoolV representing if the first number is greater than or equal to 
          the second number
  */
  (Name(">="), 
    BuiltinV({
      printedRep: ">=",
      bProc: arguments => switch(arguments) {
        | [NumV(num1), NumV(num2)] => BoolV(num1 >= num2)
        | [nonNumber, NumV(_)] => failwith(
            ">=: expects a number, given " ++ stringOfValue(nonNumber))
        | [_, nonNumber] => failwith(
            ">=: expects a number, given " ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      },
  })),
  /* 
  input: one value 
  output: a BoolV representing if the input is a number
  */
  (Name("number?"),
    BuiltinV({
      printedRep: "number?",
      bProc: argument => switch(argument) {
        | [NumV(_)] => BoolV(true)
        | [_] => BoolV(false)
        /* to suppress warning, this shouldn't have less/more than one arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      }
    })),
  /* 
  input: a NumV (failwith if otherwise)
  output: a BoolV representing if the input is zero
  */
  (Name("zero?"),
    BuiltinV({
      printedRep: "zero?",
      bProc: argument => switch(argument) {
        | [NumV(0)] => BoolV(true)
        | [NumV(_)] => BoolV(false)
        | [nonNumber] => failwith("zero?: expects a number, given " 
          ++ stringOfValue(nonNumber))
        /* to suppress warning, this shouldn't have less/more than one arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      }
    })),
  /* 
  input: two values (second value should be a list of the type of 
         the first value)
  output: a ListV which concatenates the first value onto the second value
  */
  (Name("cons"),
    BuiltinV({
      printedRep: "cons",
      bProc: arguments => switch(arguments) {
        | [hd, ListV([])] => ListV([hd])
        | [hd, ListV([lstHd, ... lstTl])] => ListV([hd, lstHd, ... lstTl])
        /* to suppress warning, this shouldn't have less/more than two arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      }
    })), 
  /* 
  input: one ListV (failwith if otherwise)
  output: the first value from the inputted ListV
  */
  (Name("first"),
    BuiltinV({
      printedRep: "first",
      bProc: argument => switch(argument) {
        | [ListV([])] => failwith("first: expects a non-empty list; given: '()")
        | [ListV([lstHd,... _])] => lstHd
        | [nonList] => failwith("first: expects a non-empty list; given: " ++ 
          stringOfValue(nonList))
        /* to suppress warning, this shouldn't have less/more than one arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      }
    })),
  /* 
  input: a ListV (failwith if otherwise)
  output: the rest of the list from the inputted ListV (everything except
          first value)
  */
  (Name("rest"),
    BuiltinV({
      printedRep: "rest",
      bProc: argument => switch(argument) {
        | [ListV([])] => failwith("rest: expects a non-empty list; given: '()")
        | [ListV([_,... lstTl])] => ListV(lstTl)
        | [nonList] => failwith("rest: expects a non-empty list; given: " ++ 
          stringOfValue(nonList))
        /* to suppress warning, this shouldn't have less/more than one arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      }
    })),
  /* 
  input: one value
  output: a BoolV representing if the input is an empty list
  */
  (Name("empty?"),
    BuiltinV({
      printedRep: "empty?",
      bProc: argument => switch(argument) {
        | [ListV([])] => BoolV(true)
        | _ => BoolV(false)
      }
    })),
  /* 
  input: one value
  output: a BoolV representing if the input is a non-empty list
  */
  (Name("cons?"),
    BuiltinV({
      printedRep: "cons?",
      bProc: argument => switch(argument) {
        | [ListV([_, ... _])] => BoolV(true)
        | _ => BoolV(false)
      }
    })),
  /* 
  input: a BoolV (failwith otherwise)
  output: a BoolV which is the opposite of the input's boolean value
  */
  (Name("not"),
    BuiltinV({
      printedRep: "not",
      bProc: argument => switch(argument) {
        | [BoolV(boolVal)] => BoolV(!boolVal)
        | [nonBool] => failwith(
          "not: expected either #true or #false; given " 
          ++ stringOfValue(nonBool))
        /* to suppress warning, this shouldn't have less/more than one arguments
            since parse will catch that*/ 
        | _ => failwith("") 
      }
    }))
]]; 

/*
 * data definitions: 
 *  - a concreteProgramPiece is either
 *    - NumberC(int), which represents a number
      - SymbolC(string), which represents a "symbol" (something whose meaning 
        we'll look up later)
      - ListC(list(concreteProgramPiece)), which accounts for embedded 
        expressions;
 *  - an expression represents something to evaluate in our abstract program 
 * 
 * examples: provided in Types.re 
 * 
 * input/output: 
 *  input: a concreteProgramPiece 
 *  output: an expression (which is an abstractProgramPiece) that represents 
 *    the conreteProgramPiece, or an error if the concreteProgramPiece has any
 *    syntax errors
 * 
 * recursion diagram #1: 
 * oi: ListC([SymbolC("+"), NumberC(4), Number(5)])
 *  ri: NumberC(4) | NumberC(5)
 *  ro: NumE(4) | NumE(5)
 *  ideation: 
 *    1. When we get a symbol, we first want to look up if it corresponds to 
 *        anything in the TLE (either a previously defined variable name or 
 *        a buit-in procedure), if not, throw an error 
 *    2. If we look it up and it's either a buit-in procedure or a user-defined
 *        procedure (which has the datatype of a closure), then we should check
 *        for the correct number of arguments 
 *          a. If we look it up, and we find the name of a variable, then we 
 *              should output NameE(Name([name])) for the abstract program 
 *              so that we know to look it up in eval 
 *    3. We can use pattern-matching (similar to what we did in Rackettecita) 
 *        to make sure that we have proper expressions
 *    4. Create a new recursive call for everyListC data type
 * oo: ApplicationE([NameE(Name("+")), NumE(4), NumE(5)[])
 * 
 * recursion diagram #2: 
 * oi: NumC(4)
 *  ri: n/a
 *  ro: n/a
 *  ideation space: When we get a NumC, return a NumE. We'll also do this if 
 *    the expression inside SymbolC is either "true" or "false". 
 * oo: NumE(4)
 * */

let binaryOperations = ["+", "-", "*", "/", "remainder", "=", "<", ">", "<=", 
  ">=", "equal?", "cons"]; 
let unaryOperators = ["number?", "zero?", "first", "rest", "empty?", "cons?", 
  "not"];

/* i/o: 
    input: a concreteProgramPiece, input, representing a pair of data in a 
      cond expression
    output: the information in input transformed into a condData datum

  recursion diagram #1: 
  oi: NumV(4)
    ri: n/a
    ro: n/a
    ideation space: return the value of numbers
  oo: "4"

  recursion diagram #2: 
  oi: ListV(NumV(4))
    ri: NumV(4)
    ro: "4"
    ideation space: iterate over the elements in the list of ListV and apply 
      stringOfValue 
  oo: "(cons 4 empty)"
*/
let rec parseCondPair: concreteProgramPiece => condData = input => {
  switch (input) {
    | NumberC(_) => failwith(
       "expected a clause with a question and an answer, but found a number")
    | SymbolC(_) => failwith(
       "expected a clause with a question and an answer, but found something else")
    | ListC([]) => failwith(
        "cond: expected a clause with a question and an answer, but found an empty part")
    | ListC([_]) => failwith(
        "cond: expected a clause with a question and an answer, but found a clause with only one part")
    | ListC([pred, expr]) => {
        conditionExpr: parseExpression(pred),
        resultExpr: parseExpression(expr),
      }
    | ListC(args) => failwith(
        "cond: expected a clause with a question and an answer, but found a clause with " 
        ++ string_of_int(length(args)) ++ " parts")
  }
}
/* input: a concreteProgramPiece, input, representing a possible name and 
    binding for a let expression
  output: a letPair corresponding to input or a failwith if input didn't have 
    the correct (number of) data types
 */
and parseLetPair: concreteProgramPiece => letPair = input => {
  switch (input) {
    | NumberC(_) => failwith(
        "let: expected a binding with a variable and an expression, but found a number")
    | SymbolC(_) => failwith(
        "let: expected a binding with a variable and an expression, but found something else")
    | ListC([]) => failwith(
      "let: expected a binding with a variable and an expression, but found a part")
    | ListC([NumberC(_), ... _]) => failwith(
      "let: expected a variable for the binding, but found a number")
    | ListC([ListC(_), ... _]) => failwith(
        "let: expected a binding with a variable and an expression, but found a part")
    | ListC([SymbolC(letPairName)]) => failwith(
      "let: expected an expression after the name " ++ letPairName ++ 
      ", but nothing's there") 
    | ListC([SymbolC(letPairName), letPairExpression]) => {
      pairName: Name(letPairName), 
      pairExpr: parseExpression(letPairExpression), 
      }
    | ListC([SymbolC(letPairName), _, ... tl]) => failwith(
        "let: expected only one expression after the name " ++ letPairName 
        ++ ", but found " ++ string_of_int(length(tl)) ++ " extra part(s)")
  }
/* input: a concreteProgramPiece, input, representing a possible name to a
  formal argument of a lambda expression
  output: a name corresponding to the input provided, or an error if a number
    or list of concreteProgramPieces was provided
 */
} and parseLambdaName: concreteProgramPiece => name = input => {
  switch(input) {
    | NumberC(_) => failwith(
        "lambda: expected a variable, but found a number")
    | SymbolC(name) => Name(name)
    | _ => failwith(
      "lambda: expected a variable, but found a part")
  }
/* input: 
    - a list of concreteProgramPieces (the "arguments") to a binary operator
    - the name of the binary operator
  output: 
    the expression representing the application of the binary operator to the 
    list of arguments, or an error if there was a mismatch in the number of 
    arguments provided (i.e., not two arguments)
 */
} and parseBinaryOperatorArguments: (list(concreteProgramPiece), string) => 
  expression = (arguments, binaryOperatorName) => {
    switch(arguments) {
      | [exp1, exp2] => 
        ApplicationE([NameE(Name(binaryOperatorName)), parseExpression(exp1), 
        parseExpression(exp2)])
      | _ => failwith(binaryOperatorName ++ ": expected two expressions, but found "
        ++ string_of_int(List.length(arguments)))
    }
/* input: 
    - a list of concreteProgramPieces (the "arguments") to a unary operator
    - the name of the unary operator
  output: 
    the expression representing the application of the unary operator to the 
    list of arguments, or an error if there was a mismactch in the number of 
    arguments provided (i.e., not one argument)
 */
} and parseUnaryOperatorArguments: (list(concreteProgramPiece), string) => 
  expression = (arguments, unaryOperatorName) => {
    switch(map(parseExpression, arguments)) {
      | [exp1] => 
        ApplicationE([NameE(Name(unaryOperatorName)), exp1])
      | _ => failwith(unaryOperatorName ++ ": expected one expression, but found "
        ++ string_of_int(List.length(arguments)))
    }
/* input: a concreteProgramPiece, input
  output: the expression corresponding to input
  
  recursion diagram #1: 
  oi: NumberC(1)
    ri: n/a 
    ro: n/a 
    ideation: return the value inside NumberC wrapped in a NumE
  oo: NumE(1)

  recursion diagram #1: 
  oi: SymbolC("true")
    ri: n/a 
    ro: n/a 
    ideation: return BoolE if the string inside SymbolC is "true" or "false"
  oo: BoolE(true)
 */
} and parseExpression: concreteProgramPiece => expression =
  input => switch(input) {
    | NumberC(num) => NumE(num) 
    | SymbolC(name) => switch(name) {
      | "true" => BoolE(true) 
      | "false" => BoolE(false) 
      | "empty" => EmptyE
      | _ => NameE(Name(name))
    }
    | ListC(innerExp) => switch (innerExp) {
      | [] => failwith(
        "function call: expected a function after the open parenthesis, but nothing's there")
      | [NumberC(num), ... _] => failwith(
        "function call: expected a function after the open parenthesis, but received "
        ++ string_of_int(num))
      | [ListC([]), ... _] => failwith(
      "function call: expected a function after the open parenthesis, but nothing's there")
      | [ListC(proc), ... args] => 
        ApplicationE(cons(parseExpression(ListC(proc)), 
          map(parseExpression, args)))
      | [SymbolC(symbol), ... tl] => switch (symbol) {
        | "and" => switch(tl) {
          | [] => failwith("and: expects at least 2 arguments, but found none")
          | [_] => 
              failwith("and: expects at least 2 arguments, but found only 1")
          | [exp1, exp2] => AndE(parseExpression(exp1), parseExpression(exp2))
          | _ => failwith("and: expects 2 arguments, but found "++ 
              string_of_int(length(tl) - 2) ++ " extra parts")
        }
        | "or" => switch(tl) {
          | [] => failwith("or: expects at least 2 arguments, but found none")
          | [_] => 
              failwith("or: expects at least 2 arguments, but found only 1")
          | [exp1, exp2] => OrE(parseExpression(exp1), parseExpression(exp2))
          | _ => failwith("or: expects 2 arguments, but found "++ 
              string_of_int(length(tl) - 2) ++ " extra parts")
        }
        | "if" => switch(tl) {
          | [] => failwith(
              "if: expected a question and two answers, but nothing's there")
          | [_] => failwith(
              "if: expected a question and two answers, but found only 1 part")
          | [_, _] => failwith(
              "if: expected a question and two answers, but found only 2 parts")
          | [pred, yesExp, noExp] => 
            IfE({
              boolExpr: parseExpression(pred),
              trueExpr: parseExpression(yesExp),
              falseExpr: parseExpression(noExp),
            })
          | _ => failwith("if: expected a question and two answers, but found " 
            ++ string_of_int(length(tl)) ++ " parts")
        }
        | "cond" => switch(tl) {
          | [] => failwith(
              "cond: expected a clause after cond, but nothing's there")
          |  _ => CondE(map(parseCondPair, tl))
        } 
        | "let" => switch(tl) {
          | [] => failwith(
            "let: expected at least one binding (in parentheses) after let, but nothing's there")
          | [NumberC(_), ... _] => failwith(
              "let: expected at least one binding (in parentheses) after let, but found a number")
          | [ListC(letPairs), ... letBody] => LetE({
            letPairs: map(parseLetPair, letPairs), 
            letBody: switch (letBody) {
              | [] => failwith(
                "let: expected an expression after the bindings, but nothing's there")
              | [exp1] => parseExpression(exp1)
              | _ => failwith(
                "let: expected only one expression after the bindings, but found "
                ++ string_of_int(length(letBody) - 1) ++ " extra part(s)")
            },
          })
        }
        | "lambda" => switch(tl) {
          | [] => failwith(
            "lambda: expected (lambda (variable ...) expression), but nothing's there")
          | [ListC(formalArguments), ... lambdaBody] => LambdaE({
            nameList: map(parseLambdaName, formalArguments),
            lambdaBody: switch(lambdaBody) {
              | [] => failwith(
                "lambda: expected an expression for the function body, but nothing's there")
              | [exp1] => parseExpression(exp1) 
              | _ =>  failwith(
                "lambda: expected only one expression for the function body, but found "
                ++ string_of_int(length(lambdaBody) - 1) ++ " extra part(s)")
            },
          })
          | [_, ... _] => failwith(
            "lambda: expected (lambda (variable ...) expression), but found something else") 
        }
        | _ => 
          if (mem(symbol, binaryOperations)) 
            parseBinaryOperatorArguments(tl, symbol)
          else if (mem(symbol, unaryOperators))
            parseUnaryOperatorArguments(tl, symbol)
          else 
            ApplicationE(cons(NameE(Name(symbol)), 
              map(parseExpression, tl)))
      }
    }
  }; 

/* data definitions and examples for concreteProgramPiece provided above and 
    in Types.re
  data definition for definition: a name and an expression that we will add 
    to the tle 
  example for definition: provided in Types.re
  i/o: 
    input: a concreteProgramPiece, which represents a line of code 
    output: a definition corresponding to the concreteProgramPiece, or an error
      if the concreteProgramPiece has any syntax errors

  recursion diagram #1: 
  oi: ListC([SymbolC("define"), SymbolC("a")])
    ri: n/a 
    ro: n/a
    ideation space: 
      1. parseDefinition should call parseExpression on the second argument. We
          can use pattern matching to first expect a symbol and then an 
          expression (if not, we'll throw an error). 
            a. When we parse the symbol, we want to make sure that it follows
                Rackette's naming guidelines for variables, and that it doesn't 
                have the name of a built-in procedure. We would run into a an
                error in eval if we get passed a name already defined in the
                environment. 
  oo: this should throw an error 

  recursion diagram #2: 
  oi: ListC([SymbolC("define"), SymbolC("a"), ListC([SymbolC("+"), 
    NumberC(4)), NumberC(5)])])
    ri: ListC([SymbolC("+"), NumberC(4)), NumberC(5)])])
    ro: ApplicationE([NameE(Name("+")), NumE(4), NumE(5))])
    ideation space: Return the output of parseExpression
  oo: (Name("a")), ApplicationE([NameE(Name("+")), NumE(4), NumE(5))]))
*/

let parseDefinition: concreteProgramPiece => definition =
  input => switch(input) {
    | ListC([SymbolC("define"), SymbolC(variableName), expr]) => 
      (Name(variableName), parseExpression(expr)) 
    | ListC([SymbolC("define")]) => failwith(
      "expected a variable name, but nothing's there"
    )
    | ListC([SymbolC("define"), SymbolC(variableName)]) => failwith(
      "expected an expression after the variable name " ++ variableName ++ 
      ", but nothing's there")
    | ListC([SymbolC("define"), SymbolC(variableName), ... tl]) => failwith(
      "expected only one expression after the variable name " ++ variableName ++ 
      ", but found " ++ string_of_int(length(tl) - 1) ++ " extra parts"
    )
    | _ => failwith("Invalid definition type") // to suppress warnings
  };

/* input: a concreteProgramPiece, which represents a line of code
   output: an abstractProgramPiece corresponding to the concreteProgramPiece */
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) => Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* input: a concreteProgram
   output: an abstractProgram representing the concreteProgram, unless we come
            across a syntax error, in which case we throw an error) */
let parse: concreteProgram => abstractProgram =
  input => List.map(parsePiece, input);

/* input: 
    a top level environment, tle
    a local environment, env
    an express, expr
  output:
    the value corresponding to the evaluated expression
  
  recursion diagram #1: 
  oi: NumE(1)
    ri: n/a 
    ro: n/a 
    ideation: return the value inside NumE wrapped in a NumV
  oo: NumV(1)

  recursion diagram #1: 
  oi: BoolE(1)
    ri: n/a 
    ro: n/a 
    ideation: return the value inside BoolE wrapped in a BoolV
  oo: BoolV(1)
 */
let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) =>
    /* NOTE: tle is top level environment and env is local environment */
    switch (expr) {
    | NumE(numInput) => NumV(numInput)
    | BoolE(boolInput) => BoolV(boolInput)
    | EmptyE => ListV([])
    | AndE(exp1, exp2) => switch(eval(tle, env, exp1)) {
      | BoolV(true) => switch(eval(tle, env, exp2)) {
        | BoolV(booleanValue) => BoolV(booleanValue)
        | evalauatedExp => 
          failwith("and: question result is not true or false: " ++ 
          stringOfValue(evalauatedExp))
      }
      | BoolV(false) => BoolV(false)
      | evalauatedExp => failwith(
          "and: question result is not true or false: " ++ 
          stringOfValue(evalauatedExp))
    }
    | OrE(exp1, exp2) => switch(eval(tle, env, exp1)) {
      | BoolV(false) => switch(eval(tle, env, exp2)) {
        | BoolV(booleanValue) => BoolV(booleanValue)
        | evalauatedExp => 
          failwith("or: question result is not true or false: " ++ 
          stringOfValue(evalauatedExp))
      }
      | BoolV(true) => BoolV(true)
      | evalauatedExp => failwith(
          "or: question result is not true or false: " ++ 
          stringOfValue(evalauatedExp))
    } 
    | IfE(ifDatum) =>
        switch(eval(tle, env, ifDatum.boolExpr)) {
          | BoolV(true) => eval(tle, env, ifDatum.trueExpr)
          | BoolV(false) => eval(tle, env, ifDatum.falseExpr)
          | evaluatedExp => failwith("if: question result is not true or false: "
            ++ stringOfValue(evaluatedExp))
        }
    | NameE(nameToLookup) => switch(lookup(nameToLookup, env)) {
      | Some(specVal) => specVal
      | None => switch(lookup(nameToLookup, tle)) {
        | Some(specVal) => specVal
        | None => failwith(":this name is not defined")
      }
    }
    | ApplicationE([procOpr,... tl]) => switch(eval(tle, env, procOpr)) {
        | BuiltinV(builtInValues) 
          => builtInValues.bProc(map(e => eval(tle, env, e), tl))
        | ClosureV(closureDatum) => 
          if (List.length(closureDatum.cNameList) != List.length(tl)) {
            failwith("number of formals do not match number of actuals")
          } else {
            let lambdaEnv = [List.map2((f, a) => (f, a), closureDatum.cNameList, 
              (List.map(var => eval(tle, env, var), tl))), ... closureDatum.cEnv];
            eval(tle, lambdaEnv, closureDatum.cExpr)
          }
        | evaluatedExp => 
          failwith("function call: expected a function after the open parenthesis, but received "
          ++ stringOfValue(evaluatedExp))
      }
    | CondE([firstCd, ... tl]) => {
      switch(eval(tle, env, firstCd.conditionExpr)) {
        | BoolV(true) => eval(tle, env, firstCd.resultExpr)
        | BoolV(false) => switch(tl) {
          | [] => failwith("cond: all question results were false")
          | _ => eval(tle, env, CondE(tl))
        }
        | _ => failwith("condition expression evaluated to a non-boolean value")
      }
    }
    | LetE(letDatum) => {
        let binder: letPair => binding = specificPair =>
          (specificPair.pairName, eval(tle, env, specificPair.pairExpr));
        let newEnv = [List.map(binder, letDatum.letPairs), ... env];

        eval(tle, newEnv, letDatum.letBody)
    }
    | LambdaE(lambdaDatum) => ClosureV({
      cNameList: lambdaDatum.nameList,
      cExpr: lambdaDatum.lambdaBody,
      cEnv: env,
    })
    | _ => failwith("Invalid expression")
    };

/*  data definitions and examples already provided in Types.re; string is an 
      atomic data type, so we don't need to provide a definition/example
    i/o: 
      input: a value
      output: a string representing that value
    
    recursion diagram #1: 
    oi: ListV([NumV(3), NumV(4)])
      ri: NumV(3) | NumV(4)
      ro: 3 | 4
      ideation space: for lists, we will attach cons when we have a non empty 
        list to parse.
    oo: (cons 3 (cons 4 empty))
 * */

/* 
  input: an abstractProgram, pieces
  output: a list of values corresponding to the results of evaluating all the
    expressions in pieces
*/
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition((definitionName, definitionBinding)), ...tl] => 
          processHelper(extendEnv([(definitionName, 
            eval(tle, [], definitionBinding))], tle), tl)   
        | [Expression(expressionToEvaluate), ...tl] => 
            [eval(tle, [], expressionToEvaluate), ... processHelper(tle, tl)]
        };
    processHelper(initialTle, pieces);
  };

/* input: a rawProgram (string)
   output: a list of the string representations of the values that all the top
    level expressions evaluate to */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

/* CHECK EXPECTS FOR PARSE */ 
// sample test: parseExpression on concreteProgramPiece
checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parse empty expression",
);
// sample test: parseExpression with read
checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "read and parse empty expression",
); 

// parseDefinition
checkError(() => parseDefinition(ListC([SymbolC("define")])), 
  "expected a variable name, but nothing's there"); 
checkError(() => parseDefinition(ListC([SymbolC("define"), SymbolC("a")])), 
  "expected an expression after the variable name a, but nothing's there");
checkError(() => parseDefinition(ListC([SymbolC("define"), SymbolC("a"), 
  NumberC(4), NumberC(5)])), 
  "expected only one expression after the variable name a, but found 1 extra parts");
checkError(() => parseDefinition(ListC([SymbolC("define"), SymbolC("a"), 
  NumberC(4), NumberC(5), NumberC(6), NumberC(7)])), 
  "expected only one expression after the variable name a, but found 3 extra parts");

// parseExpression
// numbers, symbols
checkExpectExpression(parseExpression(NumberC(4)), NumE(4), 
  "parseExpression Number Base Case"); 
checkExpectExpression(parseExpression(SymbolC("true")), BoolE(true), 
  "parseExpression Boolean Base Case True"); 
checkExpectExpression(parseExpression(SymbolC("false")), BoolE(false), 
  "parseExpression Boolean Base Case False"); 

// lists 
checkError(() => parseExpression(read("(3)")), 
  "function call: expected a function after the open parenthesis, but received 3");
checkError(() => parseExpression(read("(())")), 
  "function call: expected a function after the open parenthesis, but nothing's there");
checkError(() => parseExpression(read("(((())))")), 
  "function call: expected a function after the open parenthesis, but nothing's there");
checkError(() => parseExpression(read("(() ())")), 
  "function call: expected a function after the open parenthesis, but nothing's there");

// just terrible...
checkExpectExpression(parseExpression(read("((+ 3 4) 1 (+ 3 4))")), 
  ApplicationE([ApplicationE([NameE(Name("+")), NumE(3), NumE(4)]), NumE(1),
  ApplicationE([NameE(Name("+")), NumE(3), NumE(4)])]),
  "nested ListC, correct syntax, incorrect types"); 
checkExpectExpression(parseExpression(read("((lambda (x) 3) 1)")), 
  ApplicationE([LambdaE({
    nameList: [Name("x")], 
    lambdaBody: NumE(3),
  }), NumE(1)]), 
  "nested ListC, correct syntax for lambda application"); 


// and 
checkExpectExpression(parseExpression(ListC([SymbolC("and"), SymbolC("true"), 
  SymbolC("true")])), AndE(BoolE(true), BoolE(true)), "And correct case"); 
checkError(() => parseExpression(ListC([SymbolC("and")])), 
  "and: expects at least 2 arguments, but found none"); 
checkError(() => parseExpression(ListC([SymbolC("and"), 
  SymbolC("false")])), "and: expects at least 2 arguments, but found only 1"); 
checkError(() =>parseExpression(ListC([SymbolC("and"), SymbolC("false"), 
  SymbolC("true"), ListC([SymbolC("and"), SymbolC("true"), SymbolC("true")])])), 
  "and: expects 2 arguments, but found 1 extra parts");
checkError(() => parseExpression(ListC([SymbolC("and"), SymbolC("true"), 
  SymbolC("false"), SymbolC("true"), ListC([SymbolC("and"), SymbolC("true"), 
  SymbolC("true")])])), "and: expects 2 arguments, but found 2 extra parts");

// or
checkExpectExpression(parseExpression(ListC([SymbolC("or"), SymbolC("true"), 
  SymbolC("true")])), OrE(BoolE(true), BoolE(true)), "or correct case"); 
checkError(() => parseExpression(ListC([SymbolC("or")])), 
  "or: expects at least 2 arguments, but found none");
checkError(() => parseExpression(ListC([SymbolC("or"), SymbolC("false")])),
  "or: expects at least 2 arguments, but found only 1");
checkError(() => parseExpression(ListC([SymbolC("or"), SymbolC("false"), 
  SymbolC("true"), ListC([SymbolC("and"), SymbolC("true"), SymbolC("true")])])),
  "or: expects 2 arguments, but found 1 extra parts");
checkError(() => parseExpression(ListC([SymbolC("or"), SymbolC("true"), 
  SymbolC("false"), SymbolC("true"), ListC([SymbolC("and"), SymbolC("true"), 
  SymbolC("true")])])), "or: expects 2 arguments, but found 2 extra parts");

// if
checkError(() => parseExpression(ListC([SymbolC("if")])), 
  "if: expected a question and two answers, but nothing's there");
checkError(() => parseExpression(ListC([SymbolC("if"), NumberC(1)])), 
  "if: expected a question and two answers, but found only 1 part");
checkError(() => parseExpression(ListC([SymbolC("if"), NumberC(1), 
  NumberC(2)])), 
  "if: expected a question and two answers, but found only 2 parts");
checkError(() => parseExpression(ListC([SymbolC("if"), NumberC(1), 
  NumberC(2), NumberC(3), NumberC(4)])), 
  "if: expected a question and two answers, but found 4 parts");
checkExpectExpression(parseExpression(ListC([SymbolC("if"), NumberC(1), 
  SymbolC("true"), SymbolC("false")])), IfE({
    boolExpr: NumE(1),
    trueExpr: BoolE(true), 
    falseExpr: BoolE(false)
  }), "or correct case"); 

// cond 
checkError(() => parseExpression(ListC([SymbolC("cond")])), 
  "cond: expected a clause after cond, but nothing's there");
checkError(() => parseExpression(ListC([SymbolC("cond"), NumberC(1)])), 
  "expected a clause with a question and an answer, but found a number");
checkError(() => parseExpression(ListC([SymbolC("cond"), SymbolC("true")])), 
  "expected a clause with a question and an answer, but found something else");
checkError(() => parseExpression(ListC([SymbolC("cond"), ListC([])])), 
  "cond: expected a clause with a question and an answer, but found an empty part");
checkError(() => parseExpression(ListC([SymbolC("cond"), 
  ListC([SymbolC("true")])])), 
  "cond: expected a clause with a question and an answer, but found a clause with only one part");
checkError(() => parseExpression(ListC([SymbolC("cond"), 
  ListC([SymbolC("true"), NumberC(4)]), ListC([SymbolC("true"), NumberC(4), 
  NumberC(5)])])), 
  "cond: expected a clause with a question and an answer, but found a clause with 3 parts");
checkExpectExpression(
  parseExpression(ListC([SymbolC("cond"), ListC([SymbolC("true"), 
    ListC([SymbolC("and"), SymbolC("true"), SymbolC("false")])])])),
  CondE([{
    conditionExpr: BoolE(true), 
    resultExpr: AndE(BoolE(true), BoolE(false))
  }]), "cond: correct case");
checkExpectExpression(parseExpression(ListC([SymbolC("cond"), 
  ListC([SymbolC("true"), ListC([SymbolC("and"), SymbolC("true"), 
  ListC([SymbolC("and"), SymbolC("true"), SymbolC("false")])])])])),
  CondE([{
    conditionExpr: BoolE(true), 
    resultExpr: AndE(BoolE(true), AndE(BoolE(true), BoolE(false)))
  }]), "cond: correct case (nested expr)");

// let 
checkError(() => parseExpression(read("(let)")), 
  "let: expected at least one binding (in parentheses) after let, but nothing's there");
checkError(() => parseExpression(read("(let () )")), 
  "let: expected an expression after the bindings, but nothing's there");
checkError(() => parseExpression(read("(let () 3 4 5 6)")),
  "let: expected only one expression after the bindings, but found 3 extra part(s)");
checkExpectExpression(parseExpression(read("(let () 3)")),
  LetE({
    letPairs: [], 
    letBody: NumE(3)
  }),
  "let: correct number of arguments for the body case");
checkExpectExpression(parseExpression(read("(let ((x 4) (y 5)) 3)")),
  LetE({
    letPairs: [{
      pairName: Name("x"), 
      pairExpr: NumE(4),
    }, 
    {
      pairName: Name("y"),
      pairExpr: NumE(5),
    }], 
    letBody: NumE(3)
  }),
  "let: multiple pairs case");
checkExpectExpression(parseExpression(
  read("(let ((x 4) (y (let ((x 5)) x))) 3)")),
  LetE({
    letPairs: [{
      pairName: Name("x"), 
      pairExpr: NumE(4),
    }, 
    {
      pairName: Name("y"),
      pairExpr: LetE({
        letPairs: [{
          pairName: Name("x"), 
          pairExpr: NumE(5),
        }],
        letBody: NameE(Name("x"))
      }),
    }], 
    letBody: NumE(3)
  }),
  "let: nested let case");
checkError(() => parseExpression(read("(let (()) )")), 
  "let: expected a binding with a variable and an expression, but found a part");
checkError(() => parseExpression(read("(let (((3))) )")), 
  "let: expected a binding with a variable and an expression, but found a part");
checkError(() => parseExpression(read("(let ((x)) )")), 
  "let: expected an expression after the name x, but nothing's there");
checkError(() => parseExpression(read("(let ((x 3)) )")), 
  "let: expected an expression after the bindings, but nothing's there");
checkError(() => parseExpression(read("(let ((x 3) (y)) )")), 
  "let: expected an expression after the name y, but nothing's there");
checkError(() => parseExpression(read("(let ((x 3 4 5)) )")), 
  "let: expected only one expression after the name x, but found 2 extra part(s)");
checkError(() => parseExpression(read("(let (3) 3)")), 
  "let: expected a binding with a variable and an expression, but found a number");
checkError(() => parseExpression(read("(let (true) 5)")), 
  "let: expected a binding with a variable and an expression, but found something else");
checkError(() => parseExpression(read("(let ((3)) 5)")), 
  "let: expected a variable for the binding, but found a number");

// lambda 
checkError(() => parseExpression(read("(lambda)")),
  "lambda: expected (lambda (variable ...) expression), but nothing's there");
checkError(() => parseExpression(read("(lambda 3)")),
  "lambda: expected (lambda (variable ...) expression), but found something else");
checkError(() => parseExpression(read("(lambda hi)")),
  "lambda: expected (lambda (variable ...) expression), but found something else");
checkError(() => parseExpression(read("(lambda 3 3)")),
  "lambda: expected (lambda (variable ...) expression), but found something else");
checkError(() => parseExpression(read("(lambda (3) )")),
  "lambda: expected a variable, but found a number");
checkError(() => parseExpression(read("(lambda (x 3) )")),
  "lambda: expected a variable, but found a number");
checkError(() => parseExpression(read("(lambda (()) )")),
  "lambda: expected a variable, but found a part");
checkExpectExpression(parseExpression(read("(lambda (+ hi one two) 3)")), 
  LambdaE({
    nameList: [Name("+"), Name("hi"), Name("one"), Name("two")], 
    lambdaBody: NumE(3)
  }), 
  "lambda: multiple names in list case"
); 
checkExpectExpression(parseExpression(read("(lambda () 3)")), 
  LambdaE({
    nameList: [], 
    lambdaBody: NumE(3)
  }), 
  "lambda: empty nameList case"
);
checkError(() => parseExpression(read("(lambda () 3 false ())")),
  "lambda: expected only one expression for the function body, but found 2 extra part(s)");

// built-ins 
checkError(() => parseExpression(read("(+)")), 
  "+: expected two expressions, but found 0");
checkError(() => parseExpression(read("(+ () 4)")), 
  "function call: expected a function after the open parenthesis, but nothing's there");
checkExpectExpression(parseExpression(read("(+ 3 a)")), 
  ApplicationE([NameE(Name("+")), NumE(3), NameE(Name("a"))]), 
  "+: correct syntax"
);

checkError(() => parseExpression(read("(- 2)")), 
  "-: expected two expressions, but found 1");
checkExpectExpression(parseExpression(read("(- 3 true)")), 
  ApplicationE([NameE(Name("-")), NumE(3), BoolE(true)]), 
  "-: correct syntax"
);

checkError(() => parseExpression(read("(* 2 3 5)")), 
  "*: expected two expressions, but found 3");
checkExpectExpression(parseExpression(read("(* 3 empty)")), 
  ApplicationE([NameE(Name("*")), NumE(3), EmptyE]), 
  "*: correct syntax"
);

checkError(() => parseExpression(read("(/ 2 3 45 1)")), 
  "/: expected two expressions, but found 4");
checkExpectExpression(parseExpression(read("(/ 3 3)")), 
  ApplicationE([NameE(Name("/")), NumE(3), NumE(3)]), 
  "/: correct syntax"
);

checkExpectExpression(parseExpression(read("(remainder 3 (+ 0 1))")), 
  ApplicationE([NameE(Name("remainder")), NumE(3), 
    ApplicationE([NameE(Name("+")), NumE(0), NumE(1)])]), 
  "remainder: nested application expression with correct syntax"
);

checkExpectExpression(parseExpression(read("(remainder 3 (+ 0 1))")), 
  ApplicationE([NameE(Name("remainder")), NumE(3), 
    ApplicationE([NameE(Name("+")), NumE(0), NumE(1)])]), 
  "remainder: nested application expression with correct syntax"
);

checkExpectExpression(parseExpression(read("(= 0 0)")), 
  ApplicationE([NameE(Name("=")), NumE(0), NumE(0)]), 
  "=: correct syntax"
);

checkExpectExpression(parseExpression(read("(> 0 0)")), 
  ApplicationE([NameE(Name(">")), NumE(0), NumE(0)]), 
  ">: correct syntax"
);

checkExpectExpression(parseExpression(read("(< 0 0)")), 
  ApplicationE([NameE(Name("<")), NumE(0), NumE(0)]), 
  "<: correct syntax"
);

checkExpectExpression(parseExpression(read("(<= 0 0)")), 
  ApplicationE([NameE(Name("<=")), NumE(0), NumE(0)]), 
  "<=: correct syntax"
);

checkExpectExpression(parseExpression(read("(>= 0 0)")), 
  ApplicationE([NameE(Name(">=")), NumE(0), NumE(0)]), 
  ">=: correct syntax"
);

checkExpectExpression(parseExpression(read("(equal? 0 0)")), 
  ApplicationE([NameE(Name("equal?")), NumE(0), NumE(0)]), 
  "equal?: correct syntax"
);

checkExpectExpression(parseExpression(read("(cons 0 0)")), 
  ApplicationE([NameE(Name("cons")), NumE(0), NumE(0)]), 
  "cons: correct syntax"
);

checkError(() => parseExpression(read("(number? () ())")), 
  "function call: expected a function after the open parenthesis, but nothing's there");
checkExpectExpression(parseExpression(read("(number? 1)")), 
  ApplicationE([NameE(Name("number?")), NumE(1)]), 
  "number?: correct syntax"
);

checkError(() => parseExpression(read("(zero? () () ())")), 
  "function call: expected a function after the open parenthesis, but nothing's there");
checkExpectExpression(parseExpression(read("(number? (+ 3 4))")), 
  ApplicationE([NameE(Name("number?")), 
    ApplicationE([NameE(Name("+")), NumE(3), NumE(4)])]), 
  "number?: correct syntax"
);

checkError(() => parseExpression(read("(first (()) )")), 
  "function call: expected a function after the open parenthesis, but nothing's there");
checkExpectExpression(parseExpression(read("(first (cons 1 empty))")), 
  ApplicationE([NameE(Name("first")),
    ApplicationE([NameE(Name("cons")), NumE(1), EmptyE])]), 
  "first: correct syntax"
);

checkError(() => parseExpression(read("(rest ((())))")), 
  "function call: expected a function after the open parenthesis, but nothing's there");
checkExpectExpression(parseExpression(read("(rest (cons 1 empty))")), 
  ApplicationE([NameE(Name("rest")),
    ApplicationE([NameE(Name("cons")), NumE(1), EmptyE])]), 
  "rest: correct syntax"
);

checkExpectExpression(
  parseExpression(read("(empty? (lambda () (lambda () empty)))")), 
  ApplicationE([NameE(Name("empty?")),
    LambdaE({
      nameList: [], 
      lambdaBody: LambdaE({
        nameList: [], 
        lambdaBody: EmptyE
      })
    })
  ]),
  "empty?: nested lambdas"
);

checkExpectExpression(
  parseExpression(read("(empty? (let ((x empty)) (let ((y empty)) y)))")), 
  ApplicationE([NameE(Name("empty?")),
    LetE({
      letPairs: [{
        pairName: Name("x"),
        pairExpr: EmptyE,
        }],
      letBody: LetE({
        letPairs: [{
          pairName: Name("y"), 
          pairExpr: EmptyE,
        }],
        letBody: NameE(Name("y"))
      })
      })], 
  ),
  "empty?: nested lets"
);

checkExpectExpression(parseExpression(read("(cons? 0)")), 
  ApplicationE([NameE(Name("cons?")), NumE(0)]), 
  "cons?: correct syntax"
);

checkExpectExpression(parseExpression(read("(not true)")), 
  ApplicationE([NameE(Name("not")), BoolE(true)]), 
  "not: correct syntax"
);

/* nested lets & lambda expressions
  accounted for above: 
    let in let body: 
    lambda in lambda body: 

  accounted for below: 
    lambda in let body 
    let in let bindings 
    lambda in let bindings
    let in lambda body
*/ 

checkExpectAbstractProgram(parse(readAll("(let ((x (let ((y 5)) y))) x)")), 
  [Expression(LetE({
    letPairs: [{
      pairName: Name("x"),
      pairExpr: LetE({
        letPairs: [{
          pairName: Name("y"),
          pairExpr: NumE(5),
        }],
        letBody: NameE(Name("y"))
      })
    }],
    letBody: NameE(Name("x"))
  }))], "let nested in let bindings test case"); 

checkExpectAbstractProgram(parse(readAll("(define a (let () (lambda () 5)))")),
  [Definition((
    Name("a"), 
    LetE({
      letPairs: [], 
      letBody: LambdaE({
        nameList: [], 
        lambdaBody: NumE(5)
      })
    })
  ))], "lambda nested in let body test case");

checkExpectAbstractProgram(
  parse(readAll("(define a (let ((x (lambda () 5))) x))")),
  [Definition((
    Name("a"), 
    LetE({
      letPairs: [{
        pairName: Name("x"),
        pairExpr: LambdaE({
          nameList: [], 
          lambdaBody: NumE(5)
      })
      }], 
      letBody: NameE(Name("x"))
    })
  ))], "lambda nested in let bindings test case");

checkExpectAbstractProgram(
  parse(readAll("(lambda () (let () 5))")),
  [Expression((
    LambdaE({
      nameList: [], 
      lambdaBody: LetE({
        letPairs: [], 
        letBody: NumE(5)
      })
    })
  ))], "let nested in the lambda body");

// sample program (from hw #2)
checkExpectAbstractProgram(
  parse(readAll("(define double-one (lambda (n alon) (cons (* 2 n) alon))) (double-one 1 (cons 3 (cons 4 (cons 5 empty))))")), 
  [Definition((
    Name("double-one"), 
    LambdaE({
      nameList: [Name("n"), Name("alon")], 
      lambdaBody: ApplicationE([NameE(Name("cons")), 
        ApplicationE([NameE(Name("*")), NumE(2), NameE(Name("n"))]),
        NameE(Name("alon"))])
    })
  )), Expression(
    ApplicationE([NameE(Name("double-one")), 
      NumE(1), ApplicationE([NameE(Name("cons")), NumE(3),
        ApplicationE([NameE(Name("cons")), NumE(4), 
          ApplicationE([NameE(Name("cons")), NumE(5), EmptyE])])])])
  )], 
  "nested program case for hw #2"
); 

/* CHECK EXPECTS FOR EVAL */ 
// + 
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("+")))), "+", "+: checking printedRep");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(+ 1 2)")))), "3", "+: checking functionality of +");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(+ 1 (+ 1 1))")))), "3", "+: checking nested functionality of +");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(+ 1 false)")))), "+: expects a number, given #false");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(+ 1 +)")))), "+: expects a number, given +");

// -
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(- 4 3)")))), "1", "-: checking functionality of -");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(- 3 (+ 4 2))")))), "-3", "-: checking nested functionality of - and +");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(- 1 false)")))), "-: expects a number, given #false");

// *
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(* 4 3)")))), "12", "*: checking functionality of *");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(* 0 (+ 4 2))")))), "0", "*: checking nested functionality of * and +");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(* 1 true)")))), "*: expects a number, given #true");

// /
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(/ 4 3)")))), "1", "/: checking functionality of /");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(/ 5 (+ 4 1))")))), "1", "/: checking nested functionality of / and +");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(/ false true)")))), "/: expects a number, given #true");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(/ false 4)")))), "/: expects a number, given #false");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(/ 10 0)")))), "/: division by zero");

// remainder
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(remainder 10 3)")))), "1", 
  "remainder: checking functionality of remainder")
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(remainder 10 (* 2 3))")))), "4", 
  "remainder: checking functionality of remainder and *")
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(remainder false true)")))), 
  "remainder: expects a number, given #true");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(remainder false 4)")))), "remainder: expects a number, given #false");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(remainder 10 0)")))), "division by zero");

// =
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(= 3 3)")))), "#true", "=: checking functionality of = case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(= 3 8)")))), "#false", "=: checking functionality of = case 2");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(= 10 false)")))), "=: expects a number, given #false");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(= true false)")))), "=: expects a number, given #true");

// equal?
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(equal? 3 3)")))), "#true", 
  "equal?: checking functionality of equal? case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(equal? 3 8)")))), "#false", 
  "equal?: checking functionality of equal? case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(equal? true true)")))), "#true", 
  "equal?: checking functionality of equal? case 3");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(equal? true 3)")))), "#false", 
  "equal?: checking functionality of equal? case 4");

// <
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(< 3 3)")))), "#false", "<: checking functionality of < case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(< 3 8)")))), "#true", "<: checking functionality of < case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(< 4 2)")))), "#false", "<: checking functionality of < case 3");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(< true false)")))), "<: expects a number, given #true");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(< 5 false)")))), "<: expects a number, given #false");

// >
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(> 3 3)")))), "#false", ">: checking functionality of > case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(> 3 8)")))), "#false", ">: checking functionality of > case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(> 4 2)")))), "#true", ">: checking functionality of > case 3");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(> true false)")))), ">: expects a number, given #false");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(> 5 false)")))), ">: expects a number, given #false");

// <=
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(<= 3 3)")))), "#true", "<=: checking functionality of <= case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(<= 3 8)")))), "#true", "<=: checking functionality of <= case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(<= 4 2)")))), "#false", "<: checking functionality of <= case 3");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(<= true false)")))), "<=: expects a number, given #true");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(<= 5 false)")))), "<=: expects a number, given #false");

// >=
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(>= 3 3)")))), "#true", ">=: checking functionality of >= case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(>= 3 8)")))), "#false", ">=: checking functionality of >= case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(>= 4 2)")))), "#true", ">=: checking functionality of >= case 3");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(>= true false)")))), ">=: expects a number, given #false");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(>= 5 false)")))), ">=: expects a number, given #false");

// number? 
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(number? 1)")))), "#true", 
  "number?: checking functionality of number? case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(number? false)")))), "#false", 
  "number?: checking functionality of number? case 2");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(number? true false)")))), 
  "number?: expected one expression, but found 2");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(number?)")))), "number?: expected one expression, but found 0");


// zero? 
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(zero? 1)")))), "#false", 
  "zero?: checking functionality of zero? case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(zero? 0)")))), "#true", 
  "zero?: checking functionality of zero? case 2");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(zero? true)")))), "zero?: expects a number, given #true");

// cons
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(read(
  "(cons true (cons false empty))")))), "(cons #true (cons #false empty))", 
  "cons: checking functionality of cons case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(read(
  "(cons 1 (cons 3 empty))")))), "(cons 1 (cons 3 empty))", 
  "cons: checking functionality of cons case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(read(
  "(cons 3 empty)")))), "(cons 3 empty)", 
  "cons: checking functionality of cons case 3");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(read(
  "(cons true (cons false empty))")))), "(cons #true (cons #false empty))", 
  "cons: checking functionality of cons case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cons 1 (cons 3 empty))")))), "(cons 1 (cons 3 empty))", "cons: checking functionality of
  cons case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cons 3 empty)")))), "(cons 3 empty)", 
  "cons: checking functionality of cons case 3");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cons (cons 1 (cons 2 empty)) empty)")))), 
  "(cons (cons 1 (cons 2 empty)) empty)", 
  "cons: checking functionality of cons case 4");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cons empty empty)")))), "(cons empty empty)", 
  "cons: checking functionality of cons case 5");

// first
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(first (cons 3 empty))")))), "3", 
  "first: checking functionality of first case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(first (cons 3 (cons 4 empty)))")))), "3", 
  "first: checking functionality of first case 2");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(first empty)")))), 
  "first: expects a non-empty list; given: '()");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(first 1 2)")))), 
  "first: expected one expression, but found 2");

// rest
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(rest (cons 3 empty))")))), "empty", 
  "rest: checking functionality of rest case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(rest (cons 3 (cons 4 empty)))")))), "(cons 4 empty)", 
  "rest: checking functionality of rest case 2");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(rest empty)")))), 
  "rest: expects a non-empty list; given: '()");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(rest 1 2)")))), 
  "rest: expected one expression, but found 2");

// empty?
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(empty? (cons 1 (cons 2 (cons 3 empty))))")))), "#false", 
  "empty?: checking functionality of empty? case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(empty? (cons 1 empty))")))), "#false", 
  "empty?: checking functionality of empty? case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(empty? empty)")))), "#true", 
  "empty?: checking functionality of empty? case 3");

// cons?
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cons? (cons 1 (cons 2 (cons 3 empty))))")))), "#true", 
  "cons?: checking functionality of cons? case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cons? (cons 1 empty))")))), "#true", 
  "cons?: checking functionality of cons? case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cons? empty)")))), "#false", 
  "cons?: checking functionality of cons? case 3");

// not 
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(not true)")))), "#false", 
  "not: checking functionality of not case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(not false)")))), "#true", "not: checking functionality of not case 2");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(not 1)")))), "not: expected either #true or #false; given 1");

// and
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(and true (or true false))")))), "#true", 
  "StringOfValue And: correct syntax case 1 (nested)");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(and false (or true false))")))), "#false", 
  "StringOfValue And: correct syntax case 2 (nested)"); 
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(and 1 false)")))), "and: question result is not true or false: 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(and false 1)")))), "#false", 
  "and: double-checking the short-circuiting");


// or
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(or false true)")))), "#true", 
  "or: correct syntax case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(or false (or true false))")))), "#true", 
  "StringOfValue or: correct syntax case 2 (nested)"); 
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(or 1 false)")))), "or: question result is not true or false: 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(or true 1)")))), "#true", 
  "or: double-checking the short-circuiting");

// if 
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(read("(if true 
  true false)")))), "#true", "StringOfValue if: correct syntax case 1")
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read("(if 3 true false)")))), 
  "if: question result is not true or false: 3");

// cond
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cond ((= 1 3) 2) ((= 2 3) 3) ((= 3 3) 4))")))), "4", 
  "cond: checking functionality of cond case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("(cond ((equal? true 3) 2) ((equal? false 4) 3) ((equal? 4 4) 4))")))), 
  "4", "cond: checking functionality of cond case 2");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read ("(cond ((= 0 3) 2) ((= 1 3) 3) ((= 2 3) 4))")))), 
  "cond: all question results were false");
checkError(() => stringOfValue(eval(initialTle, [], parseExpression(
  read ("(cond ((+ 3 1) 2) ((= 1 3) 3) ((= 2 3) 4))")))), 
  "condition expression evaluated to a non-boolean value");

// let 
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(read
("(let ((x 3)
      (y 4))
  (+ x y))")))), "7", "let: checking functionality of let case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(read
("(let ((x 3)) x)")))), "3", "let: checking functionality of let case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(read
("(let ((x 3))
  (let ((y 4))
    (* x y)))")))), "12", "let: checking functionality of let case 3");

// lambda
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("((lambda (x) (+ x 4)) 3)")))), "7", 
  "lambda: checking functionality of lambda case 1");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("((lambda (x) (+ 9 0)) 3)")))), "9", 
  "lambda: checking functionality of lambda case 2");
checkExpect(stringOfValue(eval(initialTle, [], parseExpression(
  read("((lambda (x y) (* x y)) 1 1)")))), "1", 
  "lambda: checking functionality of lambda case 3");

/* CHECK EXPECTS FOR STRING OF VALUE */ 
checkExpect(stringOfValue(NumV(4)), "4", "string of values: number case"); 
checkExpect(stringOfValue(BoolV(true)), "#true" , 
  "string of values: number case");
checkExpect(stringOfValue(ListV([])), "empty", 
  "string of values: empty list case"); 
checkExpect(stringOfValue(ListV([NumV(4)])), "(cons 4 empty)", 
  "string of values: one element list case"); 
checkExpect(stringOfValue(ListV([NumV(4), NumV(4)])), "(cons 4 (cons 4 empty))", 
  "string of values: multiple element list case"); 

// built in stuff goes here
checkExpect(stringOfValue(ClosureV({
  cNameList: [], 
  cExpr: NumE(5), 
  cEnv: [[]],
})), "(lambda () ...)", "string of values: closure with no arguments");
checkExpect(stringOfValue(ClosureV({
  cNameList: [Name("x")], 
  cExpr: NumE(5), 
  cEnv: [[]],
})), "(lambda (a1) ...)", "string of values: closure with one argument");
checkExpect(stringOfValue(ClosureV({
  cNameList: [Name("x"), Name("y"), Name("z")], 
  cExpr: NumE(5), 
  cEnv: [[]],
})), "(lambda (a1 a2 a3) ...)",
"string of values: closure with three arguments");

// complicated programs...
checkExpect(rackette("
(define alon (cons 1 (cons 2 (cons 4 empty))))
(cons 17 alon)
"), 
["(cons 17 (cons 1 (cons 2 (cons 4 empty))))"
], "hw #1 problem"); 

checkExpect(rackette("(define doubleAll 1)"), [], "hyphens in name");

checkExpect(rackette("
(define doubleAll (lambda (alon)
                     (cond ((empty? alon) empty)
                           ((cons? alon) (cons (* 2 (first alon))
                                               (doubleAll (rest alon)))))))
(doubleAll empty)
(doubleAll (cons 1 (cons 8 empty)))
(doubleAll (cons 4 empty))
"), 
["empty", "(cons 2 (cons 16 empty))", "(cons 8 empty)"
], "hw #3 problem"); 

checkExpect(rackette("
(cons empty (cons (cons 2 empty) (cons 3 empty)))
"), ["(cons empty (cons (cons 2 empty) (cons 3 empty)))"], "nested lists");

checkExpect(rackette("
(define foldr (lambda (proc base alod)
  (cond
    ((empty? alod) base)
    ((cons? alod) (proc (first alod) (foldr proc base (rest alod)))))))

(define my-map
  (lambda (proc alod) 
  (foldr (lambda (d alod1) (cons (proc d) alod1)) empty alod)))

(define my-filter (lambda (pred alod)
  (foldr
   (lambda (d alod1) (if (pred d)
                         (cons d alod1)
                         alod1)) empty alod)))
                         
(my-filter cons? (cons empty (cons (cons 1 empty) empty)))
"), 
["(cons (cons 1 empty) empty)"], "hw #5"); 
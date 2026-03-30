Instructions: Users can enter in strings containing code of a much smaller 
    variant of the Racket programming language. Rackette will return errors for 
    syntactically invalid code and (for the most part) return the values of any 
    top-level expressions back to the user in a list of strings. 

    Important differences (other than the restriction of expressions and built-in 
    procedures specified in the handout) include
        - the replacement of square brackets with parentheses in cond 
        - defining variables with functions as values using lambda and not of 
            the form (define ([name] [args]*) [body])
        - the removal of floats from the programming language 
    among others. 

Program Overview: The raw program the user inputs gets passed to readAll, which 
    converts the raw program (string) into a concrete program. A concrete 
    program represents the "hard-coded" values of numbers, symbols, and lists 
    of the two. ConcretePrograms are lists of ConcreteProgramPieces, which 
    represent the lines that users would type out in an IDE like DrRacket. 
    
    Our function parse transforms each concreteProgramPiece into an 
    abstractProgramPiece, which assigns speicifc meanings to each of the 
    symbols and also specifies an order of processing that we use later. At this
    stage, each abstractProgramPiece represents either a definition, which adds
    to the environment of variables/procedures in our program, or an expression, 
    which should return some kind of value. To ensure that we transform every
    concreteProgramPiece into an abstractProgramPiece, we use map in ReasonML. 

    To get those values or modify the environment, we pass our 
    abstractProgramPiece into eval. eval transforms expressions into data types 
    we store as "values". We then map eval onto our abstractProgram to get a 
    list of values.

    Finally, to return the display the outputs of the program, we pass each 
    value into stringToValue to get a string that we can display to the user 
    that represents each value. We map stringToValue over our list of values 
    from eval, and then return the resulting list of strings. 

Bugs: While testing our code on HW #5, we tried applying my-map with cons? 
    passed as the procedure to (list empty (list 2) (list 3)). Rackette 
    outputted the cons form of (list #f #t #f), which differed from the correct 
    output: (list #f #t #t). We couldn't figure out why. 

People: Eric Kim, Roger Mao

"Extra" Features: We tried to be particular with the error messages we returned 
for parse. 

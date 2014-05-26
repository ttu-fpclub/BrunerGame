;;In this file I will give different examples of the directives and syntax for different options for the format function.

;Format has found options for the first parameter
(format t "Hello World") ;Output to the stanard output stream
(format nil "Hello World") ; format returns the generated output.
(format *stream* "Hello World") ;format will output to the specified stream
(format *string* "Hello World") ;Strings with fill pointers the formatted output 
                                ;is added to the end of the string and the fill pointer
                                ;adjusted
(format t "~$" pi) ;output a floating point variable Default 2 places
(format t "~9$" pi) ;first option is how many places to print
(format t "~V$" 10 pi) ;V consumes one format arguement and replaces itself with that value.
(format t "~#$" pi) ;# is replaced with number of remaining format arguements.

;to ignore parameters for a directive just place a comma for each one
(format t "~,9f" pi) ;F is another directive for floating point values
                     ;However the number of places to display is the second arguement.
(format t "~d" 1000000) ; D outputs a integer value
(format t "~:d" 1000000) ; : causes d to output commas (i.e. 1,000,000)
(format t "~@d" 1000000) ; @ caeses d to output a plus sign if positive (i.e. +1000000)
(format t "~4,'0d-~2,'0d-~4,'0d" 2014 6 1) ;output format example 
;A format option is for asthetic.
;It outputs the variables in a human-readable form
(format t "~a" 10) ;output 10
(format t "~a" "Hello World") ;Hello World
(format t "~a" (list 1 2 3) ; (1 2 3)
(format t "~20a" "Hello") ;appends 15 spaces to Hello string
(format t "~:a" nil) ; => ()
(format t "~a" nil) ; => nil
(format t "~20@a" "Hello") ;specifies there must be at least a column count of 20 on from 
                           ;the right of the output "               Hello"
;S output can be read with the READ function 
;S accepts all the same arguements as A
(format t "~s" 10) ; => 10
(format t "~s" "Hello World") ; => "Hello World"
(format t "~s" (list 1 2 3) ; => (1 2 3)
(format t "~s" nil) ; => ()
(format t "~s" nil) ; => nil

;New Line Direcives
(format t "~%") ;emits a new line
(format t "~&") ;emits a new line only if not at the beginning of a line
(format t "~9%") ;emits 9 new line characters
(format t "~9&") ;emits 9 or 8 new line characters depending on current line.
;Character directive
(format t "~C" #\n)
(format t "~:C" #\space) ;outputs name of special characters such as whitespace
(format t "~@C" #\space) ;outputs representation of character #\[character]
;Similar to the base-10 directive there are 3 others
;base 16
(format t "~x" 16); => 10
;base 8
(format t "~o" 8) ; => 10
;base 2 (binary)
(format t "~b" 2) ; => 10
;base r (where r is a parameter between 2-36)
(format t "~vr" 4 4); => 10 base 4

;More floating point directives
;modifiers
;1st = number of minimum digits printed after the decimal point
;2nd = number of digits to print before the decimal point if they exist;
;@ prints + or - before number
;F
(format t "~f" pi) ;outputs a floating point and will use scientific notation for very large numbers
;E
(format t "~e" pi) ;always outputs a floating point in scientific notation
;$ simlar to F but used specifically for monetary values
;1st parameter number of digits to display after decimal
;2nd parameter number of digits to display before the decimal
;@symbol prints + or -
(format t "~$" pi) ; => 3.14

;R directive
(format t "~r" 1001) ;=> one thousand one
(format t "~:r" 1001) ;=> one thousand first /: symbol causes output to be in ordinal form
(format t "~@r" 1001) ;=> MI /@ symbol causes number to be written in old roman numerals 

;P directive
(format t "dollar~p" 1) ;=> dollar generates nothing since the noun is singular
(format t "dollar~p" 2) ;=> dollars generates s since the noun is pluar based out the arguement
;with colon it uses the previous format arguement
(format t "dollar~:p" 1) ;=> dollar
(format t "dollar~:p" 2) ;=> dollars
(format t "dollar~:p" 1) ;=> dollars
;with at symbol it produces either y or ies
(format t "part~@p" 1) ;=> party
(format t "part~@p" 2) ;=> parties

;~( ~) directive
(format t "~(~a~)" "LISP IS AWESOME!") ;=> lisp is awesome! /Make all inputs between the braces lowercase.
(format t "~@(~a~)" "LISP IS AWESOME!") ;=> Lisp is awesome! /makes the first letter captilized.
(format t "~:(~a~)" "LISP IS AWESOME!") ;=> Lisp Is Awesome! /captilizes the first letter of every word.
(format t "~@:(~a~)" "lisp is awesome!") ;=>LISP IS AWESOME! /captilizes all characters.

;Conditional formatting
(format t "~[Common Lisp~;Haskell~;Erlang~]" 0) ; output the item at place 0 "Common Lisp"
;Colon makes a default output; otherwise anything greater than number of options would print nothing
(format t "~[Common Lisp~;Haskell~:;Erlang~]" 4) ;any input 2 or greater produces Erlang

;A use of the # prefix with conditional formatting directive
;# tells the number of arguements left to process
;the first one we have four so it goes with ~a, ~a option
;the second we have 2 so it goes with the ~a, etc option
(format t "~#[NONE~;~a~;~a and ~a~:; ~a, ~a~]~#[~; and ~a~:;, ~a, etc~]." 'a 'c 'c 'd) ;outputs A, B, C, etc
;You can also use the @ to interate over the remaining arguements
(format t "~{~#[<empty>~;~a~;~a and ~a~:;~@{~a~#[#~;, and ~:;, ~]~}~]~:}" '(a b c d e f))

;~* directive
;skips over parameters
(format t "~*~a" 1 2) ;=> 2
;~:* backs up to last parameter
(format t "~@r ~:*(~a)" 1) ;=> I (1)

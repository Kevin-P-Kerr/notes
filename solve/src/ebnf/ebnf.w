@*
\def\title{Programming an EBNF Parser}

We wish to write a program that takes a description 
of a language and outputs a
function $$f(x) \rightarrow \lbrace 0 \vert 1 \rbrace $$, where 
x is a string of finite length, 
and 0 and 1 are the usual boolean values 
indicating if the string conforms to our grammar.

\beginsection
EBNF Definition

Our first order of business is to describe, as precisely as possible, 
the format in which we shall 
describe the grammars we wish to parse.  
Indeed, specifying such a format within that format 
itself is a good litmus test of it's generality and power.

The notation we shall adopt is called EBNF--that is, 
Extended Backus-Nauer Form. This is given in N. Wirth's
{\it Compiler Construction}, as follows:
$$syntax=\lbrace production \rbrace.$$
$$production=identifier"="expression"."$$
$$expression=term\lbrace term \rbrace.$$
$$term=factor\brace factor \rbrace.$$
$$factor=identifier\vert string \vert "(" expression ")"
\vert "\lbrack" expression "\rbrack" \vert 
"\lbrace" expression "\rbrace".
$$identifier=letter\lbrace digit\vert letter \rbrace.$$
$$string="""\lbrace character \rbrace"""$$
$$letter="A".\vert.."Z"
$$digit="0".\vert.."9"$$


@* Index.

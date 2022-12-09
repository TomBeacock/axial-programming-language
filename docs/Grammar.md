# Axial Formal Grammar

## Main
```xml
<program>       ::= <function> | <function> <program>

<function>      ::= func <identifier> (<parameters>) = <block> end
                | func <identifier> (<parameters>) -> <type> = <block> end

<parameters>    ::= <parameter> | <parameter> , <parameters>

<parameter>     ::= <identifer> <type>

<block>         ::= <statement> | <statement> <block>

<statement>     ::= <declaration> ;
                | <assign> ;
                | <if>
                | <while>
                | <return> ;

<declaration>   ::= var <identifier> <type>
                | var <identifier> = <expression>
                | var <identifier> <type> = <expression>

<assign>        ::= <identifier> = <expression>

<if>            ::= if <expression> then <block> end
                | if <expression> then <block> <else>
                | if <expression> then <block> <else-if>

<else-if>       ::= else if <expr> then <block> end
                | else if <expr> then <block> <else>
                | else if <expr> then <block> <else-if>

<else>          ::= else <block> end

<while>         ::= while <expression> do <block> end

<return>        ::= return <expression>
```

## Identifier
```xml
<identifier>    ::= <underscores> <alpha> <id-char>
<underscores>   ::= _ <underscores> | ε
<id-char>       ::= <alphaNumUnder> <id-char> | ε
<alphaNumUnder> ::= <alpha> | 0..9 | _
<alhpa>         ::= a..z | A..Z
```

## Expression

```xml
<expression>    ::= <expression> + <term>
                | <expression> - <term>
                | <term>

<term>          ::= <expression> * <factor>
                | <expression> / <factor>
                | <expression> % <factor>
                | <factor>

<factor>        ::= <literal>
                | + <factor>
                | - <factor>
                | ( <expression> )
```
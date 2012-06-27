js-parser
=========

[![Build Status](https://secure.travis-ci.org/kengonakajima/js-parser.png)](http://travis-ci.org/kengonakajima/js-parser)

Parse and convert JavaScript to Sexp with Ruby/Racc



WHAT
====
Parse and convert JavaScript source into Ruby's executable S expression. 

It parses full node.js libst source (about 20kLOC) for about 30 seconds, jQuery(non-minified, 10KLOC) for about 0 seconds.

Status
====
In early experiments, but should parse most of existing code.


Build : no dependency on JavaScript VMs.
====

Depends only on Ruby and Racc.

Type this:

    > git clone https://github.com/kengonakajima/js-parser.git
    > cd js-parser
    > make


And then you get "js2sexp" ruby script. Depends on Racc.



Usage
====

Print usage:

    > ruby ./js2sexp
    need input file(s)
    Options:
    -q : be quiet(parse only)
    -a : print in array pretty print format
    -x : test by executing out-put sexp

To convert:

    > ruby ./js2sexp hello.js

JavaScript:

    a=1
    
Ruby (with -a option):

    [:program,
     [:stmt,
      [:exp, [:asgn, [:exp, [:id, :a]], [:op, :equal], [:exp, [:lit, 1.0]]]]]]


JavaScript:

    function hello(t) {
      console.log("world")
    }
    hello();

Ruby (with -a option):

    [:program,
     [:funcdecl,
      :hello,
      [:paramlist, :t],
      [:funcbody,
       [:stmt,
        [:exp,
         [:call,
          [:exp, [:getprop, [:exp, [:id, :console]], [:id, :log]]],
          [:args, [:arglist, [:exp, [:lit, [:str, "world"]]]]]]]]]],
     [:stmt, [:exp, [:call, [:exp, [:id, :hello]], [:args]]]]]



TODO
====
 * Shorter Sexp mode (It's a trade-off. If longer Sexp, easier to use. so it could be switchable. )
 * print line number when syntax error
 
See Also
====
JavaScript syntax in BNF is originally from  [JS BNF for kmyacc](https://gist.github.com/2963520)

Thank you for [hypzwm](http://www.h4.dion.ne.jp/~unkai/)


License
====
Apache2.0

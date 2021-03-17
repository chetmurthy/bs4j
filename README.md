
What is wrong with YAML?

(1) Complexity of the spec

you cannot divide the spec cleanly into lexical and grammatical
layers.  That is to say, the meaning of a character (e.g. ":") depends
on context in a way that is intricatedly tied-up with the grammar.
This makes it hard to write conforming implementations.  The language
spec is *enormous*, and this is a really bad thing for a language that
is supposed to simple-to-use and simple-to-understand.  Because along
with that should go: "simple to know when you have something wrong".

(2) Too many language bells&whistles that do not map to implementing languages

Things like structured dictionary keys -- how do these map to most
languages in any sort of tolerable way?  They don't.  Or
type-annotations: these are things that belong in a type-description
language, not as part of the actual data (unless the data is
fully-dynamically-typed, which, again, doesn't match many implementing
languages).

(2) Demarshalling

This complexity and size makes it hard to imagine how to write YAML
"demarshallers".  That is to say, suppose I wanted to use YAML as a
wireline for an RPC system (e.g. Thrift).  How would I write the
demarshaller?  The easiest way is to use an off-the-shelf YAML parser,
take the tree that results, and with a recursive walk, turn it into
our target data-type.  With a YAML parser that exposes events, we can
do a bit better, but even still, it's not simple.

(3) Much of the complexity is tied up with block-style scalars, and
allowing as much as possible to appear there.  So ":" and "- are
special characters, but they can also appear in block-style scalars,
and even in flow-style scalars.

(3) Variance from JSON

YAML starts off saying it's just JSON, but then it proceeds to diverge
from JSON in all sorts of ways.  Why?  What's the use, the real use,
of:

  (a) %directives
  (b) type-tags
  (c) anchors
  (d) structured (non-scalar) keys in dicts
  (e) multiple transfer-encodings (UTF8, UTF16LE, BE, UTF32, etc)
  
(a)-(d) are just not useful, and (e) should be dealt with as a
transfer-encoding issue.  It shouldn't even *appear* in the spec.

  a. maybe allowing ONE of these, before any data in the entire
     stream, would be useful.  And the only one that is useful is
     "%YAML version", because nothing else is really sensible.  The
     version directive can be used by a processor to decide if it
     supports this version of YAML.  Anything else?  Maybe it makes
     sense to ignore it, maybe it doesn't.  So the right thing to do
     is to forbid other kinds of directives.
	 
  b. type-tags: JSON has types and schema.  Reuse them.
  
  c. anchors: this is supposed to be a data-description language.
	 Flat data doesn't have pointer-structure.  Ditch these.
	 
  d. structured (non-scalar) keys in dicts: this is nuts.  So is NULL
	keys in dicts.  ditch them.

  e. multiple transfer-encodings (UTF8, UTF16LE, BE, UTF32, etc):
     should be dealt with as a transfer-encoding issue.  It shouldn't even
	 *appear* in the spec.

(4) Block Style Scalars

Block style scalars have all sorts of forms, whiz-bang features, etc.
A lot of this is to support managing indentation.  Why?  It's fine to
have block-style scalars for simple things, but the minute you get
past the simple things, why not switch to C++ raw-string-literals?
Doing this, you can preserve the property of lexing as separate from
parsing.

(5) Too Cool for School

The way in which there are all sorts of places where a value can be
omitted, and it's inferred to be null, is just too cool for school.  I
mean ... WHY DO THIS?  And JSON doesn't support NULL as a key, so why
does YAML?

Or at least, I get doing it for the value of key/value pairs, and the
members of arrays, but for KEYS?  WHy?

(6) flow style isn't JSON syntax

Why not?  I don't get it.

================================================================

What should YAML be?

1. YAML is easily readable by humans.

    And easily writable, which includes that humans can tell by easy
    inspection whether their YAML is correct or not -- it doesn't
    require running thru a processor to do that.

2. YAML data is portable between programming languages.

   FAILS

3. YAML matches the native data structures of agile languages.

   Not just "agile", but "many important" languages.  And it fails here.

4. YAML has a consistent model to support generic tools.
5. YAML supports one-pass processing.
6. YAML is expressive and extensible.

   What does extensible mean for a data-format language?  The only
   sense I can get is that it should be able to encode data that
   matches with many large and complex families of data-types.  But
   what else could it mean?

7. YAML is easy to implement and use.

   FAILS for first.

8. Every YAML document should be parseable into a JSON document structure;
   Every JSON document that passes some simple and local conformance checks
   should be a YAML document

   To whatever extend YAML needs types and type-schema, it should
   reuse those that JSON is using/defining, doing nothing new in this
   domain.

9. YAML's block-syntax MUST merely be an alternate, comfortable syntax
   for writing JSON -- nothing more.

10. YAML's specification should be small enough to be explainable in a
    few pages, and separable into regular lexical and context-free
    grammatical levels, with minimal book-keeping information in the
    lexer.
   
    Basically, the only state information the lexer should retain
    (above and beyond what lexers typically retain) between calls, is
    the indentation/flow-nesting structure of the document.  That's
    all that's necessary to properly lex, and that's all it should
    keep.  And the *only* purpose of this information should be to:
	
	a. answer how to treat initial indentation on a line -- that is,
    whether this is an indent, a dedent, or illegal.  If a dedent, how
    many indentation-levels back is it dedenting.
	
	b. decide whether, for subsequent characters on the line, we're in
    block style, or flow style.

    That is all.  Nothing more shall be inferred from the lexer state.

    YAML's lexer should be implementable using standard lexing tools;
    its parser should be implementable using standard parser tools.
    Perhaps other methods will yield more-efficient implementations,
    but these standard methods should work, so that conformant
    implementations can be rapidly created in many languages.

20. It follows from this that every JSON document SHALL be a YAML
    document, though YAML might reject it for extra well-formed-ness
    reasons (e.g. dicts must have distinct keys).

================================================================

Lexemes

'{' '}' '[' ']' ':' ','
'"' [^ '"'] '"'
false
null
true

<number>

Added for block mode

'-'
'>'
'|'
'---'
'...'
'#'

<raw-strings>

Forbidden, b/c block mode

'--'
'..'

================================================================

Plan

(1) lowest-layer lexer will recognize tokens, and also newlines and
"possibly-empty leading spaces".

(2) block-mode wrapper will track current indent-or-flow-style

When in flow-style, indent/dedent do not get passed-thru, nor do
newlines.

================================================================

Parsing plan

(1) in block mode, the start of a dict-key, or of an array-entry, sets
an indent-stop.  So

a: b: c
   d: e

is valid.

(2) also, a scalar value sets an indent-stop.  So

a: b c d
   e f g

is a single dict-entry, with value "b c d e f g" (b/c of folding)

and is equal to

a:
 b c d
 e f g

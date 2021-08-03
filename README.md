bert_convert
=====

Conversion Library for Bert/Term to Bert2

Build
-----

    $ rebar3 compile
    
Run
---

    $ ./bert_convert "<wildcard>"

or

    $ ./bert_convert <path_to_filename>

Wildcard must be a filepath, except for the following:
- Wild MUST be wrapped in quotes (\")
- "?" matches a single character
- "\*" matches any number of characters, up to the end, the next dot, or the next slash
- "\*\*" matches any files, directories, and subdirectories

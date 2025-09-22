### Simple Theorem Prover
Simple Theorem Prover, or STP for short, is a fun little project I coded just to teach myself about hand writing a lexer, parser, and interpreter to reason about natural numbers using peano axioms

### Current Issues
I realize that the parser should've been a class, something like:
```
```
class parser toks = new object (self)
  val tokens : list tokens = toks
  
  method shift = 
  match tokens with
  | _ :: tl -> tokens <- tl
  | [] -> ()
  
  method parse_small...

  method parse_big = let x = parse_small...
end
```
```

So that I can walk through the grammar instead of being limited to just recursive descent.

For now, there are better things to do, but if I want to educate others on my projects, I'll have to change it.

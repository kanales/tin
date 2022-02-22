# Tin

Tin is a lisp implemented in rust. 

## Features

- [x] Hash maps
    - [ ] standard hash map functions
- [x] Vectors
    - [ ] standard hash vector functions
- [ ] Standard "functional programming functions"
- [ ] macros

## Differences with Scheme 

Although Tin is mainly based of the Scheme dialect, it does present some differences with scheme.

### Vectors

Instead of using the `#( ... )` syntax for vectors, the `[ ... ]` is used in Tin. This is inspired
in Clojure and other (non lisp) programming languages like Python.

### Hash maps

Tin provides a hash map datatype that can be created by using the `make-hash` function on a
sequence of key-value pairs, or by using the hash construct `{ ... }`. For example: 

```scheme 
(define my-map { 'a 1 'b 2 })
```

### Index by call 

In Tin the notion of (hash)map and vector is interpreted as a _mapping_ from the index space to the
value space. Therefore, both `hash` and `vector` values can be evaluated on its indices: 

```scheme 
> ({ 'a 1 'b 2 } 'a)
1
> (['a 'b 'c] 2) ; vectors are 0-indexed *this might change in the future*
b 
```



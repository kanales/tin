# Tin


<a
   style="display: block;
  margin-left: auto;
  margin-right: auto;
  width: 50%;"
   title="Sun Ladder, CC BY-SA 3.0 &lt;https://creativecommons.org/licenses/by-sa/3.0&gt;, via Wikimedia Commons" 
   href="https://commons.wikimedia.org/wiki/File:Empty_tin_can2009-01-19.jpg">
    <img 
         width="256" alt="Empty tin can2009-01-19" 
         src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a8/Empty_tin_can2009-01-19.jpg/256px-Empty_tin_can2009-01-19.jpg">
</a>

Tin is a lisp implemented in rust. 

## Features

- [x] Hash maps
    - [ ] standard hash map functions
- [x] Vectors
    - [ ] standard hash vector functions
- [ ] Standard "functional programming functions"
- [x] macros

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


### Macros 

Macros in Tin are defined using the `defmacro` function: 

```lisp
(defmacro defn (name args body) 
    `(define ,name (lambda ,args ,body)))
(defn plus-1 (x) (+ 1 x))
```



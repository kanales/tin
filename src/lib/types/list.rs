use crate::lib::types::{Atom, Exp, Symbol};

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    head: Link,
}

type Link = Option<Box<Node>>;

#[derive(Debug, Clone, PartialEq)]
struct Node {
    elem: Exp,
    next: Link,
}

impl List {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn head(&self) -> Option<&Exp> {
        self.head.as_ref().map(|node| &node.elem)
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn as_vec(self) -> Vec<Exp> {
        self.collect()
    }

    pub fn len(&self) -> usize {
        let mut count = 0;
        let mut node = &self.head;
        while let Some(n) = node {
            count += 1;
            node = &n.next;
        }

        count
    }

    pub fn snoc(&self) -> Option<(Exp, List)> {
        let mut lst = self.clone();
        let head = lst.pop()?;
        Some((head, lst))
    }

    pub fn push(&mut self, elem: Exp) {
        let new = Box::new(Node {
            elem,
            next: self.head.take(),
        });

        self.head = Some(new);
    }

    pub fn pop(&mut self) -> Option<Exp> {
        let node = self.head.take()?;
        self.head = node.next;
        Some(node.elem)
    }
}

#[test]
fn macro_test() {
    let mut lst = List::new();
    let (x, y, z) = (
        Exp::Atom(Atom::Symbol("x".to_string())),
        Exp::Atom(Atom::Symbol("x".to_string())),
        Exp::Atom(Atom::Symbol("x".to_string())),
    );
    lst.push(x.clone());
    lst.push(y.clone());
    lst.push(z.clone());

    assert_eq!(lst, list!(x, y, z))
}

impl From<Vec<Exp>> for List {
    fn from(v: Vec<Exp>) -> Self {
        let mut this = List::new();
        for e in v.into_iter().rev() {
            this.push(e);
        }
        this
    }
}

impl Drop for List {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        while let Some(mut boxed) = cur_link {
            cur_link = boxed.next.take();
        }
    }
}

impl Iterator for List {
    type Item = Exp;
    fn next(&mut self) -> Option<Self::Item> {
        self.pop()
    }
}
use std::{iter::FromIterator, rc::Rc};

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct List<T> {
    head: Link<T>,
}

impl<T> List<T> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn cons(&self, elem: T) -> List<T> {
        List {
            head: Some(Rc::new(Node {
                elem,
                next: self.head.clone(),
            })),
        }
    }

    pub fn tail(&self) -> List<T> {
        List {
            head: self.head.as_ref().and_then(|node| node.next.clone()),
        }
    }

    pub fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }

    pub fn iter(&self) -> Iter<T> {
        Iter {
            next: self.head.as_deref(),
        }
    }

    pub fn snoc(&self) -> Option<(&T, List<T>)> {
        Some((self.head()?, self.tail()))
    }
}

type Link<T> = Option<Rc<Node<T>>>;

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
struct Node<T> {
    elem: T,
    next: Link<T>,
}

pub struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_deref();
            &node.elem
        })
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        while let Some(node) = cur_link {
            if let Ok(mut node) = Rc::try_unwrap(node) {
                cur_link = node.next.take();
            } else {
                break;
            }
        }
    }
}

impl<T> From<Vec<T>> for List<T> {
    fn from(v: Vec<T>) -> Self {
        let mut list = List::new();

        for el in v.into_iter().rev() {
            list = list.cons(el);
        }
        list
    }
}

impl<T> FromIterator<T> for List<T> {
    fn from_iter<I>(v: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        v.into_iter().collect::<Vec<_>>().into()
    }
}

#[macro_export]
macro_rules! list {
    [$($el:expr),*] => {
        vec![$($el,)*].into()
    };
}

#[cfg(test)]
mod test {
    use super::List;

    #[test]
    fn basics() {
        let list = List::new();
        assert_eq!(list.head(), None);

        let list = list.cons(1).cons(2).cons(3);
        assert_eq!(list.head(), Some(&3));

        let list = list.tail();
        assert_eq!(list.head(), Some(&2));

        let list = list.tail();
        assert_eq!(list.head(), Some(&1));

        let list = list.tail();
        assert_eq!(list.head(), None);

        // Make sure empty tail works
        let list = list.tail();
        assert_eq!(list.head(), None);
    }

    #[test]
    fn iter() {
        let list = List::new().cons(1).cons(2).cons(3);

        let mut iter = list.iter();
        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&1));
    }

    #[test]
    fn from_iter() {
        let list = List::new().cons(1).cons(2).cons(3);

        let other: List<_> = list.iter().map(|e| *e).collect();
        assert_eq!(list, other)
    }

    #[test]
    fn macro_constructor() {
        let expect = List::new().cons(1).cons(2).cons(3);
        let got = list![3, 2, 1];

        assert_eq!(expect, got)
    }
}

use persistent::list::List;

use super::types::{TinError, TinResult};

pub fn list1<A: Clone>(lst: List<A>) -> TinResult<A> {
    if lst.len() != 1 {
        return TinError::ArityMismatch(1, lst.len()).into();
    }
    Ok(lst.head().unwrap().clone())
}

pub fn list2<A: Clone>(lst: List<A>) -> TinResult<(A, A)> {
    if lst.len() != 2 {
        return TinError::ArityMismatch(1, lst.len()).into();
    }
    let mut it = lst.iter();
    Ok((it.next().unwrap().clone(), it.next().unwrap().clone()))
}

pub fn list3<A: Clone>(lst: List<A>) -> TinResult<(A, A, A)> {
    if lst.len() != 3 {
        return TinError::ArityMismatch(1, lst.len()).into();
    }
    let mut it = lst.iter();
    Ok((
        it.next().unwrap().clone(),
        it.next().unwrap().clone(),
        it.next().unwrap().clone(),
    ))
}

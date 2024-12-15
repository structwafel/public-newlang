use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LinkedList<T> {
    Node { value: T, next: Rc<LinkedList<T>> },
    Tail,
}

impl<T> Default for LinkedList<T> {
    fn default() -> Self {
        LinkedList::Tail
    }
}

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn prepend(self, value: T) -> Self {
        LinkedList::Node {
            value,
            next: Rc::new(self),
        }
    }

    pub fn iter(&self) -> ListIter<T> {
        ListIter(self)
    }

    pub fn len(&self) -> usize {
        self.iter().count()
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, LinkedList::Tail)
    }

    pub fn contains(&self, value: &T) -> bool
    where
        T: PartialEq,
    {
        self.iter().any(|item| item == value)
    }

    pub fn front(&self) -> Option<&T> {
        match self {
            LinkedList::Node { value, .. } => Some(value),
            LinkedList::Tail => None,
        }
    }

    pub fn back(&self) -> Option<&T> {
        self.iter().last()
    }

    pub fn tail(&self) -> Option<&LinkedList<T>> {
        match self {
            LinkedList::Node { next, .. } => Some(next),
            LinkedList::Tail => None,
        }
    }

    pub fn skip(&self, n: usize) -> Option<&LinkedList<T>> {
        let mut current = self;
        for _ in 0..n {
            match current {
                LinkedList::Node { next, .. } => current = next,
                LinkedList::Tail => return None,
            }
        }
        Some(current)
    }

    pub fn get(&self, n: usize) -> Option<&T> {
        self.skip(n).and_then(LinkedList::front)
    }
}

pub struct ListIter<'a, T>(&'a LinkedList<T>);

impl<'a, T> Iterator for ListIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            LinkedList::Node { value, next } => {
                self.0 = next;
                Some(value)
            }
            LinkedList::Tail => None,
        }
    }
}

#[macro_export]
macro_rules! list {
    () => {
        LinkedList::Tail
    };
    ($first:expr $(, $rest:expr)*) => {
        LinkedList::Node {
            value: $first,
            next: Rc::new(list!($($rest),*))
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn macro_test() {
        let list = list![1, 2, 3, 4, 5];

        assert_eq!(list.len(), 5);
        assert_eq!(list.is_empty(), false);
        assert_eq!(list.contains(&3), true);
        assert_eq!(list.contains(&6), false);
        assert_eq!(list.front(), Some(&1));
        assert_eq!(list.back(), Some(&5));
        assert_eq!(list.get(2), Some(&3));
        assert_eq!(list.get(10), None);
        assert_eq!(list.tail().map(|t| t.front()), Some(Some(&2)));
        assert_eq!(list.skip(2).map(|l| l.front()), Some(Some(&3)));
        assert_eq!(list.skip(10), None);

        fn check_for_len_6(ll: LinkedList<u32>) {
            assert_eq!(ll.len(), 6);
        }

        check_for_len_6(list.prepend(0));
    }
}

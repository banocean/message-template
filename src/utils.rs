use std::iter::Peekable;

pub trait Second<T> {
    fn second(self) -> Option<T>;
}

impl<T, U> Second<T> for Option<(U, T)> {
    fn second(self) -> Option<T> {
        self.map(|(_, result)| result)
    }
}

/// [`Peekable`] but with ability to peek two items ahead
pub struct DoublePeekable<I: Iterator> {
    iter: Peekable<I>,
    second_peeked: Option<I::Item>,
}

impl<I: Iterator> DoublePeekable<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
            second_peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.iter.peek()
    }

    pub fn peek2(&mut self) -> Option<&I::Item> {
        if self.second_peeked.is_none() {
            let mut temp_peek = self.iter.peek();
            if temp_peek.is_some() {
                self.second_peeked = self.iter.next();
            }
        }
        self.second_peeked.as_ref()
    }
}

impl<I: Iterator> Iterator for DoublePeekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.second_peeked.take() {
            Some(item)
        } else {
            self.iter.next()
        }
    }
}
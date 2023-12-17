pub trait Second<T> {
    fn second(self) -> Option<T>;
}

impl<T, U> Second<T> for Option<(U, T)> {
    fn second(self) -> Option<T> {
        self.map(|(_, result)| result)
    }
}

use std::ops::Index;

pub enum CircularBuffer<A> {
    Empty {
        capacity: usize,
    },
    CircularBuffer {
        data: Vec<A>,
        capacity: usize,
        start: usize,
    },
}

impl<A> CircularBuffer<A> {
    pub fn new(capacity: usize) -> Self {
        assert!(capacity > 0, "capacity must be greater than 0");

        CircularBuffer::Empty { capacity }
    }

    pub fn push(&mut self, item: A) {
        match self {
            CircularBuffer::Empty { capacity } => {
                *self = CircularBuffer::CircularBuffer {
                    data: {
                        let mut data = Vec::with_capacity(*capacity);
                        data.push(item);
                        data
                    },
                    capacity: *capacity,
                    start: 0,
                };
            }
            CircularBuffer::CircularBuffer {
                data,
                capacity,
                start,
            } => {
                debug_assert!(data.len() <= *capacity);
                debug_assert!(*start < data.len());

                if data.len() == *capacity {
                    data[*start] = item;

                    let next_start = *start + 1;
                    if next_start == data.len() {
                        *start = 0;
                    } else {
                        *start += 1;
                    }
                } else {
                    // self.data.len() < self.capacity
                    data.push(item)
                }
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            CircularBuffer::Empty { .. } => true,
            CircularBuffer::CircularBuffer { .. } => false,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            CircularBuffer::Empty { .. } => 0,
            CircularBuffer::CircularBuffer { data, .. } => data.len(),
        }
    }

    pub fn iter(&self) -> Iter<A> {
        Iter {
            buffer: self,
            index: 0,
        }
    }
}

impl<A> Index<usize> for CircularBuffer<A> {
    type Output = A;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            CircularBuffer::Empty { .. } => panic!("index {} out of bounds", index),
            CircularBuffer::CircularBuffer {
                data,
                capacity,
                start,
            } => {
                assert!(index < data.len(), "index {} out of bounds", index);

                let unchecked_index = start + index;
                let checked_index = if unchecked_index >= *capacity {
                    unchecked_index - capacity
                } else {
                    unchecked_index
                };
                &data[checked_index]
            }
        }
    }
}

pub struct Iter<'a, A> {
    buffer: &'a CircularBuffer<A>,
    index: usize,
}

impl<'a, A> Iterator for Iter<'a, A> {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        debug_assert!(self.index <= self.buffer.len());

        if self.index == self.buffer.len() {
            None
        } else {
            // self.index < self.buffer.capacity
            let value = &self.buffer[self.index];
            self.index += 1;
            Some(value)
        }
    }
}

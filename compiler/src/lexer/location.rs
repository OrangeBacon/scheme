use std::ops::Range;

/// Wrapper providing source location information for a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithLocation<T> {
    /// The file being parsed, an index into the environment's file list
    pub(crate) file: usize,

    /// The number of characters this location spans
    pub(crate) length: usize,

    /// The character offset into the file that this span starts at
    pub(crate) start_offset: usize,

    /// The data that the span's source has been converted into
    pub(crate) content: T,
}

impl<T> WithLocation<T> {
    /// Get a ref to the content stored
    pub fn content(&self) -> &T {
        &self.content
    }

    /// Separate the content from the location data
    pub fn split(self) -> (T, WithLocation<()>) {
        (
            self.content,
            WithLocation {
                content: (),
                file: self.file,
                length: self.length,
                start_offset: self.start_offset,
            },
        )
    }

    /// Join location data with different contents
    pub fn join<U>(val: T, loc: &WithLocation<U>) -> WithLocation<T> {
        WithLocation {
            content: val,
            file: loc.file,
            length: loc.length,
            start_offset: loc.start_offset,
        }
    }

    /// Extends the source location of self until it contains up to
    /// the end of other.  If the spans are in different files, takes
    /// the file name of the first span.
    pub fn extend<U>(self, other: &WithLocation<U>) -> WithLocation<T> {
        WithLocation {
            length: other.start_offset + other.length - self.start_offset,
            ..self
        }
    }

    /// Extract the location without consuming the contents
    pub fn extract(&self) -> WithLocation<()> {
        WithLocation {
            file: self.file,
            length: self.length,
            start_offset: self.start_offset,
            content: (),
        }
    }

    pub fn file_idx(&self) -> usize {
        self.file
    }

    pub fn source_range(&self) -> Range<usize> {
        self.start_offset..(self.start_offset + self.length)
    }
}

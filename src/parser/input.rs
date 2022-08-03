use crate::span::Span;
use codespan::{ByteIndex, FileId};
use nom::{
    error::ParseError, AsBytes, Compare, IResult, InputIter, InputLength,
    InputTake, Offset, ParseTo, Parser, Slice, UnspecializedInput,
};
use std::{
    ops::{Range, RangeFrom, RangeTo},
    str::{CharIndices, Chars},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Input<'a> {
    start: usize,
    file: FileId,
    text: &'a str,
}

impl<'a> Input<'a> {
    pub fn new(text: &'a str, file: FileId) -> Self {
        Self {
            start: text.as_ptr() as usize,
            file,
            text,
        }
    }

    pub const fn to_str(self) -> &'a str {
        self.text
    }

    fn offset(self) -> ByteIndex {
        u32::try_from(self.text.as_ptr() as usize - self.start)
            .unwrap()
            .into()
    }
}

impl<'a> InputIter for Input<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.text.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.text.iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.text.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.text.slice_index(count)
    }
}

impl InputLength for Input<'_> {
    fn input_len(&self) -> usize {
        self.text.len()
    }
}

impl InputTake for Input<'_> {
    fn take(&self, count: usize) -> Self {
        Self {
            text: self.text.take(count),
            ..*self
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let split = self.text.take_split(count);
        (
            Self {
                text: split.0,
                ..*self
            },
            Self {
                text: split.1,
                ..*self
            },
        )
    }
}

impl<'a> Slice<RangeFrom<usize>> for Input<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self {
            text: &self.text[range],
            ..*self
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Input<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self {
            text: &self.text[range],
            ..*self
        }
    }
}

impl<'a> Slice<Range<usize>> for Input<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        Self {
            text: &self.text[range],
            ..*self
        }
    }
}

impl<'a> AsBytes for Input<'a> {
    fn as_bytes(&self) -> &[u8] {
        self.text.as_bytes()
    }
}

impl UnspecializedInput for Input<'_> {}

impl<'a, T> ParseTo<T> for Input<'a>
where
    &'a str: ParseTo<T>,
{
    fn parse_to(&self) -> Option<T> {
        self.text.parse_to()
    }
}

impl Offset for Input<'_> {
    fn offset(&self, second: &Self) -> usize {
        self.text.offset(second.text)
    }
}

impl<'a, T> Compare<T> for Input<'a>
where
    &'a str: Compare<T>,
{
    fn compare(&self, t: T) -> nom::CompareResult {
        self.text.compare(t)
    }

    fn compare_no_case(&self, t: T) -> nom::CompareResult {
        self.text.compare_no_case(t)
    }
}

pub fn spanned<'a, O, E: ParseError<Input<'a>>, F>(
    mut parser: F,
) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, (Span, O), E>
where
    F: Parser<Input<'a>, O, E>,
{
    move |input| {
        let start = input.offset();
        let (input, o) = parser.parse(input)?;
        let end = input.offset();
        Ok((
            input,
            (
                Span {
                    position: codespan::Span::new(start, end),
                    file: input.file,
                },
                o,
            ),
        ))
    }
}

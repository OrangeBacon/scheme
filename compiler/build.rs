use std::{env, fs, ops::Range, path::Path};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("unicode.rs");
    let unicode_version = format!(
        "{}.{}.{}",
        char::UNICODE_VERSION.0,
        char::UNICODE_VERSION.1,
        char::UNICODE_VERSION.2
    );

    let unicode_data = reqwest::blocking::get(format!(
        "https://www.unicode.org/Public/{}/ucd/UnicodeData.txt",
        unicode_version
    ))?
    .text()?;

    let data = unicode_data
        .lines()
        .map(|line| line.split(';').collect::<Vec<_>>())
        .flat_map(|line| Some((u32::from_str_radix(line[0], 16).ok()?, line[2])))
        .flat_map(|(id, name)| Some((id, GeneralCategory::from_str(name)?)))
        .collect::<Vec<_>>();

    let id_start = data
        .iter()
        .filter(|(_, id)| id.id_start())
        .map(|(num, _)| *num)
        .collect::<Vec<_>>();
    let id_start = compress_data(&id_start);

    let id_continue = data
        .iter()
        .filter(|(_, id)| id.id_continue())
        .map(|(num, _)| *num)
        .collect::<Vec<_>>();
    let id_continue = compress_data(&id_continue);

    fs::write(
        &dest_path,
        format!(
            "pub const ID_START: &[std::ops::Range<char>] = &{:?};
pub const ID_CONTINUE: &[std::ops::Range<char>] = &{:?};",
            id_start, id_continue,
        ),
    )?;

    println!("cargo:rerun-if-changed=build.rs");

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum GeneralCategory {
    UppercaseLetter,
    LowercaseLetter,
    TitlecaseLetter,
    ModifierLetter,
    OtherLetter,
    NonspacingMark,
    SpacingMark,
    EnclosingMark,
    DecimalNumber,
    LetterNumber,
    OtherNumber,
    ConnectorPunctuation,
    DashPunctuation,
    OpenPunctuation,
    ClosePunctuation,
    InitialPunctuation,
    FinalPunctuation,
    OtherPunctuation,
    MathSymbol,
    CurrencySymbol,
    ModifierSymbol,
    OtherSymbol,
    SpaceSeparator,
    LineSeparator,
    ParagraphSeparator,
    Control,
    Format,
    Surrogate,
    PrivateUse,
    Unassigned,
}

impl GeneralCategory {
    fn from_str(name: &str) -> Option<Self> {
        use GeneralCategory::*;

        Some(match name {
            "Lu" => UppercaseLetter,
            "Ll" => LowercaseLetter,
            "Lt" => TitlecaseLetter,
            "Lm" => ModifierLetter,
            "Lo" => OtherLetter,
            "Mn" => NonspacingMark,
            "Mc" => SpacingMark,
            "Me" => EnclosingMark,
            "Nd" => DecimalNumber,
            "Nl" => LetterNumber,
            "No" => OtherNumber,
            "Pc" => ConnectorPunctuation,
            "Pd" => DashPunctuation,
            "Ps" => OpenPunctuation,
            "Pe" => ClosePunctuation,
            "Pi" => InitialPunctuation,
            "Pf" => FinalPunctuation,
            "Po" => OtherPunctuation,
            "Sm" => MathSymbol,
            "Sc" => CurrencySymbol,
            "Sk" => ModifierSymbol,
            "So" => OtherSymbol,
            "Zs" => SpaceSeparator,
            "Zl" => LineSeparator,
            "Zp" => ParagraphSeparator,
            "Cc" => Control,
            "Cr" => Format,
            "Cs" => Surrogate,
            "Co" => PrivateUse,
            "Cn" => Unassigned,
            _ => return None,
        })
    }

    // definitions from r7rs

    fn id_start(&self) -> bool {
        use GeneralCategory::*;

        match self {
            UppercaseLetter | LowercaseLetter | TitlecaseLetter | ModifierLetter | OtherLetter
            | NonspacingMark | LetterNumber | OtherNumber | DashPunctuation
            | ConnectorPunctuation | OtherPunctuation | CurrencySymbol | MathSymbol
            | ModifierSymbol | OtherSymbol | PrivateUse => true,
            _ => false,
        }
    }

    fn id_continue(&self) -> bool {
        use GeneralCategory::*;

        self.id_start()
            || match self {
                DecimalNumber | SpacingMark | EnclosingMark => true,
                _ => false,
            }
    }
}

/// Create a smaller representation of the unicode data, reduces output file from
/// ~1MB to ~31KB, using run length encoding
fn compress_data(input: &[u32]) -> Vec<Range<char>> {
    let mut output = vec![];

    for data in input {
        if output
            .last()
            .map(|last: &Range<u32>| last.end == *data)
            .unwrap_or(false)
        {
            output.last_mut().unwrap().end += 1;
        } else {
            output.push(*data..(data + 1));
        }
    }

    let output = output
        .iter()
        .flat_map(|r| Some(char::from_u32(r.start)?..char::from_u32(r.end)?))
        .collect::<Vec<_>>();

    output
}

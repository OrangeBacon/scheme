use std::{env, fs, path::Path};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("unicode.rs");
    let unicode_version = "13.0.0";

    let prop_list = reqwest::blocking::get(format!(
        "https://www.unicode.org/Public/{}/ucd/PropList.txt",
        unicode_version
    ))?
    .text()?;

    let props = prop_list
        .lines()
        .filter(|l| !l.starts_with('#'))
        .map(|l| match l.split_once('#') {
            Some((l, _)) => l,
            None => l,
        })
        .filter_map(|l| l.split_once(';').map(|(a, b)| (a.trim(), b.trim())))
        .filter(|(_, l)| *l == "White_Space")
        .map(|l| l.0)
        .flat_map(|l| match l.split_once("..") {
            Some((a, b)) => {
                let a = u32::from_str_radix(a, 16).unwrap();
                let b = u32::from_str_radix(b, 16).unwrap();
                a..(b + 1)
            }
            None => {
                let num = u32::from_str_radix(l, 16).unwrap();
                num..(num + 1)
            }
        })
        .filter_map(|i| char::from_u32(i))
        .collect::<Vec<_>>();

    fs::write(
        dest_path,
        format!("pub const WHITESPACE: &[char] = &{:?};", props),
    )?;

    println!("cargo:rerun-if-changed=build.rs");

    Ok(())
}

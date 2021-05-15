const INFO: &str = include_str!("../../docs/docs.txt");

pub fn info(input: Option<&str>) {
    let info = parse_docs();
    assert!(!info.is_empty(), "Unable to get documentation");

    let input = if let Some(input) = input {
        input
    } else {
        print_all_info(&info);
        return;
    };

    if let Ok(idx) = input.parse::<usize>() {
        if idx > info.len() {
            println!(
                "Unable to get numbered documentation entry \"{}\", maximum number is {}",
                input,
                info.len()
            );
        } else if idx == 0 {
            println!("Unable to get documentation entry 0, minimum value is 1");
        } else {
            print_info(&info[idx - 1]);
        }
        return;
    }

    let input = input.to_lowercase();

    // search for exact string match
    for info in &info {
        let name = info.0.to_lowercase();
        if name == input {
            print_info(info);
            return;
        }
    }

    // try fuzzy string matching
    let matches = info
        .iter()
        .enumerate()
        .map(|(idx, (name, _))| {
            let name = name.to_lowercase();

            (idx, strsim::normalized_damerau_levenshtein(&name, &input))
        })
        .fold(
            // find the largest similarity
            (0, f64::NAN),
            |acc, (idx, sim)| {
                if sim > acc.1 || acc.1.is_nan() {
                    (idx, sim)
                } else {
                    acc
                }
            },
        );

    let (idx, sim) = matches;

    if sim < 0.25 {
        println!("Unable to find relevant documentation topic");
    } else {
        println!("Found topic with {}% similarity", (sim * 100.0) as i32);
        print_info(&info[idx]);
    }
}

/// Print a single documentation message
/// Todo: stdout | less
fn print_info(info: &(String, String)) {
    println!("Documentation on {}:\n{}", info.0, info.1);
}

/// Print the name of all available documentation topics
fn print_all_info(info: &[(String, String)]) {
    println!("Available documentation topics:");

    for (name, _) in info {
        println!("{}", name);
    }
}

/// Parse the documentation file
/// Todo: This should probably happen at compile time, not every time docs are
/// asked for
fn parse_docs() -> Vec<(String, String)> {
    let remove_carriage_return = INFO.chars().filter(|&c| c != '\r').collect::<String>();

    let remove_comments = remove_carriage_return
        .lines()
        .filter(|s| !s.starts_with('#'))
        .collect::<Vec<_>>();
    let remove_comments = remove_comments.join("\n");
    let remove_comments = remove_comments.trim();

    remove_comments
        .split("\n\n-")
        .filter_map(|s| {
            if let Some((name, content)) = s.split_once('\n') {
                Some((
                    name.trim_start_matches("- ").to_string(),
                    content.to_string(),
                ))
            } else {
                None
            }
        })
        .collect()
}

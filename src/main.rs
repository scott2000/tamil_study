use std::fs;
use std::collections::HashMap;
use std::io::{self, Write};
use std::fmt::{self, Display, Formatter};
use rand::{prelude::*, distributions::{Standard, Uniform}};

const PRONOUNS: &[&str] = &[
    "அது",
    "அவை",

    "நான்",
    "நாங்கள்",
    "நாம்",
    "நீ",
    "நீங்கள்",

    "அவன்",
    "அவள்",
    "அவர்",
    "அவர்கள்",
];

const PRONOUNS_ENGLISH: &[&[&str]] = &[
    &["adhu", "athu", "adu", "atu"],
    &["avai"],

    &["naan", "nan"],
    &["naangal", "nangal"],
    &["naam", "nam"],
    &["nee", "nii", "ni"],
    &["neengal", "niingal", "ningal"],

    &["avan"],
    &["aval"],
    &["avar"],
    &["avarkal"],
];

const CATEGORIES: &[&str] = &[
    "1a", // 0
    "1b", // 1
    "1c", // 2
    "2",  // 3
    "3",  // 4
    "4a", // 5
    "4b", // 6
    "5",  // 7
    "6",  // 8
    "7",  // 9
];

const PAST_MIDDLES: &[&str] = &[
    "-த்",
    "ன்ற்",
    "ண்ட்",
    "-ந்த்",
    "-இன்",
    "ட்ட்",
    "ற்ற்",
    "-MISSING PAST",
    "-த்த்",
    "-ந்த்",
];

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
enum Strength {
    Weak,
    Mixed,
    Strong,
}

impl Strength {
    fn from_category(cat: usize) -> Strength {
        match cat {
            0..=6 => Strength::Weak,
            7     => Strength::Mixed,
            8..=9 => Strength::Strong,
            _ => unreachable!("invalid category"),
        }
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
enum Tense {
    Past,
    Present,
    Future,
}

impl Distribution<Tense> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Tense {
        use Tense::*;
        [Past, Present, Future][rng.sample(Uniform::new(0, 3))]
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
enum Conjugation {
    Tense {
        tense: Tense,
        pronoun: usize,
    },
    Adverb,
    Infinitive,
}

impl Distribution<Conjugation> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Conjugation {
        use Conjugation::*;
        let kind = rng.sample(Uniform::new_inclusive(0b000, 0b111));
        if kind & 0b011 != 0 {
            let pronoun_dist = if rng.sample(Uniform::new(0, 3)) == 0 {
                // அது and அவை are the most irregular so they should appear often (1/3)
                Uniform::new(0, 2)
            } else {
                // The others will appear randomly otherwise (2/3)
                Uniform::new(2, PRONOUNS.len())
            };
            Tense {
                tense: rng.sample(Standard),
                pronoun: rng.sample(pronoun_dist),
            }
        } else if kind == 0 {
            Adverb
        } else {
            Infinitive
        }
    }
}

impl Display for Conjugation {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Conjugation::*;
        match self {
            &Tense { tense, pronoun } =>
                write!(f, "in the {:?} tense for {}", tense, PRONOUNS[pronoun]),
            Adverb =>
                write!(f, "as an Adverb"),
            Infinitive =>
                write!(f, "as an Infinitive"),
        }
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
struct Verb<'a> {
    tamil: &'a str,
    category: usize,
    english: Vec<&'a str>,
    past: Option<&'a str>,
    stem: Option<&'a str>,
    adv: Option<&'a str>,
    inf: Option<&'a str>,
}

impl<'a> Verb<'a> {
    fn pick_english(&self, rng: &mut impl Rng) -> &'a str {
        let next: u8 = rng.sample(Standard);
        self.english[next.trailing_zeros() as usize % self.english.len()]
    }

    fn all_english(&self) -> String {
        let mut string = String::new();
        for (i, english) in self.english.iter().enumerate() {
            if i != 0 {
                string.push_str(", ");
            }
            string.push_str(english);
        }
        string
    }

    fn base(&self, conjugation: &Conjugation) -> &'a str {
        use Conjugation::*;
        use self::Tense::*;
        match conjugation {
            Tense { tense: Past, pronoun: 1 } if self.category == 4 => self.adv.or(self.past),
            Adverb => self.adv.or(self.past),
            Tense { tense, .. } => {
                match tense {
                    Past => self.past,
                    _ => self.stem,
                }
            }
            Infinitive => self.stem,
        }.unwrap_or(self.tamil)
    }

    fn middle(&self, conjugation: &Conjugation) -> &'a str {
        use Conjugation::*;
        use self::Tense::*;
        match conjugation {
            Tense { tense: Past, pronoun: 1 } if self.category == 4 => {
                if self.adv.is_none() && self.past.is_some() {
                    "-"
                } else {
                    "-இன்"
                }
            },
            Adverb => {
                if let Some(_) = self.adv {
                    "-"
                } else if let Some(_) = self.past {
                    "-உ"
                } else {
                    const ADVERB_MIDDLES: &[&str] = &[
                        "-து",
                        "ன்று",
                        "ண்டு",
                        "-ந்து",
                        "-இ",
                        "ட்டு",
                        "ற்று",
                        "-MISSING ADVERB",
                        "-த்து",
                        "-ந்து",
                    ];
                    ADVERB_MIDDLES[self.category]
                }
            }
            Infinitive | Tense { tense: Future, pronoun: 0 } => {
                if let Some(inf) = self.inf {
                    inf
                } else if let Strength::Strong = Strength::from_category(self.category) {
                    "-க்க்"
                } else {
                    "-"
                }
            }
            &Tense { tense, pronoun } => {
                match tense {
                    Past => {
                        if let Some(_) = self.past {
                            "-"
                        } else {
                            PAST_MIDDLES[self.category]
                        }
                    },
                    Present => {
                        match (pronoun, Strength::from_category(self.category)) {
                            (1, Strength::Strong) => "-க்கின்ற்",
                            (1, _) => "-கின்ற்",
                            (_, Strength::Strong) => "-க்கிற்",
                            (_, _) => "-கிற்",
                        }
                    }
                    Future => {
                        match Strength::from_category(self.category) {
                            Strength::Weak => "-வ்",
                            Strength::Mixed => "-ப்",
                            Strength::Strong => "-ப்ப்",
                        }
                    }
                }
            }
        }
    }

    fn endings(&self, conjugation: &Conjugation) -> &'static [&'static str] {
        use Conjugation::*;
        use self::Tense::*;
        match conjugation {
            &Tense { tense, pronoun } => {
                match tense {
                    Past if self.category == 4 => {
                        match pronoun {
                            0 if self.past.is_none() => return &["யது", "-அது", "ற்று"],
                            1 => return &["-அ"],
                            _ => {}
                        }
                    },
                    Future if pronoun == 0 => return &["-உம்"],
                    _ => {}
                }
                const ENDINGS: &[&[&str]] = &[
                    &["-அது"],
                    &["-அன"],

                    &["-ஏன்"],
                    &["-ஓம்"],
                    &["-ஓம்"],
                    &["-ஆய்"],
                    &["-ஈர்கள்"],

                    &["-ஆன்"],
                    &["-ஆள்"],
                    &["-ஆர்"],
                    &["-ஆர்கள்", "-அனர்"],
                ];
                ENDINGS[pronoun]
            }
            Adverb => &["-"],
            Infinitive => {
                match self.category {
                    0 => &["-ய"],
                    1 => &["-ல"],
                    2 => &["-ள"],
                    _ => &["-அ"],
                }
            }
        }
    }

    fn is_irregular(&self, conjugation: &Conjugation) -> bool {
        use Conjugation::*;
        use self::Tense::*;
        match conjugation {
            Adverb =>
                self.adv.is_some() || self.past.is_some(),
            Infinitive | Tense { tense: Future, pronoun: 0 } =>
                self.inf.is_some() || self.stem.is_some(),
            Tense { tense: Past, .. } =>
                self.past.is_some(),
            _ =>
                self.stem.is_some(),
        }
    }

    fn is_ever_irregular(&self) -> bool {
        self.past.is_some() || self.adv.is_some() || self.stem.is_some() || self.inf.is_some()
    }

    fn from_line(s: &str) -> Option<Verb> {
        let mut iter = s.split(';');
        let tamil = iter.next()?.trim();
        let english = iter.next()?.split(',').map(str::trim).collect();
        let category = iter.next()?.trim();
        let mut map = HashMap::new();
        if let Some(irregularities) = iter.next() {
            for irregular in irregularities.split(',') {
                let mut parts = irregular.split(':');
                let key = parts.next()?.trim();
                let value = parts.next()?.trim();
                map.insert(key, value);
            }
        }
        if iter.next().is_some() {
            return None;
        }
        let verb = Verb {
            tamil,
            category: CATEGORIES.iter().position(|&cat| cat == category)?,
            english,
            past: map.remove("past"),
            stem: map.remove("stem"),
            adv: map.remove("adv"),
            inf: map.remove("inf"),
        };
        if map.is_empty() {
            Some(verb)
        } else {
            None
        }
    }
}

const CONSONANTS: &[char] = &[
    'க', 'ங', 'ச', 'ஞ', 'ட', 'ண', 'த', 'ந', 'ப', 'ம',
    'ய', 'ர', 'ல', 'வ', 'ழ', 'ள', 'ற', 'ன',
];

const VOWELS: &[char] = &[
    'அ', 'ஆ', 'இ', 'ஈ', 'உ', 'ஊ',
    'எ', 'ஏ', 'ஐ', 'ஒ', 'ஓ', 'ஔ',
];

const VOWEL_MARKS: &[&str] = &[
    "", "\u{bbe}", "\u{bbf}", "\u{bc0}", "\u{bc1}", "\u{bc2}",
    "\u{bc6}", "\u{bc7}", "\u{bc8}", "\u{bca}", "\u{bcb}", "\u{bcc}",
];

fn suffix(base: &mut String, suffix: &str) {
    let mut suffix = suffix.chars().peekable();
    if let Some('-') = suffix.peek() {
        suffix.next();
    } else {
        while let Some(ch) = base.pop() {
            if CONSONANTS.contains(&ch) {
                break;
            }
        }
    }
    if base.is_empty() {
        push_iter(base, suffix);
        return;
    }
    let first = if let Some(first) = suffix.next() {
        first
    } else {
        return;
    };
    let vowel_index = if let Some(index) = VOWELS.iter().position(|&ch| ch == first) {
        index
    } else {
        base.push(first);
        push_iter(base, suffix);
        return;
    };
    let mark = VOWEL_MARKS[vowel_index];
    let ch = base.pop().unwrap();
    if ch != '\u{bcd}' && ch != '\u{bc1}' {
        base.push(ch);
        if !CONSONANTS.contains(&ch) {
            base.push(match ch {
                '\u{bbf}' | '\u{bc0}' | '\u{bc6}' | '\u{bc7}' | '\u{bc8}'  => 'ய',
                _ => 'வ',
            });
        }
    }
    base.push_str(mark);
    push_iter(base, suffix);
}

fn push_iter(base: &mut String, iter: impl Iterator<Item=char>) {
    for ch in iter {
        base.push(ch);
    }
}

fn main() -> io::Result<()> {
    const SEPARATOR: &str = "============================================================";
    let contents = fs::read_to_string("verbs.txt")?;
    let mut verbs = Vec::new();
    for line in contents.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('=') {
            continue;
        }
        if let Some(verb) = Verb::from_line(trimmed) {
            verbs.push(verb);
        } else {
            eprintln!("cannot read line: {}", trimmed);
        }
    }
    verbs.sort_unstable();
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    'main_loop: loop {
        let study = loop {
            println!("{:=>60}", " MENU");
            println!("Do you want to study or look up a word?");
            println!("0. Exit");
            println!("1. Study");
            println!("2. Look up a word");
            println!("3. List words in dictionary");
            print!("> ");
            stdout.flush()?;
            let mut response = String::new();
            let code = stdin.read_line(&mut response)?;
            let response = response.trim();
            if code == 0 || response.is_empty() || response == "." {
                return Ok(());
            }
            if let Ok(num) = response.parse() {
                match num {
                    0 => return Ok(()),
                    1 => break true,
                    2 => break false,
                    3 => {
                        println!("{:=>60}", " DICTIONARY");
                        println!("Listing {} verbs in sorted order:", verbs.len());
                        for verb in verbs.iter() {
                            let irregular = if verb.is_ever_irregular() {
                                "irregular, "
                            } else {
                                ""
                            };
                            println!(
                                "  - {}: {} [{}#{}]",
                                verb.tamil,
                                verb.all_english(),
                                irregular,
                                CATEGORIES[verb.category],
                            );
                        }
                    }
                    _ => {}
                }
            }
        };
        if study {
            println!("{:=>60}", " STUDY");
            println!("Studying with {} verbs... (type '.' to exit)", verbs.len());
            let mut rng = rand::thread_rng();
            verbs.shuffle(&mut rng);
            let mut total = 0;
            let mut correct = 0;
            let mut current = verbs.clone();
            let mut missed = Vec::new();
            loop {
                let count = current.len();
                'verb_loop: for verb in current {
                    let conj = rng.sample(Standard);
                    let english = verb.pick_english(&mut rng);
                    let mut disambiguation = String::new();
                    for other in verbs.iter() {
                        if other.tamil != verb.tamil && other.english.contains(&english) {
                            disambiguation = format!(" ({})", verb.tamil);
                            break;
                        }
                    }
                    println!("{:=>60}", format!(" [{}/{}]", correct, total));
                    println!("Conjugate \"{}\"{} {}", english, disambiguation, conj);
                    print!("> ");
                    stdout.flush()?;
                    let mut response = String::new();
                    let code = stdin.read_line(&mut response)?;
                    let response = response.trim();
                    if code == 0 || response == "." {
                        println!("{}", SEPARATOR);
                        println!("Stopping with {}/{} correct!", correct, total);
                        continue 'main_loop;
                    }
                    total += 1;
                    let mut base = String::from(verb.base(&conj));
                    suffix(&mut base, verb.middle(&conj));
                    let endings = verb.endings(&conj);
                    let irregular = if verb.is_irregular(&conj) {
                        "irregular, "
                    } else {
                        ""
                    };
                    let mut conjugations = Vec::new();
                    let info = format!("[{}#{}]", irregular, CATEGORIES[verb.category]);
                    for ending in endings {
                        let mut conjugated = base.clone();
                        suffix(&mut conjugated, ending);
                        if response == conjugated {
                            print!("Correct! {}", info);
                            if let Some(primary) = conjugations.first() {
                                print!(" (but {} is more common)", primary);
                            }
                            println!();
                            correct += 1;
                            continue 'verb_loop;
                        }
                        conjugations.push(conjugated);
                    }
                    missed.push(verb);
                    print!("Incorrect {}, ", info);
                    if conjugations.len() == 1 {
                        let conjugation = conjugations.pop().unwrap();
                        println!("expected: {}", conjugation);
                    } else {
                        println!("expected one of:");
                        for (i, conjugation) in conjugations.into_iter().enumerate() {
                            let common = if i == 0 {
                                " (most common)"
                            } else {
                                ""
                            };
                            println!("  - {}{}", conjugation, common);
                        }
                    }
                }
                println!("{}", SEPARATOR);
                if missed.is_empty() {
                    println!("All {} verbs conjugated, restarting with new questions...", count);
                    current = verbs.clone();
                } else {
                    println!("All {} verbs conjugated, reviewing {} missed verbs...", count, missed.len());
                    current = missed;
                    missed = Vec::new();
                }
            }
        } else {
            'lookup: loop {
                println!("{:=>60}", " LOOKUP");
                println!("Enter an English or Tamil word to look up: (or '.' to exit)");
                print!("> ");
                stdout.flush()?;
                let mut response = String::new();
                let code = stdin.read_line(&mut response)?;
                let word = response.trim();
                if code == 0 || word.is_empty() || word == "." {
                    continue 'main_loop;
                }
                let word = word.to_ascii_lowercase();
                let mut candidates = verbs.iter()
                    .filter(|verb| &word == verb.tamil || verb.english.contains(&word.as_str()))
                    .collect::<Vec<_>>();
                match candidates.as_slice() {
                    &[] => {
                        println!("{}", SEPARATOR);
                        println!("Could not find English or Tamil word: {}", word);
                    }
                    &[verb] => {
                        let english = verb.all_english();
                        let mut uppercase = english.to_ascii_uppercase();
                        uppercase.insert(0, ' ');
                        println!("{:=>60}", uppercase);
                        println!("{:<35}Tamil: {}", format!("English: {}", english), verb.tamil);
                        let irregular = if verb.is_ever_irregular() {
                            " [irregular]"
                        } else {
                            ""
                        };
                        println!("                                   Category: #{}{}", CATEGORIES[verb.category], irregular);
                        println!("Enter a conjugation to print: (type '.' to exit)");
                        loop {
                            print!("> ");
                            stdout.flush()?;
                            let mut response = String::new();
                            let code = stdin.read_line(&mut response)?;
                            let conjugation = response.trim();
                            if code == 0 || conjugation.is_empty() || conjugation == "." {
                                continue 'lookup;
                            }
                            let conjugation = conjugation.to_ascii_lowercase();
                            let mut parts = conjugation.split_ascii_whitespace();
                            let kind = parts.next().unwrap();
                            let conj = if kind.starts_with('a') {
                                Conjugation::Adverb
                            } else if kind.starts_with('i') {
                                Conjugation::Infinitive
                            } else {
                                let tense = if kind.starts_with("pr") {
                                    Tense::Present
                                } else if kind.starts_with('p') {
                                    Tense::Past
                                } else if kind.starts_with('f') {
                                    Tense::Future
                                } else {
                                    println!("Invalid tense: {}", kind);
                                    continue
                                };
                                let pronoun = if let Some(pronoun) = parts.next() {
                                    if let Some(index) = PRONOUNS.iter().position(|&s| s == pronoun) {
                                        index
                                    } else if let Some(index) = PRONOUNS_ENGLISH.iter()
                                        .position(|s| s.contains(&pronoun))
                                    {
                                        index
                                    } else {
                                        println!("Invalid subject: {}", pronoun);
                                        continue
                                    }
                                } else {
                                    7
                                };
                                Conjugation::Tense {
                                    tense,
                                    pronoun,
                                }
                            };
                            println!("Conjugating {}:", conj);
                            let mut base = String::from(verb.base(&conj));
                            suffix(&mut base, verb.middle(&conj));
                            let endings = verb.endings(&conj);
                            for (i, ending) in endings.iter().enumerate() {
                                let mut conjugated = base.clone();
                                suffix(&mut conjugated, ending);
                                let (arrow, note) = if i == 0 {
                                    let note = if endings.len() == 1 {
                                        ""
                                    } else {
                                        " (most common)"
                                    };
                                    ("=>", note)
                                } else {
                                    ("  ", "")
                                };
                                println!("{} {}{}", arrow, conjugated, note);
                            }
                        }
                    }
                    _ => {
                        println!("{}", SEPARATOR);
                        println!("English word \"{}\" could refer to multiple entries:", word);
                        candidates.sort_unstable();
                        for candidate in candidates {
                            println!("  - {}: {}", candidate.tamil, candidate.all_english());
                        }
                    }
                }
            }
        }
    }
}

use std::fs;
use std::io;
use std::process;

pub mod token_type;

struct Lox {
    had_error: bool
}

impl Lox {
    fn main(&self, args: &mut [&str]) {
        if args.len() > 1 {
            println!("Usage: stata [script]")
        } else if args.len() == 1 {
            self.run_file(args[0])
        } else {
            self.run_prompt()
        }
    }

    pub fn run_file(&self, filepath: &str) {
        let code = fs::read_to_string(filepath).unwrap();
        self.run(code);
        if self.had_error {
            process::exit(65)
        }
    }

    pub fn run_prompt(&self) {
        let mut buffer: String = String::new();
        let stdin: io::Stdin = io::stdin();
        while stdin.read_line(&mut buffer).is_ok() {
            let trimmed: &str = buffer.trim_end();
            println!("> {}", trimmed);
            self.run(trimmed.to_string());
            buffer.clear();
        }
    }

    pub fn run(&self, code: String) {
        println!("Run code: {}", code)
        /*let mut scanner: scanner::Scanner = scanner::Scanner::new(code);
    
        scanner.scan_tokens();

        for token in scanner.tokens {
            println!("{}", token.lexeme)
        }*/
    }

    fn error(line: u32, message: &str) {
        Self::report(line, "", message)
    }

    fn report(line: u32, at: &str, message: &str) {
        println!("[line {}] Error {}: {}", line, at, message)
    }

}

fn main() {
    let mut input = ["C:/Users/Mael/Documents/Temp/test.lox"];
    let lox: Lox = Lox {
        had_error: false
    };
    lox.main(&mut input)
}

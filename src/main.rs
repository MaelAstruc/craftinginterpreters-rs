use std::fs;
use std::io;

struct Lox {
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

    pub fn run (&self, code: String) {
        println!("Run code: {}", code)
        /*let mut scanner: scanner::Scanner = scanner::Scanner::new(code);
    
        scanner.scan_tokens();

        for token in scanner.tokens {
            println!("{}", token.lexeme)
        }*/
    }
}

fn main() {
    let mut input: [&str; 1] = ["C:/Users/Mael/Documents/Temp/test.lox"];
    let lox: Lox = Lox {};
    lox.main(&mut input)
}

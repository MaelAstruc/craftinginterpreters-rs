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
        println!("Run file path: {}", filepath)
    }

    pub fn run_prompt(&self) {
        println!("Run prompt")
    }
}

fn main() {
    let mut input: [&str; 1] = ["C:/Users/Mael/Documents/Temp/test.lox"];
    let lox: Lox = Lox {};
    lox.main(&mut input)
}

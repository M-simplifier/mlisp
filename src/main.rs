use std::io;

fn main() {
    let mut buf = String::new();
    io::stdin()
        .read_line(&mut buf)
        .expect("Error: at read_line().");
    println!("{buf}");
}

mod hal;

fn main() {
    let hal = hal::Cpu::new();
    println!("Hello, world! HAL: {:?}", hal);
}

fn main() {
	// Instruct Cargo to rebuild the project if the linker script has changed.
	println!("cargo:rerun-if-changed=linker.ld");
}

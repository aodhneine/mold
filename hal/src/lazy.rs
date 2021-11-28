use core::sync::atomic::AtomicU8;
use core::sync::atomic::Ordering::{ Acquire, AcqRel, Release };

/// Atomically-synchronised value that can be initialised only once.
pub struct Once<T> {
	state: AtomicU8,
	inner: core::cell::UnsafeCell<core::mem::MaybeUninit<T>>,
}

const UNINITIALISED: u8 = 0x1;
const INITIALISING: u8 = 0x2;
const INITIALISED: u8 = 0x4;

impl<T> Once<T> {
	/// Creates new uninitialised cell.
	pub const fn new() -> Self {
		return Self {
			state: AtomicU8::new(UNINITIALISED),
			inner: core::cell::UnsafeCell::new(core::mem::MaybeUninit::uninit()),
		};
	}

	pub fn set_once(&self, value: T) {
		match self.state.compare_exchange(UNINITIALISED, INITIALISING, AcqRel, Acquire) {
			Err(_) => panic!("cell already set"),
			Ok(_) => unsafe {
				(*self.inner.get()).write(value)
			},
		};

		self.state.store(INITIALISED, Release);
	}

	pub fn get(&self) -> &T {
		return match self.state.load(Acquire) {
			INITIALISED => unsafe {
				&*((*self.inner.get()).as_ptr())
			},
			INITIALISING => panic!("cell panicked while initialising"),
			UNINITIALISED => panic!("cell not set"),
			// SAFETY: We know that state only holds one of these three values, as
			// this is guaranteed by our control flow, so we can call unreachable
			// without risking undefined behaviour.
			_ => unsafe {
				core::hint::unreachable_unchecked()
			},
		};
	}
}

unsafe impl<T: Sync + Send> Sync for Once<T> {}

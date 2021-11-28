/*! Spinlock-based synchronisation primitives. */

use core::sync::atomic::AtomicBool;
use core::sync::atomic::Ordering::{ Acquire, Release };

/// Naive spinlock implementation for embedded systems.
#[repr(transparent)]
pub struct Spinlock {
	locked: AtomicBool,
}

impl Spinlock {
	pub fn new() -> Self {
		return Self {
			locked: AtomicBool::new(false),
		};
	}

	// Based on the source code taken from https://gpuopen.com/gdc-presentations/2019/gdc-2019-s2-amd-ryzen-processor-software-optimization.pdf (page 46)
	// and https://probablydance.com/2019/12/30/measuring-mutexes-spinlocks-and-how-bad-the-linux-scheduler-really-is/.
	pub fn lock(&self) {
		loop {
			if self.locked.compare_exchange(false, true, Acquire, Acquire).is_ok() {
				break;
			}

			core::hint::spin_loop();
		}
	}

	pub fn unlock(&self) {
		self.locked.store(false, Release);
	}
}

// TODO: Add exponential backoff, see https://stackoverflow.com/a/28732630.
/// Mutually exclusive data implemented using spinlocks.
pub struct Mutex<T> {
	lock: Spinlock,
	data: core::cell::UnsafeCell<T>,
}

// Same impl's as the standard library's Mutex.
unsafe impl<T: Send> Send for Mutex<T> {}
unsafe impl<T: Send> Sync for Mutex<T> {}

impl<T> Mutex<T> {
	pub fn new(value: T) -> Self {
		return Self {
			lock: Spinlock::new(),
			data: core::cell::UnsafeCell::new(value),
		};
	}

	#[must_use = "this will immediately drop the mutex guard"]
	pub fn lock(&self) -> MutexGuard<T> {
		self.lock.lock();

		return MutexGuard {
			mutex: self,
		};
	}

	/// Force unlock the mutex. Only use this function when you know that there
	/// is no mutex guard left (i.e. when dropping the mutex guard) or you know
	/// that no other thread can use the mutex (i.e. when in the interrupt).
	pub unsafe fn unlock(&self) {
		self.lock.unlock();
	}
}

pub struct MutexGuard<'a, T> {
	mutex: &'a Mutex<T>,
}

impl<'a, T> core::ops::Deref for MutexGuard<'a, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		return unsafe {
			&*self.mutex.data.get()
		};
	}
}

impl<'a, T> core::ops::DerefMut for MutexGuard<'a, T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		return unsafe {
			&mut *self.mutex.data.get()
		};
	}
}

impl<'a, T> Drop for MutexGuard<'a, T> {
	fn drop(&mut self) {
		// SAFETY: After `drop` the mutex guard is gone, so we can safely unlock
		// the mutex at this point.
		unsafe {
			self.mutex.unlock()
		};
	}
}

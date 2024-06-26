#[derive(Copy, Clone)]
pub union Word {
    pub as_u64: u64,
    pub as_i64: i64,
    pub as_f64: f64,
    pub as_ptr: *const u8
}

impl std::fmt::Debug for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            f.debug_struct("Word")
                .field("as_u64", &self.as_u64)
                .field("as_i64", &self.as_i64)
                .field("as_f64", &self.as_f64)
                .field("as_ptr", &self.as_ptr)
                .finish()
        }
    }
}

impl Word {
    pub fn from_le_bytes(bytes: &[u8]) -> Self {
        let mut word = Word {
            as_u64: 0
        };

        match bytes.len() {
            8 => {
                word.as_u64 = u64::from_le_bytes(bytes.try_into().unwrap());
                word.as_i64 = i64::from_le_bytes(bytes.try_into().unwrap());
                word.as_f64 = f64::from_le_bytes(bytes.try_into().unwrap());
            }
            4 => {
                word.as_u64 = u32::from_le_bytes(bytes.try_into().unwrap()) as u64;
                word.as_i64 = i32::from_le_bytes(bytes.try_into().unwrap()) as i64;
                word.as_f64 = f32::from_le_bytes(bytes.try_into().unwrap()) as f64;
            }
            _ => {}
        }

        word
    }
}

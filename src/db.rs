#[salsa::db]
#[derive(Clone, Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<Self>,
}

impl DatabaseImpl {
    pub fn new() -> Self {
        Self::default()
    }
}

#[salsa::db]
impl salsa::Database for DatabaseImpl {}

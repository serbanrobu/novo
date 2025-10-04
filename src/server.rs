use std::{net::IpAddr, sync::Arc};

use axum::{
    Json, Router,
    extract::{DefaultBodyLimit, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::post,
};
use im::HashMap;
use indexmap::IndexMap;
use miette::{GraphicalReportHandler, GraphicalTheme, miette};
use salsa::Setter;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use tokio::{io, net::TcpListener, sync::Mutex};

use crate::{
    compile::compile_and_interpret,
    db::DatabaseImpl,
    ir::{Context, Diagnostic, SourceProgram},
    value::Value,
};

#[derive(Clone)]
pub struct AppState {
    db: Arc<Mutex<DatabaseImpl>>,
    source_program: SourceProgram,
    context: Context,
}

pub async fn serve(host: IpAddr, port: u16) -> io::Result<()> {
    let db = DatabaseImpl::new();
    let source_program = SourceProgram::new(&db, Default::default());
    let context = Context::new(&db, Default::default());

    let state = AppState {
        db: Arc::new(Mutex::new(db)),
        source_program,
        context,
    };

    let app = Router::new()
        .route("/", post(root))
        .layer(DefaultBodyLimit::disable())
        .with_state(state);

    let listener = TcpListener::bind((host, port)).await?;
    let local_addr = listener.local_addr()?;
    println!("listening on {}", local_addr);
    axum::serve(listener, app).await
}

#[derive(Debug, Deserialize)]
struct Payload {
    #[serde(rename(deserialize = "formula"))]
    code: String,
    #[serde(rename(deserialize = "inputValues"))]
    environment: HashMap<SmolStr, Value>,
}

async fn root(
    State(state): State<AppState>,
    Json(payload): Json<Payload>,
) -> Result<OkResponse, ErrResponse> {
    let mut db = state.db.lock().await;
    let source = Arc::new(payload.code);
    state.source_program.set_text(&mut *db).to(source.clone());

    state
        .context
        .set_environment(&mut *db)
        .to(payload.environment);

    let outputs = compile_and_interpret(&*db, state.source_program, state.context).map_err(
        |diagnostics| ErrResponse {
            errors: diagnostics
                .into_iter()
                .map(|d| render_diagnostic(d.clone(), source.clone()))
                .collect(),
        },
    )?;

    Ok(OkResponse {
        data: state
            .context
            .environment(&*db)
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .chain(
                outputs
                    .into_iter()
                    .map(|o| (o.ident.clone(), o.value.clone())),
            )
            .collect(),
    })
}

#[derive(Serialize)]
pub struct OkResponse {
    data: IndexMap<SmolStr, Value>,
}

impl IntoResponse for OkResponse {
    fn into_response(self) -> Response {
        Json(self).into_response()
    }
}

#[derive(Serialize)]
pub struct ErrResponse {
    errors: Vec<String>,
}

impl IntoResponse for ErrResponse {
    fn into_response(self) -> Response {
        (StatusCode::UNPROCESSABLE_ENTITY, Json(self)).into_response()
    }
}

fn render_diagnostic(diagnostic: Diagnostic, source: Arc<String>) -> String {
    let mut buf = String::new();
    let handler = GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor());

    handler
        .render_report(
            &mut buf,
            miette!(diagnostic).with_source_code(source).as_ref(),
        )
        .expect("failed to render miette report");

    buf
}

use ariadne::{Color, Config, Label, Report, ReportKind, Source};
use thiserror::Error;

const REPORT_ERR: ReportKind = ReportKind::Custom("ꡃꡨꡀꡃꡕ", Color::Unset);

#[derive(Debug, Error)]
pub enum ScmError {
    #[error("parsing failed")]
    ParsingError(ParsingError),
}

impl ScmError {
    pub fn to_report(&self, source: &str) -> String {
        let report = match self {
            Self::ParsingError(e) => e.to_report(),
        };

        let source = Source::from(source);
        let mut buf = Vec::new();
        report.write(source, &mut buf).unwrap();

        String::from_utf8(buf).unwrap()
    }
}

#[derive(Debug)]
pub struct ParsingError {
    pub start: usize,
    pub end: usize,
}

impl ParsingError {
    fn to_report(&self) -> Report {
        Report::build(REPORT_ERR, (), self.start)
            .with_message("kaathe.encyc.sim.tape_err")
            .with_label(Label::new(self.start..self.end).with_message("「unexpected input」"))
            .with_config(Config::default().with_color(false))
            .finish()
    }
}

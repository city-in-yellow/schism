use ariadne::{Color, Config, Label, Report, ReportKind, Source};
use smol_str::SmolStr;
use thiserror::Error;

const REPORT_ERR: ReportKind = ReportKind::Custom("ꡃꡨꡀꡃꡕ", Color::Unset);

#[derive(Debug, Error)]
pub enum ScmError {
    #[error("parsing failed")]
    ParsingError(ParsingError),

    #[error("interpreting failed")]
    InterpretingError(InterpretingError),
}

impl ScmError {
    pub fn to_report(&self, source: &str) -> String {
        let report = match self {
            Self::ParsingError(e) => e.to_report(),
            Self::InterpretingError(e) => e.to_report(),
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

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum InterpretingError {
    #[error("no such variable")]
    LookupError(LookupError),

    #[error("non-list for selector")]
    ForError(ForError),

    #[error("non-boolean match predicate")]
    MatchError(MatchError),

    #[error("non-func application")]
    FuncError(FuncError),

    #[error("type application error")]
    TypeError(TypeError),
}

impl InterpretingError {
    pub fn to_report(&self) -> Report {
        match self {
            Self::LookupError(e) => e.to_report(),
            Self::MatchError(e) => e.to_report(),
            Self::ForError(e) => e.to_report(),
            Self::FuncError(e) => e.to_report(),
            Self::TypeError(e) => e.to_report(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MatchError {
    pub match_start: usize,
    pub match_end: usize,
    pub predicate_start: usize,
    pub predicate_end: usize,
    pub predicate_type: SmolStr,
}

impl MatchError {
    fn to_report(&self) -> Report {
        Report::build(REPORT_ERR, (), self.match_start)
            .with_message("kaathe.encyc.sim.veracity_err")
            .with_label(
                Label::new(self.match_start..self.match_end)
                    .with_message("「if statement predicates must be of type Bool」"),
            )
            .with_label(
                Label::new(self.predicate_start..self.predicate_end).with_message(format!(
                    "expected type Bool but this is type {}",
                    self.predicate_type
                )),
            )
            .with_config(Config::default().with_color(false))
            .finish()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForError {
    pub for_start: usize,
    pub for_end: usize,
    pub range_start: usize,
    pub range_end: usize,
    pub range_type: SmolStr,
}

impl ForError {
    fn to_report(&self) -> Report {
        Report::build(REPORT_ERR, (), self.for_start)
            .with_message("kaathe.encyc.sim.sequential_err")
            .with_label(
                Label::new(self.for_start..self.for_end)
                    .with_message("「for statement ranges must be of type List」"),
            )
            .with_label(
                Label::new(self.range_start..self.range_end).with_message(format!(
                    "expected type List but this is type {}",
                    self.range_type
                )),
            )
            .with_config(Config::default().with_color(false))
            .finish()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LookupError {
    pub start: usize,
    pub end: usize,
}

impl LookupError {
    fn to_report(&self) -> Report {
        Report::build(REPORT_ERR, (), self.start)
            .with_message("kaathe.encyc.sim.lookup_err")
            .with_label(Label::new(self.start..self.end).with_message("「unknown symbol」"))
            .with_config(Config::default().with_color(false))
            .finish()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FuncError {
    pub outer_start: usize,
    pub outer_end: usize,
    pub func_start: usize,
    pub func_end: usize,
    pub func_type: SmolStr,
}

impl FuncError {
    fn to_report(&self) -> Report {
        Report::build(REPORT_ERR, (), self.outer_start)
            .with_message("kaathe.encyc.sim.proc_err")
            .with_label(
                Label::new(self.outer_start..self.outer_end)
                    .with_message("「can only apply arguments to functions」"),
            )
            .with_label(
                Label::new(self.func_start..self.func_end).with_message(format!(
                    "expected type Func but this is type {}",
                    self.func_type
                )),
            )
            .with_config(Config::default().with_color(false))
            .finish()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeError {
    pub arg_start: usize,
    pub arg_end: usize,
    pub func_start: usize,
    pub func_end: usize,
    pub current_type: SmolStr,
    pub valid_types: Vec<SmolStr>,
}

impl TypeError {
    fn to_report(&self) -> Report {
        Report::build(REPORT_ERR, (), self.func_start)
            .with_message("kaathe.encyc.sim.pat_err")
            .with_label(
                Label::new(self.func_start..self.func_end)
                    .with_message("「argument type is invalid」"),
            )
            .with_label(
                Label::new(self.arg_start..self.arg_end).with_message(format!(
                    "expected one of ({}) but this is type {}",
                    self.valid_types.join(", "),
                    self.current_type,
                )),
            )
            .with_config(Config::default().with_color(false))
            .finish()
    }
}

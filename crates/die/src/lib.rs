use ast::hir::Val;
use cached::{proc_macro::cached, Return};
use fxhash::{FxBuildHasher, FxHashMap, FxHasher};
use lazy_static::lazy_static;
use rayon::prelude::*;
use rug::{Integer, Rational};
use smol_str::SmolStr;
use std::{
    cmp::Ordering,
    collections::HashMap,
    hash::{Hash, Hasher},
    sync::{Arc, Mutex},
};

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd)]
pub struct Pq {
    pub queue: Vec<(Val, u32)>,
}

impl Pq {
    fn as_ref(&self) -> PqRef {
        PqRef { queue: &self.queue }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd)]
pub struct PqRef<'a> {
    pub queue: &'a [(Val, u32)],
}

impl<'a> PqRef<'a> {
    fn pop(self) -> (&'a Val, u32, Self) {
        assert!(!self.queue.is_empty());

        let (val, prob) = &self.queue[0];
        (
            val,
            *prob,
            Self {
                queue: &self.queue[1..],
            },
        )
    }
}

#[derive(Clone, Debug)]
pub struct State {
    pub m: HashMap<Val, Integer, FxBuildHasher>,
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    fn new() -> Self {
        Self {
            m: FxHashMap::default(),
        }
    }

    pub fn singleton(k: Val, v: Integer) -> Self {
        let mut s = Self::new();
        s.insert(k, v);
        s
    }

    pub fn insert(&mut self, k: Val, v: Integer) {
        match self.m.remove(&k) {
            None => self.m.insert(k, v),
            Some(vv) => self.m.insert(k, v + vv),
        };
    }
}

#[cached]
fn pq_from_base(base: u32) -> Pq {
    Pq {
        queue: (1..=base)
            .map(|x| (Val::Number(x as isize), 1))
            .rev()
            .collect(),
    }
}

#[cached]
fn binomial(n: u32, k: u32) -> Integer {
    Integer::from(n).binomial(k)
}

fn hash_solve(pq: &PqRef, n: u32) -> u64 {
    let mut state = FxHasher::default();

    pq.hash(&mut state);
    n.hash(&mut state);

    state.finish()
}

#[cached(
    key = "u64",
    convert = r#"{hash_solve(&pq, n)}"#,
    with_cached_flag = true
)]
fn solve(pq: PqRef, n: u32, next: fn(&Val, &Val, &Val) -> Val) -> Return<State> {
    let (outcome, prob, pq) = pq.pop();

    if pq.queue.is_empty() {
        let state = next(&Val::Unit, outcome, &Val::Number(n.try_into().unwrap()));
        return Return::new(State::singleton(state, 1.into()));
    };

    let mut result = State::new();
    (0..=n).for_each(|k| {
        let tail = solve(pq.clone(), n - k, next);
        let tail = {
            // println!("cached: {}", tail.was_cached);
            tail.value
        };

        tail.m.into_iter().for_each(|(state, weight)| {
            let state = next(&state, outcome, &Val::Number(k.try_into().unwrap()));
            let next_weight = weight * binomial(n, k).to_u128_wrapping() * prob as u128;
            result.insert(state, next_weight);
        });
    });

    Return::new(result)
}

#[cached(key = "u64", convert = r#"{hash_solve(&pq, n)}"#)]
fn solve_and_finalize(
    pq: PqRef,
    n: u32,
    next: fn(&Val, &Val, &Val) -> Val,
) -> std::vec::Vec<(Val, Rational)> {
    let total = Arc::new(Mutex::new(Rational::ZERO.clone()));

    let res = solve(pq, n, next)
        .value
        .m
        .into_par_iter()
        .map(|(k, v)| {
            {
                let mut total = total.lock().unwrap();
                *total += &v;
            }

            (k, v)
        })
        .collect::<Vec<_>>();

    let total = { total.lock().unwrap().clone() };
    let mut res = res
        .into_par_iter()
        .map(|(k, v)| (k, (v / total.clone())))
        .collect::<Vec<_>>();
    res.sort_unstable_by(|(l, _), (r, _)| match l.partial_cmp(r) {
        None => Ordering::Equal,
        Some(o) => o,
    });

    res
}

lazy_static! {
    static ref D2_PROB_QUEUE: Pq = BasicDie::D2.gen_prob_queue();
    static ref D4_PROB_QUEUE: Pq = BasicDie::D4.gen_prob_queue();
    static ref D6_PROB_QUEUE: Pq = BasicDie::D6.gen_prob_queue();
    static ref D8_PROB_QUEUE: Pq = BasicDie::D8.gen_prob_queue();
    static ref D10_PROB_QUEUE: Pq = BasicDie::D10.gen_prob_queue();
    static ref D12_PROB_QUEUE: Pq = BasicDie::D12.gen_prob_queue();
    static ref D20_PROB_QUEUE: Pq = BasicDie::D20.gen_prob_queue();
}

#[derive(Clone, Debug)]
pub enum BasicDie {
    D2,
    D4,
    D6,
    D8,
    D10,
    D12,
    D20,
}

impl BasicDie {
    pub fn base(&self) -> u32 {
        match self {
            BasicDie::D2 => 2,
            BasicDie::D4 => 4,
            BasicDie::D6 => 6,
            BasicDie::D8 => 8,
            BasicDie::D10 => 10,
            BasicDie::D12 => 12,
            BasicDie::D20 => 20,
        }
    }

    pub(crate) fn gen_prob_queue(&self) -> Pq {
        pq_from_base(self.base())
    }

    pub fn prob_queue(&self) -> Pq {
        match self {
            BasicDie::D2 => D2_PROB_QUEUE.to_owned(),
            BasicDie::D4 => D4_PROB_QUEUE.to_owned(),
            BasicDie::D6 => D6_PROB_QUEUE.to_owned(),
            BasicDie::D8 => D8_PROB_QUEUE.to_owned(),
            BasicDie::D10 => D10_PROB_QUEUE.to_owned(),
            BasicDie::D12 => D12_PROB_QUEUE.to_owned(),
            BasicDie::D20 => D20_PROB_QUEUE.to_owned(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Die {
    Basic {
        count: u32,
        kind: BasicDie,
        repr: Option<SmolStr>,
    },
    Custom {
        count: u32,
        prob_queue: Pq,
        repr: SmolStr,
    },
}

impl Die {
    pub fn of_base(base: u32) -> Die {
        let count = 1;
        match base {
            2 => Die::Basic {
                count,
                kind: BasicDie::D2,
                repr: None,
            },
            4 => Die::Basic {
                count,
                kind: BasicDie::D4,
                repr: None,
            },
            6 => Die::Basic {
                count,
                kind: BasicDie::D6,
                repr: None,
            },
            8 => Die::Basic {
                count,
                kind: BasicDie::D8,
                repr: None,
            },
            10 => Die::Basic {
                count,
                kind: BasicDie::D10,
                repr: None,
            },
            12 => Die::Basic {
                count,
                kind: BasicDie::D12,
                repr: None,
            },
            20 => Die::Basic {
                count,
                kind: BasicDie::D20,
                repr: None,
            },
            _ => Die::Custom {
                count,
                prob_queue: pq_from_base(base),
                repr: format!("d{base}").into(),
            },
        }
    }

    pub fn of_count_base(count: u32, base: u32) -> Die {
        match base {
            2 => Die::Basic {
                count,
                kind: BasicDie::D2,
                repr: None,
            },
            4 => Die::Basic {
                count,
                kind: BasicDie::D4,
                repr: None,
            },
            6 => Die::Basic {
                count,
                kind: BasicDie::D6,
                repr: None,
            },
            8 => Die::Basic {
                count,
                kind: BasicDie::D8,
                repr: None,
            },
            10 => Die::Basic {
                count,
                kind: BasicDie::D10,
                repr: None,
            },
            12 => Die::Basic {
                count,
                kind: BasicDie::D12,
                repr: None,
            },
            20 => Die::Basic {
                count,
                kind: BasicDie::D20,
                repr: None,
            },
            _ => Die::Custom {
                count,
                prob_queue: pq_from_base(base),
                repr: format!("d{base}").into(),
            },
        }
    }

    pub fn count(&self) -> u32 {
        match self {
            Die::Basic { count, .. } | Die::Custom { count, .. } => *count,
        }
    }

    pub fn prob_queue(&self) -> Pq {
        match self {
            Die::Basic { kind, .. } => kind.prob_queue(),
            Die::Custom { prob_queue, .. } => prob_queue.clone(),
        }
    }

    pub fn solve(&self, next: fn(&Val, &Val, &Val) -> Val) -> Vec<(Val, Rational)> {
        solve_and_finalize(self.prob_queue().as_ref(), self.count(), next)
    }
}

#[cfg(test)]
mod test {
    use ast::hir::Val;
    use insta::assert_snapshot;

    use crate::Die;

    fn e(d: Die, next: fn(&Val, &Val, &Val) -> Val) -> String {
        format!("{:#?}\n", d.solve(next))
    }

    fn next_state(state: &Val, outcome: &Val, count: &Val) -> Val {
        match state {
            Val::Unit => Val::Number(outcome.as_num() * count.as_num()),
            x => Val::Number(x.as_num() + outcome.as_num() * count.as_num()),
        }
    }

    #[test]
    fn test() {
        // println!("{:?}", Die::of_count_base(150, 20).prob_queue());
        // assert_snapshot!(e(Die::of_count_base(8, 4), next_state));
        // assert_snapshot!(e(Die::of_count_base(80, 4), next_state));
        // assert_snapshot!(e(Die::of_count_base(150, 20), next_state));

        Die::of_count_base(150, 20).solve(next_state);
    }
}

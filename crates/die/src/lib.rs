#![feature(step_trait)]

use ast::hir::Val;
use cached::proc_macro::cached;
use fxhash::FxHasher;
use lazy_static::lazy_static;
use rayon::prelude::*;
use rpds::{HashTrieMapSync, QueueSync};
use rug::{Complete, Integer, Rational};
use smol_str::SmolStr;
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    iter::Step,
    ops::Range,
    sync::{Arc, Mutex},
};

type ProbQueue = QueueSync<(Val, Rational)>;
type ProbMap = HashTrieMapSync<Val, Rational>;

fn hash_binomial(n: &Integer, k: &Integer) -> u64 {
    let mut state = FxHasher::default();

    n.hash(&mut state);
    k.hash(&mut state);

    state.finish()
}

#[cached(key = "u64", convert = r#"{hash_binomial(&n, k)}"#)]
fn binomial(n: &Integer, k: &Integer) -> Integer {
    n.clone().binomial(k.to_u32_wrapping())
}

#[cached]
fn prob_queue_from_base(base: u32) -> ProbQueue {
    (1..=base)
        .map(|x| (Val::Number(x as isize), Rational::from((x, base))))
        .rev()
        .collect()
}

fn pop_pq(pq: ProbQueue) -> (Val, ProbQueue) {
    assert!(!pq.is_empty());

    let (val, _) = pq.peek().unwrap().to_owned();
    let next = pq.dequeue().unwrap();
    (val, next)
}

fn hash_solve(pq: &ProbQueue, n: &Integer) -> u64 {
    let mut state = FxHasher::default();

    for (k, v) in pq.iter() {
        k.hash(&mut state);
        v.hash(&mut state)
    }

    n.hash(&mut state);

    state.finish()
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct IntegerWrapper(Integer);

impl IntegerWrapper {
    fn r_inc(lhs: &Integer, rhs: &Integer) -> Range<IntegerWrapper> {
        Range {
            start: IntegerWrapper(lhs.clone()),
            end: IntegerWrapper(rhs.clone() + 1),
        }
    }
}

impl Step for IntegerWrapper {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        (&end.0 - &start.0).complete().to_usize()
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(IntegerWrapper(start.0 + count))
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        Some(IntegerWrapper(start.0 - count))
    }
}

#[cached(key = "u64", convert = r#"{hash_solve(&pq, n)}"#)]
fn solve(
    pq: ProbQueue,
    n: &Integer,
    pm: &ProbMap,
    next: fn(&Val, &Val, &Val) -> Val,
) -> HashTrieMapSync<Val, Rational> {
    let (outcome, pq) = pop_pq(pq);

    if pq.is_empty() {
        let state = next(&Val::Unit, &outcome, &Val::Number(n.to_isize_wrapping()));
        return HashTrieMapSync::new_sync().insert(state, 1.into());
    };

    let mut result: HashTrieMapSync<Val, Rational> = HashTrieMapSync::new_sync();
    for k in IntegerWrapper::r_inc(&Integer::ZERO, n) {
        let k = k.0;

        let tail = solve(pq.clone(), &(n - &k).complete(), pm, next);
        for (state, weight) in tail.into_iter() {
            let state = next(state, &outcome, &Val::Number(k.to_isize_wrapping()));
            let weight = (weight * binomial(n, &k)).complete() * 1;

            result = match result.get(&state) {
                None => result.insert(state, weight),
                Some(o) => result.insert(state, o + weight),
            }
        }
    }

    result
}

#[cached(key = "u64", convert = r#"{hash_solve(&pq, n)}"#)]
fn solve_and_finalize(
    pq: ProbQueue,
    n: &Integer,
    next: fn(&Val, &Val, &Val) -> Val,
) -> std::vec::Vec<(Val, f64)> {
    let total = Arc::new(Mutex::new(Rational::ZERO.clone()));

    let pm = {
        let mut pm = HashTrieMapSync::new_sync();

        let mut total = Rational::ZERO.clone();
        for (_, p) in pq.iter() {
            total += p;
        }

        for (v, p) in pq.iter() {
            pm = pm.insert(v.clone(), p / total.clone());
        }

        pm
    };

    let res = solve(pq, n, &pm, next)
        .into_iter()
        .par_bridge()
        .map(|(k, v)| {
            {
                let mut total = total.lock().unwrap();
                *total += v;
            }

            (k.clone(), v.clone())
        })
        .collect::<Vec<_>>();

    let total = { total.lock().unwrap().clone() };
    let mut res = res
        .into_par_iter()
        .map(|(k, v)| (k, (v / total.clone()).to_f64() * 100.))
        .collect::<Vec<_>>();
    res.sort_unstable_by(|(l, _), (r, _)| match l.partial_cmp(r) {
        None => Ordering::Equal,
        Some(o) => o,
    });

    res
}

lazy_static! {
    static ref D2_PROB_QUEUE: ProbQueue = BasicDie::D2.gen_prob_queue();
    static ref D4_PROB_QUEUE: ProbQueue = BasicDie::D4.gen_prob_queue();
    static ref D6_PROB_QUEUE: ProbQueue = BasicDie::D6.gen_prob_queue();
    static ref D8_PROB_QUEUE: ProbQueue = BasicDie::D8.gen_prob_queue();
    static ref D10_PROB_QUEUE: ProbQueue = BasicDie::D10.gen_prob_queue();
    static ref D12_PROB_QUEUE: ProbQueue = BasicDie::D12.gen_prob_queue();
    static ref D20_PROB_QUEUE: ProbQueue = BasicDie::D20.gen_prob_queue();
}

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

    pub(crate) fn gen_prob_queue(&self) -> ProbQueue {
        prob_queue_from_base(self.base())
    }

    pub fn prob_queue(&self) -> ProbQueue {
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

pub enum Die {
    Basic {
        count: Integer,
        kind: BasicDie,
        repr: Option<SmolStr>,
    },
    Custom {
        count: Integer,
        prob_queue: ProbQueue,
        repr: SmolStr,
    },
}

impl Die {
    pub fn of_base(base: u32) -> Die {
        let count = Integer::ONE.clone();
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
                prob_queue: prob_queue_from_base(base),
                repr: format!("d{base}").into(),
            },
        }
    }

    pub fn of_count_base(count: u32, base: u32) -> Die {
        let count = Integer::from(count);
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
                prob_queue: prob_queue_from_base(base),
                repr: format!("d{base}").into(),
            },
        }
    }

    pub fn count(&self) -> Integer {
        match self {
            Die::Basic { count, .. } | Die::Custom { count, .. } => count.clone(),
        }
    }

    pub fn prob_queue(&self) -> ProbQueue {
        match self {
            Die::Basic { kind, .. } => kind.prob_queue(),
            Die::Custom { prob_queue, .. } => prob_queue.clone(),
        }
    }

    pub fn solve(&self, next: fn(&Val, &Val, &Val) -> Val) -> Vec<(Val, f64)> {
        solve_and_finalize(self.prob_queue(), &self.count(), next)
    }
}

#[cfg(test)]
mod test {
    use ast::hir::Val;

    use crate::Die;

    fn next_state(state: &Val, outcome: &Val, count: &Val) -> Val {
        match state {
            Val::Unit => Val::Number(outcome.as_num() * count.as_num()),
            x => Val::Number(x.as_num() + outcome.as_num() * count.as_num()),
        }
    }

    #[test]
    fn test() {
        let die = Die::of_count_base(150, 20);

        assert_eq!("", format!("{:?}", die.solve(next_state)))
    }
}

/*

 * d20 + 17
 * d10 + 11 + 2d6 + 4 + 1d6 + 10
 * 1d4 + 11 + 2d6 + 4

 * d20 + 17 - 5
 * d20 + 9 + 4 + 1d6 + 10 + 10

 * (2 * (4d10 + 2d6 + 1d6)) + 11 + 4 + 10

 * 184
 * 176
 * 170
 * 157
 * 148
 * 127
 * 115
 * 105
 * 92
 * 63
 * 39
 * 33
 * 

 */

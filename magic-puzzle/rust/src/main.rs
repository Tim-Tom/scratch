extern crate rayon;

use rayon::prelude::*;

// const WIDTH : usize = 3;
const WIDTH : usize = 4;
const WM : usize = WIDTH - 1;
const WP : usize = WIDTH + 1;
const SIZE : usize = WIDTH * WIDTH;

const NUM_PERMUTATIONS : usize = 8;

// const GOAL : i32 = 21;
// const CHOICES : [i32; SIZE] = [3,4,5,6,7,8,9,10,11];
const GOAL : i32 = 34;
const CHOICES : [i32; SIZE] = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];


const TL : usize = 0;
const TR : usize = (WIDTH - 1);
const BL : usize = ((WIDTH - 1)*WIDTH);
const BR : usize = (WIDTH*WIDTH - 1);


type Perform = fn(usize, &mut[bool; SIZE], &mut[i32; SIZE]);

struct Action {
    pos : usize,
    action : Perform,
    indexes : [usize; WIDTH],
}

const ACTIONS : [Action; SIZE + 4] = [
    // Action { pos : 0, action : choose,    indexes : [0, 0, 0] },
    // Action { pos : 2, action : choose_tr, indexes : [0, 0, 0] },
    // Action { pos : 1, action : decide,    indexes : [0, 2, 0] },
    // Action { pos : 6, action : choose_bl, indexes : [0, 0, 0] },
    // Action { pos : 3, action : decide,    indexes : [0, 6, 0] },
    // Action { pos : 4, action : decide,    indexes : [2, 6, 0] },
    // Action { pos : 5, action : decide,    indexes : [3, 4, 0] },
    // Action { pos : 7, action : decide,    indexes : [1, 4, 0] },
    // Action { pos : 8, action : decide,    indexes : [6, 7, 0] },
    // Action { pos : 0, action : validate,  indexes : [0, 4, 8] },
    // Action { pos : 0, action : validate,  indexes : [2, 5, 8] },
    // Action { pos : 0, action : check_br,  indexes : [0, 0, 0] },
    // Action { pos : 0, action : solution,  indexes : [0, 0, 0] }

    Action { pos :  0, action : choose,    indexes: [ 0, 0, 0, 0] },
    Action { pos :  3, action : choose_tr, indexes: [ 0, 0, 0, 0] },
    Action { pos :  1, action : choose,    indexes: [ 0, 0, 0, 0] },
    Action { pos :  2, action : decide,    indexes: [ 0, 1, 3, 0] },
    Action { pos : 12, action : choose_bl, indexes: [ 0, 0, 0, 0] },
    Action { pos :  6, action : choose,    indexes: [ 0, 0, 0, 0] },
    Action { pos :  9, action : decide,    indexes: [ 3, 6,12, 0] },
    Action { pos : 10, action : choose,    indexes: [ 0, 0, 0, 0] },
    Action { pos : 14, action : decide,    indexes: [ 2, 6,10, 0] },
    Action { pos :  5, action : choose,    indexes: [ 0, 0, 0, 0] },
    Action { pos : 15, action : decide,    indexes: [ 0, 5,10, 0] },
    Action { pos :  0, action : check_br,  indexes: [ 0, 0, 0, 0] },
    Action { pos : 13, action : decide,    indexes: [ 1, 5, 9, 0] },
    Action { pos :  0, action : validate,  indexes: [12,13,14,15] },
    Action { pos :  4, action : choose,    indexes: [ 0, 0, 0, 0] },
    Action { pos :  7, action : decide,    indexes: [ 4, 5, 6, 0] },
    Action { pos :  8, action : decide,    indexes: [ 0, 4,12, 0] },
    Action { pos : 11, action : decide,    indexes: [ 8, 9,10, 0] },
    Action { pos :  0, action : validate,  indexes: [ 3, 7,11,15] },
    Action { pos :  0, action : solution,  indexes: [ 0, 0, 0, 0] }
];

const PERMUTATIONS : [[usize; SIZE]; NUM_PERMUTATIONS] = [
    // [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ],
    // [ 6, 7, 8, 3, 4, 5, 0, 1, 2 ],
    // [ 2, 1, 0, 5, 4, 3, 8, 7, 6 ],
    // [ 8, 7, 6, 5, 4, 3, 2, 1, 0 ],
    // [ 0, 3, 6, 1, 4, 7, 2, 5, 8 ],
    // [ 6, 3, 0, 7, 4, 1, 8, 5, 2 ],
    // [ 2, 5, 8, 1, 4, 7, 0, 3, 6 ],
    // [ 8, 5, 2, 7, 4, 1, 6, 3, 0 ]

  [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ],
  [ 12, 13, 14, 15,  8,  9, 10, 11,  4,  5,  6,  7,  0,  1,  2,  3 ],
  [  3,  2,  1,  0,  7,  6,  5,  4, 11, 10,  9,  8, 15, 14, 13, 12 ],
  [ 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0 ],
  [  0,  4,  8, 12,  1,  5,  9, 13,  2,  6, 10, 14,  3,  7, 11, 15 ],
  [ 12,  8,  4,  0, 13,  9,  5,  1, 14, 10,  6,  2, 15, 11,  7,  3 ],
  [  3,  7, 11, 15,  2,  6, 10, 14,  1,  5,  9, 13,  0,  4,  8, 12 ],
  [ 15, 11,  7,  3, 14, 10,  6,  2, 13,  9,  5,  1, 12,  8,  4,  0 ]
];

fn choice_search(n : i32) -> usize {
    for i in 0 .. SIZE {
        if CHOICES[i] == n {
            return i;
        }
    }
    return SIZE + 1;
}

fn choose_p(ai : usize, picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    let action = &ACTIONS[ai];
    (0 .. SIZE).into_par_iter().for_each(|i| {
        let mut thread_picked = picked.clone();
        let mut thread_a = a.clone();
        thread_a[action.pos] = CHOICES[i];
        thread_picked[i] = true;
        (ACTIONS[ai + 1].action)(ai + 1, &mut thread_picked, &mut thread_a);
        thread_picked[i] = false;
    });
}

fn choose(ai : usize, picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    let action = &ACTIONS[ai];
     for i in 0 .. SIZE {
        if !picked[i] {
            a[action.pos] = CHOICES[i];
            picked[i] = true;
            (ACTIONS[ai + 1].action)(ai + 1, picked, a);
            picked[i] = false;
        }
    }
}

fn choose_tr(ai : usize, picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    let action = &ACTIONS[ai];
    let tli = choice_search(a[TL]);
    for i in tli + 1 .. SIZE {
        if !picked[i] {
            a[action.pos] = CHOICES[i];
            picked[i] = true;
            (ACTIONS[ai + 1].action)(ai + 1, picked, a);
            picked[i] = false;
        }
    }   
}

fn choose_bl(ai : usize, picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    let action = &ACTIONS[ai];
    let tri = choice_search(a[TR]);
    for i in tri + 1 .. SIZE {
        if !picked[i] {
            a[action.pos] = CHOICES[i];
            picked[i] = true;
            (ACTIONS[ai + 1].action)(ai + 1, picked, a);
            picked[i] = false;
        }
    }
}

fn check_br(ai : usize, picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    if a[BL] < a[BR] || a[TL] < a[BR] {
        (ACTIONS[ai + 1].action)(ai + 1, picked, a);
    }
}
    
fn decide(ai : usize, picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    let action = &ACTIONS[ai];
    let mut n = GOAL;
    for i in 0 .. WM {
        n = n - a[action.indexes[i]];
    }
    let possible = choice_search(n);
    if possible == SIZE + 1 || picked[possible] {
        return
    }
    a[action.pos] = n;
    picked[possible] = true;
    (ACTIONS[ai + 1].action)(ai + 1, picked, a);
    picked[possible] = false;
}

fn validate(ai : usize, picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    let action = &ACTIONS[ai];
    let mut n = GOAL;
    for i in 0 .. WIDTH {
        n = n - a[action.indexes[i]];
    }
    if n == 0 {
        (ACTIONS[ai + 1].action)(ai + 1, picked, a);
    }
}

fn print_solution(a : &[i32; SIZE], indexes : &[usize; SIZE]) {
    println!("--- Solution ---");
    let mut i : usize = 0;
    let mut c = 1;
    loop {
        let sep;
        if c == WIDTH {
            sep = '\n';
            c = 1;
        } else {
            sep = ' ';
            c = c + 1;
        }
        print!("{:2}{:}", a[indexes[i]], sep);
        i = i + 1;
        if i == SIZE {
            break;
        }
    }
}

fn solution(_ai : usize, _picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    for i in 0 .. NUM_PERMUTATIONS {
        print_solution(a, &PERMUTATIONS[i]);
    }
}


fn main() {
    // let mut picked : [bool; SIZE] = [false, false, false, false, false, false, false, false, false];
    // let mut a : [i32; SIZE] = [-1, -1, -1, -1, -1, -1, -1, -1, -1];
    let mut picked : [bool; SIZE] = [false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false];
    let mut a : [i32; SIZE] = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1];
    (ACTIONS[0].action)(0, &mut picked, &mut a);
}

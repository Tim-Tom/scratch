// extern crate rayon;

// use rayon::prelude::*;

// const WIDTH : usize = 3;
const WIDTH : usize = 4;
const WM : usize = WIDTH - 1;
const WP : usize = WIDTH + 1;
const SIZE : usize = WIDTH * WIDTH;

// const GOAL : i32 = 21;
// const CHOICES : [i32; SIZE] = [3,4,5,6,7,8,9,10,11];
const GOAL : i32 = 34;
const CHOICES : [i32; SIZE] = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];

type Perform = fn(usize, &mut[bool; SIZE], &mut[i32; SIZE]);

struct Action {
    pos : usize,
    action : Perform,
    indexes : [usize; WIDTH],
}

const ACTIONS : [Action; SIZE + 3] = [
    // Action { pos : 0, action : choose,   indexes : [0, 0, 0] },
    // Action { pos : 1, action : choose,   indexes : [0, 0, 0] },
    // Action { pos : 2, action : decide,   indexes : [0, 1, 0] },
    // Action { pos : 4, action : choose,   indexes : [0, 0, 0] },
    // Action { pos : 8, action : decide,   indexes : [0, 4, 0] },
    // Action { pos : 5, action : decide,   indexes : [2, 8, 0] },
    // Action { pos : 3, action : decide,   indexes : [4, 5, 0] },
    // Action { pos : 6, action : decide,   indexes : [0, 3, 0] },
    // Action { pos : 7, action : decide,   indexes : [6, 8, 0] },
    // Action { pos : 0, action : validate, indexes : [1, 4, 7] },
    // Action { pos : 0, action : validate, indexes : [2, 4, 6] },
    // Action { pos : 0, action : solution, indexes : [0, 0, 0] }
    Action { pos :  0, action : choose,   indexes: [0, 0, 0, 0] },
    Action { pos :  1, action : choose,   indexes: [0, 0, 0, 0] },
    Action { pos :  2, action : choose,   indexes: [0, 0, 0, 0] },
    Action { pos :  3, action : decide,   indexes: [0, 1, 2, 0] },
    Action { pos :  5, action : choose,   indexes: [0, 0, 0, 0] },
    Action { pos :  9, action : choose,   indexes: [0, 0, 0, 0] },
    Action { pos : 13, action : decide,   indexes: [1, 5, 9, 0] },
    Action { pos : 10, action : choose,   indexes: [0, 0, 0, 0] },
    Action { pos : 15, action : decide,   indexes: [0, 5,10, 0] },
    Action { pos :  6, action : choose,   indexes: [0, 0, 0, 0] },
    Action { pos : 14, action : decide,   indexes: [2, 6,10, 0] },
    Action { pos : 12, action : decide,   indexes: [13,14,15,0] },
    Action { pos :  4, action : choose,   indexes: [0, 0, 0, 0] },
    Action { pos :  7, action : decide,   indexes: [4, 5, 6, 0] },
    Action { pos :  8, action : decide,   indexes: [0, 4,12, 0] },
    Action { pos : 11, action : decide,   indexes: [8, 9,10, 0] },
    Action { pos :  0, action : validate, indexes: [3, 7,11,15] },
    Action { pos :  0, action : validate, indexes: [3, 6, 9,12] },
    Action { pos :  0, action : solution, indexes: [0, 0, 0, 0] }
];

fn choice_search(n : i32) -> usize {
    for i in 0 .. SIZE {
        if CHOICES[i] == n {
            return i;
        }
    }
    return SIZE + 1;
}

fn choose(ai : usize, picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    let action = &ACTIONS[ai];
    // (0 .. SIZE).into_iter().for_each(|i| {
    //     if !picked[i] {
    //         let mut thread_picked = picked.clone();
    //         let mut thread_a = a.clone();
    //         thread_a[action.pos] = CHOICES[i];
    //         thread_picked[i] = true;
    //         (ACTIONS[ai + 1].action)(ai + 1, &mut thread_picked, &mut thread_a);
    //         thread_picked[i] = false;
    //     }
    // });
    for i in 0 .. SIZE {
        if !picked[i] {
            a[action.pos] = CHOICES[i];
            picked[i] = true;
            (ACTIONS[ai + 1].action)(ai + 1, picked, a);
            picked[i] = false;
        }
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

fn solution(_ai : usize, _picked : &mut[bool; SIZE], a : &mut[i32; SIZE]) {
    let mut i : usize = 0;
    let mut c = 1;
    println!("--- Solution ---");
    loop {
        let sep;
        if c == WIDTH {
            sep = '\n';
            c = 1;
        } else {
            sep = ' ';
            c = c + 1;
        }
        print!("{:2}{:}", a[i], sep);
        i = i + 1;
        if i == SIZE {
            break;
        }
    }
}


fn main() {
    let mut picked : [bool; SIZE] = [false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false];
    let mut a : [i32; SIZE] = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1];
    (ACTIONS[0].action)(0, &mut picked, &mut a);
}

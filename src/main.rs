use std::env;
use rusttype::{Point, point, vector};
use std::str::FromStr;
use std::collections::HashSet;

type HashT = usize;
type RenderDistance = u8;
type Coord = i32;
type Pos = Point<Coord>;
type Permaloader = (Pos, Pos);
type QuestionableBool = Option<()>;

const YES: QuestionableBool = Some(());

fn main() {
    if run().is_some() {
        println!("Successful");
    }
}

fn run() -> QuestionableBool {
    let args: Vec<_> = env::args().collect();
    if args.len() < 8 || (args.len() - 8) % 4 != 0 {
        println!("{} <hash-size> <render-distance> <spawn-x> <spawn-z> <glass-cx> <glass-cz> <rectangle-width> (<permaloader-start-cx> <permaloader-start-cz> <permaloader-end-cx> <permaloader-end-cz>)...", args[0]);
        return None;
    }

    let hash_size: HashT = parse(&args[1], "hash size")?;
    if !hash_size.is_power_of_two() {
        println!("Hash size must be a power of 2");
        return None;
    }
    if hash_size < 4 {
        println!("Hash size is too small");
        return None;
    }

    let render_distance: RenderDistance = parse(&args[2], "render distance")?;
    if render_distance < 2 || render_distance > 64 {
        println!("Render distance must be between 2 and 64");
        return None;
    }

    let spawn_x: Coord = parse(&args[3], "spawn x")?;
    let spawn_z: Coord = parse(&args[4], "spawn z")?;

    let glass_cx: Coord = parse(&args[5], "glass chunk x")?;
    let glass_cz: Coord = parse(&args[6], "glass chunk z")?;

    let rectangle_width: Coord = parse(&args[7], "rectangle width")?;

    let mut permaloaders = Vec::new();
    for i in (8..args.len()).step_by(4) {
        let permaloader_start_cx: Coord = parse(&args[i], "permaloader start chunk x")?;
        let permaloader_start_cz: Coord = parse(&args[i + 1], "permaloader start chunk z")?;
        let permaloader_end_cx: Coord = parse(&args[i + 2], "permaloader end chunk x")?;
        let permaloader_end_cz: Coord = parse(&args[i + 3], "permaloader end chunk z")?;
        if ![-1, 0].contains(&(permaloader_start_cx ^ permaloader_start_cz)) || (permaloader_end_cx ^ permaloader_end_cz) != (permaloader_start_cx ^ permaloader_start_cz) {
            println!("Invalid permaloader");
            return None;
        }
        permaloaders.push((point(permaloader_start_cx, permaloader_start_cz), point(permaloader_end_cx, permaloader_end_cz)));
    }

    find(hash_size,
         render_distance,
         point(spawn_x, spawn_z),
         point(glass_cx, glass_cz),
         rectangle_width,
         &permaloaders)?;

    return YES;
}

fn parse<T>(val: &String, name: &str) -> Option<T> where T: FromStr {
    match val.parse() {
        Ok(v) => Some(v),
        _ => {
            println!("Invalid {}", name);
            None
        }
    }
}

fn find(hash_size: HashT, render_distance: RenderDistance, spawn_pos: Pos, glass_chunk: Pos, rectangle_width: Coord, permaloaders: &Vec<Permaloader>) -> QuestionableBool {
    let mut hashmap = OpenHashMap::new(hash_size);

    prefill_hashmap(hash_size, spawn_pos, &glass_chunk, permaloaders, &mut hashmap)?;

    let buffer_chunk = find_buffer_chunk(hash_size, render_distance, &glass_chunk, &mut hashmap);
    println!("Buffer chunk: ({}, {})", buffer_chunk.x, buffer_chunk.y);
    hashmap.insert(buffer_chunk);

    return YES;
}

fn find_buffer_chunk(hash_size: usize, render_distance: u8, glass_chunk: &Point<i32>, hashmap: &mut OpenHashMap) -> Pos {
    let mut radius = render_distance as i32 + 1;
    loop {
        for dx in -radius..=radius {
            for sign in &[-1, 1] {
                let dz = (radius - dx) * sign;
                if dx.abs() > render_distance as i32 || dz.abs() > render_distance as i32 {
                    let chunk_pos = *glass_chunk + vector(dx, dz);
                    if !hashmap.contains(&chunk_pos) && hash(&chunk_pos, hash_size) == hash(&glass_chunk, hash_size) {
                        return chunk_pos;
                    }
                }
            }
        }
        radius += 1;
    }
}

fn prefill_hashmap(hash_size: usize, spawn_pos: Point<i32>, glass_chunk: &Point<i32>, permaloaders: &Vec<(Point<i32>, Point<i32>)>, hashmap: &mut OpenHashMap) -> QuestionableBool {
    let mut invalid_hashes = HashSet::new();
    for spawn_x in get_spawn_chunks_range(spawn_pos.x) {
        for spawn_z in get_spawn_chunks_range(spawn_pos.y) {
            let spawn_chunk = point(spawn_x, spawn_z);
            if spawn_x != 0 || spawn_z != 0 {
                invalid_hashes.insert(hash(&spawn_chunk, hash_size));
            }
            hashmap.insert(spawn_chunk)?;
        }
    }

    for (permaloader_start, permaloader_end) in permaloaders {
        let permaloader_length = (permaloader_start.x - permaloader_end.x).abs();
        for i in 0..=permaloader_length {
            let delta = vector((permaloader_end.x - permaloader_start.x) * i / permaloader_length, (permaloader_end.y - permaloader_start.y) * i / permaloader_length);
            let permaloader_chunk = *permaloader_start + delta;
            if permaloader_chunk.x != 0 || permaloader_chunk.y != 0 {
                invalid_hashes.insert(hash(&permaloader_chunk, hash_size));
            }
            hashmap.insert(permaloader_chunk)?;
        }
    }

    if invalid_hashes.contains(&hash(&glass_chunk, hash_size)) {
        println!("Glass chunk has hash collision with the spawn chunks or permaloader!");
        if invalid_hashes.len() == hash_size {
            println!("All hashes were taken up. Try a higher hash size");
            return None;
        }
        println!("But valid chunks were found nearby...");
        let mut radius = 1;
        let mut found = false;
        while radius % 5 != 0 || !found {
            for dx in -radius..=radius {
                let mut chunk = *glass_chunk + vector(dx, radius - dx);
                if !invalid_hashes.contains(&hash(&chunk, hash_size)) {
                    found = true;
                    println!("({}, {})", chunk.x, chunk.y);
                }
                chunk = *glass_chunk + vector(dx, dx - radius);
                if !invalid_hashes.contains(&hash(&chunk, hash_size)) {
                    found = true;
                    println!("({}, {})", chunk.x, chunk.y);
                }
            }

            radius += 1;
        }

        return None;
    }

    return YES;
}

fn get_spawn_chunks_range(coord: Coord) -> impl Iterator<Item = i32> {
    div_ceil(coord - 128 - 8, 16)..=(coord + 128 - 8).div_euclid(16)
}

fn div_ceil(a: i32, b: i32) -> i32 {
    (a + (b - 1)).div_euclid(b)
}

fn hash(pos: &Pos, hash_size: HashT) -> HashT {
    let long = ((pos.x as u64) << 32) | pos.y as u64;
    let mut hashed = long.wrapping_mul(0x9E3779B97F4A7C15);
    hashed ^= hashed >> 32;
    hashed ^= hashed >> 16;
    return (hashed & ((hash_size - 1) as u64)) as HashT;
}

#[derive(Clone)]
struct OpenHashMap {
    vec: Vec<Option<Pos>>,
    mask: HashT,
    size: usize,
    max_fill: usize,
    contains_zero: bool
}
impl OpenHashMap {
    fn new(hash_size: HashT) -> OpenHashMap {
        OpenHashMap {
            vec: vec![None; hash_size],
            mask: hash_size - 1,
            size: 0,
            max_fill: hash_size * 3 / 4,
            contains_zero: false
        }
    }

    fn contains(&self, pos: &Pos) -> bool {
        if pos.x == 0 && pos.y == 0 {
            return self.contains_zero;
        }
        let mut i = hash(pos, self.vec.len());
        while self.vec[i].is_some() {
            if self.vec[i].unwrap() == *pos {
                return true;
            }
            i = (i + 1) & self.mask;
        }
        return false;
    }

    fn insert(&mut self, pos: Pos) -> Option<bool> {
        if pos.x == 0 && pos.y == 0 {
            if self.contains_zero {
                return Some(false);
            }
            if self.size >= self.max_fill {
                println!("Hashmap exceeded rehash threshold. Consider using a larger hash size.");
                return None;
            }
            self.size += 1;
            self.contains_zero = true;
            return Some(true);
        }

        let mut i = hash(&pos, self.vec.len());
        while self.vec[i].is_some() {
            if self.vec[i].unwrap().x == pos.x && self.vec[i].unwrap().y == pos.y {
                return Some(false);
            }
            i = (i + 1) & self.mask;
        }

        if self.size >= self.max_fill {
            println!("Hashmap exceeded rehash threshold. Consider using a larger hash size.");
            return None;
        }

        self.vec[i] = Some(pos);
        self.size += 1;

        return Some(true);
    }
}

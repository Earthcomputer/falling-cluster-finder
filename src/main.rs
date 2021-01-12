use std::env;
use rusttype::{Point, point, vector};
use std::str::FromStr;
use crate::RectangleCheckResult::{FAILED, SUCCESS};
use std::io::Write;

type HashT = usize;
type RenderDistance = i32;
type Coord = i32;
type Pos = Point<Coord>;
type Permaloader = (Pos, Pos);
type QuestionableBool = Option<()>;

const YES: QuestionableBool = Some(());

fn main() {
    if run().is_some() {
        println!("----------");
        println!("Successful");
    }
}

fn run() -> QuestionableBool {
    const NUM_FIXED_ARGS: usize = 14;

    let args: Vec<_> = env::args().collect();

    if args.len() == 1 {
        return run_interactive_mode();
    }

    if args.len() < NUM_FIXED_ARGS || (args.len() - NUM_FIXED_ARGS) % 4 != 0 {
        println!("{} <hash-size> <render-distance> <spawn-x> <spawn-z> <glass-cx> <glass-cz> <unload-chunk-search-cx> <unload-chunk-search-cz> <rectangle-width> <cluster-chunks> <cluster-search-cx> <cluster-search-cz> <min-search> (<permaloader-start-cx> <permaloader-start-cz> <permaloader-end-cx> <permaloader-end-cz>)...", args[0]);
        return None;
    }

    let hash_size: HashT = parse(&args[1], "hash size")?;
    validate_hash_size(hash_size)?;

    let render_distance: RenderDistance = parse(&args[2], "render distance")?;
    validate_render_distance(render_distance)?;

    let spawn_x: Coord = parse(&args[3], "spawn x")?;
    let spawn_z: Coord = parse(&args[4], "spawn z")?;

    let glass_cx: Coord = parse(&args[5], "glass chunk x")?;
    let glass_cz: Coord = parse(&args[6], "glass chunk z")?;

    let unload_search_cx: Coord = parse(&args[7], "unload chunk search x")?;
    let unload_search_cz: Coord = parse(&args[8], "unload chunk search z")?;

    let rectangle_width: Coord = parse(&args[9], "rectangle width")?;
    let cluster_chunks: usize = parse(&args[10], "cluster chunks")?;

    let cluster_search_cx: Coord = parse(&args[11], "cluster search chunk x")?;
    let cluster_search_cz: Coord = parse(&args[12], "cluster search chunk z")?;

    let min_search: usize = parse(&args[13], "min search")?;

    let mut permaloaders = Vec::new();
    for i in (NUM_FIXED_ARGS..args.len()).step_by(4) {
        let permaloader_start_cx: Coord = parse(&args[i], "permaloader start chunk x")?;
        let permaloader_start_cz: Coord = parse(&args[i + 1], "permaloader start chunk z")?;
        let permaloader_end_cx: Coord = parse(&args[i + 2], "permaloader end chunk x")?;
        let permaloader_end_cz: Coord = parse(&args[i + 3], "permaloader end chunk z")?;
        validate_permaloader(permaloader_start_cx, permaloader_start_cz, permaloader_end_cx, permaloader_end_cz)?;
        permaloaders.push((point(permaloader_start_cx, permaloader_start_cz), point(permaloader_end_cx, permaloader_end_cz)));
    }

    find(hash_size,
         render_distance,
         point(spawn_x, spawn_z),
         point(glass_cx, glass_cz),
         point(unload_search_cx, unload_search_cz),
         rectangle_width,
         cluster_chunks,
         point(cluster_search_cx, cluster_search_cz),
         min_search,
         &permaloaders)?;

    return YES;
}

fn run_interactive_mode() -> QuestionableBool {
    println!("Running in interactive mode. Run with --help for non-interactive usage.");
    println!();

    print!("Hash size: ");
    let hash_size: HashT = parse(&read_line()?, "hash size")?;
    validate_hash_size(hash_size)?;

    print!("Render distance: ");
    let render_distance: RenderDistance = parse(&read_line()?, "render distance")?;
    validate_render_distance(render_distance)?;

    print!("Spawn block X: ");
    let spawn_x: Coord = parse(&read_line()?, "spawn x")?;
    print!("Spawn block Z: ");
    let spawn_z: Coord = parse(&read_line()?, "spawn z")?;

    print!("Glass chunk X: ");
    let glass_cx: Coord = parse(&read_line()?, "glass chunk x")?;
    print!("Glass chunk Z: ");
    let glass_cz: Coord = parse(&read_line()?, "glass chunk z")?;

    print!("Unload search chunk X: ");
    let unload_search_cx: Coord = parse(&read_line()?, "unload chunk search x")?;
    print!("Unload search chunk Z: ");
    let unload_search_cz: Coord = parse(&read_line()?, "unload chunk search z")?;

    print!("Rectangle width: ");
    let rectangle_width: Coord = parse(&read_line()?, "rectangle width")?;
    print!("Cluster chunks: ");
    let cluster_chunks: usize = parse(&read_line()?, "cluster chunks")?;

    print!("Cluster search chunk X: ");
    let cluster_search_cx: Coord = parse(&read_line()?, "cluster search chunk x")?;
    print!("Cluster search chunk Z: ");
    let cluster_search_cz: Coord = parse(&read_line()?, "cluster search chunk z")?;

    print!("Min search: ");
    let min_search: usize = parse(&read_line()?, "min search")?;

    let mut permaloaders = Vec::new();
    loop {
        print!("Permaloader start chunk X (press enter for no more permaloader diagonals): ");
        let result = match read_line() {
            Some(v) => v,
            _ => break
        };
        if result.is_empty() {
            break;
        }
        let permaloader_start_cx: Coord = parse(&result, "permaloader start chunk x")?;

        print!("Permaloader start chunk Z: ");
        let permaloader_start_cz: Coord = parse(&read_line()?, "permaloader start chunk z")?;
        print!("Permaloader end chunk X: ");
        let permaloader_end_cx: Coord = parse(&read_line()?, "permaloader end chunk x")?;
        print!("Permaloader end chunk Z: ");
        let permaloader_end_cz: Coord = parse(&read_line()?, "permaloader end chunk z")?;
        validate_permaloader(permaloader_start_cx, permaloader_start_cz, permaloader_end_cx, permaloader_end_cz)?;

        permaloaders.push((point(permaloader_start_cx, permaloader_start_cz), point(permaloader_end_cx, permaloader_end_cz)));
    }

    println!();

    find(hash_size,
         render_distance,
         point(spawn_x, spawn_z),
         point(glass_cx, glass_cz),
         point(unload_search_cx, unload_search_cz),
         rectangle_width,
         cluster_chunks,
         point(cluster_search_cx, cluster_search_cz),
         min_search,
         &permaloaders)?;

    return YES;
}

fn read_line() -> Option<String> {
    std::io::stdout().flush().ok()?;
    let mut line = String::new();
    std::io::stdin().read_line(&mut line).ok()?;
    return Some(String::from(line.trim_end()));
}

fn validate_hash_size(hash_size: HashT) -> QuestionableBool {
    if !hash_size.is_power_of_two() {
        println!("Hash size must be a power of 2");
        return None;
    }
    if hash_size < 4 {
        println!("Hash size is too small");
        return None;
    }
    return YES;
}

fn validate_render_distance(render_distance: RenderDistance) -> QuestionableBool {
    if render_distance < 2 || render_distance > 64 {
        println!("Render distance must be between 2 and 64");
        return None;
    }
    return YES;
}

fn validate_permaloader(permaloader_start_cx: Coord, permaloader_start_cz: Coord, permaloader_end_cx: Coord, permaloader_end_cz: Coord) -> QuestionableBool {
    if (permaloader_end_cx - permaloader_start_cx).abs() != (permaloader_end_cz - permaloader_start_cz).abs() {
        println!("Invalid permaloader");
        return None;
    }
    return YES;
}

fn is_permaloader_protected(chunk: &Pos) -> bool {
    ![-1, 0].contains(&(chunk.x ^ chunk.y))
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

fn find(hash_size: HashT,
        render_distance: RenderDistance,
        spawn_pos: Pos,
        glass_chunk: Pos,
        unload_search_origin: Pos,
        rectangle_width: Coord,
        cluster_chunks: usize,
        cluster_search_origin: Pos,
        min_search: usize,
        permaloaders: &Vec<Permaloader>
) -> QuestionableBool {
    let mut hashmap = OpenHashMap::new(hash_size);

    prefill_hashmap(hash_size, spawn_pos, &glass_chunk, permaloaders, &mut hashmap)?;

    let glass_hash = hash(&glass_chunk, hash_size);

    let mut size_before = cluster_chunks;
    account_for_existing_cluster_chunks(&hashmap, glass_hash, &mut size_before);
    size_before += hashmap.size;
    let hash_size_before = get_min_hash_size(size_before);

    let swap_chunk = find_glass_hash_chunk(glass_hash, hash_size, &glass_chunk, 1, &hashmap, |_| true);
    println!("Swap chunk: ({}, {})", swap_chunk.x, swap_chunk.y);
    let mut illegal_chunks = hashmap.clone();
    for dx in -render_distance..=render_distance {
        for dz in -render_distance..=render_distance {
            illegal_chunks.insert(glass_chunk + vector(dx, dz))?;
        }
    }
    illegal_chunks.insert(swap_chunk)?;

    let unload_chunk = find_glass_hash_chunk(glass_hash, hash_size, &unload_search_origin, 0, &illegal_chunks, |chunk| {
        for dx in 0..render_distance*2+2 {
            for dz in 0..render_distance*2+2 {
                if (dx != 0 || dz != 0) && (dx != render_distance*2+1 || dz != render_distance*2+1) {
                    let other_chunk = *chunk + vector(dx, dz);
                    if hash(&other_chunk, hash_size) == glass_hash {
                        return false;
                    }
                }
            }
        }
        return true;
    });
    for dx in 0..render_distance*2+2 {
        for dz in 0..render_distance*2+2 {
            if dx != render_distance*2+1 || dz != render_distance*2+1 {
                let other_chunk = unload_chunk + vector(dx, dz);
                illegal_chunks.insert(other_chunk);
            }
        }
    }
    println!("Unload chunk: ({}, {})", unload_chunk.x, unload_chunk.y);
    illegal_chunks.insert(unload_chunk)?;

    find_cluster_chunks(hash_size, cluster_search_origin, rectangle_width, cluster_chunks, &hashmap, glass_hash, &illegal_chunks, min_search)?;

    let hash_size_after = get_min_hash_size(illegal_chunks.size);

    if hash_size_before != hash_size_after {
        println!("!!!!!");
        println!("Warning: upwards rehash will occur while loading player chunks.");
        println!("To mitigate this, load {} chunks before you load the cluster chunks, and unload them as you load the player chunks.", hash_size_after * 3 / 8 - size_before);
        println!("Make sure during the player loading, you always stay above {} loaded chunks in the world.", hash_size_after / 4);
        if size_before <= hash_size_after / 4 {
            println!("!!!!!");
            println!("Warning: downwards rehash will occur when unloading player chunks.");
            println!("To mitigate, do the above after every re-attempt.");
        }
    }

    return YES;
}

fn get_min_hash_size(size_before: usize) -> usize {
    if size_before >= size_before.next_power_of_two() * 3 / 4 {
        size_before.next_power_of_two() * 2
    } else {
        size_before.next_power_of_two()
    }
}

fn find_cluster_chunks(hash_size: usize, origin: Point<i32>, rectangle_width: i32, num_cluster_chunks: usize, hashmap: &OpenHashMap, glass_hash: usize, illegal_chunks: &OpenHashMap, min_search: usize) -> QuestionableBool {
    let mut radius: i32 = 1;
    let mut best_rectangle_origin = Pos::default();
    let mut best_rectangle_size = point(10000, 10000);
    let mut best_cluster_chunks = Vec::new();
    let mut searched = 0;
    let mut reconsidered = 0;
    while reconsidered == 0 || searched < min_search {
        println!("Radius: {}, searched {}, reconsidered {} times", radius, searched, reconsidered);
        for dx in -radius..=radius {
            for sign in &[-1, 1] {
                let dz = (radius - dx.abs()) * sign;
                let rectangle_origin = origin + vector(dx, dz);
                let mut length = 1;
                let mut cluster_chunks = Vec::new();
                loop {
                    let rectangle_size = point(rectangle_width, length);
                    let result = check_rectangle(&rectangle_origin, &rectangle_size, &hashmap, &illegal_chunks, glass_hash, hash_size, num_cluster_chunks, &mut cluster_chunks);
                    if result == FAILED {
                        break;
                    } else if result == SUCCESS {
                        searched += 1;
                        if rectangle_size.x * rectangle_size.y < best_rectangle_size.x * best_rectangle_size.y {
                            best_rectangle_size = rectangle_size;
                            best_rectangle_origin = rectangle_origin;
                            best_cluster_chunks = cluster_chunks;
                            reconsidered += 1;
                        }
                        break;
                    }
                    length += 1;
                }
                let mut length = 1;
                let mut cluster_chunks = Vec::new();
                loop {
                    let rectangle_size = point(length, rectangle_width);
                    let result = check_rectangle(&rectangle_origin, &rectangle_size, &hashmap, &illegal_chunks, glass_hash, hash_size, num_cluster_chunks, &mut cluster_chunks);
                    if result == FAILED {
                        break;
                    } else if result == SUCCESS {
                        searched += 1;
                        if rectangle_size.x * rectangle_size.y < best_rectangle_size.x * best_rectangle_size.y {
                            best_rectangle_size = rectangle_size;
                            best_rectangle_origin = rectangle_origin;
                            best_cluster_chunks = cluster_chunks;
                            reconsidered += 1;
                        }
                        break;
                    }
                    length += 1;
                }
            }
        }
        radius += 1;
    }

    // test for rehash
    let mut test_hashmap = illegal_chunks.clone();
    for chunk in &best_cluster_chunks {
        test_hashmap.insert(chunk.clone())?;
    }

    best_cluster_chunks.sort();

    save_cluster_chunk_regions(&best_cluster_chunks).ok()?;
    println!("Cluster chunks in region from ({}, {}) to ({}, {}) saved to cluster_chunks.csv", best_rectangle_origin.x, best_rectangle_origin.y, best_rectangle_origin.x + best_rectangle_size.x - 1, best_rectangle_origin.y + best_rectangle_size.y - 1);

    save_cluster_setup_mcfunction(&best_cluster_chunks).ok()?;
    println!("Cluster setup mcfunction saved to cluster_setup.mcfunction");

    save_cluster_load_mcfunction(glass_hash, hash_size, best_cluster_chunks.clone()).ok()?;
    println!("Cluster load mcfunction saved to cluster_load.mcfunction");

    return YES;
}

fn save_cluster_chunk_regions(cluster_chunks: &Vec<Pos>) -> std::io::Result<()> {
    let mut file = String::new();
    for chunk in cluster_chunks {
        file += &*chunk.x.to_string();
        file += " ";
        file += &*chunk.y.to_string();
        file += "\n";
    }

    std::fs::write("cluster_chunks.csv", file)?;

    return Ok(());
}

fn save_cluster_setup_mcfunction(cluster_chunks: &Vec<Pos>) -> std::io::Result<()> {
    let mut file = String::new();
    file += "# File generated by falling-cluster-finder\n";
    file += "kill @e[tag=cluster]\n";
    file += "summon armor_stand ~ ~ ~ {Tags:[\"cluster\"]}\n";
    file += "scoreboard objectives add numchunks dummy\n";
    file += "tellraw @a \"Run \\\"scoreboard players set @e[tag=cluster] numchunks <n>\\\" to set the number of chunks you want to load.\"\n";
    for chunk in cluster_chunks {
        file += "loadchunk ";
        file += &*chunk.x.to_string();
        file += " ";
        file += &*chunk.y.to_string();
        file += "\n";
        let names = ["Xcom", "Kerb", "Cheater", "Earthcomputer", "coolmann"];
        let name = names[(chunk.x * 31 + chunk.y) as usize % names.len()];
        file += "setblock ";
        file += &*(chunk.x * 16 + 8).to_string();
        file += " 0 ";
        file += &*(chunk.y * 16 + 8).to_string();
        file += " hopper default replace {CustomName:\"Don't listen to ";
        file += name;
        file += "!\", Items:[{Slot: 2b, id: \"minecraft:bedrock\", tag:{display:{Name: \"Gray Concrete\"}}}]}\n";
        file += "setblock ";
        file += &*(chunk.x * 16 + 8).to_string();
        file += " 1 ";
        file += &*(chunk.y * 16 + 8).to_string();
        file += " dropper\n";
    }

    std::fs::write("cluster_setup.mcfunction", file)?;

    return Ok(());
}

fn save_cluster_load_mcfunction(glass_hash: HashT, hash_size: HashT, mut cluster_chunks: Vec<Pos>) -> std::io::Result<()> {
    cluster_chunks.sort_by_key(|pos| (hash(pos, hash_size) + hash_size - glass_hash) & (glass_hash - 1));

    let mut file = String::new();
    file += "# File generated by falling-cluster-finder\n";
    for (i, chunk) in cluster_chunks.iter().enumerate() {
        file += "execute @e[score_numchunks_min=";
        file += &*(i + 1).to_string();
        file += "] ~ ~ ~ loadchunk ";
        file += &*chunk.x.to_string();
        file += " ";
        file += &*chunk.y.to_string();
        file += "\n";
    }

    std::fs::write("cluster_load.mcfunction", file)?;

    return Ok(());
}

fn find_glass_hash_chunk<F>(glass_hash: HashT, hash_size: usize, origin: &Pos, min_radius: i32, illegal_chunks: &OpenHashMap, predicate: F) -> Pos
    where F: Fn(&Pos) -> bool {

    let mut radius = min_radius;
    loop {
        for dx in -radius..=radius {
            for sign in &[-1, 1] {
                let dz = (radius - dx.abs()) * sign;
                let chunk_pos = *origin + vector(dx, dz);
                if !illegal_chunks.contains(&chunk_pos) && is_permaloader_protected(&chunk_pos) && hash(&chunk_pos, hash_size) == glass_hash && predicate(&chunk_pos) {
                    return chunk_pos;
                }
            }
        }
        radius += 1;
    }
}

fn prefill_hashmap(hash_size: usize, spawn_pos: Point<i32>, glass_chunk: &Point<i32>, permaloaders: &Vec<(Point<i32>, Point<i32>)>, hashmap: &mut OpenHashMap) -> QuestionableBool {
    for spawn_x in get_spawn_chunks_range(spawn_pos.x) {
        for spawn_z in get_spawn_chunks_range(spawn_pos.y) {
            let spawn_chunk = point(spawn_x, spawn_z);
            hashmap.insert(spawn_chunk)?;
        }
    }

    for (permaloader_start, permaloader_end) in permaloaders {
        let permaloader_length = (permaloader_start.x - permaloader_end.x).abs();
        for i in 0..=permaloader_length {
            let delta = vector((permaloader_end.x - permaloader_start.x) * i / permaloader_length, (permaloader_end.y - permaloader_start.y) * i / permaloader_length);
            let permaloader_chunk = *permaloader_start + delta;
            hashmap.insert(permaloader_chunk)?;
        }
    }

    if (glass_chunk.x == 0 && glass_chunk.y == 0) || hashmap.vec[hash(&glass_chunk, hash_size)].is_some() {
        println!("Glass chunk has hash collision with the spawn chunks or permaloader!");
        println!("But valid chunks were found nearby...");
        let mut radius = 1;
        let mut found = false;
        while radius % 5 != 0 || !found {
            for dx in -radius..=radius {
                let mut chunk = *glass_chunk + vector(dx, radius - dx.abs());
                if hashmap.vec[hash(&glass_chunk, hash_size)].is_none() {
                    found = true;
                    println!("({}, {})", chunk.x, chunk.y);
                }
                chunk = *glass_chunk + vector(dx, dx.abs() - radius);
                if hashmap.vec[hash(&glass_chunk, hash_size)].is_none() {
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

fn check_rectangle(rectangle_pos: &Pos, rectangle_size: &Pos, hashmap: &OpenHashMap, illegal_chunks: &OpenHashMap, glass_hash: HashT, hash_size: HashT, mut num_cluster_chunks: usize, cluster_chunks: &mut Vec<Pos>) -> RectangleCheckResult {
    account_for_existing_cluster_chunks(hashmap, glass_hash, &mut num_cluster_chunks);

    let mut hashes = Vec::with_capacity((rectangle_size.x * rectangle_size.y) as usize);
    for dx in 0..rectangle_size.x {
        for dz in 0..rectangle_size.y {
            let chunk_pos = *rectangle_pos + vector(dx, dz);
            if illegal_chunks.contains(&chunk_pos) || !is_permaloader_protected(&chunk_pos) {
                continue;
            }
            let h = hash(&chunk_pos, hash_size);
            if h != glass_hash {
                let transformed_hash = (h + hash_size - glass_hash) & hashmap.mask;
                hashes.push((transformed_hash, chunk_pos));
            }
        }
    }
    if hashes.len() < num_cluster_chunks {
        return RectangleCheckResult::CONTINUE;
    }

    hashes.sort_by_key(|(hash, _)| *hash);

    return if hashes.iter().take(num_cluster_chunks).enumerate().all(|(index, (hash, _))| *hash <= index + 1) {
        for (_, chunk) in &hashes[0..num_cluster_chunks] {
            cluster_chunks.push(*chunk);
        }
        RectangleCheckResult::SUCCESS
    } else {
        RectangleCheckResult::CONTINUE
    }
}

fn account_for_existing_cluster_chunks(hashmap: &OpenHashMap, glass_hash: usize, num_cluster_chunks: &mut usize) {
    let mut index = (glass_hash + *num_cluster_chunks + 1) & hashmap.mask;
    while index != glass_hash {
        if hashmap.vec[index].is_some() {
            *num_cluster_chunks -= 1;
        }
        if index == 0 {
            index = hashmap.mask;
        } else {
            index -= 1;
        }
    }
}

#[derive(Eq, PartialEq)]
enum RectangleCheckResult {
    SUCCESS, CONTINUE, FAILED
}

fn get_spawn_chunks_range(coord: Coord) -> impl Iterator<Item = i32> {
    div_ceil(coord - 128 - 8, 16)..=(coord + 128 - 8).div_euclid(16)
}

fn div_ceil(a: i32, b: i32) -> i32 {
    (a + (b - 1)).div_euclid(b)
}

fn hash(pos: &Pos, hash_size: HashT) -> HashT {
    let long = ((pos.y as u64) << 32) | (pos.x as u64 & 0xffffffff);
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

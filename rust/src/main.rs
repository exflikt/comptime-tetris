use macroquad::color::{colors, Color};
use macroquad::color_u8;
use macroquad::input::{self, KeyCode};
use macroquad::shapes;
use macroquad::text;
use macroquad::window;
use rand::distributions;

const GRID_CELL_SIZE: f32 = 32.;
const MARGIN: f32 = 20.;
const PIECE_PREVIEW_WIDTH: f32 = GRID_CELL_SIZE * 5.0;
const SCREEN_WIDTH: f32 =
    Grid::WIDTH as f32 * GRID_CELL_SIZE + MARGIN * 2.0 + PIECE_PREVIEW_WIDTH + MARGIN;
const SCREEN_HEIGHT: f32 = MARGIN + Grid::HEIGHT as f32 * GRID_CELL_SIZE + MARGIN;

const BORDER_COLOR: Color = color_u8!(3, 2, 1, 255);
const BACKGROUND_COLOR: Color = color_u8!(29, 38, 57, 255);

struct Game {
    state: State,
    grid: Grid,
    pos: (u8, u8),
    tetromino: Tetromino,
    rot: Rotation,
    holding_tetromino: Option<Tetromino>,
    swapped: bool,
    next_tetromino: Tetromino,
    level: Level,
    tick: u32,
    score: u32,
}

#[derive(Default, PartialEq, Eq)]
enum State {
    #[default]
    Start,
    Play,
    Pause,
    Over,
    WindowClose,
}

struct Grid {
    cells: [Option<Tetromino>; Grid::WIDTH as usize * Grid::HEIGHT as usize],
}

impl Grid {
    const WIDTH: u8 = 10;
    const HEIGHT: u8 = 20;

    const fn new() -> Grid {
        Grid {
            cells: [None; Grid::WIDTH as usize * Grid::HEIGHT as usize],
        }
    }

    fn enumerate_2d(&self) -> impl Iterator<Item = ((u8, u8), &Option<Tetromino>)> {
        (0..Grid::HEIGHT)
            .flat_map(|y| (0..Grid::WIDTH).map(move |x| (x, y)))
            .zip(self.cells.iter())
    }

    fn enumerate_2d_mut(&mut self) -> impl Iterator<Item = ((u8, u8), &mut Option<Tetromino>)> {
        (0..Grid::HEIGHT)
            .flat_map(|y| (0..Grid::WIDTH).map(move |x| (x, y)))
            .zip(self.cells.iter_mut())
    }

    /// Remove filled rows and move other rows downward.
    /// Returns the score according to the number of rows deleted.
    fn squash_filled_rows(&mut self) -> u32 {
        let mut src_range_indices: Vec<u8> = Vec::new();
        let mut min_y = Grid::HEIGHT;
        for y in (0..Grid::HEIGHT).rev() {
            let mut min_y_updated = false;
            let mut filled = true;
            for x in 0..Grid::WIDTH {
                if self[(x, y)].is_some() {
                    if !min_y_updated {
                        min_y = min_y.min(y);
                        min_y_updated = true;
                    } else if !filled {
                        break;
                    }
                } else if filled {
                    filled = false;
                }
            }
            if filled {
                src_range_indices.push(y);
            }
        }
        let no_filled_rows = src_range_indices.len();
        if min_y != Grid::HEIGHT {
            src_range_indices.push(min_y.saturating_sub(1));
        }

        for (nth_del, rows) in src_range_indices.windows(2).enumerate() {
            let y_dst_base = rows[0] + nth_del as u8;
            let y_src_range = (rows[1] + 1)..rows[0];
            for (y_src_i, y_src) in y_src_range.rev().enumerate() {
                let y_dst = y_dst_base - y_src_i as u8;
                for x in 0..Grid::WIDTH {
                    self[(x, y_dst)] = self[(x, y_src)];
                }
            }
        }
        if min_y != Grid::HEIGHT {
            for y_dst in min_y..min_y + no_filled_rows as u8 {
                for x in 0..Grid::WIDTH {
                    self[(x, y_dst)] = None;
                }
            }
        }

        Grid::_to_score(no_filled_rows)
    }

    const fn _to_score(no_deleted_rows: usize) -> u32 {
        assert!(no_deleted_rows <= 4);
        match no_deleted_rows {
            0 => 0,
            1 => 1,
            2 => 3,
            3 => 5,
            4 => 8,
            _ => unreachable!(),
        }
    }
}

impl std::ops::Index<(u8, u8)> for Grid {
    type Output = Option<Tetromino>;

    fn index(&self, (x, y): (u8, u8)) -> &Self::Output {
        let idx = (y * Grid::WIDTH + x) as usize;
        self.cells.index(idx)
    }
}

impl std::ops::IndexMut<(u8, u8)> for Grid {
    fn index_mut(&mut self, (x, y): (u8, u8)) -> &mut Self::Output {
        let idx = (y * Grid::WIDTH + x) as usize;
        self.cells.index_mut(idx)
    }
}

#[allow(dead_code)] // TODO: this lint can be omitted (probably)
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Tetromino {
    I,
    O,
    T,
    J,
    L,
    S,
    Z,
}

impl Tetromino {
    const fn fill_color(self) -> Color {
        match self {
            // TODO: replace Color::new calls with a color_u8! macro when Rust 1.82 arrives
            Tetromino::I => Color::new(0.0, 1.0, 1.0, 1.),
            Tetromino::O => Color::new(1.0, 1.0, 0.0, 1.),
            Tetromino::T => Color::new(1.0, 0.0, 1.0, 1.),
            Tetromino::J => Color::new(0.0, 0.0, 1.0, 1.),
            Tetromino::L => Color::new(1.0, 0.5, 0.0, 1.),
            Tetromino::S => Color::new(0.0, 1.0, 0.0, 1.),
            Tetromino::Z => Color::new(1.0, 0.0, 0.0, 1.),
        }
    }

    const fn ghost_color(self) -> Color {
        let mut color = self.fill_color();
        color.a = 0.3;
        color
    }

    const fn neighbors(self, rot: Rotation) -> [(i8, i8); 4] {
        use Rotation::{DEG0, DEG180, DEG270, DEG90};
        match (self, rot) {
            (Tetromino::I, DEG0 | DEG180) => [(-1, 0), (0, 0), (1, 0), (2, 0)],
            (Tetromino::I, DEG90 | DEG270) => [(0, -1), (0, 0), (0, 1), (0, 2)],
            (Tetromino::O, _) => [(0, 0), (1, 0), (0, 1), (1, 1)],
            (Tetromino::T, DEG0) => [(0, -1), (-1, 0), (0, 0), (1, 0)],
            (Tetromino::T, DEG90) => [(0, -1), (0, 0), (1, 0), (0, 1)],
            (Tetromino::T, DEG180) => [(-1, 0), (0, 0), (1, 0), (0, 1)],
            (Tetromino::T, DEG270) => [(0, -1), (-1, 0), (0, 0), (0, 1)],
            (Tetromino::J, DEG0) => [(0, -1), (0, 0), (-1, 1), (0, 1)],
            (Tetromino::J, DEG90) => [(-1, -1), (-1, 0), (0, 0), (1, 0)],
            (Tetromino::J, DEG180) => [(0, -1), (1, -1), (0, 0), (0, 1)],
            (Tetromino::J, DEG270) => [(-1, 0), (0, 0), (1, 0), (1, 1)],
            (Tetromino::L, DEG0) => [(0, -1), (0, 0), (0, 1), (1, 1)],
            (Tetromino::L, DEG90) => [(-1, 0), (0, 0), (1, 0), (-1, 1)],
            (Tetromino::L, DEG180) => [(-1, -1), (0, -1), (0, 0), (0, 1)],
            (Tetromino::L, DEG270) => [(1, -1), (-1, 0), (0, 0), (1, 0)],
            (Tetromino::S, DEG0 | DEG180) => [(0, 0), (1, 0), (-1, 1), (0, 1)],
            (Tetromino::S, DEG90 | DEG270) => [(0, -1), (0, 0), (1, 0), (1, 1)],
            (Tetromino::Z, DEG0 | DEG180) => [(-1, 0), (0, 0), (0, 1), (1, 1)],
            (Tetromino::Z, DEG90 | DEG270) => [(0, -1), (-1, 0), (0, 0), (-1, 1)],
        }
    }
}

impl distributions::Distribution<Tetromino> for distributions::Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Tetromino {
        let variant: u8 = rng.gen_range(0..=Tetromino::Z as u8);
        // SAFETY: we've restricted the range of the random number generator to the number of
        // variants in `Tetromino` enum.
        unsafe { std::mem::transmute(variant) }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
enum Rotation {
    #[default]
    DEG0,
    DEG90,
    DEG180,
    DEG270,
}

impl Rotation {
    const fn spin_cw(self) -> Rotation {
        match self {
            Rotation::DEG0 => Rotation::DEG90,
            Rotation::DEG90 => Rotation::DEG180,
            Rotation::DEG180 => Rotation::DEG270,
            Rotation::DEG270 => Rotation::DEG0,
        }
    }

    const fn spin_acw(self) -> Rotation {
        match self {
            Rotation::DEG0 => Rotation::DEG270,
            Rotation::DEG90 => Rotation::DEG0,
            Rotation::DEG180 => Rotation::DEG90,
            Rotation::DEG270 => Rotation::DEG180,
        }
    }
}

struct Level {
    tick_rate: u32,
    piece_count: u32,
}

impl Level {
    const fn new() -> Level {
        Level {
            tick_rate: 30,
            piece_count: 1,
        }
    }

    fn update(&mut self) {
        self.piece_count += 1;

        let prev_rate = self.tick_rate;

        self.tick_rate = match self.piece_count {
            0..=25 => 30,
            26..=100 => 25,
            101..=200 => 20,
            201..=300 => 15,
            301..=400 => 12,
            401..=500 => 10,
            501..=700 => 8,
            701..=800 => 6,
            _ => 5,
        };

        if self.tick_rate != prev_rate {
            eprintln!("tick_rate: {}", self.tick_rate)
        }
    }
}

impl Game {
    fn new() -> Self {
        let tetromino = rand::random();
        Game {
            state: State::Start,
            grid: Grid::new(),
            pos: (Grid::WIDTH / 2, 1),
            tetromino,
            rot: Default::default(),
            holding_tetromino: None,
            swapped: false,
            next_tetromino: rand::random(),
            level: Level::new(),
            tick: 0,
            score: 0,
        }
    }

    fn _reset_grid_and_level(&mut self) {
        self.grid = Grid::new();
        self.level = Level::new();
        self.holding_tetromino = None;
    }

    fn _reset_piece(&mut self) {
        self.pos = (Grid::WIDTH / 2, 1);
        self.tetromino = self.next_tetromino;
        self.next_tetromino = rand::random();
        self.rot = Default::default();
        self.swapped = false;
        if !self._movable_with(self.rot)(0, 0) {
            self.state = State::Over;
        }
    }

    fn _movable_with(&self, rot: Rotation) -> impl Fn(i8, i8) -> bool + '_ {
        let (x_from, y_from) = self.pos;
        let neighbors = self.tetromino.neighbors(rot);
        move |x_dir: i8, y_dir: i8| {
            for (dx, dy) in neighbors {
                let x = x_from.checked_add_signed(dx + x_dir);
                let y = y_from.checked_add_signed(dy + y_dir);
                if let [Some(x), Some(y)] = [x, y] {
                    if x >= Grid::WIDTH || y >= Grid::HEIGHT || self.grid[(x, y)].is_some() {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            true
        }
    }

    fn update(&mut self) {
        match self.state {
            State::Start => {
                if input::is_key_pressed(KeyCode::Enter) {
                    self.state = State::Play;
                }
            }
            State::Play => {
                if input::is_key_pressed(KeyCode::Escape) {
                    self.state = State::Pause;
                    return;
                }

                fn keys_registered(key_codes: &[KeyCode]) -> bool {
                    use std::sync::RwLock;
                    // TODO: create multiple duration locks for each key pair
                    static FREEZE_DURATION: RwLock<u8> = RwLock::new(0);

                    let duration = *FREEZE_DURATION.read().unwrap();
                    if duration == 0 && key_codes.iter().any(|&k| input::is_key_down(k)) {
                        *FREEZE_DURATION.write().unwrap() = 60;
                        return true;
                    } else if duration > 0 {
                        *FREEZE_DURATION.write().unwrap() -= 1;
                    }
                    false
                }
                if keys_registered(&[KeyCode::Left, KeyCode::A])
                    && self._movable_with(self.rot)(-1, 0)
                {
                    self.pos.0 -= 1;
                } else if keys_registered(&[KeyCode::Right, KeyCode::D])
                    && self._movable_with(self.rot)(1, 0)
                {
                    self.pos.0 += 1;
                } else if keys_registered(&[KeyCode::Down, KeyCode::S]) {
                    if self._movable_with(self.rot)(0, 1) {
                        self.pos.1 += 1;
                    } else {
                        let neighbors = self.tetromino.neighbors(self.rot);
                        place_tetromino(&mut self.grid, self.tetromino, self.pos, neighbors);
                        self.score += self.grid.squash_filled_rows();
                        self._reset_piece();
                        self.level.update();
                        return;
                    }
                } else if keys_registered(&[KeyCode::Space, KeyCode::RightControl]) {
                    // drop the tetromino
                    while self._movable_with(self.rot)(0, 1) {
                        self.pos.1 += 1;
                    }
                    let neighbors = self.tetromino.neighbors(self.rot);
                    place_tetromino(&mut self.grid, self.tetromino, self.pos, neighbors);
                    self.score += self.grid.squash_filled_rows();
                    self._reset_piece();
                    self.level.update();
                    return;
                } else if keys_registered(&[KeyCode::Up, KeyCode::W]) {
                    let new_rot = self.rot.spin_cw();
                    for x_offset in [0, -1, 1, -2i8, 2] {
                        if self._movable_with(new_rot)(x_offset, 0) {
                            self.pos.0 = self.pos.0.saturating_add_signed(x_offset);
                            self.rot = new_rot;
                            return;
                        }
                    }
                } else if keys_registered(&[KeyCode::LeftShift, KeyCode::RightShift]) {
                    let new_rot = self.rot.spin_acw();
                    for x_offset in [0, -1, 1, -2i8, 2] {
                        if self._movable_with(new_rot)(x_offset, 0) {
                            self.pos.0 = self.pos.0.saturating_add_signed(x_offset);
                            self.rot = new_rot;
                            return;
                        }
                    }
                } else if keys_registered(&[KeyCode::C]) {
                    if !self.swapped {
                        if let Some(hold) = self.holding_tetromino {
                            self.holding_tetromino = Some(self.tetromino);
                            self.tetromino = hold;
                            self.pos = (Grid::WIDTH / 2, 1);
                            self.rot = Default::default();
                        } else {
                            self.holding_tetromino = Some(self.tetromino);
                            self._reset_piece();
                        }
                        self.swapped = true;
                        return;
                    }
                }

                // TODO:remove this if-block later
                if input::is_key_pressed(KeyCode::O) {
                    self.state = State::Over;
                    return;
                }

                if self.tick >= self.level.tick_rate {
                    if self._movable_with(self.rot)(0, 1) {
                        self.pos.1 += 1;
                    } else {
                        let neighbors = self.tetromino.neighbors(self.rot);
                        place_tetromino(&mut self.grid, self.tetromino, self.pos, neighbors);
                        self.score += self.grid.squash_filled_rows();
                        self._reset_piece();
                        self.level.update();
                    }
                    self.tick = 0;
                } else {
                    self.tick += 1;
                }

                fn place_tetromino(
                    grid: &mut Grid,
                    tetromino: Tetromino,
                    (x, y): (u8, u8),
                    neighbors: [(i8, i8); 4],
                ) {
                    for (dx, dy) in neighbors {
                        let (x, x_of) = x.overflowing_add_signed(dx);
                        let (y, y_of) = y.overflowing_add_signed(dy);
                        assert!(!x_of && !y_of);
                        grid[(x, y)] = Some(tetromino);
                    }
                }
            }
            State::Pause => {
                if input::is_key_released(KeyCode::Enter) {
                    self.state = State::Play;
                } else if input::is_key_pressed(KeyCode::Q) {
                    self.state = State::WindowClose;
                }
            }
            State::Over => {
                if input::is_key_pressed(KeyCode::Enter) {
                    self._reset_grid_and_level();
                    self._reset_piece();
                    self.score = 0;
                    self.state = State::Play;
                } else if input::is_key_pressed(KeyCode::Q) {
                    self.state = State::WindowClose;
                }
            }
            State::WindowClose => {
                panic!("`update` method should not be called when the state is in WindowClose");
            }
        }
    }

    fn draw(&mut self) {
        window::clear_background(BORDER_COLOR);

        draw_grid(&self.grid);

        let neighbors = self.tetromino.neighbors(self.rot);
        let mut ghost_offset = 0;
        while self._movable_with(self.rot)(0, ghost_offset + 1) {
            ghost_offset += 1;
        }
        draw_tetromino(self.pos, self.tetromino, neighbors, ghost_offset);

        let right_bar: f32 = MARGIN + (10. * GRID_CELL_SIZE) + MARGIN;
        let y_score = MARGIN + GRID_CELL_SIZE;
        let y_hold = draw_score(self.score, (right_bar, y_score));
        let y_next = draw_tetromino_box(self.holding_tetromino, (right_bar, y_hold));
        let y_next = y_next + GRID_CELL_SIZE * 5.;
        draw_tetromino_box(Some(self.next_tetromino), (right_bar, y_next));

        if self.state != State::Play {
            let [x, y] = [MARGIN; 2];
            let (w, h) = (
                GRID_CELL_SIZE * f32::from(Grid::WIDTH),
                GRID_CELL_SIZE * f32::from(Grid::HEIGHT),
            );
            shapes::draw_rectangle(x, y, w, h, color_u8!(3, 2, 1, 192));
        }
        if self.state == State::Pause {
            let [base_x, base_y] = [GRID_CELL_SIZE * 2.5, SCREEN_HEIGHT / 2.];
            let [x, y] = [base_x + GRID_CELL_SIZE, base_y - 50.];
            text::draw_text("PAUSED", x, y, 50., colors::WHITE);
            let msg = "Press Enter to unpause";
            text::draw_text(msg, base_x, base_y, 20., colors::LIGHTGRAY);
        }
        if self.state == State::Over {
            let [base_x, base_y] = [GRID_CELL_SIZE * 2.5, SCREEN_HEIGHT / 2.];
            let [x, y] = [base_x + GRID_CELL_SIZE / 2., base_y - 50.];
            text::draw_text("GAME OVER", x, y, 42., colors::WHITE);
            let msg = "Press ENTER to restart";
            text::draw_text(msg, base_x, base_y, 20., colors::LIGHTGRAY);
        }
        if self.state == State::Start {
            let [base_x, base_y] = [GRID_CELL_SIZE * 2.5, SCREEN_HEIGHT / 2.];
            let [x, y] = [base_x + GRID_CELL_SIZE, base_y - 50.];
            text::draw_text("TETRIS", x, y, 50., colors::WHITE);
            let msg = "Press ENTER to start";
            text::draw_text(msg, base_x, base_y, 20., colors::LIGHTGRAY);
            let [x, y] = [base_x + GRID_CELL_SIZE, base_y + 50.];
            text::draw_text("Press Q to quit", x, y, 20., colors::LIGHTGRAY);
        }

        fn draw_grid(grid: &Grid) {
            let [x_base, y_base] = [MARGIN; 2];
            let [w, h] = [
                Grid::WIDTH as f32 * GRID_CELL_SIZE + MARGIN,
                Grid::HEIGHT as f32 * GRID_CELL_SIZE + MARGIN,
            ];
            let [x, y] = [x_base - MARGIN / 2., y_base - MARGIN / 2.];
            shapes::draw_rectangle_lines(x, y, w, h, MARGIN, BACKGROUND_COLOR);
            for ((x, y), cell) in grid.enumerate_2d() {
                let [w, h] = [GRID_CELL_SIZE; 2];
                let [x, y] = [x_base + w * x as f32, y_base + h * y as f32];
                let color = cell.map(Tetromino::fill_color).unwrap_or(BORDER_COLOR);
                shapes::draw_rectangle(x, y, w, h, color);
            }
        }

        fn draw_tetromino(
            (x, y): (u8, u8),
            tetromino: Tetromino,
            neighbors: [(i8, i8); 4],
            ghost_offset: i8,
        ) {
            let [x_base, y_base] = [MARGIN; 2];
            for (dx, dy) in neighbors {
                let [w, h] = [GRID_CELL_SIZE; 2];
                let x = x_base + w * x.saturating_add_signed(dx) as f32;
                let y_orig = y_base + h * y.saturating_add_signed(dy) as f32;
                shapes::draw_rectangle(x, y_orig, w, h, tetromino.fill_color());
                let y_ghost = y_base + h * y.saturating_add_signed(dy + ghost_offset) as f32;
                shapes::draw_rectangle(x, y_ghost, w, h, tetromino.ghost_color());
            }
        }

        fn draw_score(score: u32, (x_base, y_base): (f32, f32)) -> f32 {
            let draw_text_at_y = |text: &str, y: f32| {
                text::draw_text(text, x_base, y, 20., colors::LIGHTGRAY);
            };
            draw_text_at_y("Score:", y_base);
            draw_text_at_y(&score.to_string(), y_base + MARGIN);
            y_base + MARGIN * 2.
        }

        fn draw_tetromino_box(tetromino: Option<Tetromino>, (x_base, y_base): (f32, f32)) -> f32 {
            let box_margin = GRID_CELL_SIZE + MARGIN;
            let [w, h] = [
                GRID_CELL_SIZE * 2. + box_margin * 2.,
                GRID_CELL_SIZE * 1. + box_margin * 2.,
            ];
            let [x, y] = [x_base, y_base + MARGIN / 2.];
            shapes::draw_rectangle(x, y, w, h, BACKGROUND_COLOR);

            if let Some(tetromino) = tetromino {
                let [x_base, y_base] = [x + box_margin, y + box_margin];
                for (dx, dy) in tetromino.neighbors(Default::default()) {
                    let [w, h] = [GRID_CELL_SIZE; 2];
                    let [x, y] = [x_base + w * dx as f32, y_base + h * dy as f32];
                    let (x, y) = match tetromino {
                        Tetromino::O => (x, y - h / 2.),
                        Tetromino::T => (x + w / 2., y + h / 2.),
                        Tetromino::J => (x + w, y),
                        Tetromino::S => (x + w / 2., y - h / 2.),
                        Tetromino::Z => (x + w / 2., y - h / 2.),
                        _ => (x, y),
                    };
                    shapes::draw_rectangle(x, y, w, h, tetromino.fill_color());
                }
            }
            y_base + h + MARGIN
        }
    }
}

fn run_tetris() -> Option<u32> {
    use std::sync::OnceLock;
    static SCORE_CELL: OnceLock<u32> = OnceLock::new();

    macroquad::Window::new("comptime_tetris", async {
        let mut game = Game::new();
        use macroquad::audio;
        //let bg_music = audio::load_sound("9-samurai_rmx.ogg").await.unwrap();
        //println!("aaa");
        //audio::play_sound(
        //    &bg_music,
        //    audio::PlaySoundParams {
        //        looped: true,
        //        volume: 0.0,
        //    },
        //);
        window::request_new_screen_size(SCREEN_WIDTH, SCREEN_HEIGHT);
        while game.state != State::WindowClose {
            game.update();
            game.draw();
            window::next_frame().await
        }

        SCORE_CELL.set(game.score).unwrap();
    });

    SCORE_CELL.get().copied()
}

fn main() {
    let score = run_tetris();
    println!("{:?}", score);

    //let score = tetris_macro::run!();
    //println!("{score}");
}

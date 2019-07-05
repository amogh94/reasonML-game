open Reprocessing;

type direction =
  | Up
  | Down
  | Left
  | Right
  | Space;
type state = {
  direction,
  position: (int, int),
  level: int,
  score: int,
  high_score: int,
  squares:list((int,int))
};

let setup = env => {
  Env.size(~width=600, ~height=800, env);
  {direction: Space, position: (0, 200), level: 5, score: 0,squares:[
    (50, 250),
    (150, 350),
    (250, 450),
    (400,250),
    (200,500),
    (450,600),
    (400,400),
    (300,700)
  ], high_score:0};
};

let rec has = (list,func) => {
  switch(list){
    |[] => false
    |[elem,...rest] => func(elem) || has(rest,func)
  }
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=255, ~g=255, ~b=0, ~a=255), env);
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=255, ~a=255), env);
  Draw.rect(~pos=(0,200), ~width=600, ~height=600, env);
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=100), env);
  Draw.text(~body=string_of_int(state.score / 10), ~pos=(20, 20), env);
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=100), env);
  Draw.text(~body=string_of_int(state.high_score / 10), ~pos=(500, 20), env);
  Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=255), env);
  Draw.rect(~pos=state.position, ~width=7, ~height=7, env);
  Draw.fill(Utils.color(~r=127, ~g=127, ~b=0, ~a=255), env);

  let squares = state.squares;
  
  List.iter(
    pos => {
      Draw.rect(~pos,~width=50,~height=50,env);
    },
    squares,
  );

  let currentState = state;
  // returns bool: is key pressed?
  let direction =
    if (Env.keyPressed(Down, env)) {
      Down;
    } else if (Env.keyPressed(Up, env)) {
      Up;
    } else if (Env.keyPressed(Left, env)) {
      Left;
    } else if (Env.keyPressed(Right, env)) {
      Right;
    } else if (Env.keyPressed(Events.Space,env)) {
      Space;
    } else {
      currentState.direction;
    };
  let (x, y) = currentState.position;

  let position =
    switch (direction) {
    | Up => y == 200 ? (x, 800) : (x, y - state.level)
    | Down => y == 800 ? (x, 200) : (x, y + state.level)
    | Left => x == 0 ? (600, y) : (x - state.level, y)
    | Right => x == 600 ? (0, y) : (x + state.level, y)
    | Space => (x,y)
    };

  let squares = List.map((square)=>{
    let random = Utils.random(~min=0,~max=4);
    let (x, y) = square;
    switch(random){
      |1 => (x<547)?(x+3,y):(x,y)
      |2 => (x>3)?(x-3,y):(x,y)
      |3 => (y<747)?(x,y+3):(x,y)
      |_ => (y>200)?(x,y-3):(x,y)
    }
  },squares);

  has(squares, square => {
    let (sq_x, sq_y) = square;
    (x<=sq_x+50 && x>=sq_x && y<=sq_y+50 && y>=sq_y)
  })?
    (direction == Space)?{...state, direction: Right, position: (0, 200), score: 0, high_score:( state.high_score > state.score ) ? state.high_score : state.score}
    : {...state,direction, high_score:( state.high_score > state.score ) ? state.high_score : state.score}
  : (direction != Space)?{...state, score: state.score + 1,position,direction, squares}:{...state,position,squares}
};

run(~setup, ~draw, ());
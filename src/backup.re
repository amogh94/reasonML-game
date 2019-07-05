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
};

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  {direction: Right, position: (0, 0), level: 8, score: 0};
};

let rec has = (list,func) => {
  switch(list){
    |[] => false
    |[elem,...rest] => func(elem) || has(rest,func)
  }
};

let draw = (state, env) => {
  // state is initially ret val of setup which is (0,0)

  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=255, ~a=255), env);
  Draw.text(~body=string_of_int(state.score / 10), ~pos=(20, 20), env);
  Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=255), env);
  Draw.rect(~pos=state.position, ~width=5, ~height=5, env);

  let squares = [
    (50, 50),
    (150, 150),
    (250, 250),
    (400,50),
    (200,300),
    (450,300),
    (550,500)
  ];
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
    }else {
      currentState.direction;
    };
  let (x, y) = currentState.position;

  let position =
    switch (direction) {
    | Up => y == 0 ? (x, 600) : (x, y - state.level)
    | Down => y == 600 ? (x, 0) : (x, y + state.level)
    | Left => x == 0 ? (600, y) : (x - state.level, y)
    | Right => x == 600 ? (0, y) : (x + state.level, y)
    | Space => (0,0)
    };


  // state
  has(squares, square => {
    // find if point is on edge of square
    let (sq_x, sq_y) = square;
    (x<=sq_x+50 && x>=sq_x && y<=sq_y+50 && y>=sq_y)
  })
    ? {direction: Right, position: (0, 0), level: 8, score: 0} : {...state, direction, position, score: state.score + 1};
};

run(~setup, ~draw, ());
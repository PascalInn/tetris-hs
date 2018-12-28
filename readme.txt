Tetrs Game implementation with haskell-platform , coding by TC Gu (backend) and CY Ding (UI)

reference:
http://github.com/SamTay/tetris
https://github.com/hauxir/haskell-tetris
http://github.com/jtdaugherty/brick
http://tetris.wikia.com/wiki/
http://github.com/haskell/network
Real World Haskell Chapter 27


How to build:

  Cabal configure
  
  Cabal install --only-dependencies
  
  Cabal build
  

Run game after building:

  Cabal run
  

Single playing:

  Click on 'Single' bottum and game will start after 3 seconds.
  
  Control the tetrimino with keyboard.
  

Instruction

  up:      rotate counterclockwise
  
  left:    left move
  
  right:   right move
  
  down:    down move
  
  space:   fast drop
  
  Esc:     quit
  
  r:       restart
  

Dual playing (demo):

  Play with a friend. Ping friend's IP ensuring connection.
  
  Click Player1 and play as a server.
  
  Click Player2 and input the IP of server to join playing with server
  

Further construction:

  More interface in dual playing mode.
  

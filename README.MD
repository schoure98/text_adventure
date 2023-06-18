## Build the text-adventure game

spago build


## Play the Game

spago run -a "-p playerName" 


## Game Controls

The game is controlled through a command line interface. 
Use the following commands to interact with the game:

- `look` : Get the location where you are currently.
- `north`: Explore room on north.
- `south`: Explore room on south.
- `east`: Explore room on east.
- `west`: Explore room on west.
- `inventory`: View the items in your inventory.
- `take [item]`: Add a specific item to your inventory.
- `use [item]`: Use a specific item from your inventory.
- `cheat` : get all the items in the inventory.

References: 
PureScript Book: https://book.purescript.org/chapter11.html 
PureScript Github: https://github.com/purescript-contrib/purescript-book
Node.ReadLine: https://pursuit.purescript.org/packages/purescript-node-readline/4.0.1/docs/Node.ReadLine 


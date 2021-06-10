# Scritch

## Welcome to Scritch!
See the section below for information pertaining to our [Milestone 2](#milestone-2) submission. For instructions running Scritch see: [Running The Project](#running-the-project). If you need help installing stack (required the build the project), see: [Install Stack](#install-stack). If you are having difficulty building Scritch, you likely need to acquire some external libraries (as required by Gloss, our package for drawing OpenGL render windows). These libraries are listed in the [Troubleshooting](#troubleshooting) section.

If you have any issues with the project, please reach out directly and we will be more than glad to help get it running for you!

Thanks!

## Milestone 2
### Project goals and progress:
The main goal of the project, as originally stated, is to provide an educational functional programming experience. However, the focus of our work thus far has been towards creating an interactive programming (perhaps design?) experience. Our goal for the work after milestone 2 is to tie our final submission closer to the original goal. Some specific features we would like to add to the object language are variables (setting and getting) and ```While``` loops. Variables will require some model of state.

### What does the project do?
Right now, when running the project (see: [Running the Project](#running-the-project)), a browser window will be launched to the Scritch IDE. Objects and their animations can be defined within the text area. New objects can be added and removed with the buttons. Pressing run will launch a render window with your defined animations. If a syntax error is detected, there will be some text in the render window telling you where the error occured. Because of how our parser works, many type errors are considered syntax errors.

The basic, high level syntax of our language is ```Object definition -> {semicolon separated list of timed transformations}```, where the syntax of a timed transformation is ```time -> transformation```. The [AnimationLib.hs](src/AnimationLib.hs) and [Parser.hs](src/Parser.hs) files contain all of the information you'll need to write your own fun animations! Note the ```Get``` and ```Compose``` are not yet implemented.

Here are some sample definitions that should help you derive some interesting animations:

`Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 1 -> Step -10; 1 -> Step 10}`

`Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 10 -> Combine [Step 10, Pivot 360]}`

`Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 5 -> Grow 25}`

`Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 10 -> Combine [Grow 25, Step 10, Pivot 360]}`


You could do something interesting like:
```
Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 5 -> Combine [Grow 20, Step 10, Pivot 360]}
Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 4 -> Combine [Grow 20, Step 10, Pivot 360]}
Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 3 -> Combine [Grow 20, Step 10, Pivot 360]}
Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 2 -> Combine [Grow 20, Step 10, Pivot 360]}
Obj (x, Circle, 0, 0,0 ,20 ,0) -> {0 -> Move 250 250; 1 -> Combine [Grow 20, Step 10, Pivot 360]}
```

Here's a different example:
![Demo Output](https://user-images.githubusercontent.com/43552143/119758893-27dd1100-be5c-11eb-81cc-fccee4e891e2.gif)

And Another:
![Demo Output2](https://user-images.githubusercontent.com/43552143/119771419-87deb200-be72-11eb-9201-baf6c23fb8fe.gif)

### Design Decisions
1. One significant design choice was the use of GADTs in our representation of the abstract syntax. The highly type-restricted abstract syntax makes writing parsers for the language safer, and fairly mechanical. Most notable is the ```Function``` datatype, which encodes the type of all functions in our language. In addition to making parsing safer, this type greatly reduces the amount of code needed to evaluate operators - they are all handled by the ```op``` function, regardless of the number and type of arguments.   

   The cost of the type-directed parsing enforced by our GADTs is that certain things which could be done with one less safe parser, by ignoring the types of internal expressions, now require multiple parsers. The best example of this is the parsers ```iexpr``` and ```bexpr```. The primary benefit of our parsers is that they allow us to catch a large class of type errors while parsing, without having to separately type-check the program.

2. Another related design choice was writing our own parsers, instead of using an external library. Our approach was heavily based on Hutton and Meijer's "Monadic parsing in Haskell" (http://www.cs.nott.ac.uk/~pszgmh//pearl.pdf) and a similar chapter in Hutton's "Programming in Haskell". We kept our ```Parser``` type simple, only implementing ```Monad``` (and ```Functor``` and ```Applicative```) and ```Alternative```. We believe this approach makes our code clean and easy to read, as well as reducing a small amount of overhead from another external library. It also makes fine-tuning our parsers easy, which is important when making (and, of course, re-making) decisions about the concrete syntax of our object language.

3. The final design choice we'll discuss involves the implementation of the UI, which we call the IDE. As you may know, implementing a GUI can be quite a heavy-weight task, which is why we opted to use [threepenny-gui](http://hackage.haskell.org/package/threepenny-gui). The implementation with threepenny is quite light, defined with a UI monad, and provides an interactive web interface at (http://localhost:8023). From within the GUI, we even have the ability to spawn a child process for Gloss's render window (local to the server). We could go into some of the implementation details and why they're quite interesting, but instead I suggest you explore yourself in [WebIDE.hs](/app/WebIDE.hs).

## Milestone 1
Project goals and progress:

With this project we set out to create an educational functional programming language. While it is still in its very earlier stages, we believe that we have made some progress in a couple of key areas:

1. We have made the design decision to focus our program's output on generating simple animations. We believe that this will allow the user to more easily see with their own eyes what their program is *doing*, a very helpful thing for novice programmers.

2. We have some experience with the Gloss graphics package.

3. We have been able to translate a basic version of our abstract syntax into a Gloss animation. This is easier said than done.

Design questions:

What other features would you like to see in our user language (based on the current trnasformation commands we have implemented)?

How can we extend the language so that objects can moved based on things other than time (current position, user input, proximity to other objects, etc.)?

What should our "concrete syntax" look like, whether this is text, visual, or a combination (like Scratch's blocks), keeping in mind our emphasis on the functional paradigm?

What should we call it? Scritch is a placeholder!

## Install Stack

Install Stack:

https://docs.haskellstack.org/en/stable/install_and_upgrade/

If you already have Stack, check the version:

`stack --version`

If you version is older (project was created with version 2.5.1) you *may* need to upgrade it:

`stack upgrade`

## Running the Project

Clone the repo and run the following:

`stack build`

Problems? Go to the [Troubleshooting](#troubleshooting) section.

All good? Good! Right now there are 2 different modes (!!).

Now you can run the project with:

`stack exec scritch-play`

This differs from the previous version in that there is a drop down box that gives 2 different options for input programs. The first is Animate, which matches the previous version of the program (except there is a single input box, new lines denote new objects). The second is Play, which is a new stateful execution that is entirely event driven. Give it a whirl!

## Troubleshooting

Missing C library GL?

`sudo apt install libgl1-mesa-dev`

Now missing GLU?

`sudo apt install freeglut3{,-dev}`

You're gonna need these:

`sudo apt install libglfw3-dev`

`sudo apt install xorg-dev`

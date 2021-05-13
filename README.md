# scritch

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

All good? Now run this:

`stack exec scritch-exe`

## Troubleshooting

Missing C library GL?

`sudo apt install libgl1-mesa-dev`


Now missing GLU?

`sudo apt install freeglut3{,-dev}`

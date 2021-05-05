# scritch

## Running the Project

Clone the repo and run the following:

`stack build`

Problems? Go to the troubleshooting section.

All good? Now run this:

`stack exec scritch-exe`

## Troubleshooting

Missing C library GL?
`sudo apt install libgl1-mesa-dev`

Now missing GLU?
`sudo apt install freeglut3{,-dev}`
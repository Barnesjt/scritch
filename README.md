# scritch

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
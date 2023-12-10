# Installation Instructions

1. Install Bogue and required dependencies using `opam install bogue`

  - Follow the instructions on https://github.com/sanette/bogue#installation 

  - Make sure you have all the prerequisites and follow the instructions for pinning, upgrading opam, and unpinning if required.

  - If missing system dependencies, install them. From a fresh Ubuntu VM, we had to run <br />
  `sudo apt-get update` <br />
  `sudo apt-get install libffi-dev libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev`<br />

2. Install bimage with `opam install bimage`

3. Install bimage-unix with `opam install bimage-unix`

4. To run the program, run `make build`, then `make gui`
  - If running `make build` raises an error about bogue, make sure to check if the bogue repo is pinned as per the instructions in step 1, and update/upgrade opam again.
  - If running make gui raises Fatal error: exception Failure("end of file"), install imagemagick with your local package manager.
    - `sudo apt-get install imagemagick`


# Usage Instructions

1. After running `make gui`, you will be prompted in the terminal to enter directories. The instructions are listed above where your cursor is. If you enter nothing (just press enter twice), our default directories `./strain/` and `./input/` will be inputted automatically in the backend.

2. After this, expect to wait around 10 seconds for the images to load in, and then you can use the GUI.
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
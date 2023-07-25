# Lintia

### Fast static linter for Julia (Work In Progress)

## Prerequisites
* [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)

## Installation
1. Clone the repository: `git clone https://github.com/vdayanand/lintia`
2. Navigate to the Lintia directory: `cd lintia`
3. Run Lintia with the following command:
   ```
   cargo run <julia_main_file> --project=<JULIA_PROJECT_PATH>
   ```
   For example: 
   ```
   cargo run ~/code/Example/src/Example.jl --project=~/code/Example
   ```

# Chemodan
A simple reversible programming language written in Scala
## Requirements
* scala
* sbt

Scala uses a sophisticated project building system called sbt. It will update
itself and Scala to the version, necessary for the project and download all the needed packages
when the script "chemodan" from the root directory is ran for the first time.
## Usage
./chemodan [-r: run backwards] <filename.che>
## Examples
### Run factorial program forwards:
./chemodan ./programs/factorial.che
### Run factorization program backwards:
./chemodan -r ./programs/factorization.che

# simulations

# Building javascript

Requires nix (sorry)
run `nix-build .` (or `nix-build -f.`)
`cp ./result/bin/all.js* .`
`java -jar closure-compiler-*.jar all.js --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=all.js.externs > all.min.js`

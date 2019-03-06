all: \
    pokemon/arcanine.png \
    pokemon/bulbasaur.png \
    pokemon/cubone.png \
    pokemon/drowzee.png \
    pokemon/lapras.png \
    pokemon/metapod.png \
    pokemon/pikachu.png \
    pokemon/snorlax.png \

images/%.dot.png: images/%.dot images/*.h
	cpp -P $< | dot -Tpng -Gdpi=150 -o $@

pokemon/%.png:
	curl -Lo $@ https://img.pokemondb.net/sprites/x-y/normal/$*.png
	mogrify -trim $@

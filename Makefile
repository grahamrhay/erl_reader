PROJECT = erl_reader

DEPS = atomizer seymour
dep_atomizer = git https://github.com/grahamrhay/atomizer master
dep_seymour = git https://github.com/grahamrhay/seymour master

include erlang.mk
